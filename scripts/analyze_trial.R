#!/usr/bin/env Rscript
# ==============================================================================
# 通用分析模块「分析」—— 命令行入口
#
# 用法：
#   Rscript scripts/analyze_trial.R --list
#   Rscript scripts/analyze_trial.R --exp YIELD_20260527153649_1915
#   Rscript scripts/analyze_trial.R --exp YIELD_... --out 分析输出/宿州产比 --zip
#   Rscript scripts/analyze_trial.R --exp YIELD_... --rank 40 --plant 40 --lodging 9,7
#
# 走 book_plot 自带分析管线（run_analysis），与 Shiny 应用内「分析」同一套口径。
# ==============================================================================

suppressPackageStartupMessages({
  library(shiny); library(shinyjs); library(bslib); library(DT)
  library(dplyr); library(ggplot2); library(soyplant)
  library(DBI); library(RSQLite); library(openxlsx)
  library(corrplot); library(fmsb); library(ggrepel); library(htmltools)
  library(jsonlite)
})

# ---- 定位项目根目录（本脚本位于 <root>/scripts/）----
.args_all <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args_all, value = TRUE)
.script_path <- if (length(.file_arg)) sub("^--file=", "", .file_arg[1]) else ""
.project_root <- normalizePath(file.path(dirname(.script_path), ".."), mustWork = FALSE)
setwd(.project_root)

source("shared/helpers.R")
source("shared/db_persistence.R")
source("shared/mod_analysis.R")

# 倒伏等级编码（与 UI 下拉一致）
LODGING_LEVEL_MAP <- c("1" = "1-不倒", "3" = "3-轻倒", "5" = "5-中倒",
                        "7" = "7-重倒", "9" = "9-严重倒")

usage <- function() {
  cat("通用分析模块「分析」\n\n",
      "  Rscript scripts/analyze_trial.R --list\n",
      "  Rscript scripts/analyze_trial.R --exp <试验ID> [--type yield_test|population|line_selection]\n",
      "        [--out <输出目录>] [--db <sqlite路径>] [--zip]\n",
      "        [--rank <晋级位次阈值>] [--plant <分离选单株阈值>] [--lodging 9,7]\n",
      "        [--place <地点>]\n",
      "        [--json] [--json-full] [--json-out <文件>]\n\n",
      "  --rank/--plant 不传时，默认取「参试材料数的前 25%」（不含对照）\n",
      "  --place       只分析该地点（多点试验做单点分析用），位次/增产率按该地点内计算\n",
      "  --json        以 JSON 输出（摘要 + 三类名单）到 stdout，供脚本/大模型消费\n",
      "  --json-full   额外包含全部原始明细表（体积大，建议配合 --json-out）\n",
      "  --json-out F  把 JSON 写到文件 F（隐含 --json）\n",
      "  注：--json 且未指定 --out 时只计算、不落盘，跑得快\n\n", sep = "")
}

parse_args <- function(args) {
  opt <- list(exp = NULL, type = NULL, out = NULL, db = NULL,
              rank = NULL, plant = NULL, lodging = NULL, place = NULL,
              zip = FALSE, list = FALSE,
              json = FALSE, json_full = FALSE, json_out = NULL)
  i <- 1
  while (i <= length(args)) {
    a <- args[i]
    val <- if (i < length(args)) args[i + 1] else NA_character_
    switch(a,
      "--exp"     = { opt$exp     <- val; i <- i + 2 },
      "--type"    = { opt$type    <- val; i <- i + 2 },
      "--out"     = { opt$out     <- val; i <- i + 2 },
      "--db"      = { opt$db      <- val; i <- i + 2 },
      "--rank"    = { opt$rank    <- as.integer(val); i <- i + 2 },
      "--plant"   = { opt$plant   <- as.integer(val); i <- i + 2 },
      "--lodging" = { opt$lodging <- val; i <- i + 2 },
      "--place"   = { opt$place   <- val; i <- i + 2 },
      "--zip"     = { opt$zip     <- TRUE; i <- i + 1 },
      "--list"    = { opt$list    <- TRUE; i <- i + 1 },
      "--json"      = { opt$json      <- TRUE; i <- i + 1 },
      "--json-full" = { opt$json      <- TRUE; opt$json_full <- TRUE; i <- i + 1 },
      "--json-out"  = { opt$json      <- TRUE; opt$json_out  <- val; i <- i + 2 },
      "--help"    = { usage(); quit(status = 0) },
      { cat("未知参数：", a, "\n"); usage(); quit(status = 1) }
    )
  }
  opt
}

opt <- parse_args(commandArgs(trailingOnly = TRUE))
db_path <- if (is.null(opt$db)) defaultDbPath() else opt$db

# ---- 列出可分析试验 ----
if (opt$list) {
  trials <- analysis_list_trials(db_path)
  if (nrow(trials) == 0) {
    cat("没有可分析的试验。\n")
  } else {
    cat(sprintf("%-34s %-6s %-28s %s\n", "试验ID", "类型", "试验名", "记录数"))
    for (i in seq_len(nrow(trials))) {
      cat(sprintf("%-34s %-6s %-28s %d\n", trials$experiment_id[i], trials$type_label[i],
                  substr(trials$experiment_name[i], 1, 26), trials$n[i]))
    }
    cat("\n共 ", nrow(trials), " 个试验。\n", sep = "")
  }
  quit(status = 0)
}

if (is.null(opt$exp)) {
  cat("错误：缺少 --exp <试验ID>（用 --list 查看全部）。\n\n")
  usage()
  quit(status = 1)
}

# ---- 组装筛选参数（与 Shiny 滑块同一套参数名）----
screening_params <- NULL
if (!is.null(opt$rank) || !is.null(opt$plant) || !is.null(opt$lodging)) {
  screening_params <- list()
  if (!is.null(opt$rank))  screening_params$rank_threshold_select <- opt$rank
  if (!is.null(opt$plant)) screening_params$rank_threshold_plant  <- opt$plant
  if (!is.null(opt$lodging)) {
    codes <- trimws(strsplit(opt$lodging, ",")[[1]])
    lv <- unname(LODGING_LEVEL_MAP[codes])
    lv <- lv[!is.na(lv)]
    if (length(lv) > 0) screening_params$exclude_lodging <- lv
  }
}

module_type <- if (is.null(opt$type)) "yield_test" else opt$type
use_json    <- isTRUE(opt$json)
# 纯 JSON 查询默认不落盘（不生成图表，跑得快）；指定了 --out 才写文件
save_artifacts <- !use_json || !is.null(opt$out)

res <- analysis_run(
  experiment_id    = opt$exp,
  module_type      = module_type,
  db_path          = db_path,
  out_dir          = opt$out,
  screening_params = screening_params,
  place            = opt$place,
  zip_output       = opt$zip && save_artifacts,
  save_artifacts   = save_artifacts
)

r <- res$result

# ---- JSON 输出（脚本 / 大模型消费）----
if (use_json) {
  payload <- build_analysis_json(r, experiment_id = opt$exp,
                                  module_type = module_type, full = opt$json_full)
  txt <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE,
                                        na = "null", digits = NA, pretty = 2))
  if (!is.null(opt$json_out)) {
    writeLines(txt, opt$json_out, useBytes = TRUE)
    cat("JSON 已写入：", opt$json_out, "\n", sep = "")
  } else {
    cat(txt, "\n", sep = "")
  }
  quit(status = 0)
}

cat("\n== 分析：", opt$exp, "==\n")
cat("\n-- 试验类型 --\n")
cat("  ", r$trial_info$label, "：", r$trial_info$desc, "\n")

cat("\n-- 提示 --\n")
if (length(r$messages) == 0) cat("   （无）\n") else for (m in r$messages) cat("   -", m, "\n")

cat("\n-- 产量核心统计 --\n")
if (!is.null(r$tables$yield_stats)) print(r$tables$yield_stats) else cat("   （无）\n")

if (!is.null(r$tables$screening_summary)) {
  s <- r$tables$screening_summary
  cat("\n-- 品种筛选 --\n")
  cat(sprintf("   共 %d → 晋级 %d → 分离选单株 %d → 淘汰 %d\n",
              s$total_n, s$promoted_n, s$select_plant_n, s$eliminated_n))
  if (!is.null(s$rank_threshold_select)) {
    cat(sprintf("   参试材料 %d，晋级位次阈值 %d（默认 = 前 25%%）\n",
                if (is.null(s$n_material)) NA else s$n_material,
                s$rank_threshold_select))
  }
}

cat("\n-- 输出 --\n")
cat("   目录：", res$out_dir, "\n", sep = "")
for (f in res$files) cat("   - ", f, "\n", sep = "")
