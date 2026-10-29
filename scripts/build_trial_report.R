#!/usr/bin/env Rscript
# ==============================================================================
# 试验报告生成器 CLI
# ------------------------------------------------------------------------------
#   Rscript scripts/build_trial_report.R --list
#   Rscript scripts/build_trial_report.R --trial N-4.2 --rank 100
#   Rscript scripts/build_trial_report.R --trial N-4.2 --preflight-only
#   Rscript scripts/build_trial_report.R --trial N-4.2 --skeleton-only
#   Rscript scripts/build_trial_report.R --trial N-4.2 --to-vault --vault-root <dir>
#
# 铁律：全程在进程内吃 result$tables（不落 CSV 往返）；叙述.md 只创建不覆盖；
#       vault 目标非空默认拒绝（需 --force）。
# ==============================================================================

suppressPackageStartupMessages({
  library(yaml); library(jsonlite)
})

.args_all <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args_all, value = TRUE)
.script_path <- if (length(.file_arg)) sub("^--file=", "", .file_arg[1]) else ""
.project_root <- normalizePath(file.path(dirname(.script_path), ".."), mustWork = FALSE)
setwd(.project_root)

.args <- commandArgs(trailingOnly = TRUE)
.get_opt <- function(name, default = NULL) {
  i <- which(.args == name)
  if (length(i) && i[1] < length(.args)) .args[i[1] + 1] else default
}
.has_opt <- function(name) any(.args == name)

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

opt_list        <- .has_opt("--list")
opt_trial       <- .get_opt("--trial")
opt_config      <- .get_opt("--config")
opt_rank        <- as.integer(.get_opt("--rank", "100"))
opt_plant       <- as.integer(.get_opt("--plant", "100"))
opt_lodging     <- .get_opt("--lodging", "9,7")
opt_narrative   <- .get_opt("--narrative")
opt_module      <- .get_opt("--module-type", "yield_test")
opt_preflight   <- .has_opt("--preflight-only")
opt_skeleton    <- .has_opt("--skeleton-only")
opt_to_vault    <- .has_opt("--to-vault")
opt_vault_root  <- .get_opt("--vault-root")
opt_force       <- .has_opt("--force")

CONFIG_DIR <- file.path("报告配置", "trials")
.rpt_info <- function(...) cat(..., "\n", sep = "")

# ---- --list ---------------------------------------------------------------
if (opt_list) {
  files <- sort(list.files(CONFIG_DIR, pattern = "\\.ya?ml$", full.names = TRUE))
  if (!length(files)) {
    .rpt_info("没有可用试验配置（", CONFIG_DIR, " 为空）")
  } else {
    .rpt_info("可用试验：")
    for (f in files) {
      y <- yaml::read_yaml(f)
      .rpt_info(sprintf("  %-10s %-24s %s / %s / %s",
                        y$trial_no %||% "?", y$experiment_id %||% "?",
                        y$地点 %||% "?", y$year %||% "?", y$体裁 %||% "?"))
    }
  }
  quit(save = "no", status = 0)
}

if (is.null(opt_trial)) {
  .rpt_info("用法：Rscript scripts/build_trial_report.R --trial <编号> [--rank 100] ...")
  quit(save = "no", status = 1)
}

# ---- 配置 ------------------------------------------------------------------
cfg_path <- if (!is.null(opt_config)) opt_config else
  file.path(CONFIG_DIR, paste0(opt_trial, ".yaml"))
if (!file.exists(cfg_path)) stop("找不到试验配置：", cfg_path)
cfg <- yaml::read_yaml(cfg_path)

experiment_id <- cfg$experiment_id
if (is.null(experiment_id)) stop("配置缺少 experiment_id：", cfg_path)
meta <- cfg
meta$template_id <- "primary"
meta$template_dir <- "shared/analysis/report_templates"

# ---- 载入管线 --------------------------------------------------------------
suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(soyplant)
  library(DBI); library(RSQLite); library(openxlsx)
  library(corrplot); library(fmsb); library(ggrepel); library(htmltools)
})
source("shared/helpers.R")
source("shared/db_persistence.R")
source("shared/mod_analysis.R")
source("shared/analysis/report_rules.R")
source("shared/analysis/report_stats.R")
source("shared/analysis/report_preflight.R")
source("shared/analysis/report_blocks.R")
source("shared/analysis/report_assemble.R")

lodging_labels <- c("9" = "9-严重倒", "7" = "7-重倒", "5" = "5-中倒", "3" = "3-轻倒")
lodging_codes <- trimws(strsplit(opt_lodging, ",", fixed = TRUE)[[1]])
exclude_lodging <- unname(lodging_labels[lodging_codes])
exclude_lodging <- exclude_lodging[!is.na(exclude_lodging)]
if (!length(exclude_lodging)) exclude_lodging <- c("9-严重倒", "7-重倒")

screening_params <- list(rank_threshold_select = opt_rank,
                          rank_threshold_plant = opt_plant,
                          exclude_lodging = exclude_lodging)

out_root <- file.path("分析输出", paste0(experiment_id, "_", opt_module))
rep_dir  <- file.path(out_root, "报告")
dir.create(rep_dir, recursive = TRUE, showWarnings = FALSE)

.rpt_info("▶ 跑管线：", experiment_id, "（rank=", opt_rank, " plant=", opt_plant,
          " 排除倒伏=", paste(exclude_lodging, collapse = "/"), "）")
res <- analysis_run(experiment_id, module_type = opt_module, out_dir = out_root,
                    screening_params = screening_params,
                    save_artifacts = TRUE, zip_output = FALSE)
result <- res$result
tables <- result$tables          # ← 进程内吃表，不做 CSV 往返

ctx <- compute_report_stats(result, meta = meta, parent_threshold = 60L)
pf  <- report_preflight(result, ctx = ctx, meta = meta)
.rpt_info("  preflight ok =", pf$ok, " 触发规则", nrow(pf$triggered), "条")

# ---- 事实卡 ----------------------------------------------------------------
facts_card <- function(meta, ctx, pf) {
  f <- ctx
  lines <- c(
    paste0("# 事实卡 —— ", meta$trial_no, "（", meta$experiment_id, "）"),
    "",
    paste0("- 试验编号：", meta$trial_no),
    paste0("- 试验类型：", meta$体裁),
    paste0("- 地点 / 年份：", meta$地点, " / ", meta$year),
    paste0("- 小区数：", f$n_plots, "（材料 ", f$n_material, " + 对照 ", f$n_ck, "）"),
    paste0("- 有产量小区：", f$n_with_yield),
    paste0("- 晋级 / 分离选单株 / 淘汰：", f$promoted_n, " / ", f$select_plant_n,
           " / ", f$eliminated_n),
    paste0("- 晋级位次阈值：", f$screening_threshold, "；亲本口径阈值：", f$parent_threshold),
    paste0("- preflight ok：", pf$ok),
    "",
    "## 触发规则",
    "")
  if (nrow(pf$triggered)) {
    lines <- c(lines, paste0("- [", pf$triggered$severity, "] ", pf$triggered$rule_id,
                              "：", pf$triggered$title), "")
  } else lines <- c(lines, "- 无", "")
  lines <- c(lines, "## 图表分诊", "")
  tr <- pf$triage
  lines <- c(lines, paste0("- ", tr$plot_key, " → ", tr$verdict,
                            ifelse(nzchar(tr$reason), paste0("（", tr$reason, "）"), "")))
  paste(lines, collapse = "\n")
}

# ---- 报告装配 --------------------------------------------------------------
narrative_path <- opt_narrative
if (is.null(narrative_path)) narrative_path <- file.path(rep_dir, "叙述.md")
narrative <- rpt_parse_narrative(narrative_path)

# 叙述侧车：只创建，永不覆盖
if (!file.exists(narrative_path)) {
  tmpl_files <- sort(list.files(file.path(meta$template_dir, "sections"),
                                pattern = "\\.md\\.tmpl$", full.names = TRUE))
  blanks <- character(0)
  for (f in tmpl_files) {
    tx <- paste(readLines(f, warn = FALSE), collapse = "\n")
    m <- regmatches(tx, gregexpr("<<[^<>\\n]+>>", tx, perl = TRUE))[[1]]
    if (length(m) == 1 && m[1] == -1) m <- character(0)
    if (length(m)) blanks <- c(blanks, trimws(sub("^<<", "", sub(">>$", "", m))))
  }
  blanks <- unique(blanks)
  dir.create(dirname(narrative_path), recursive = TRUE, showWarnings = FALSE)
  con <- file(narrative_path, open = "wt", encoding = "UTF-8")
  hdr <- c(
    paste0("# 叙述侧车 —— ", cfg$trial_no),
    "",
    "> 本文件是试验报告的**唯一人工输入**。每个 `## <名称>` 段落对应报告里的一个留白。",
    "> 生成器只读不写本文件；同名段落缺省时报告里保留 `<<名称>>` 留白。",
    "",
    paste0("待写段落（", length(blanks), " 个）：",
           if (length(blanks)) paste0("、", paste(blanks, collapse = "、")) else ""),
    "",
    "")
  writeLines(hdr, con)
  for (b in blanks) {
    writeLines(c(paste0("## ", b), ""), con)
  }
  close(con)
  .rpt_info("  已创建叙述侧车：", narrative_path)
}

img_src_dir  <- file.path(out_root, "图表")
img_dir_name <- "试验报告_images"
img_dst_dir  <- file.path(rep_dir, img_dir_name)
dir.create(img_dst_dir, recursive = TRUE, showWarnings = FALSE)
if (dir.exists(img_src_dir)) {
  pngs <- list.files(img_src_dir, pattern = "\\.png$", full.names = TRUE)
  if (length(pngs)) file.copy(pngs, file.path(img_dst_dir, basename(pngs)), overwrite = TRUE)
}
meta$images_dir <- img_dir_name
meta$page_name  <- "试验报告"

if (opt_preflight) {
  writeLines(facts_card(meta, ctx, pf), file.path(rep_dir, "事实卡.md"), useBytes = TRUE)
  .rpt_info("✔ --preflight-only：只写事实卡 → ", file.path(rep_dir, "事实卡.md"))
  quit(save = "no", status = 0)
}

asm <- report_assemble(result, pf, meta, narrative = narrative,
                        template_dir = meta$template_dir,
                        template_id = meta$template_id,
                        skeleton = opt_skeleton)

report_md <- file.path(rep_dir, "试验报告.md")
writeLines(asm$md, report_md, useBytes = TRUE)
writeLines(facts_card(meta, ctx, pf), file.path(rep_dir, "事实卡.md"), useBytes = TRUE)

# ---- 清理被剔除图的孤儿 PNG（正文未引用）----
if (dir.exists(img_dst_dir)) {
  all_pngs <- list.files(img_dst_dir, pattern = "\\.png$")
  used_pngs <- unique(regmatches(asm$md,
    gregexpr(paste0(img_dir_name, "/[^)\\n]+\\.png"), asm$md, perl = TRUE))[[1]])
  used_pngs <- sub(paste0(img_dir_name, "/"), "", used_pngs, fixed = TRUE)
  orphan <- setdiff(all_pngs, used_pngs)
  if (length(orphan)) {
    file.remove(file.path(img_dst_dir, orphan))
    .rpt_info("  已清理孤儿 PNG：", paste(orphan, collapse = "、"))
  }
}

jsonlite::write_json(
  list(trial_no = cfg$trial_no, experiment_id = experiment_id,
       registry = asm$registry, missing_narrative = asm$missing_narrative,
       unfinished = asm$slots$unfinished,
       facts = lapply(asm$slots$facts, function(v)
         if (is.numeric(v)) v else if (is.null(v)) NULL else as.character(v))),
  file.path(rep_dir, "slots.json"), auto_unbox = TRUE, pretty = TRUE)

yaml::write_yaml(
  list(trial_no = cfg$trial_no, experiment_id = experiment_id,
       module_type = opt_module, stage_no = cfg$stage_no,
       generated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
       screening = list(rank_threshold_select = opt_rank,
                        rank_threshold_plant = opt_plant,
                        exclude_lodging = exclude_lodging),
       files = c("试验报告.md", "叙述.md", "事实卡.md", "slots.json", "manifest.yml"),
       registry = asm$registry,
       missing_narrative = asm$missing_narrative,
       unfinished = asm$slots$unfinished,
       preflight_ok = pf$ok),
  file.path(rep_dir, "manifest.yml"))

.rpt_info("✔ 报告已生成：", report_md)
.rpt_info("  表/图块数：", length(asm$registry),
          "  留白（missing_narrative）：", length(asm$missing_narrative))
if (length(asm$slots$unfinished))
  .rpt_info("  未完成块：", paste(asm$slots$unfinished, collapse = ", "))

# ---- --to-vault ------------------------------------------------------------
if (opt_to_vault) {
  vault_root <- if (!is.null(opt_vault_root)) opt_vault_root else
    file.path(path.expand("~"), "GitHub", "Obsidian", "02wiki", "02-项目", "2026年试验报告")
  dir.create(vault_root, recursive = TRUE, showWarnings = FALSE)
  page_name <- paste0(cfg$trial_no, "-", cfg$体裁, "-", cfg$地点, "-", cfg$year, "年试验报告")
  target <- file.path(vault_root, paste0(page_name, ".md"))
  if (file.exists(target) && file.info(target)$size > 0 && !opt_force) {
    stop("目标已存在且非空，拒绝覆盖（加 --force 强制）：", target)
  }
  vault_img_dir <- file.path(vault_root, paste0(page_name, "_images"))
  dir.create(vault_img_dir, recursive = TRUE, showWarnings = FALSE)
  if (dir.exists(img_dst_dir)) {
    file.copy(list.files(img_dst_dir, pattern = "\\.png$", full.names = TRUE),
              vault_img_dir, overwrite = TRUE)
  }
  md_vault <- gsub(paste0("(", img_dir_name, "/"), paste0("(", page_name, "_images/"),
                   asm$md, fixed = TRUE)
  writeLines(md_vault, target, useBytes = TRUE)
  .rpt_info("✔ 已写入 vault：", target)
}
