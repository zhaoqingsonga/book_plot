#!/usr/bin/env Rscript
# ==============================================================================
# 试验报告交付自检 —— 命令行入口
#
# 用法：
#   Rscript scripts/check_trial_report.R <报告.md>
#   Rscript scripts/check_trial_report.R <报告.md> --strict
#   Rscript scripts/check_trial_report.R <报告.md> --json
#
# 打印摘要 + 逐项 ✅/❌，任一失败项即以退出码 1 结束。
# 输出格式与 Python 版 check_report.py 对齐，便于两个版本直接 diff。
#
# 只依赖 base R（不加载 dplyr / ggplot2 / shiny），跑得快、可独立部署。
# ==============================================================================

# ---- 定位项目根目录（本脚本位于 <root>/scripts/），与 analyze_trial.R 同法 ----
.args_all <- commandArgs(trailingOnly = FALSE)
.file_arg <- grep("^--file=", .args_all, value = TRUE)
.script_path <- if (length(.file_arg)) sub("^--file=", "", .file_arg[1]) else ""
.project_root <- normalizePath(file.path(dirname(.script_path), ".."), mustWork = FALSE)
setwd(.project_root)

source(file.path(.project_root, "shared/analysis/report_check.R"))

usage <- function() {
  cat("试验报告交付自检（对照 trial-report-writer skill §七 清单）\n\n",
      "  Rscript scripts/check_trial_report.R <报告.md> [--strict] [--json]\n\n",
      "  --strict  把警告项也当失败（退出码 1）\n",
      "  --json    以 JSON 输出（机器可读）\n", sep = "")
}

parse_args <- function(args) {
  opt <- list(report = NULL, strict = FALSE, json = FALSE)
  i <- 1
  while (i <= length(args)) {
    a <- args[i]
    switch(a,
      "--strict" = { opt$strict <- TRUE; i <- i + 1 },
      "--json"   = { opt$json   <- TRUE; i <- i + 1 },
      "--help"   = { usage(); quit(status = 0) },
      {
        if (startsWith(a, "-")) {
          cat("未知参数：", a, "\n", sep = "")
          usage()
          quit(status = 1)
        }
        if (is.null(opt$report)) {
          opt$report <- a
        } else {
          cat("只接受一份报告，多余参数：", a, "\n", sep = "")
          usage()
          quit(status = 1)
        }
        i <- i + 1
      }
    )
  }
  opt
}

opt <- parse_args(commandArgs(trailingOnly = TRUE))

if (is.null(opt$report)) {
  cat("错误：缺少报告路径。\n\n")
  usage()
  quit(status = 1)
}

if (!file.exists(opt$report)) {
  cat("错误：报告文件不存在 —— ", opt$report, "\n", sep = "")
  quit(status = 1)
}

res <- rc_check_report(opt$report, strict = opt$strict)

ok_rows <- Filter(function(r) isTRUE(r$passed), res$results)
bad_rows <- Filter(function(r) !isTRUE(r$passed) && identical(r$severity, "error"), res$results)
warn_rows <- Filter(function(r) !isTRUE(r$passed) && identical(r$severity, "warn"), res$results)

# ---- JSON 输出（优先 jsonlite，缺失时退回内置最小实现）----
rc_json_escape <- function(s) {
  s <- gsub("\\", "\\\\", s, fixed = TRUE)
  s <- gsub("\"", "\\\"", s, fixed = TRUE)
  s <- gsub("\n", "\\n", s, fixed = TRUE)
  s <- gsub("\r", "\\r", s, fixed = TRUE)
  s <- gsub("\t", "\\t", s, fixed = TRUE)
  paste0("\"", s, "\"")
}

rc_row_json <- function(r) {
  paste0("{\"label\":", rc_json_escape(r$label),
         ",\"passed\":", if (isTRUE(r$passed)) "true" else "false",
         ",\"severity\":", rc_json_escape(r$severity),
         ",\"detail\":", rc_json_escape(r$detail), "}")
}

if (opt$json) {
  payload <- list(
    report = res$path,
    summary = res$summary,
    n_pass = res$n_pass, n_fail = res$n_fail, n_warn = res$n_warn,
    n_total = res$n_total, ok = res$ok, strict = opt$strict,
    results = res$results
  )
  if (requireNamespace("jsonlite", quietly = TRUE)) {
    cat(as.character(jsonlite::toJSON(payload, auto_unbox = TRUE, na = "null",
                                      digits = NA, pretty = 2)), "\n", sep = "")
  } else {
    rows <- paste(vapply(payload$results, rc_row_json, character(1)), collapse = ",\n    ")
    cat("{\n  \"report\": ", rc_json_escape(res$path), ",\n", sep = "")
    cat("  \"summary\": {\"chars\": ", res$summary$chars,
        ", \"lines\": ", res$summary$lines,
        ", \"n_fig\": ", res$summary$n_fig,
        ", \"n_tab\": ", res$summary$n_tab,
        ", \"n_img\": ", res$summary$n_img, "},\n", sep = "")
    cat("  \"n_pass\": ", res$n_pass, ", \"n_fail\": ", res$n_fail,
        ", \"n_warn\": ", res$n_warn, ", \"n_total\": ", res$n_total,
        ", \"ok\": ", if (isTRUE(res$ok)) "true" else "false", ",\n", sep = "")
    cat("  \"results\": [\n    ", rows, "\n  ]\n}\n", sep = "")
  }
  quit(status = if (isTRUE(res$ok)) 0L else 1L)
}

# ---- 文本输出（与 Python 版 check_report.py 的版式对齐）----
cat(sprintf("报告：%s\n", res$path))
cat(sprintf("字数 %d，行数 %d\n", res$summary$chars, res$summary$lines))
cat(sprintf("图 %d 张，表 %d 张，图片引用 %d 处\n",
            res$summary$n_fig, res$summary$n_tab, res$summary$n_img))
cat("\n")

for (x in ok_rows) {
  cat(sprintf("  ✅ %s%s\n", x$label,
              if (nzchar(x$detail)) paste0(" —— ", x$detail) else ""))
}
cat("\n")

if (length(bad_rows) > 0L) {
  for (x in bad_rows) {
    cat(sprintf("  ❌ %s%s\n", x$label,
                if (nzchar(x$detail)) paste0(" —— ", x$detail) else ""))
  }
}
if (length(warn_rows) > 0L) {
  cat("\n")
  for (x in warn_rows) {
    cat(sprintf("  %s %s%s\n", if (opt$strict) "❌" else "⚠️", x$label,
                if (nzchar(x$detail)) paste0(" —— ", x$detail) else ""))
  }
}

if (length(bad_rows) > 0L || (opt$strict && length(warn_rows) > 0L)) {
  cat(sprintf("\n通过 %d / %d\n", res$n_pass, res$n_total))
  quit(status = 1)
}
cat(sprintf("✅ 全部通过（%d 项）\n", res$n_pass))
quit(status = 0)
