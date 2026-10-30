#!/usr/bin/env Rscript
# ==============================================================================
# 试验报告生成器 CLI
# ------------------------------------------------------------------------------
#   Rscript scripts/build_trial_report.R --list
#   Rscript scripts/build_trial_report.R --trial N-4.2 --rank 100
#   Rscript scripts/build_trial_report.R --trial N-4.2 --preflight-only
#   Rscript scripts/build_trial_report.R --trial N-4.2 --skeleton-only
#   Rscript scripts/build_trial_report.R --trial N-4.2 --to-vault --vault-root <dir>
#   Rscript scripts/build_trial_report.R --trial N-5.2 --place 安徽宿州  # 多点试验只出单点
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
opt_place       <- .get_opt("--place")
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
# 编排模板：配置项 template 可指定（primary = 初级产比 / multi_site = 多点鉴定）；
# 缺省 primary，不影响既有报告的编号与结构。
meta$template_id <- if (!is.null(cfg$template) && nzchar(cfg$template)) cfg$template else "primary"
meta$template_dir <- "shared/analysis/report_templates"
# --trial 实际接受的是配置文件名（如 OT-北组 / N-4.2），报告里的命令行示例要用这个
meta$trial_key <- sub("\\.yaml$", "", basename(cfg_path))
if (!file.exists(file.path(meta$template_dir, paste0(meta$template_id, ".yml")))) {
  stop("找不到编排模板：", file.path(meta$template_dir, paste0(meta$template_id, ".yml")))
}

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
                    place = opt_place %||% cfg$place,
                    save_artifacts = TRUE, zip_output = FALSE)
result <- res$result
tables <- result$tables          # ← 进程内吃表，不做 CSV 往返

# ---- 多点鉴定筛选口径（覆盖单点口径）---------------------------------------
# ⚠️ 单点筛选用在多点试验上会严重失真：
#    analyze_screening() 按「较临近/较平均对照位次」筛，只要**任意一个点**
#    进了阈值就算晋级 ⇒ 北组 56 份材料全部晋级（100%），毫无区分度。
#
# 多点鉴定的真实口径（用户 2026-10-29 确认）：
#    ① 每个点单独排名，取前 25%（该点材料数的 25%）
#    ② 必须在 ≥80% 的点都进入前 25% 才算晋级
#    ⇒ 北组 56 份 → 2 份（菏育6号、中黄122）
#
# 由配置项 screening.mode: multi_site 触发；缺省仍走单点口径（不影响既有报告）。
scr_mode <- cfg$screening$mode %||% "single_site"
if (identical(scr_mode, "multi_site")) {
  if (!exists("analyze_multi_site_screening", mode = "function")) {
    source("shared/analysis/multi_site_screening.R")
  }
  ms <- analyze_multi_site_screening(
    tables$export_data,
    pct          = cfg$screening$pct %||% 0.25,
    min_hit_rate = cfg$screening$min_hit_rate %||% 0.8
  )
  .rpt_info(sprintf("  多点鉴定筛选：每点前 %.0f%%、需 ≥%.0f%% 的点入围 ⇒ 晋级 %d / %d 份（%.1f%%）",
                    (cfg$screening$pct %||% 0.25) * 100,
                    (cfg$screening$min_hit_rate %||% 0.8) * 100,
                    ms$summary$n_promoted, ms$summary$n_material,
                    ms$summary$promoted_rate * 100))
  tables$promoted          <- ms$promoted
  tables$multi_site_detail <- ms$detail
  tables$multi_site_summary <- ms$summary
  # ⚠️ screening_summary 也必须同步覆盖 —— compute_report_stats() 的 promoted_n
  #    读的是它，不是 tables$promoted。只换 promoted 会导致事实卡仍显示旧数
  #    （实测：promoted 已是 2 份，事实卡却写 470）。
  tables$screening_summary <- list(
    total_n   = nrow(tables$export_data),
    n_material = ms$summary$n_material,
    rank_threshold_select = NA_integer_,
    rank_threshold_plant  = NA_integer_,
    promoted_n       = ms$summary$n_promoted,
    segregated_n     = sum(!is.na(tables$promoted$性状分离) &
                           nzchar(as.character(tables$promoted$性状分离)) &
                           !as.character(tables$promoted$性状分离) %in% c("NA", "否")),
    select_plant_n   = 0L,
    eliminated_n     = ms$summary$n_material - ms$summary$n_promoted,
    breakdown = list(rank_fail = 0L, separated = 0L, lodging = 0L),
    mode = "multi_site",
    pct = ms$summary$pct,
    min_hit_rate = ms$summary$min_hit_rate,
    need_hits = ms$summary$need_hits
  )
  # 淘汰表也重算：多点口径下「未晋级」= 非晋级材料
  tables$eliminated <- tables$export_data[
    !tables$export_data$name %in% ms$detail$材料[ms$detail$晋级] &
    !(suppressWarnings(as.integer(as.character(tables$export_data$is_ck))) %in% 1), ,
    drop = FALSE]
  result$tables <- tables
  # ⚠️ Excel 必须按多点口径重出 ——
  #    管线在 analysis_run(save_artifacts=TRUE) 里已经按**单点**位次口径写过一份
  #    （北组晋级 470 份），多点覆盖之后不重写，Excel 的晋级清单就与报告打架
  #    （报告说晋级 4 份，Excel 列 470 份）＝ 口径分诊坑 #25 的 Excel 版。
  tryCatch({
    build_analysis_excel(result, file.path(out_root, "分析报告.xlsx"))
    .rpt_info("  已按多点口径重出 Excel：", file.path(out_root, "分析报告.xlsx"))
  }, error = function(e) {
    message("Excel 重出失败：", e$message)
  })
}

ctx <- compute_report_stats(result, meta = meta, parent_threshold = 60L)

# ---- 叙述侧车（须在 preflight 之前解析：规则 ㉑ 要拿它比对数字）--------
narrative_path <- opt_narrative
if (is.null(narrative_path)) narrative_path <- file.path(rep_dir, "叙述.md")
narrative <- rpt_parse_narrative(narrative_path)

# facts 只依赖 result/ctx/meta，可以提前算出来给 preflight 用
facts <- rpt_derive_facts(result, ctx, meta)

pf  <- report_preflight(result, ctx = ctx, meta = meta,
                        narrative = narrative, facts = facts)
.rpt_info("  preflight ok =", pf$ok, " 触发规则", nrow(pf$triggered), "条")
if (!is.null(pf$facts$narrative_mismatch) && pf$facts$narrative_mismatch > 0) {
  .rpt_info("  ⚠️ 叙述侧车数字与管线不符：", pf$facts$narrative_mismatch, "处")
}

# ---- 事实卡 ----------------------------------------------------------------
facts_card <- function(meta, ctx, pf) {
  # pf$facts 是 preflight 跑完后的 ctx（含分诊结果与叙述数字校验结果），
  # 比原始 ctx 全 ⇒ 用它，缺了才退回 ctx
  f <- if (!is.null(pf$facts)) pf$facts else ctx
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
    paste0("- 叙述侧车数字校验：",
           if (is.null(f$narrative_mismatch)) "未执行（无叙述侧车）"
           else if (f$narrative_mismatch > 0)
             paste0("不通过（", f$narrative_mismatch, " 处与管线不符）")
           else "通过（计数量与管线一致）"),
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
# （narrative / narrative_path 已在上面 preflight 之前解析）

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
if (length(asm$slots$residual_slots))
  .rpt_info("  ⚠️ 未填充的事实槽位（报告里会残留 {{...}}）：",
            paste(asm$slots$residual_slots, collapse = ", "))

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
