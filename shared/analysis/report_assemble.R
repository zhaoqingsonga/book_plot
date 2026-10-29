# ==============================================================================
# 试验报告装配层（report_assemble.R）
# ------------------------------------------------------------------------------
# 职责：
#   ① 读 primary.yml（编排唯一真相源）→ 分配表/图编号（表、图各自独立计数）
#   ② 渲染各节模板（.md.tmpl）→ 填充 {{事实}} / {{tbl_x}} / {{fig_x}} /
#      {{ref_tbl_x}} / {{ref_fig_x}}
#   ③ 注入叙述侧车（叙述.md）→ 缺项保留 <<名称>> 并登记 missing_narrative
#
# 铁律：装配层不产生任何数字，只搬运块与事实。
#
# 调用方：scripts/build_trial_report.R
# 依赖：shared/analysis/report_blocks.R、shared/analysis/report_rules.R
# ==============================================================================

#' 读 primary.yml
rpt_load_template <- function(template_dir, template_id = "primary") {
  path <- file.path(template_dir, paste0(template_id, ".yml"))
  if (!file.exists(path)) stop("找不到模板：", path)
  yaml::read_yaml(path)
}

#' 读节模板（按文件名排序）
rpt_load_sections <- function(template_dir, template_id = "primary") {
  dir <- file.path(template_dir, "sections")
  files <- sort(list.files(dir, pattern = "\\.md\\.tmpl$", full.names = TRUE))
  lapply(files, function(f) list(file = f, name = sub("\\.md\\.tmpl$", "", basename(f)),
                                 text = paste(readLines(f, warn = FALSE), collapse = "\n")))
}

#' 解析叙述侧车：## <名称> 段落 → named list
rpt_parse_narrative <- function(path) {
  out <- list()
  if (is.null(path) || !file.exists(path)) return(out)
  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  cur <- NULL; buf <- character(0)
  flush <- function() {
    if (!is.null(cur)) {
      txt <- sub("^\n+", "", sub("\n+$", "", paste(buf, collapse = "\n")))
      body <- gsub("<!--[^>]*-->", "", txt, perl = TRUE)
      if (nzchar(trimws(body))) out[[cur]] <<- body
    }
  }
  for (ln in lines) {
    m <- regmatches(ln, regexec("^##\\s+(.+?)\\s*$", ln))[[1]]
    if (length(m) == 2L) { flush(); cur <- m[2]; buf <- character(0) }
    else if (!is.null(cur)) buf <- c(buf, ln)
  }
  flush()
  out
}

#' 事实格式化：整数值不带小数，其余按名定小数位
rpt_fact_digits <- function(nm) {
  one <- c("below_ck_pct", "growth_mean", "growth_median", "growth_min",
           "growth_max", "mat_growth_mean", "ck_growth_mean")
  if (nm %in% one) 1L else 2L
}

rpt_fact_fmt <- function(nm, x) {
  if (is.null(x) || length(x) == 0) return("")
  if (is.data.frame(x)) return(paste(capture.output(print(x)), collapse = " "))
  if (is.list(x)) return(paste(as.character(x), collapse = "、"))
  if (is.character(x)) return(x)
  if (is.numeric(x)) {
    if (is.na(x)) return("—")
    d <- rpt_fact_digits(nm)
    if (d == 2 && x != 0 && round(x, 2) == 0)
      d <- 4
    s <- formatC(x, format = "f", digits = d)
    s <- sub("0+$", "", s); s <- sub("\\.$", "", s)
    return(s)
  }
  as.character(x)
}

#' 替换 {{slot}}（标量事实）
rpt_fill_facts <- function(txt, facts) {
  pat <- "\\{\\{([A-Za-z0-9_.一-龥]+)\\}\\}"
  m <- regmatches(txt, gregexpr(pat, txt, perl = TRUE))[[1]]
  if (!length(m)) return(txt)
  for (ph in unique(m)) {
    nm <- sub("^\\{\\{", "", sub("\\}\\}$", "", ph))
    val <- if (!is.null(facts[[nm]])) rpt_fact_fmt(nm, facts[[nm]]) else ""
    txt <- gsub(ph, val, txt, fixed = TRUE)
  }
  txt
}

#' 替换叙述留白 <<名称>>
rpt_fill_narrative <- function(txt, narrative) {
  m <- regmatches(txt, gregexpr("<<[^<>\\n]+>>", txt, perl = TRUE))[[1]]
  if (!length(m)) return(txt)
  for (ph in unique(m)) {
    nm <- trimws(sub("^<<", "", sub(">>$", "", ph)))
    if (!is.null(narrative[[nm]])) txt <- gsub(ph, narrative[[nm]], txt, fixed = TRUE)
  }
  txt
}

#' 由试验元信息推导 frontmatter / H1 标题 / 前置信息块所需字段
rpt_header_facts <- function(meta, facts) {
  `%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
  trial_no <- facts$trial_no %||% meta$trial_no %||% "?"
  genre    <- facts$体裁 %||% meta$体裁 %||% ""
  place    <- facts$place %||% meta$地点 %||% meta$place %||% ""
  year     <- facts$year %||% meta$year %||% ""
  expid    <- facts$experiment_id %||% meta$experiment_id %||% ""
  parts <- strsplit(as.character(genre), "-", fixed = TRUE)[[1]]
  cat   <- if (length(parts) >= 1) parts[1] else ""
  stage <- if (length(parts) >= 2) paste(parts[-1], collapse = "-") else ""
  date8 <- sub("^(\\d{8}).*", "\\1", sub("^YIELD_", "", as.character(expid)))
  sop_no <- sub("^[A-Za-z]+-", "", as.character(trial_no))
  list(
    page_title = paste0(trial_no, "-", genre, "-", place, "-", year, "年试验报告"),
    doc_title  = paste0(cat, "-大豆", stage, "试验总结（", trial_no, " ", place, " ", year, "）"),
    tags_list  = paste0("[试验报告, ", cat, ", ", year, ", ", place, ", ", stage, ", ", trial_no, "]"),
    sources_list = paste0("[", facts$sop_source %||% meta$sop_source %||% "",
                          ", book_plot:data/field_book.sqlite]"),
    trial_full_id = paste0(trial_no, "-", genre, "-", place, "-", date8),
    trial_type_desc = paste0(stage, "试验（", cat, " · SOP ", sop_no, "）"),
    created = facts$created %||% meta$created %||% format(Sys.Date(), "%Y-%m-%d"),
    updated = facts$updated %||% meta$updated %||% format(Sys.Date(), "%Y-%m-%d"),
    place = place, expid = expid,
    n_reps = facts$n_reps %||% meta$design$n_reps %||% "",
    n_blocks = facts$blocks %||% meta$design$blocks %||% ""
  )
}

#' 生成「摘要：」段（数字全部来自 facts，不下育种判断）
rpt_abstract <- function(facts) {
  F. <- function(x, d = 2) {
    if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) return("—")
    rpt_fmt(as.numeric(x), d)
  }
  ia <- facts$inc_avg; il <- facts$inc_lin
  s <- paste0(
    "本试验选用 ", facts$n_material, " 份参试材料（来源于 ", facts$n_combos,
    " 个杂交组合）、", facts$n_ck, " 个对照小区（对照品种", facts$ck_variety, "），共 ",
    facts$n_plots, " 个小区，采用", facts$arrangement, "、", facts$n_reps, " 次重复。",
    "结果显示：", facts$n_with_yield, " 个有产量小区平均亩产 ", F.(facts$mean_all),
    " kg、中位数 ", F.(facts$median_all), " kg、变异系数 ", F.(facts$cv_all), "%；",
    "管线按 1.5×IQR 剔除离群对照小区 ", facts$outlier_ck_ids, " 后，",
    facts$n_ck_valid, " 个有效对照基准均值 ", F.(facts$ck_mean), " kg/亩。",
    facts$n_material, " 份材料中 ", facts$n_mat_yield, " 份有实收产量、均值 ",
    F.(facts$mat_mean), " kg/亩，", F.(facts$below_ck_pct, 1), "%（", facts$below_ck_n,
    " 份）低于对照基准；较平均对照增产 ", ia$gt0, " 份（超 5% ", ia$gt5, " 份、超 8% ",
    ia$gt8, " 份、超 10% ", ia$gt10, " 份）；较临近对照增产 ", il$gt0, " 份（超 5% ",
    il$gt5, " 份、超 8% ", il$gt8, " 份、超 10% ", il$gt10, " 份）。",
    "生育期 ", facts$growth_min, "–", facts$growth_max, " 天、均值 ", F.(facts$growth_mean, 1),
    " 天，与对照（", F.(facts$ck_growth_mean, 1), " 天）基本适配；生育期与亩产呈弱正相关（材料层 r = ",
    F.(facts$corr_growth_yield), "，n = ", facts$corr_growth_yield_n, "）。")
  if (!is.null(facts$trait_text) && nzchar(facts$trait_text))
    s <- paste0(s, "质量性状分布集中：", facts$trait_text, "。")
  pr <- if (is.numeric(facts$promoted_n) && is.numeric(facts$n_material) && facts$n_material > 0)
    F.(facts$promoted_n / facts$n_material * 100, 1) else "—"
  s <- paste0(s, "经筛选确定晋级材料 ", facts$promoted_n, " 份（材料口径晋级率 ", pr, "%），其中 ",
              facts$promoted_no_sep_n, " 份未记性状分离、", facts$promoted_sep_n,
              " 份记有性状分离需先提纯。")
  if (!is.null(facts$cross_top_text) && nzchar(facts$cross_top_text))
    s <- paste0(s, facts$cross_top_text)
  paste0("**摘要：**", s)
}

#' 生成「## 关联」段（wikilink 相对 vault 报告页）
rpt_related_section <- function(meta, facts) {
  has <- function(x) !is.null(x) && length(x) >= 1L && nzchar(as.character(x[1]))
  year <- if (has(meta$year)) meta$year else facts$year
  links <- character(0)
  sop <- if (has(meta$sop_source)) meta$sop_source else facts$sop_source
  if (has(sop)) links <- c(links, paste0("[[", sub("\\.md$", "", sop), "]]"))
  fn <- if (has(meta$field_notes_md)) meta$field_notes_md else NULL
  if (has(fn)) {
    fnx <- path.expand(fn)
    if (file.exists(fnx)) {
      txt <- paste(readLines(fnx, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      wl <- unique(regmatches(txt, gregexpr("\\[\\[[^]|#\\n]+", txt, perl = TRUE))[[1]])
      wl <- trimws(sub("^\\[\\[", "", wl))
      wl <- wl[nzchar(wl) & !grepl("试验报告|时间线", wl)]
      if (length(wl) && has(year))
        links <- c(links, paste0("[[../", year, "年田间调查/A. 田间观察/", wl, "]]"))
    }
    rel <- sub("^.*02-项目/", "", fnx)
    if (nzchar(rel) && rel != fnx)
      links <- c(links, paste0("[[../", sub("\\.md$", "", rel), "]]（全部轮次评价时间线）"))
  }
  if (has(year)) links <- c(links, paste0("[[../收获计划/", year, "-收获计划]]"))
  links <- unique(links)
  paste0("## 关联\n\n", paste0("- ", links, collapse = "\n"),
         "\n\n> 本报告数据由 book_plot 田间记录本导出（`yield_test_field_records`）；§七 全部数字由 book_plot 自带分析管线 `run_analysis()` 产出，与应用内「分析」标签页、命令行 `scripts/analyze_trial.R` 同源同口径，可直接复算。")
}

# ==============================================================================
# 主装配函数
# ==============================================================================
#' @param result   analysis_run() 的 result
#' @param preflight report_preflight() 的结果
#' @param meta      试验元信息（含 trial_no/place/year/design 等）
#' @param narrative 叙述侧车解析结果（named list）或 NULL
#' @return list(md, registry, slots, sections, missing_narrative)
report_assemble <- function(result, preflight, meta,
                             narrative = NULL,
                             template_dir = "shared/analysis/report_templates",
                             template_id = "primary",
                             skeleton = FALSE) {

  tpl  <- rpt_load_template(template_dir, template_id)
  secs <- rpt_load_sections(template_dir, template_id)
  ctx  <- preflight$facts
  facts <- rpt_derive_facts(result, ctx, meta)
  facts$a4_bundle <- rpt_field_notes_bundle(result, meta)
  sp <- rpt_split_promoted(result$tables$promoted)
  facts$promoted_no_sep_n <- nrow(sp$no_sep)
  facts$promoted_sep_n <- nrow(sp$with_sep)

  # ---- 追加：判定层结论句 / 组合与亲本速览 --------------------------------
  dropped <- preflight$conclusions[["plots_dropped"]]
  if (is.null(dropped)) {
    tr <- preflight$triage
    d <- tr[tr$verdict %in% c("drop", "degenerate"), , drop = FALSE]
    dropped <- if (nrow(d) == 0) "无" else
      paste0(paste0(d$title, "（", d$reason, "）", collapse = "；"), "。")
  }
  facts$plots_dropped_statement <- paste0("本试验不予采用的图：", dropped)

  stmts <- unlist(c(preflight$conclusions, preflight$statements), use.names = FALSE)
  stmts <- unique(stmts[!is.na(stmts) & nzchar(stmts)])
  facts$preflight_statements <- if (length(stmts))
    paste0("- ", stmts, collapse = "\n") else "- 无口径差异。"

  cs <- as.data.frame(result$tables$cross_stats)
  cs <- cs[cs$晋级品种数 > 0, , drop = FALSE]
  cs <- cs[order(-cs$晋级率, -cs$配制品种数, cs$亲本组合), , drop = FALSE]
  topc <- utils::head(cs, 3)
  facts$cross_top_text <- if (nrow(topc)) paste0(
    "亲本组合层面，晋级率最高的 ", nrow(topc), " 个组合为：",
    paste(vapply(seq_len(nrow(topc)), function(i) sprintf(
      "%s（%s 配 %s，晋级率 %.1f%%，平均亩产 %s kg/亩）",
      topc$亲本组合[i], topc$ma[i], topc$pa[i], topc$晋级率[i] * 100,
      rpt_fmt(topc$平均亩产[i], 2)), character(1)), collapse = "；"), "。") else ""

  ps <- as.data.frame(result$tables$parent_stats)
  ps <- ps[ps$晋级品种数 > 0, , drop = FALSE]
  ps <- ps[order(-ps$晋级率, -ps$配制品种数, ps$亲本名称), , drop = FALSE]
  topp <- utils::head(ps, 3)
  facts$parent_top_text <- if (nrow(topp)) paste0(
    "亲本层面，晋级率最高的 ", nrow(topp), " 个亲本为：",
    paste(vapply(seq_len(nrow(topp)), function(i) sprintf(
      "%s%s（配制 %d 份，晋级 %d 份，晋级率 %.1f%%，平均亩产 %s kg/亩）",
      topp$亲本类型[i], topp$亲本名称[i], topp$配制品种数[i], topp$晋级品种数[i],
      topp$晋级率[i] * 100, rpt_fmt(topp$平均亩产[i], 2)), character(1)), collapse = "；"),
    "。亲本晋级口径的位次阈值由管线写死为 ", ctx$parent_threshold,
    "，与筛选阈值 ", ctx$screening_threshold, " 不同口径。") else ""

  # ---- ① 编号（表、图各自独立计数；附录用 appendix_prefix） ----------------
  blocks <- tpl$blocks
  stage_no <- tpl$stage_no
  apx <- tpl$appendix_prefix
  nt <- 0L; nf <- 0L; na <- 0L
  registry <- list()
  for (b in blocks) {
    is_apx <- grepl("^A\\.", b$section)
    if (b$kind == "table") {
      if (is_apx) { na <- na + 1L; num <- na; stage <- apx }
      else { nt <- nt + 1L; num <- nt; stage <- as.character(stage_no) }
      label <- sprintf("表 %s-%d", stage, num)
    } else {
      if (is_apx) { na <- na + 1L; num <- na; stage <- apx }
      else { nf <- nf + 1L; num <- nf; stage <- as.character(stage_no) }
      label <- sprintf("图 %s-%d", stage, num)
    }
    registry[[b$id]] <- list(id = b$id, kind = b$kind, number = num,
                             stage = stage, label = label, section = b$section)
  }

  # ---- ② 渲染块 ----------------------------------------------------------
  blocks_md <- list()
  for (b in blocks) {
    reg <- registry[[b$id]]
    built <- NULL
    if (b$kind == "figure") {
      built <- rpt_blk_figure(result, ctx, meta, facts, b)
    } else {
      fn <- switch(b$id,
        tbl_yield_core      = rpt_blk_tbl_yield_core,
        tbl_trait_dist      = rpt_blk_tbl_trait_dist,
        tbl_growth_bin      = rpt_blk_tbl_growth_bin,
        tbl_screening_rules = rpt_blk_tbl_screening_rules,
        tbl_promoted_no_sep = rpt_blk_tbl_promoted_no_sep,
        tbl_promoted_sep    = rpt_blk_tbl_promoted_sep,
        tbl_threshold_scan  = rpt_blk_tbl_threshold_scan,
        tbl_cross_stats     = rpt_blk_tbl_cross_stats,
        tbl_parent_stats    = rpt_blk_tbl_parent_stats,
        tbl_top5            = rpt_blk_tbl_top5,
        tbl_a4_rounds       = rpt_blk_tbl_a4_rounds,
        tbl_a4_sources      = rpt_blk_tbl_a4_sources,
        tbl_a4_field_eval   = rpt_blk_tbl_a4_field_eval,
        NULL)
      if (!is.null(fn)) built <- fn(result, ctx, meta, facts)
    }
    if (is.null(built)) {
      blocks_md[[b$id]] <- paste0("_（块 ", b$id, " 未生成）_")
      next
    }
    if (isTRUE(skeleton)) {
      blocks_md[[b$id]] <- paste0("_（", reg$label, " ", rpt_fill_facts(built$title, facts),
                                 " —— 骨架占位，数据块待生成）_")
      next
    }
    title <- rpt_fill_facts(built$title, facts)
    notes <- c(built$note, preflight$caveats[[b$id]])
    notes <- notes[!is.na(notes) & nzchar(notes)]
    note_txt <- if (length(notes)) paste(notes, collapse = " ")
                 else "本块数据由 book_plot 分析管线产出。"
    if (b$kind == "table")
      md <- paste0("**", reg$label, " ", title, "**\n\n", built$body, "\n\n注：", note_txt)
    else
      md <- paste0(built$body, "\n\n", reg$label, " ", title, "\n\n注：", note_txt)
    blocks_md[[b$id]] <- md
  }

  # ---- ③ 交叉引用标签 -----------------------------------------------------
  ref_labels <- vapply(registry, function(r) r$label, character(1))

  # ---- ④ 渲染各节 --------------------------------------------------------
  body <- character(0); section_names <- character(0)
  for (s in secs) {
    txt <- s$text
    for (id in names(blocks_md))
      txt <- gsub(paste0("{{", id, "}}"), blocks_md[[id]], txt, fixed = TRUE)
    for (id in names(registry))
      txt <- gsub(paste0("{{ref_", id, "}}"), ref_labels[[id]], txt, fixed = TRUE)
    txt <- rpt_fill_narrative(txt, narrative)
    txt <- rpt_fill_facts(txt, facts)
    body <- c(body, txt)
    section_names <- c(section_names, s$name)
  }
  md <- paste(body, collapse = "\n\n")

  # ---- frontmatter + H1 + 前置信息块 + 盖章（黄金报告固定结构） ------------
  hf <- rpt_header_facts(meta, facts)
  header <- paste0(
    "---\n",
    "title: ", hf$page_title, "\n",
    "type: report\n",
    "tags: ", hf$tags_list, "\n",
    "sources: ", hf$sources_list, "\n",
    "created: ", hf$created, "\n",
    "updated: ", hf$updated, "\n",
    "置信度: INFERRED\n",
    "---\n\n",
    "# ", hf$doc_title, "\n\n",
    "> **试验编号**：", hf$trial_full_id, "\n",
    "> **试验类型**：", hf$trial_type_desc, "\n",
    "> **试验地点**：", hf$place, "｜**重复数**：", hf$n_reps, "｜**区组数**：", hf$n_blocks, "\n",
    "> **数据来源**：book_plot 田间记录本 `data/field_book.sqlite` → `yield_test_field_records`（试验 `", hf$expid, "`）\n",
    "> **分析入口**：book_plot 分析管线 `run_analysis()`，与应用内「分析」标签页、命令行 `scripts/analyze_trial.R --exp ", hf$expid, "` 同源同口径\n",
    "> **编制日期**：", hf$created
  )
  md <- paste0(header, "\n\n", rpt_abstract(facts), "\n\n", md,
               "\n\n", rpt_related_section(meta, facts),
               "\n\n承试单位（盖章）　　　　　　　　　年　　月　　日\n")

  # ---- ⑤ 收尾自检 --------------------------------------------------------
  rb <- regmatches(md, gregexpr("\\{\\{[^}]*\\}\\}", md, perl = TRUE))[[1]]
  if (length(rb) == 1 && rb[1] == -1) rb <- character(0)
  blanks <- unique(regmatches(md, gregexpr("<<[^<>\\n]+>>", md, perl = TRUE))[[1]])
  if (length(blanks) == 1 && blanks[1] == -1) blanks <- character(0)
  missing_narrative <- trimws(sub("^<<", "", sub(">>$", "", blanks)))

  slots <- list(
    facts = facts, registry = registry, blocks = blocks_md,
    residual_slots = as.character(rb),
    missing_narrative = missing_narrative,
    unfinished = names(blocks)[vapply(blocks, function(b) {
      m <- blocks_md[[b$id]]
      !is.null(m) && grepl("待生成", m, fixed = TRUE)
    }, logical(1))]
  )

  list(md = md, registry = registry, slots = slots,
       sections = section_names, missing_narrative = missing_narrative)
}
