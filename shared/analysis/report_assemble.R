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
#'
#' 支持体裁覆盖：`sections/<template_id>/` 下的同名文件优先于 `sections/`。
#' 这样新体裁（如多点鉴定）可以只覆盖需要改的节（如 07_results），
#' 其余节沿用公共版本，不必复制一整套。
rpt_load_sections <- function(template_dir, template_id = "primary") {
  base_dir <- file.path(template_dir, "sections")
  ovr_dir  <- file.path(base_dir, template_id)

  read_all <- function(d) {
    if (!dir.exists(d)) return(list())
    files <- sort(list.files(d, pattern = "\\.md\\.tmpl$", full.names = TRUE))
    stats::setNames(lapply(files, function(f) {
      list(file = f, name = sub("\\.md\\.tmpl$", "", basename(f)),
           text = paste(readLines(f, warn = FALSE), collapse = "\n"))
    }), sub("\\.md\\.tmpl$", "", basename(files)))
  }

  secs <- read_all(base_dir)
  ovr  <- read_all(ovr_dir)
  for (nm in names(ovr)) secs[[nm]] <- ovr[[nm]]   # 同名覆盖
  secs[order(names(secs))]
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
#'
#' ⚠️ 槽位取不到时**保留原文不替换**——空串替换会让数字静默消失（报告里出现
#' 「  份增产」这类空洞）。保留 `{{slot}}` 后由 report_assemble() ⑤ 步的
#' residual_slots 自检兜住，CLI 会把缺槽位打印出来。
rpt_fill_facts <- function(txt, facts) {
  pat <- "\\{\\{([A-Za-z0-9_.一-龥]+)\\}\\}"
  m <- regmatches(txt, gregexpr(pat, txt, perl = TRUE))[[1]]
  if (!length(m)) return(txt)
  for (ph in unique(m)) {
    nm <- sub("^\\{\\{", "", sub("\\}\\}$", "", ph))
    if (is.null(facts[[nm]])) next
    txt <- gsub(ph, rpt_fact_fmt(nm, facts[[nm]]), txt, fixed = TRUE)
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
  # 数据源按试验 ID 前缀判断：
  #   OT_* → data/other_trials.sqlite（外部导入的多点鉴定等）
  #   其他 → data/field_book.sqlite（book_plot 自身录入）
  # ⚠️ 原实现把 field_book.sqlite 写死，OT 试验的报告会谎报数据来源。
  is_other <- grepl("^OT_", as.character(expid))
  src_db   <- if (is_other) "data/other_trials.sqlite" else "data/field_book.sqlite"
  src_tbl  <- if (is_other) "other_trial_data" else "yield_test_field_records"
  src_desc <- if (is_other)
    paste0("book_plot 导入库 `", src_db, "` → `", src_tbl, "`（试验 `", expid, "`）")
  else
    paste0("book_plot 田间记录本 `", src_db, "` → `", src_tbl, "`（试验 `", expid, "`）")
  src_entry <- paste0("book_plot:", src_db)
  # ⚠️ sop_source 为空时不能拼出 "[, xxx]" —— 那是非法 YAML，pandoc 直接解析失败。
  #    实测：OT-中组 报告因此无法转 PDF（YAML parse exception at line 4）。
  src_list <- c(facts$sop_source %||% meta$sop_source, src_entry)
  src_list <- src_list[!is.na(src_list) & nzchar(as.character(src_list))]
  list(
    page_title = paste0(trial_no, "-", genre, "-", place, "-", year, "年试验报告"),
    doc_title  = paste0(cat, "-大豆", stage, "试验总结（", trial_no, " ", place, " ", year, "）"),
    tags_list  = paste0("[试验报告, ", cat, ", ", year, ", ", place, ", ", stage, ", ", trial_no, "]"),
    sources_list = paste0("[", paste(unique(src_list), collapse = ", "), "]"),
    source_desc  = src_desc,
    source_db    = src_db,
    is_other_src = is_other,
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
  # ⚠️ 外部鉴定试验（other_trials）没有杂交组合与亲本信息。
  #    原实现无条件写「来源于 N 个杂交组合」，OT 试验会输出「来源于 0 个杂交组合」，
  #    等于报告自己承认数据缺失，读起来像程序出错。
  combo_clause <- if (!is.null(facts$n_combos) && is.finite(facts$n_combos) && facts$n_combos > 0)
    paste0("（来源于 ", facts$n_combos, " 个杂交组合）") else ""
  s <- paste0(
    "本试验选用 ", facts$n_material, " 份参试材料", combo_clause, "、", facts$n_ck, " 个对照小区（对照品种", facts$ck_variety, "），共 ",
    facts$n_plots, " 个小区，采用", facts$arrangement, "、", facts$n_reps, " 次重复。",
    "结果显示：", facts$n_with_yield, " 个有产量小区平均亩产 ", F.(facts$mean_all),
    " kg、中位数 ", F.(facts$median_all), " kg、变异系数 ", F.(facts$cv_all), "%；",
    "管线按 1.5×IQR 剔除离群对照小区 ", facts$outlier_ck_ids, " 后，",
    facts$n_ck_valid, " 个有效对照基准均值 ", F.(facts$ck_mean), " kg/亩。",
    # ⚠️ 口径陷阱：n_mat_yield 是「有产量的**记录行数**」。
    #    单点试验里一材料一行，等于材料份数；
    #    多点试验里一材料每点一行 ⇒ 87 份材料会写成「774 份有实收产量」，
    #    87 份材料不可能有 774 份 —— 实测就是这么错的。
    #    ⇒ 多点时改成「材料·点次」的说法，并把材料份数单独说清楚。
    if (isTRUE(facts$is_multi_site))
      paste0(facts$n_material, " 份材料共 ", facts$n_mat_yield, " 个材料·点次有实收产量、均值 ")
    else
      paste0(facts$n_material, " 份材料中 ", facts$n_mat_yield, " 份有实收产量、均值 "),
    F.(facts$mat_mean), " kg/亩，", F.(facts$below_ck_pct, 1), "%（", facts$below_ck_n,
    " 份）低于对照基准；较平均对照增产 ", ia$gt0, " 份（超 5% ", ia$gt5, " 份、超 8% ",
    ia$gt8, " 份、超 10% ", ia$gt10, " 份）；较临近对照增产 ", il$gt0, " 份（超 5% ",
    il$gt5, " 份、超 8% ", il$gt8, " 份、超 10% ", il$gt10, " 份）。",
    "生育期 ", facts$growth_min, "–", facts$growth_max, " 天、均值 ", F.(facts$growth_mean, 1),
    " 天，与对照（", F.(facts$ck_growth_mean, 1), " 天）基本适配；生育期与亩产呈",
    facts$corr_growth_yield_word %||% "弱正相关", "（材料层 r = ",
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
#' 从 md 的 frontmatter 里读 sources 列表（相对路径，如 `../B. 调查·初选·评价/xxx`）
#'
#' 支持两种写法：
#'   sources: [a, b]        （行内数组）
#'   sources:
#'     - a
#'     - b                  （块列表）
#'
#' @param lines md 文件的行向量
#' @return 字符向量（去掉 .md 后缀由调用方处理）；无 sources 返回 character(0)
#' @noRd
.rpt_frontmatter_sources <- function(lines) {
  if (!length(lines) || !nzchar(trimws(lines[1])) || !grepl("^---", trimws(lines[1])))
    return(character(0))
  end <- which(trimws(lines[-1]) == "---")
  if (!length(end)) return(character(0))
  fm <- lines[seq.int(2L, end[1])]
  i <- grep("^sources\\s*:", fm)
  if (!length(i)) return(character(0))
  out <- character(0)
  first <- sub("^sources\\s*:\\s*", "", fm[i[1]])
  if (nzchar(trimws(first))) {
    # 行内数组 [a, b]
    first <- gsub("^\\[|\\]$", "", trimws(first))
    if (nzchar(trimws(first)))
      out <- trimws(strsplit(first, ",", fixed = TRUE)[[1]])
  } else {
    # 块列表：紧随其后的 `  - xxx` 行
    j <- i[1] + 1L
    while (j <= length(fm)) {
      ln <- fm[j]
      if (!grepl("^\\s+-\\s+", ln)) break
      out <- c(out, trimws(sub("^\\s+-\\s+", "", ln)))
      j <- j + 1L
    }
  }
  out <- gsub("^[\"']|[\"']$", "", out)
  out <- out[nzchar(out) & !is.na(out)]
  unique(out)
}

#' 折叠路径里的 `..`（normalizePath(mustWork=FALSE) 在 macOS 上不折叠）
#' @param p 绝对路径
#' @return 折叠后的路径
#' @noRd
.rpt_collapse_dotdot <- function(p) {
  if (!nzchar(p)) return(p)
  abs_p <- grepl("^/", p)
  segs <- strsplit(p, "/", fixed = TRUE)[[1]]
  out <- character(0)
  for (s in segs) {
    if (s == "" || s == ".") next
    if (s == "..") {
      if (length(out) > 0L) out <- out[-length(out)]
      next
    }
    out <- c(out, s)
  }
  paste0(if (abs_p) "/" else "", paste(out, collapse = "/"))
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
      fn_lines <- readLines(fnx, warn = FALSE, encoding = "UTF-8")

      # ---- ① 优先用时间线 frontmatter 的 sources ----
      # ⚠️ sources 里的相对路径是相对**时间线文件所在目录**的，例如：
      #      时间线在 02wiki/02-项目/2026年田间调查/B. 调查·初选·评价/
      #      sources 写 `../B. 调查·初选·评价/xxx`  → 02-项目/2026年田间调查/B. xxx/xxx
      #      sources 写 `../../品种档案/N26CB292`   → 02-项目/品种档案/N26CB292
      # 报告页在 02wiki/02-项目/2026年试验报告/，
      # ⇒ 必须先用时间线的**绝对路径**把 sources 解析成绝对路径，
      #    再换算成「相对报告页」的路径，否则硬拼出来的链接全是错的。
      src_links <- .rpt_frontmatter_sources(fn_lines)
      if (length(src_links)) {
        tl_dir <- dirname(fnx)
        # 用绝对路径解析，再相对 02-项目/ 归一
        resolved <- vapply(src_links, function(s) {
          p <- file.path(tl_dir, s)
          # normalizePath(mustWork=FALSE) 在 macOS 上**不折叠 ..** ⇒ 手动折叠
          p <- normalizePath(p, winslash = "/", mustWork = FALSE)
          p <- .rpt_collapse_dotdot(p)
          m <- regmatches(p, regexpr("02-项目/.*$", p))
          if (!length(m) || !nzchar(m)) return(NA_character_)
          # 只留 02-项目/ 之后的部分（报告页在 02-项目/2026年试验报告/，用 ../ 回退一级）
          sub("^02-项目/", "", m)
        }, character(1), USE.NAMES = FALSE)
        resolved <- resolved[!is.na(resolved)]
        resolved <- sub("\\.md$", "", resolved)
        if (length(resolved))
          links <- c(links, paste0("[[../", resolved, "]]"))
      }

      # ---- ② 正文内嵌 wikilink 兜底（sources 取不到时）----
      if (!length(src_links)) {
        txt <- paste(fn_lines, collapse = "\n")
        wl <- unique(regmatches(txt, gregexpr("\\[\\[[^]|#\\n]+", txt, perl = TRUE))[[1]])
        wl <- trimws(sub("^\\[\\[", "", wl))
        wl <- wl[nzchar(wl) & !grepl("试验报告|时间线|/", wl)]
        if (length(wl) && has(year))
          links <- c(links, paste0("[[../", year, "年田间调查/A. 田间观察/", wl, "]]"))
      }
    }
    rel <- sub("^.*02-项目/", "", fnx)
    if (nzchar(rel) && rel != fnx)
      links <- c(links, paste0("[[../", sub("\\.md$", "", rel), "]]（全部轮次评价时间线）"))
  }
  if (has(year)) {
    # ⚠️ 只在收获计划页**真实存在**时才建链接。
    #    原实现无条件生成 [[../收获计划/<year>-收获计划]]，
    #    但 2025 年并没有这个页面 ⇒ 自检器报「wikilink 指向的页面不存在」。
    vault_hint <- Sys.getenv("RPT_VAULT_ROOT")
    hp <- if (nzchar(vault_hint)) {
      file.path(vault_hint, "02-项目", "收获计划", paste0(year, "-收获计划.md"))
    } else ""
    if (nzchar(hp) && file.exists(hp))
      links <- c(links, paste0("[[../收获计划/", year, "-收获计划]]"))
  }
  links <- unique(links)
  paste0("## 关联\n\n", paste0("- ", links, collapse = "\n"),
         "\n\n> 本报告数据由 book_plot 田间记录本导出（`yield_test_field_records`）；§七 全部数字由 book_plot 自带分析管线 `run_analysis()` 产出，与应用内「分析」标签页、命令行 `scripts/analyze_trial.R` 同源同口径，可直接复算。")
}

#' 把块所在小节号归一成单值字符串
#'
#' YAML 里 `section:` 有三种「不写」的方式：整个键缺失（NULL）、写空值（NULL）、
#' 写 NA（字符串 "NA"）。三种都会让 `as.character(x)[1]` 变成 NA 或长度 0，
#' 直接拿去做 startsWith() 会抛错 —— 整份报告生成不出来。
#' （实测 yaml::read_yaml：缺键与空值都得 NULL，`section: NA` 得字符串 "NA"。）
#'
#' @param section 块所在小节号
#' @return 单值字符串（空串 = 没有小节号）
#' @noRd
rpt_norm_section <- function(section) {
  if (is.null(section)) return("")
  s <- trimws(as.character(section)[1])
  if (length(s) != 1L || is.na(s)) return("")
  s
}

#' 由块所在小节号推导表/图编号前缀（"7.1" → "7"）
#'
#' 表图编号跟着正文小节走：§七 的表是「表 7-x」、图是「图 7-x」，
#' 附录仍是「表 A-x」。旧实现固定用模板的 stage_no（初级产比 4 / 多点 5），
#' 表号跟小节号脱节 —— 表在第几节，编号就该是第几节。
#'
#' @param section 块所在小节号，如 "7.1" / "A.4"
#' @param default 推不出来时的兜底前缀（**报告正文前缀**，不是试验阶段号）
#' @return 字符型前缀
#' @noRd
rpt_section_stage <- function(section, default = "7") {
  s <- rpt_norm_section(section)
  if (!nzchar(s)) return(as.character(default)[1])
  m <- regmatches(s, regexpr("^[0-9]+", s))
  if (length(m) == 1L && nzchar(m)) return(m)
  as.character(default)[1]
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

  # ---- 组合 / 亲本速览 ------------------------------------------------------
  # ⚠️ 外部鉴定试验（other_trials）没有亲本信息，cross_stats / parent_stats
  #    可能是空表甚至 NULL ⇒ 必须做空值保护，否则 order() 直接报错中断装配。
  #    实测：黄淮多点鉴定-北组 因 cross_stats 为空，order(-cs$晋级率) 报
  #    「一元运算符的参数无效」，报告完全生成不出来。
  .safe_top <- function(tb, key_col, n) {
    if (is.null(tb)) return(NULL)
    cs <- tryCatch(as.data.frame(tb), error = function(e) NULL)
    if (is.null(cs) || nrow(cs) == 0 || !key_col %in% names(cs)) return(NULL)
    cs <- cs[!is.na(cs[[key_col]]) & cs[[key_col]] > 0, , drop = FALSE]
    if (nrow(cs) == 0) return(NULL)
    ord_cols <- intersect(c("晋级率", "配制品种数"), names(cs))
    if (length(ord_cols))
      cs <- cs[do.call(order, c(lapply(ord_cols, function(cc) -cs[[cc]]), list(na.last = TRUE))), , drop = FALSE]
    utils::head(cs, n)
  }
  cs <- .safe_top(result$tables$cross_stats, "晋级品种数", 3)
  topc <- if (is.null(cs)) data.frame() else cs
  facts$cross_top_text <- if (nrow(topc) && all(c("亲本组合","ma","pa","晋级率","平均亩产") %in% names(topc))) paste0(
    "亲本组合层面，晋级率最高的 ", nrow(topc), " 个组合为：",
    paste(vapply(seq_len(nrow(topc)), function(i) sprintf(
      "%s（%s 配 %s，晋级率 %.1f%%，平均亩产 %s kg/亩）",
      topc$亲本组合[i], topc$ma[i], topc$pa[i], topc$晋级率[i] * 100,
      rpt_fmt(topc$平均亩产[i], 2)), character(1)), collapse = "；"), "。") else ""

  ps <- .safe_top(result$tables$parent_stats, "晋级品种数", 3)
  topp <- if (is.null(ps)) data.frame() else ps
  facts$parent_top_text <- if (nrow(topp) && all(c("亲本类型","亲本名称","配制品种数","晋级品种数","晋级率","平均亩产") %in% names(topp))) paste0(
    "亲本层面，晋级率最高的 ", nrow(topp), " 个亲本为：",
    paste(vapply(seq_len(nrow(topp)), function(i) sprintf(
      "%s%s（配制 %d 份，晋级 %d 份，晋级率 %.1f%%，平均亩产 %s kg/亩）",
      topp$亲本类型[i], topp$亲本名称[i], topp$配制品种数[i], topp$晋级品种数[i],
      topp$晋级率[i] * 100, rpt_fmt(topp$平均亩产[i], 2)), character(1)), collapse = "；"),
    "。亲本晋级口径的位次阈值由管线写死为 ", ctx$parent_threshold,
    "，与筛选阈值 ", ctx$screening_threshold, " 不同口径。") else ""

  # ---- ① 渲染块（先渲染，编号留到 ② 只发给「真有内容」的块）--------------
  # ⚠️ 编号绝不能在看渲染结果之前分配。
  #    块函数可能返回 NULL（表为空 / 图缺失 / 分诊 drop），该块不输出，
  #    但编号已被占掉 ⇒ 正文出现 1,5,6,7 这种断号，自检器报「缺号」。
  #    实测：多点鉴定体裁 表 5 缺 2/3/4、图 5 缺 5/6/7/14（共 7 项自检失败）。
  #    ⇒ 必须先渲染全部块，再只给 built != NULL 的块编号。
  blocks <- tpl$blocks
  stage_no <- if (!is.null(meta$stage_no) && !is.na(meta$stage_no)) meta$stage_no else tpl$stage_no
  triage_df <- preflight$triage
  dropped_ids <- character(0)
  if (!is.null(triage_df) && nrow(triage_df) > 0) {
    dropped_ids <- triage_df$id[triage_df$verdict %in% c("drop", "degenerate")]
  }
  is_dropped <- function(bid) bid %in% dropped_ids

  built_map <- list()
  for (b in blocks) {
    # 分诊剔除的图：整块跳过，正文里不留图、不留图题、不留注
    if (b$kind == "figure" && is_dropped(b$id)) next
    built <- NULL
    if (b$kind == "figure") {
      built <- tryCatch(rpt_blk_figure(result, ctx, meta, facts, b), error = function(e) NULL)
    } else {
      fn <- switch(b$id,
        tbl_yield_core      = rpt_blk_tbl_yield_core,
        tbl_growth_bin      = rpt_blk_tbl_growth_bin,
        tbl_screening_rules = rpt_blk_tbl_screening_rules,
        tbl_promoted_no_sep = rpt_blk_tbl_promoted_no_sep,
        tbl_promoted_sep    = rpt_blk_tbl_promoted_sep,
        tbl_threshold_scan  = rpt_blk_tbl_threshold_scan,
        tbl_cross_stats     = rpt_blk_tbl_cross_stats,
        tbl_cross_location_avg = rpt_blk_tbl_cross_location_avg,
        tbl_cross_site_matrix  = rpt_blk_tbl_cross_site_matrix,
        tbl_gge_stability      = rpt_blk_tbl_gge_stability,
        tbl_ms_threshold_scan  = rpt_blk_tbl_ms_threshold_scan,
        tbl_parent_stats    = rpt_blk_tbl_parent_stats,
        tbl_top5            = rpt_blk_tbl_top5,
        tbl_a4_rounds       = rpt_blk_tbl_a4_rounds,
        tbl_a4_sources      = rpt_blk_tbl_a4_sources,
        tbl_a4_field_eval   = rpt_blk_tbl_a4_field_eval,
        NULL)
      if (!is.null(fn)) built <- tryCatch(fn(result, ctx, meta, facts), error = function(e) NULL)
    }
    if (!is.null(built)) built_map[[b$id]] <- built
  }

  # ---- ② 编号（只给渲染成功的块；表、图各自独立计数；附录用 appendix_prefix）
  # ⚠️ 编号前缀 = 块所在小节号（§七 ⇒ 7），不是试验阶段号（初级产比 4 / 多点 5）。
  #    兜底**也不能**用 stage_no —— 那会把前缀又拉回「试验阶段」，
  #    出现「表 7-3」与「表 9-1」混排（自检会判正文前缀不唯一而 FAIL）。
  apx_raw <- tpl$appendix_prefix
  apx <- if (is.null(apx_raw)) "" else trimws(as.character(apx_raw)[1])
  if (is.na(apx) || !nzchar(apx)) apx <- "A"

  # 正文前缀：模板里所有数字小节号中最常见的一个（primary / multi_site 都是 7）。
  # 块自己没写 section（或写了非数字小节）时用它 ⇒ 全篇前缀一致。
  sec_norm <- vapply(blocks, function(b) rpt_norm_section(b$section), character(1))
  body_pref <- character(0)
  for (s in sec_norm) {
    m <- regmatches(s, regexpr("^[0-9]+", s))
    if (length(m) == 1L && nzchar(m)) body_pref <- c(body_pref, m)
  }
  body_pref <- setdiff(unique(body_pref), apx)
  body_prefix <- if (length(body_pref)) {
    tb <- table(body_pref)
    names(sort(tb, decreasing = TRUE))[1]
  } else {
    # 模板里一个数字小节号都没有 ⇒ 只能退回试验阶段号（最后的兜底）
    fb <- as.character(stage_no)[1]
    if (is.na(fb) || !nzchar(fb)) "7" else fb
  }
  # 计数按「种类 × 前缀」分组：附录表与附录图各走各的序号。
  # ⚠️ 共用一个计数器的话，附录加一张图就会把后面的表号顶掉一位 ⇒ 自检报缺号。
  ctr <- list()
  ctr_next <- function(key) {
    n <- if (!is.null(ctr[[key]])) ctr[[key]] else 0L
    n <- n + 1L
    ctr[[key]] <<- n
    n
  }
  registry <- list()
  blocks_md <- list()
  for (b in blocks) {
    built <- built_map[[b$id]]
    if (is.null(built)) {
      # 块没有实际内容时，不要简单留空 —— 空槽会让「表后紧邻注」类检查失去对象，
      # 读者也看不出是「无此项」还是「漏掉了」。给一句明确说明更好。
      blocks_md[[b$id]] <- ""
      next
    }
    sec <- rpt_norm_section(b$section)
    is_apx <- nzchar(apx) && (sec == apx || startsWith(sec, paste0(apx, ".")))
    stage <- if (is_apx) apx else rpt_section_stage(sec, body_prefix)
    num <- ctr_next(paste0(b$kind, "|", stage))
    label <- sprintf("%s %s-%d", if (b$kind == "table") "表" else "图", stage, num)
    reg <- list(id = b$id, kind = b$kind, number = num,
                stage = stage, label = label, section = b$section)
    registry[[b$id]] <- reg

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

  # ---- ②b. 诊断：模板块顺序 vs 正文占位符顺序 ----------------------------
  # 编号按 YAML 块顺序分配，但表/图在正文里出现的先后由 {{tbl_*}}/{{fig_*}}
  # 占位符的位置决定。两者不一致 ⇒ 正文里编号乱序（自检会报「不升序」），
  # 这里提前点名，免得排查时只看到「编号不升序」却不知道为什么。
  ph_order <- character(0)
  for (s in secs) {
    m <- regmatches(s$text, gregexpr("\\{\\{[A-Za-z0-9_]+\\}\\}", s$text, perl = TRUE))[[1]]
    if (length(m)) ph_order <- c(ph_order, sub("^\\{\\{", "", sub("\\}\\}$", "", m)))
  }
  yaml_order <- vapply(blocks, function(b) as.character(b$id)[1], character(1))
  cmp_yaml <- yaml_order[yaml_order %in% ph_order]
  cmp_ph <- ph_order[ph_order %in% yaml_order]
  if (length(cmp_yaml) && !identical(cmp_yaml, cmp_ph)) {
    warning(sprintf(
      paste0("表图顺序不一致：模板块顺序与正文占位符顺序不符，正文会出现乱序编号。\n",
             "  模板 YAML：%s\n  正文占位符：%s"),
      paste(cmp_yaml, collapse = "、"), paste(cmp_ph, collapse = "、")))
  }

  # ---- ③ 交叉引用标签 -----------------------------------------------------
  ref_labels <- vapply(registry, function(r) r$label, character(1))

  # ---- ④ 渲染各节 --------------------------------------------------------
  body <- character(0); section_names <- character(0)
  for (s in secs) {
    txt <- s$text
    for (id in names(blocks_md))
      txt <- gsub(paste0("{{", id, "}}"), blocks_md[[id]], txt, fixed = TRUE)
    txt <- rpt_fill_narrative(txt, narrative)
    # 交叉引用放在叙述注入之后解析 —— 叙述侧车里也能写 {{ref_tbl_x}}，
    # 表序号变动（如删表）时不必手改叙述里的表号
    for (id in names(registry))
      txt <- gsub(paste0("{{ref_", id, "}}"), ref_labels[[id]], txt, fixed = TRUE)
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
    # 多点试验是 1 个试点 1 个区组，说「区组数」会被误读成随机区组设计；
    # 改成「试点数」更准确（单点试验仍说区组数）。
    "> **试验地点**：", hf$place, "｜**重复数**：", hf$n_reps,
    if (isTRUE(facts$is_multi_site)) paste0("｜**试点数**：", facts$n_place, "\n")
    else paste0("｜**区组数**：", hf$n_blocks, "\n"),
    "> **数据来源**：", hf$source_desc, "\n",
    "> **分析入口**：book_plot 分析管线 `run_analysis()`，与应用内「分析」标签页、命令行 `scripts/build_trial_report.R --trial ", meta$trial_key %||% meta$trial_no %||% hf$trial_no, "` 同源同口径\n",
    "> **编制日期**：", hf$created
  )
  md <- paste0(header, "\n\n", rpt_abstract(facts), "\n\n", md,
               "\n\n", rpt_related_section(meta, facts),
               "\n\n承试单位（盖章）　　　　　　　年　　月　　日\n")

  # ---- ④.5 清理本模板用不到的槽位 -----------------------------------------
  # ⚠️ sections/*.md.tmpl 是所有体裁**共用**的，里面写死了 primary 模板的槽位
  #    （如 {{tbl_cross_stats}} / {{tbl_parent_stats}} / {{fig_parent_plot}}）。
  #    多点鉴定体裁没有组合与亲本分析 ⇒ 这些块不在 tpl$blocks 里 ⇒ 槽位没人填，
  #    报告里会残留 {{...}}（自检器报「未填充模板槽」）。
  #    ⇒ 组装完成后，把「模板里没出现过的块 id」对应槽位一并抹掉。
  all_ids <- unique(c(RPT_PLOT_REGISTRY$id, RPT_TABLE_IDS, names(blocks_md)))
  unused_ids <- setdiff(all_ids, vapply(blocks, `[[`, character(1), "id"))
  for (uid in unused_ids) {
    md <- gsub(paste0("{{", uid, "}}"), "", md, fixed = TRUE)
  }
  # 清理因此产生的连续空行（3 行以上压成 2 行）
  md <- gsub("\n{3,}", "\n\n", md, perl = TRUE)

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
