# ==============================================================================
# 试验报告判定引擎（preflight）
# ------------------------------------------------------------------------------
# 做两件事：
#   ① 跑规则表 RPT_RULES —— 判定本次试验有哪些口径问题、触发哪些结论句与注文
#   ② 图表分诊 —— 判定 16 张管线出图里哪些进正文、哪些剔除、哪些退化
#
# 设计要点：
#   · 规则是**数据**（RPT_RULES 表 + RPT_PREDICATES 命名谓词），不是 if 分支。
#     ⇒ 规则可被逐条枚举、可被自检器逐条跑、可被导出成文档。
#   · 分诊**不读 PNG**。判据全部来自管线数据：
#     依赖字段整列为空 ⇒ 该图必然空白 ⇒ 剔除。
#   · 判定与措辞分离：规则表只管「触发没有」，结论句与注文在 report_rules.R 里。
#
# 依赖：shared/analysis/report_rules.R、shared/analysis/report_stats.R
# ==============================================================================

#' 判断某些字段在 export_data 里是否整列为空
#' @param df export_data
#' @param cols 字段名（拼音）
#' @return 整列为空的字段名向量
#' @noRd
rpt_all_na_cols <- function(df, cols) {
  cols[cols %in% colnames(df) & vapply(cols, function(nm) all(is.na(df[[nm]])), logical(1))]
}

#' 图表分诊：判定 16 张图各自进不进正文
#'
#' 判据（全部来自管线数据，不读 PNG）：
#'   · 依赖的源字段整列为空 ⇒ 渲染出来是空白 ⇒ 剔除
#'   · 管线没生成这张图 ⇒ 剔除（记原因）
#'   · 亲本口径退化 ⇒ 亲本晋级表现图零区分度 ⇒ 剔除
#'   · 无对照 ⇒ 增产率/雷达图不成立 ⇒ 剔除
#'
#' @param result analysis_run() 的 result
#' @param ctx     compute_report_stats() 的事实表
#' @param registry RPT_PLOT_REGISTRY
#' @return data.frame(id, plot_key, title, png_name, verdict, reason)
#'   verdict ∈ keep / drop / degenerate
#' @export
rpt_triage_plots <- function(result, ctx, registry = RPT_PLOT_REGISTRY) {
  df <- result$tables$export_data
  have_plots <- names(result$plots)
  rows <- list()

  for (i in seq_len(nrow(registry))) {
    reg <- registry[i, ]
    src <- strsplit(reg$source_cols, ";", fixed = TRUE)[[1]]
    src <- src[nzchar(src)]
    verdict <- "keep"
    reason  <- ""

    # ① 依赖字段整列为空 ⇒ 空白图
    na_cols <- rpt_all_na_cols(df, src)
    if (length(na_cols) > 0) {
      verdict <- "drop"
      reason  <- paste0("依赖字段（", paste(na_cols, collapse = "、"),
                        "）在全部小区中均为空，渲染后为空白，无信息量")
    } else if (!reg$plot_key %in% have_plots) {
      # ② 管线没生成
      verdict <- "drop"
      reason  <- "管线本次未生成该图"
    } else if (reg$plot_key == "parent_plot" &&
               isTRUE(RPT_PREDICATES$pf_parent_degenerate(ctx))) {
      # ③ 亲本口径退化 ⇒ 晋级率恒为 1.000，零区分度
      verdict <- "degenerate"
      reason  <- paste0("亲本口径退化（最大较平均对照位次 ",
                        format(ctx$max_avg_ck_rank), " ≤ 阈值 ",
                        format(ctx$parent_threshold),
                        "），各组合晋级率恒为 1.000，无区分度")
    } else if (ctx$n_ck == 0 &&
               reg$plot_key %in% c("increase_dist", "radar", "comparison")) {
      # ④ 无对照 ⇒ 依赖对照基准的图不成立
      verdict <- "drop"
      reason  <- "本试验无对照小区，该图依赖对照基准，不成立"
    }

    rows[[i]] <- data.frame(
      id = reg$id, plot_key = reg$plot_key, title = reg$title,
      png_name = reg$png_name, verdict = verdict, reason = reason,
      stringsAsFactors = FALSE)
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL

  # ⑤ 管线出了图但登记表里没有 ⇒ 也得剔除，否则编号会漏
  extra <- setdiff(have_plots, registry$plot_key)
  if (length(extra) > 0) {
    out <- rbind(out, data.frame(
      id = paste0("fig_extra_", seq_along(extra)), plot_key = extra,
      title = extra, png_name = paste0(extra, ".png"), verdict = "drop",
      reason = "管线出图但未登记进图表注册表，报告不采用",
      stringsAsFactors = FALSE))
  }

  out
}

#' 把剔除的图汇总成一句话（插在 §7.3 末尾，替代手工写的「不予采用的 N 张图」）
#' @param triage rpt_triage_plots() 的结果
#' @return 字符串
#' @export
rpt_dropped_reason <- function(triage) {
  d <- triage[triage$verdict %in% c("drop", "degenerate"), ]
  if (nrow(d) == 0) return("无")
  paste0(paste0(d$title, "（", d$reason, "）", collapse = "；"), "。")
}

#' 跑一遍判定引擎
#'
#' @param result analysis_run() 的 result
#' @param ctx    事实表；NULL 时自己算
#' @param meta   试验元信息（meta$design$n_material 用于一致性断言）
#' @param parent_threshold 亲本口径阈值（管线写死 60）
#' @param rules  规则表
#' @param predicates 谓词注册表
#' @return list(ok, triggered, facts, caveats, statements, conclusions, triage)
#'   ok          有 blocker 触发则 FALSE
#'   triggered   data.frame(rule_id, title, severity, scope, blocks, conclusion, note, doc_ref)
#'   facts       全部事实（含分诊补充的字段）
#'   caveats     按块 id 挂的注文：caveats[["tbl_x"]] = c("注：…", "注：…")
#'   statements  按块 id 挂的整句披露：statements[["fig_x"]] = c("…", "…")
#'   conclusions trial 级结论句（写进正文/附录的整句话）
#'   triage      图表分诊表
#' @export
report_preflight <- function(result, ctx = NULL, meta = NULL,
                             parent_threshold = 60L,
                             narrative = NULL, facts = NULL,
                             rules = RPT_RULES, predicates = RPT_PREDICATES) {

  if (is.null(ctx)) {
    ctx <- compute_report_stats(result, meta = meta,
                                parent_threshold = parent_threshold)
  }

  # ---- 分诊（先做，因为规则 ⑳ 与结论句要用分诊结果）----
  triage <- rpt_triage_plots(result, ctx)
  ctx$n_plots_dropped      <- sum(triage$verdict %in% c("drop", "degenerate"))
  ctx$dropped_plots_reason <- rpt_dropped_reason(triage)

  # ---- 叙述侧车数字校验（规则 ㉑）----
  # 表和摘要的数字由 facts 插值，天然与管线同源；叙述段落是自由文本，
  # 手抄错了没人发现 ⇒ 这里拿叙述文本与事实表逐条比对。
  if (!is.null(narrative) && !is.null(facts)) {
    mm <- tryCatch(rpt_check_narrative_numbers(narrative, facts),
                   error = function(e) NULL)
    if (!is.null(mm) && nrow(mm) > 0) {
      ctx$narrative_mismatch      <- nrow(mm)
      ctx$narrative_mismatch_detail <- mm
    } else {
      ctx$narrative_mismatch      <- 0L
      ctx$narrative_mismatch_detail <- NULL
    }
  }

  # ---- 逐条跑规则 ----
  trig <- list()
  for (i in seq_len(nrow(rules))) {
    rule <- rules[i, ]
    fn <- predicates[[rule$predicate]]
    if (is.null(fn)) next
    hit <- isTRUE(tryCatch(fn(ctx), error = function(e) {
      warning("规则 ", rule$rule_id, " 求值出错：", conditionMessage(e))
      FALSE
    }))
    if (!hit) next
    txt <- rpt_rule_text(rule, ctx)
    trig[[length(trig) + 1]] <- data.frame(
      rule_id = rule$rule_id, title = rule$title, severity = rule$severity,
      scope = rule$scope, blocks = rule$blocks,
      conclusion = if (is.null(txt$conclusion)) NA_character_ else txt$conclusion,
      note = if (is.null(txt$note)) NA_character_ else txt$note,
      doc_ref = if (is.null(rule$doc_ref)) NA_character_ else rule$doc_ref,
      stringsAsFactors = FALSE)
  }
  triggered <- if (length(trig) == 0) {
    data.frame(rule_id = character(), title = character(), severity = character(),
               scope = character(), blocks = character(),
               conclusion = character(), note = character(),
               doc_ref = character(), stringsAsFactors = FALSE)
  } else {
    do.call(rbind, trig)
  }
  rownames(triggered) <- NULL

  # ---- 注文按块挂载 ----
  # caveats  = 表/图下方的「注：」短句
  # statements= 贴在某个块旁边的整句披露（口径打架点、剔除说明等）
  caveats    <- list()
  statements <- list()
  blk <- triggered[triggered$scope == "block", , drop = FALSE]
  for (i in seq_len(nrow(blk))) {
    ids <- trimws(strsplit(blk$blocks[i], ",", fixed = TRUE)[[1]])
    ids <- ids[nzchar(ids)]
    if (length(ids) == 0) next
    for (bid in ids) {
      if (!is.na(blk$note[i])) {
        caveats[[bid]] <- c(caveats[[bid]], blk$note[i])
      }
      if (!is.na(blk$conclusion[i])) {
        statements[[bid]] <- c(statements[[bid]], blk$conclusion[i])
      }
    }
  }

  # ---- trial 级结论句 ----
  tr <- triggered[triggered$scope == "trial" & !is.na(triggered$conclusion), ]
  conclusions <- setNames(as.list(tr$conclusion), tr$rule_id)

  ok <- !any(triggered$severity == "blocker")

  list(
    ok = ok,
    triggered = triggered,
    facts = ctx,
    caveats = caveats,
    statements = statements,
    conclusions = conclusions,
    triage = triage,
    blockers = triggered[triggered$severity == "blocker", , drop = FALSE],
    warns = triggered[triggered$severity == "warn", , drop = FALSE]
  )
}
