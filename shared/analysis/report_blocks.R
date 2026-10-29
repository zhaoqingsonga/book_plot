# ==============================================================================
# 试验报告块构建器（report_blocks.R）
# ------------------------------------------------------------------------------
# 一个块 = 一个函数。输入 result（管线产出）+ ctx/preflight（判定层事实）+
# meta（试验元信息），输出 Markdown 片段。
#
# 铁律：
#   · 所有数字来自 result$tables（内存 data.frame）或 preflight$facts。
#   · 禁止手算、禁止从 PNG 读数、禁止写死统计值。
#   · 注文「注：」由装配层统一追加（块内只给默认注文与口径说明）。
#
# 块输出约定（由 report_assemble.R 负责编号与排版）：
#   表块 → list(kind="table", title=, body=<md 表格>, note=<注文>)
#   图块 → list(kind="figure", title=, body=<图片行>, note=<注文>)
#
# 调用方：shared/analysis/report_assemble.R
# 依赖：shared/analysis/report_rules.R（RPT_PLOT_REGISTRY）
# ==============================================================================

# ---- 小工具 ---------------------------------------------------------------

#' 把 data.frame 渲染成 Markdown 表格
rpt_md_table <- function(df) {
  if (is.null(df) || nrow(df) == 0L) return("_（本次试验无对应数据）_")
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  hdr <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  body <- vapply(seq_len(nrow(df)), function(i) {
    paste0("| ", paste(unlist(df[i, ], use.names = FALSE), collapse = " | "), " |")
  }, character(1))
  paste(c(hdr, sep, body), collapse = "\n")
}

#' 数值格式化：默认保留 2 位小数；NA → —
rpt_fmt <- function(x, digits = 2) {
  ifelse(is.na(x), "—", formatC(as.numeric(x), format = "f", digits = digits))
}

#' 百分比格式化（输入已是百分数，如 86.9）
rpt_fmt_pct <- function(x, digits = 1) {
  ifelse(is.na(x), "—", paste0(formatC(round(as.numeric(x), digits),
                                       format = "f", digits = digits), "%"))
}

#' 与黄金报告一致：as.character(round(x, 2))（不补尾零）
rpt_fmt_loose <- function(x) {
  ifelse(is.na(x), "—", as.character(round(as.numeric(x), 2)))
}

#' 取对照标记
rpt_is_ck <- function(ed) {
  if ("is_ck" %in% names(ed)) return(as.numeric(as.character(ed$is_ck)) == 1)
  if ("对照" %in% names(ed))  return(as.numeric(as.character(ed$对照)) == 1)
  rep(FALSE, nrow(ed))
}

#' 取非空字符串
rpt_chr <- function(x) {
  if (is.null(x)) return(character(0))
  x <- as.character(x)
  x[is.na(x)] <- ""
  trimws(x)
}

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a

# ==============================================================================
# 派生事实（report_stats.R 没覆盖、但报告正文需要的量）
# ==============================================================================
rpt_derive_facts <- function(result, ctx, meta = NULL) {
  tb <- result$tables
  ed <- tb$export_data
  is_ck <- rpt_is_ck(ed)
  mat <- ed[!is_ck, , drop = FALSE]
  ck  <- ed[is_ck, , drop = FALSE]
  y <- ed$MuChan
  ck_y <- ck$MuChan[!is.na(ck$MuChan)]
  mat_y <- mat$MuChan[!is.na(mat$MuChan)]

  ys <- tb$yield_stats; gs <- tb$growth_stats; ist <- tb$increase_stats
  ov <- tb$overview
  get_ov <- function(k) {
    v <- ov[["数值"]][ov[["指标"]] == k]
    if (length(v) == 0) NA else v[1]
  }

  mean_all <- if (nrow(ys)) ys[["平均亩产"]][1] else NA
  med_all  <- if (nrow(ys)) ys[["产量中位数"]][1] else NA
  max_all  <- if (nrow(ys)) ys[["最高亩产"]][1] else NA
  min_all  <- if (nrow(ys)) ys[["最低亩产"]][1] else NA
  sd_all   <- if (nrow(ys)) ys[["产量标准差"]][1] else NA
  cv_all   <- if (nrow(ys)) ys[["产量变异系数"]][1] else NA

  ck_mean <- mean(ck_y); ck_sd <- stats::sd(ck_y)
  ck_cv <- if (is.na(ck_mean) || ck_mean == 0) NA else ck_sd / ck_mean * 100
  mat_mean <- mean(mat_y); mat_sd <- stats::sd(mat_y)
  mat_cv <- if (is.na(mat_mean) || mat_mean == 0) NA else mat_sd / mat_mean * 100
  below_n <- sum(mat_y < ck_mean)
  below_pct <- if (length(mat_y)) below_n / length(mat_y) * 100 else NA

  mat_g <- mat$ShengYuQi[!is.na(mat$ShengYuQi)]
  ck_g  <- ck$ShengYuQi[!is.na(ck$ShengYuQi)]

  ok <- !is.na(mat$ShengYuQi) & !is.na(mat$MuChan)
  corr_g <- if (sum(ok) > 2) stats::cor(mat$ShengYuQi[ok], mat$MuChan[ok]) else NA

  maturity <- rpt_chr(ed$ChengShuQi); maturity[maturity == ""] <- NA
  mat_tab <- sort(table(sub("^[0-9]{4}-", "", maturity[!is.na(maturity)])),
                   decreasing = TRUE)
  maturity_text <- if (length(mat_tab)) {
    paste(vapply(seq_along(mat_tab), function(i) {
      d <- names(mat_tab)[i]; n <- as.integer(mat_tab[i])
      sprintf("%s 共 %d 小区（%.1f%%）", d, n, n / nrow(ed) * 100)
    }, character(1)), collapse = "、")
  } else ""

  trait_map <- list(YeXing = "叶形", HuaSe = "花色", RongMaoSe = "茸毛色",
                    JieJiaXiXing = "结荚习性", ZhuXing = "株型",
                    LuoYeXing = "落叶性", DaoFuXing = "倒伏性")
  trait_bits <- character(0)
  for (nm in names(trait_map)) {
    if (!nm %in% names(ed)) next
    v <- rpt_chr(ed[[nm]]); v[v == ""] <- NA; v <- v[!is.na(v)]
    if (!length(v)) next
    tb_ <- sort(table(v), decreasing = TRUE); top <- tb_[1]
    trait_bits <- c(trait_bits, sprintf("%s以「%s」为主（%d 个小区，%.1f%%）",
      trait_map[[nm]], names(top), as.integer(top), as.integer(top) / nrow(ed) * 100))
  }
  trait_text <- paste(trait_bits, collapse = "；")

  facts <- list(
    n_plots = nrow(ed),
    n_material = nrow(mat),
    n_ck = nrow(ck),
    n_with_yield = sum(!is.na(y)),
    n_mat_yield = length(mat_y),
    n_ck_valid = length(ck_y),
    mean_all = mean_all, median_all = med_all, sd_all = sd_all, cv_all = cv_all,
    max_yield = max_all, min_yield = min_all,
    ck_mean = ck_mean, ck_min = min(ck_y), ck_max = max(ck_y),
    ck_sd = ck_sd, ck_cv = ck_cv, ck_n = length(ck_y),
    mat_mean = mat_mean, mat_sd = mat_sd, mat_cv = mat_cv,
    below_ck_n = below_n, below_ck_pct = below_pct,
    mat_growth_mean = if (length(mat_g)) mean(mat_g) else NA,
    mat_growth_n = length(mat_g),
    ck_growth_mean = if (length(ck_g)) mean(ck_g) else NA,
    ck_growth_n = length(ck_g),
    growth_mean = if (nrow(gs)) gs[["平均生育期"]][1] else NA,
    growth_median = if (nrow(gs)) gs[["生育期中位数"]][1] else NA,
    growth_max = if (nrow(gs)) gs[["最长生育期"]][1] else NA,
    growth_min = if (nrow(gs)) gs[["最短生育期"]][1] else NA,
    inc_avg_mean = if (nrow(ist)) ist[["平均增产率"]][1] else NA,
    inc_avg_max = if (nrow(ist)) ist[["最高增产率"]][1] else NA,
    inc_avg_min = if (nrow(ist)) ist[["最大减产率"]][1] else NA,
    corr_growth_yield = corr_g,
    corr_growth_yield_n = sum(ok),
    n_combos = get_ov("杂交组合数"),
    maturity_text = maturity_text,
    trait_text = trait_text
  )
  for (nm in names(ctx)) if (is.null(facts[[nm]])) facts[[nm]] <- ctx[[nm]]
  # ---- 注入试验元信息与设计参数（模板 {{trial_no}}/{{place}}/{{year}}/{{ck_variety}} 等依赖） ----
  if (!is.null(meta)) {
    for (nm in c("trial_no", "experiment_id", "stage_no", "体裁", "地点", "year",
                 "created", "updated", "sop_source")) {
      if (is.null(facts[[nm]]) && !is.null(meta[[nm]])) facts[[nm]] <- meta[[nm]]
    }
    if (is.null(facts$place)) {
      facts$place <- if (!is.null(meta[["地点"]])) meta[["地点"]]
                     else if (!is.null(meta$place)) meta$place else ""
    }
    d <- meta$design
    if (is.list(d)) for (nm in names(d)) if (is.null(facts[[nm]])) facts[[nm]] <- d[[nm]]
  }
  facts
}

# ==============================================================================
# 阈值敏感性扫描（管线实跑：对每个位次阈值重跑 analyze_screening）
# ==============================================================================
rpt_threshold_scan <- function(ed, thresholds = c(20, 30, 40, 50, 60, 70, 80, 100),
                                plant_threshold = 100,
                                exclude_lodging = c("9-严重倒", "7-重倒")) {
  is_ck <- rpt_is_ck(ed)
  mat <- ed[!is_ck, , drop = FALSE]
  ck_y <- ed$MuChan[is_ck & !is.na(ed$MuChan)]
  ck_mean <- mean(ck_y)
  rows <- list()
  for (T in thresholds) {
    sc <- analyze_screening(ed, rank_threshold_select = T,
                            rank_threshold_plant = plant_threshold,
                            exclude_lodging = exclude_lodging,
                            radar_top_n = 2)
    pr <- sc$promoted; el <- sc$eliminated; sp <- sc$select_plant
    py <- pr$MuChan[!is.na(pr$MuChan)]
    has_rank <- !is.na(el$JiaoLinJinDuiZhaoWeiCi) & !is.na(el$JiaoPingJunDuiZhaoWeiCi)
    rank_fail <- sum(has_rank & !(el$JiaoLinJinDuiZhaoWeiCi <= T &
                                  el$JiaoPingJunDuiZhaoWeiCi <= T))
    lodg <- sum(as.character(el$DaoFuXing) %in% exclude_lodging)
    no_rank <- sum(!has_rank)
    rows[[length(rows) + 1]] <- data.frame(
      晋级位次阈值 = T,
      晋级材料 = nrow(pr),
      分离选单株 = nrow(sp),
      淘汰小区 = nrow(el),
      晋级率_材料口径 = rpt_fmt(nrow(pr) / nrow(mat) * 100, 1),
      晋级率_小区口径 = rpt_fmt(nrow(pr) / nrow(ed) * 100, 1),
      晋级材料平均亩产 = rpt_fmt(mean(py), 2),
      晋级材料亩产区间 = if (length(py)) paste0(rpt_fmt(min(py), 2), "–", rpt_fmt(max(py), 2)) else "—",
      晋级群体最低亩产较对照均值 = rpt_fmt(min(py) - ck_mean, 2),
      淘汰_位次不达标 = rank_fail,
      淘汰_倒伏 = lodg,
      淘汰_无位次 = no_rank,
      stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows); rownames(out) <- NULL
  out
}

# ==============================================================================
# 各块构建器  (result, ctx, meta, facts) → list(title, body, note)
# ==============================================================================

# ---- 表 产量核心统计 ------------------------------------------------------
rpt_blk_tbl_yield_core <- function(result, ctx, meta, facts) {
  inc <- ctx$inc_avg; lin <- ctx$inc_lin
  gr <- ctx$grade_counts; gp <- ctx$grade_pct
  ed <- result$tables$export_data
  area <- if (!is.null(meta$design$plot_area_m2)) as.numeric(meta$design$plot_area_m2) else NA
  out_id <- ctx$outlier_ck_ids
  raw_kg <- NA; mu_out <- NA
  if (nzchar(out_id)) {
    i <- which(as.character(ed$stageid) == out_id)
    if (length(i)) {
      raw_kg <- ed$XiaoQuChanLiang[i[1]]
      mu_out <- if (!is.na(raw_kg) && !is.na(area)) raw_kg / area * (10000 / 15) else NA
    }
  }
  df <- data.frame(
    指标 = c("小区总数", "有产量小区数", "对照小区数",
             "全部有产量小区平均亩产", "全部有产量小区产量中位数",
             "最高亩产", "最低亩产", "产量标准差", "产量变异系数",
             sprintf("对照基准均值（%d 个有效对照）", facts$n_ck_valid),
             sprintf("对照亩产范围（%d 个有效对照）", facts$n_ck_valid),
             "对照标准差 / 变异系数",
             sprintf("离群对照小区 %s", out_id),
             sprintf("材料平均亩产（%d 份有产量）", facts$n_mat_yield),
             "低于对照基准的材料比例", "平均增产率", "最高增产率 / 最大减产率",
             "增产品种数（较平均对照 / 较临近对照）",
             "高产（≥200 kg/亩）材料", "生育期区间 / 均值",
             "材料生育期均值 / 对照生育期均值"),
    数值 = c(
      sprintf("%d（材料 %d + 对照 %d）", facts$n_plots, facts$n_material, facts$n_ck),
      sprintf("%d（材料 %d + 对照 %d）", facts$n_with_yield, facts$n_mat_yield, facts$n_ck_valid),
      sprintf("%d（离群剔除后参与基准计算 %d 个）", facts$n_ck, facts$n_ck_valid),
      sprintf("%s kg/亩", rpt_fmt(facts$mean_all)),
      sprintf("%s kg/亩", rpt_fmt(facts$median_all)),
      sprintf("%s kg/亩", rpt_fmt(facts$max_yield)),
      sprintf("%s kg/亩", rpt_fmt(facts$min_yield)),
      sprintf("%s kg/亩", rpt_fmt(facts$sd_all)),
      sprintf("%s%%", rpt_fmt(facts$cv_all)),
      sprintf("%s kg/亩", rpt_fmt(facts$ck_mean)),
      sprintf("%s–%s kg/亩", rpt_fmt(facts$ck_min), rpt_fmt(facts$ck_max)),
      sprintf("%s kg/亩 / %s%%", rpt_fmt(facts$ck_sd), rpt_fmt(facts$ck_cv)),
      sprintf("小区产量 %s kg（≈%s kg/亩），管线按 1.5×IQR 判为离群并置空，未计入基准",
              rpt_fmt(raw_kg, 0), rpt_fmt(mu_out, 1)),
      sprintf("%s kg/亩（标准差 %s，CV %s%%）", rpt_fmt(facts$mat_mean),
              rpt_fmt(facts$mat_sd), rpt_fmt(facts$mat_cv)),
      sprintf("%s%%（%d/%d）", rpt_fmt(facts$below_ck_pct, 1),
              facts$below_ck_n, facts$n_mat_yield),
      sprintf("%s%%", rpt_fmt(facts$inc_avg_mean)),
      sprintf("+%s%% / %s%%", rpt_fmt(facts$inc_avg_max), rpt_fmt(facts$inc_avg_min)),
      sprintf("%d / %d", inc$gt0, lin$gt0),
      sprintf("%d 份（%s%%）", gr[["ge200"]], rpt_fmt(gp[["ge200"]], 1)),
      sprintf("%s–%s 天 / %s 天（中位 %s）", rpt_fmt(facts$growth_min, 0),
              rpt_fmt(facts$growth_max, 0), rpt_fmt(facts$growth_mean, 1),
              rpt_fmt(facts$growth_median, 0)),
      sprintf("%s 天（n = %d）/ %s 天（n = %d）", rpt_fmt(facts$mat_growth_mean, 1),
              facts$mat_growth_n, rpt_fmt(facts$ck_growth_mean, 1), facts$ck_growth_n)),
    stringsAsFactors = FALSE)
  list(title = "产量核心统计",
       body = rpt_md_table(df),
       note = paste0("全部统计由 book_plot 分析管线产出，未手算。小区层口径（含对照），分母 ",
                     facts$n_plots, " 个小区；其中 ", ctx$n_na_yield,
                     " 个小区无产量（", ctx$n_missing_material, " 份缺产材料 + 离群对照 ",
                     ctx$outlier_ck_ids, "），故有产量小区 ", facts$n_with_yield, " 个。",
                     ctx$n_reps, " 次重复，无重复间误差可估计。",
                     "离群对照按 1.5×IQR 判定后置空，不计入有效对照基准（基准用 ",
                     facts$n_ck_valid, " 个）。"))
}

# ---- 表 形态性状分布 ------------------------------------------------------
rpt_blk_tbl_trait_dist <- function(result, ctx, meta, facts) {
  ed <- result$tables$export_data
  trait_map <- list(YeXing = "叶形", HuaSe = "花色", RongMaoSe = "茸毛色",
                    JieJiaXiXing = "结荚习性", ZhuXing = "株型",
                    LuoYeXing = "落叶性", DaoFuXing = "倒伏性")
  rows <- list()
  for (nm in names(trait_map)) {
    if (!nm %in% names(ed)) next
    v <- rpt_chr(ed[[nm]]); v[v == ""] <- NA; v[is.na(v)] <- "（未记载）"
    tb_ <- table(v); tb_ <- tb_[order(-as.integer(tb_), names(tb_))]
    for (k in seq_along(tb_)) {
      rows[[length(rows) + 1]] <- data.frame(
        性状 = trait_map[[nm]], 类别 = names(tb_)[k],
        小区数 = as.integer(tb_[k]),
        占参试比例 = sprintf("%.1f%%", as.integer(tb_[k]) / nrow(ed) * 100),
        stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, rows); rownames(out) <- NULL
  list(title = sprintf("形态性状分布（%d 小区）", nrow(ed)),
       body = rpt_md_table(out),
       note = paste0("统计口径为全部 ", nrow(ed), " 个小区（材料 ", facts$n_material,
                     " 份 + 对照 ", facts$n_ck, " 个），分母为小区总数；",
                     "空值记为「（未记载）」；占比保留 1 位小数。"))
}

# ---- 表 生育期分箱与平均亩产 ----------------------------------------------
rpt_blk_tbl_growth_bin <- function(result, ctx, meta, facts) {
  ed <- result$tables$export_data
  is_ck <- rpt_is_ck(ed)
  m <- ed[!is_ck & !is.na(ed$MuChan) & !is.na(ed$ShengYuQi), , drop = FALSE]
  g <- m$ShengYuQi
  days <- sort(unique(g))
  n <- vapply(days, function(d) sum(g == d), integer(1))
  mu <- vapply(days, function(d) mean(m$MuChan[g == d]), numeric(1))
  half <- ceiling(length(days) / 2)
  l1 <- seq_len(half); l2 <- if (length(days) > half) (half + 1):length(days) else integer(0)
  hdr <- "| 生育期（天） | 份数 | 平均亩产（kg/亩） | 生育期（天） | 份数 | 平均亩产（kg/亩） |"
  sep <- "| --- | --- | --- | --- | --- | --- |"
  body <- vapply(seq_len(half), function(i) {
    a <- paste0("| ", days[l1[i]], " | ", n[l1[i]], " | ", rpt_fmt(mu[l1[i]], 1), " |")
    if (length(l2) >= i) paste0(a, " ", days[l2[i]], " | ", n[l2[i]], " | ", rpt_fmt(mu[l2[i]], 1), " |")
    else paste0(a, " | | |")
  }, character(1))
  list(title = sprintf("生育期分箱与平均亩产（材料层，n = %d）", nrow(m)),
       body = paste(c(hdr, sep, body), collapse = "\n"),
       note = paste0("仅统计有实收产量且有生育期记载的材料小区（n = ", nrow(m),
                     "）；平均亩产为该生育期箱内材料亩产均值，保留 1 位小数。"))
}

# ---- 表 晋级筛选规则 ------------------------------------------------------
rpt_blk_tbl_screening_rules <- function(result, ctx, meta, facts) {
  th <- ctx$screening_threshold
  df <- data.frame(
    规则项 = c("晋级位次阈值", "晋级判定", "分离选单株阈值", "倒伏排除",
             "晋级结果", "分离选单株结果", "淘汰结果", "口径闭合校验",
             "亲本晋级口径阈值"),
    管线设置 = c("较临近对照位次与较平均对照位次均 ≤ 阈值（AND）",
             "两套位次同时满足才晋级", "较临近对照位次 < 阈值",
             "排除「9-严重倒」「7-重倒」小区",
             "晋级 + 分离选单株 + 淘汰 = 全部小区", "—", "—", "—",
             "管线写死较平均对照位次 ≤ 60"),
    本试验取值 = c(as.character(th), "同时满足", as.character(th),
               sprintf("本次淘汰倒伏小区 %d 个", ctx$elim_lodging),
               sprintf("晋级 %d 份", ctx$promoted_n),
               sprintf("%d 份", ctx$select_plant_n),
               sprintf("%d 个小区", ctx$eliminated_n),
               sprintf("%d + %d + %d = %d", ctx$promoted_n, ctx$select_plant_n,
                       ctx$eliminated_n, ctx$screening_closed_total),
               as.character(ctx$parent_threshold)),
    stringsAsFactors = FALSE)
  list(title = "晋级筛选规则（管线口径）",
       body = rpt_md_table(df),
       note = paste0("本表由管线实际运行参数生成：晋级位次阈值 = ", th,
                     "，倒伏排除集合为「9-严重倒」「7-重倒」；",
                     "亲本口径阈值由管线写死为 60，与筛选阈值不同口径，已在附录 A.5 披露。"))
}

# ---- 表 晋级材料（无/有性状分离） ------------------------------------------
rpt_split_promoted <- function(pr) {
  sep <- rpt_chr(pr[["性状分离"]])
  list(no_sep = pr[!nzchar(sep), , drop = FALSE],
       with_sep = pr[nzchar(sep), , drop = FALSE])
}

rpt_promoted_table <- function(pr, with_sep) {
  out <- data.frame(
    阶段名称 = as.character(pr$stageid),
    品系名称 = as.character(pr$name),
    母本 = ifelse(rpt_chr(pr$ma) == "", "—", rpt_chr(pr$ma)),
    父本 = ifelse(rpt_chr(pr$pa) == "", "—", rpt_chr(pr$pa)),
    生育期_d = rpt_fmt(pr$ShengYuQi, 0),
    亩产_kg = rpt_fmt(pr$MuChan, 2),
    较临近对照增产_pct = rpt_fmt_loose(pr$JiaoLinJinDuiZhaoZengChan),
    较临近对照位次 = rpt_fmt(pr$JiaoLinJinDuiZhaoWeiCi, 0),
    较平均对照增产_pct = rpt_fmt_loose(pr$JiaoPingJunDuiZhaoZengChan),
    较平均对照位次 = rpt_fmt(pr$JiaoPingJunDuiZhaoWeiCi, 0),
    stringsAsFactors = FALSE)
  if (with_sep) out$性状分离 <- ifelse(rpt_chr(pr[["性状分离"]]) == "", "—", rpt_chr(pr[["性状分离"]]))
  out
}

rpt_blk_tbl_promoted_no_sep <- function(result, ctx, meta, facts) {
  sp <- rpt_split_promoted(result$tables$promoted)
  n <- nrow(sp$no_sep)
  list(title = sprintf("晋级材料小区产量分析——无性状分离（%d 份）", n),
       body = rpt_md_table(rpt_promoted_table(sp$no_sep, FALSE)),
       note = paste0("来自管线 promoted 表，筛选口径为两套位次同时 ≤ ",
                     ctx$screening_threshold, " 且排除严重倒伏/重倒；",
                     "本表为未记性状分离的晋级材料，共 ", n, " 份；",
                     "小区面积 ", meta$design$plot_area_m2, " m²，位次分母为 ",
                     facts$n_plots, " 个小区。"))
}

rpt_blk_tbl_promoted_sep <- function(result, ctx, meta, facts) {
  sp <- rpt_split_promoted(result$tables$promoted)
  n <- nrow(sp$with_sep)
  list(title = sprintf("晋级材料小区产量分析——有性状分离、需提纯（%d 份）", n),
       body = rpt_md_table(rpt_promoted_table(sp$with_sep, TRUE)),
       note = paste0("性状分离判据：性状字段取值含「99-分离」，或备注/田间记载命中",
                     "「分离」「不纯」「提纯」；本表共 ", n, " 份；",
                     "小区面积 ", meta$design$plot_area_m2, " m²，位次分母为 ",
                     facts$n_plots, " 个小区。"))
}

# ---- 表 晋级位次阈值敏感性 ------------------------------------------------
rpt_blk_tbl_threshold_scan <- function(result, ctx, meta, facts) {
  scan <- rpt_threshold_scan(result$tables$export_data,
                             plant_threshold = ctx$screening_threshold)
  list(title = "晋级位次阈值敏感性（管线实跑）",
       body = rpt_md_table(scan),
       note = paste0("每个阈值均对 export_data 重新实跑一遍筛选管线；",
                     "晋级率材料口径分母为材料小区数（", facts$n_material,
                     "），小区口径分母为全部 ", facts$n_plots, " 个小区；",
                     "对照基准均值 ", rpt_fmt(facts$ck_mean), " kg/亩；",
                     "淘汰三列（位次不达标/倒伏/无位次）互相重叠，不可相加；",
                     "本次有 ", ctx$elim_unknown, " 个无位次淘汰小区、",
                     ctx$n_missing_material, " 份缺产材料。"))
}

# ---- 表 优异组合筛选 ------------------------------------------------------
rpt_blk_tbl_cross_stats <- function(result, ctx, meta, facts) {
  cs <- as.data.frame(result$tables$cross_stats)
  cs <- cs[cs$晋级品种数 > 0, , drop = FALSE]
  cs <- cs[order(-cs$晋级率, -cs$配制品种数, cs$亲本组合), , drop = FALSE]
  top <- utils::head(cs, 8)
  rest <- if (nrow(cs) > nrow(top)) cs[-seq_len(nrow(top)), , drop = FALSE] else cs[0, ]
  df <- data.frame(
    亲本组合 = top$亲本组合, 母本 = top$ma, 父本 = top$pa,
    配制品种数 = top$配制品种数, 晋级品种数 = top$晋级品种数,
    晋级率 = rpt_fmt(top$晋级率, 3), 平均亩产 = rpt_fmt(top$平均亩产, 2),
    stringsAsFactors = FALSE)
  note_extra <- ""
  if (nrow(rest)) {
    note_extra <- paste0("其余 ", nrow(rest), " 个有晋级的组合：",
      paste(vapply(seq_len(nrow(rest)), function(i) {
        sprintf("%s（%d 配 %d，%.1f%%，%s）", rest$亲本组合[i], rest$配制品种数[i],
                rest$晋级品种数[i], rest$晋级率[i] * 100, rpt_fmt(rest$平均亩产[i], 2))
      }, character(1)), collapse = "；"), "。")
  }
  list(title = "优异组合筛选",
       body = rpt_md_table(df),
       note = paste0("来自管线 cross_stats，按晋级率降序取前 8；",
                     "晋级率 = 晋级品种数 / 配制品种数；平均亩产为组合内材料层均值；",
                     note_extra))
}

# ---- 表 优异亲本筛选 ------------------------------------------------------
rpt_blk_tbl_parent_stats <- function(result, ctx, meta, facts) {
  ps <- as.data.frame(result$tables$parent_stats)
  ps <- ps[ps$晋级品种数 > 0, , drop = FALSE]
  ps <- ps[order(-ps$晋级率, -ps$配制品种数, ps$亲本名称), , drop = FALSE]
  df <- data.frame(
    亲本类型 = ps$亲本类型, 亲本名称 = ps$亲本名称,
    配制品种数 = ps$配制品种数, 晋级品种数 = ps$晋级品种数,
    晋级率 = rpt_fmt(ps$晋级率, 3), 平均亩产 = rpt_fmt(ps$平均亩产, 2),
    stringsAsFactors = FALSE)
  list(title = "优异亲本筛选",
       body = rpt_md_table(df),
       note = paste0("来自管线 parent_stats，仅保留晋级品种数 > 0 的亲本，共 ",
                     nrow(df), " 个；",
                     "亲本晋级口径的位次阈值由管线写死为 ", ctx$parent_threshold,
                     "（与筛选阈值 ", ctx$screening_threshold,
                     " 不同），差异已在附录 A.5 披露；",
                     "亲本名称为「未知」者表示库中亲本来源字段为空，不具备亲本评价意义。"))
}

# ---- 表 产量前 5 材料 -----------------------------------------------------
rpt_blk_tbl_top5 <- function(result, ctx, meta, facts) {
  tb <- result$tables
  yr <- as.data.frame(tb$yield_ranking)
  ed <- as.data.frame(tb$export_data)
  if ("对照" %in% names(yr)) yr <- yr[!(as.numeric(as.character(yr$对照)) == 1), , drop = FALSE]
  yr <- yr[!is.na(yr[["亩产 (kg)"]]), , drop = FALSE]
  yr <- yr[order(-yr[["亩产 (kg)"]]), , drop = FALSE]
  top <- utils::head(yr, 5)
  ed_key <- if ("stageid" %in% names(ed)) ed$stageid else ed[[1]]
  idx <- match(as.character(top$阶段), as.character(ed_key))
  trait_of <- function(i, col) {
    if (is.na(i) || !col %in% names(ed)) return("—")
    v <- rpt_chr(ed[[col]][i]); if (nzchar(v)) v else "—"
  }
  df <- data.frame(
    材料编号 = as.character(top$阶段),
    品系名称 = as.character(top$品种名称),
    亲本来源 = paste0(ifelse(rpt_chr(top$母本) == "", "—", rpt_chr(top$母本)),
                       " × ", ifelse(rpt_chr(top$父本) == "", "—", rpt_chr(top$父本))),
    田间性状 = vapply(seq_len(nrow(top)), function(i) {
      paste(vapply(c("JieJiaXiXing", "DaoFuXing", "HuaSe"),
                   function(cc) trait_of(idx[i], cc), character(1)), collapse = " · ")
    }, character(1)),
    亩产_kg = rpt_fmt(top[["亩产 (kg)"]], 2),
    较临近对照_pct = rpt_fmt(top[["较临近对照增产 (%)"]], 2),
    较平均对照_pct = rpt_fmt(top[["较平均对照增产 (%)"]], 2),
    生育期 = rpt_fmt(top[["生育期 (天)"]], 0),
    成熟期 = vapply(seq_len(nrow(top)), function(i) trait_of(idx[i], "ChengShuQi"), character(1)),
    倒伏性 = vapply(seq_len(nrow(top)), function(i) trait_of(idx[i], "DaoFuXing"), character(1)),
    stringsAsFactors = FALSE)
  list(title = "产量前 5 材料",
       body = rpt_md_table(df),
       note = paste0("来自管线 yield_ranking（已按亩产降序），剔除对照与无产量记录；",
                     "位次为材料层口径（分母 = 有产量的材料数 ", facts$n_mat_yield,
                     "，不含对照），两套基准都给出；",
                     "田间性状、成熟期、倒伏性取自 export_data 对应小区。"))
}

# ---- 附录 A.4 三张表 ------------------------------------------------------
rpt_field_notes_bundle <- function(result, meta) {
  empty <- list(rounds = NULL, sources = NULL, eval = NULL, ok = FALSE)
  fpath <- file.path("shared", "analysis", "report_field_notes.R")
  if (!file.exists(fpath)) return(empty)
  ok <- tryCatch({ source(fpath); TRUE }, error = function(e) FALSE)
  if (!ok) return(empty)
  ed <- as.data.frame(result$tables$export_data)
  md_path <- NULL
  cand <- c(meta$field_notes_md, Sys.getenv("RPT_FIELD_NOTES_MD"))
  cand <- cand[!is.na(cand) & nzchar(cand)]
  for (p in cand) if (file.exists(path.expand(p))) { md_path <- path.expand(p); break }
  tryCatch({
    tl <- extract_timeline_notes(md_path, corrections = FIELD_NOTE_CORRECTIONS)
    db <- extract_db_notes(ed)
    notes <- merge_field_notes(tl, db)
    cov <- summarize_field_note_coverage(tl, db, n_total = nrow(ed))
    ev <- build_field_eval_table(ed, notes, ck_variety = meta$design$ck_variety)
    list(rounds = tl, sources = cov, eval = ev, ok = TRUE)
  }, error = function(e) empty)
}

rpt_blk_tbl_a4_rounds <- function(result, ctx, meta, facts) {
  b <- facts$a4_bundle
  if (is.null(b) || !isTRUE(b$ok) || is.null(b$rounds) || nrow(b$rounds) == 0)
    return(list(title = "四次田间评价", body = "<<A.4 田间评价表待生成>>",
                note = "田间评价时间线来源缺失，本表待生成。"))
  d <- as.data.frame(b$rounds)
  df <- data.frame(次序 = seq_len(nrow(d)), 日期 = as.character(d[[1]]),
                   规模与内容 = "—", stringsAsFactors = FALSE)
  list(title = "四次田间评价", body = rpt_md_table(df),
       note = "来自田间调查评价时间线（report_field_notes.R 抽取）。")
}

rpt_blk_tbl_a4_sources <- function(result, ctx, meta, facts) {
  b <- facts$a4_bundle
  if (is.null(b) || !isTRUE(b$ok) || is.null(b$sources))
    return(list(title = "各来源覆盖情况", body = "<<A.4 田间评价表待生成>>",
                note = "田间评价来源覆盖统计待生成。"))
  list(title = "各来源覆盖情况", body = rpt_md_table(as.data.frame(b$sources)),
       note = "来自 report_field_notes.R 的来源覆盖统计。")
}

rpt_blk_tbl_a4_field_eval <- function(result, ctx, meta, facts) {
  b <- facts$a4_bundle
  if (is.null(b) || !isTRUE(b$ok) || is.null(b$eval))
    return(list(title = "材料田间评价表（按亩产降序）", body = "<<A.4 田间评价表待生成>>",
                note = "材料田间评价表待生成。"))
  list(title = "材料田间评价表（按亩产降序）",
       body = rpt_md_table(as.data.frame(b$eval)),
       note = "来自 report_field_notes.R 的田间评价抽取。")
}

# ==============================================================================
# 图块：按图表登记表取 PNG 名
# ==============================================================================
RPT_FIG_TITLES <- list(
  fig_yield_dist = "大豆品种初级产比试验亩产分布",
  fig_yield_grade = "大豆品种初级产比试验产量等级分布",
  fig_increase = "大豆品种初级产比试验较对照增产分布（较临近对照口径）",
  fig_growth = "大豆品种初级产比试验生育期分布",
  fig_scatter_growth = "大豆品种初级产比试验生育期与产量相关性分析",
  fig_comparison = "大豆品种初级产比试验筛选前后核心农艺性状比较",
  fig_parent_plot = "大豆品种初级产比试验亲本晋级表现",
  fig_radar = "大豆品种初级产比试验产量前 5 材料综合性能比较（亩产、生育期、较临近对照增产率）",
  fig_quality_huase = "大豆品种初级产比试验花色分布",
  fig_quality_yexing = "大豆品种初级产比试验叶形分布",
  fig_quality_rongmaose = "大豆品种初级产比试验茸毛色分布",
  fig_quality_jiejiaxixing = "大豆品种初级产比试验结荚习性分布",
  fig_quality_daofuxing = "大豆品种初级产比试验倒伏性分布"
)

#' 构建一个图块
rpt_blk_figure <- function(result, ctx, meta, facts, block) {
  key <- block$plot_key
  reg <- RPT_PLOT_REGISTRY
  i <- match(key, reg$plot_key)
  if (is.na(i)) return(NULL)
  png_name <- reg$png_name[i]
  alt <- sub("\\.png$", "", png_name)
  img_dir <- if (!is.null(meta$images_dir)) meta$images_dir else "图表"
  title <- RPT_FIG_TITLES[[block$id]] %||% reg$title[i]
  if (block$id == "fig_yield_dist") {
    title <- sprintf("%s（黄虚线＝全部有产量小区均值 %s kg；红虚线＝对照 %s kg）",
                     title, rpt_fmt(facts$mean_all, 1), rpt_fmt(facts$ck_mean, 1))
  }
  list(kind = "figure",
       title = title,
       body = sprintf("![%s](%s/%s)", alt, img_dir, png_name),
       note = paste0("本图由 book_plot 分析管线 run_analysis() 产出（图表登记条目：",
                     key, "），未从 PNG 读数。"))
}
