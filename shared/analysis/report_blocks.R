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
#
# 多点鉴定口径依赖 shared/analysis/multi_site_screening.R（多点筛选 + 口径扫描）。
# 本文件被 source() 时可能还没加载它 ⇒ 这里按项目根相对路径补一次
# （与 analysis_module.R 里加载 other_trials_loader.R 同一套路）。
# ==============================================================================
if (!exists("ms_threshold_scan", mode = "function")) {
  for (.p in c("shared/analysis/multi_site_screening.R",
               "../shared/analysis/multi_site_screening.R")) {
    if (file.exists(.p)) { source(.p); break }
  }
}

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
    # ⚠️ 参试材料份数 = 去重后的材料个体数，不是记录行数。
    #    单点试验里一材料一行，nrow(mat) 就是份数；
    #    多点试验里一材料每点一行（56 品种 × 11 点 = 616 行），
    #    nrow(mat) 会得出 616 —— 实测就是这么错的（应为 56）。
    #    ⇒ 按 name（品种标识）去重；没有 name 列时退回 stageid。
    n_material = {
      key <- if ("name" %in% names(mat)) mat$name else mat$stageid
      length(unique(key[!is.na(key) & nzchar(as.character(key))]))
    },
    n_records = nrow(ed),
    n_material_records = nrow(mat),
    # 是否多点试验（一材料多行）—— 决定摘要/说明里的口径措辞
    is_multi_site = {
      np <- if ("place" %in% names(ed)) length(unique(ed$place[!is.na(ed$place) & nzchar(as.character(ed$place))])) else 1L
      np > 1L
    },
    n_place = {
      if ("place" %in% names(ed)) length(unique(ed$place[!is.na(ed$place) & nzchar(as.character(ed$place))])) else 1L
    },
    # 多点鉴定筛选口径参数（供报告正文引用，避免正文与配置脱节）
    ms_pct_pct = {
      v <- ctx$ms_pct
      if (!is.null(v) && is.finite(v)) round(v * 100) else NA_real_
    },
    ms_hit_pct = {
      v <- ctx$ms_min_hit_rate
      if (!is.null(v) && is.finite(v)) round(v * 100) else NA_real_
    },
    # 晋级率（跨点稳定性口径）：晋级份数 / 材料份数
    promoted_pct = {
      nm <- {
        key <- if ("name" %in% names(mat)) mat$name else mat$stageid
        length(unique(key[!is.na(key) & nzchar(as.character(key))]))
      }
      pn <- if (!is.null(ctx$promoted_n) && is.finite(ctx$promoted_n)) ctx$promoted_n else NA_real_
      if (nm > 0 && !is.na(pn)) round(pn / nm * 100, 1) else NA_real_
    },
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
    # 相关性方向的措辞（**按符号取值**，不能写死「弱正相关」）
    # ⚠️ 写死会自相矛盾：实测北组 r = -0.27，报告却写「呈弱正相关」。
    #    措辞分三档：|r|<0.2 弱、0.2~0.4 中、>0.4 强；方向由符号定。
    corr_growth_yield_word = {
      r0 <- suppressWarnings(as.numeric(corr_g))
      if (is.na(r0)) "未计算"
      else {
        dir_w <- if (r0 < 0) "负" else "正"
        str_w <- if (abs(r0) < 0.3) "弱" else if (abs(r0) < 0.5) "中等" else "强"
        paste0(str_w, dir_w, "相关")
      }
    },
    n_combos = get_ov("杂交组合数"),
    maturity_text = maturity_text,
    trait_text = trait_text
  )
  for (nm in names(ctx)) if (is.null(facts[[nm]])) facts[[nm]] <- ctx[[nm]]
  # ---- 扁平化：模板里写死的是 {{inc_avg_gt0}} / {{grade_ge200_n}} 这类标量槽位 ----
  # ctx 里存的是嵌套结构（inc_avg / inc_lin 是 list，grade_counts / grade_pct 是命名向量），
  # 不扁平化 rpt_fill_facts 取不到 → 数字被替换成空串（静默失数，报告里出现「  份增产」）。
  ia <- ctx$inc_avg; il <- ctx$inc_lin
  facts$inc_avg_gt0  <- if (!is.null(ia)) ia$gt0  else NA_integer_
  facts$inc_avg_gt5  <- if (!is.null(ia)) ia$gt5  else NA_integer_
  facts$inc_avg_gt8  <- if (!is.null(ia)) ia$gt8  else NA_integer_
  facts$inc_avg_gt10 <- if (!is.null(ia)) ia$gt10 else NA_integer_
  facts$inc_lin_gt0  <- if (!is.null(il)) il$gt0  else NA_integer_
  facts$inc_lin_gt5  <- if (!is.null(il)) il$gt5  else NA_integer_
  facts$inc_lin_gt8  <- if (!is.null(il)) il$gt8  else NA_integer_
  facts$inc_lin_gt10 <- if (!is.null(il)) il$gt10 else NA_integer_
  facts$grade_ge200_n   <- if (!is.null(ctx$grade_counts)) ctx$grade_counts[["ge200"]] else NA_integer_
  facts$grade_ge200_pct <- if (!is.null(ctx$grade_pct))    ctx$grade_pct[["ge200"]]    else NA_real_
  # 模板 §7.2 用的槽位名是 {{trait_dist_text}}，facts 里存的是 trait_text —— 给个别名
  facts$trait_dist_text <- facts$trait_text
  # 多点判定显式化（模板与摘要都按它分「份」与「材料·点次」两套口径）
  facts$is_multi_site <- isTRUE(facts$is_multi_site)
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
  # ⚠️ 多点鉴定体裁：规则表必须换口径。
  #    单点规则表的条目（位次阈值 / 分离选单株 / 倒伏排除 / 口径闭合校验）
  #    在多点报告里全部不适用 —— 实测会印出「晋级位次阈值 = NA」
  #    「两套位次同时满足才晋级」，与正文的跨点稳定性口径自相矛盾。
  if (isTRUE(facts$is_multi_site)) {
    return(rpt_blk_tbl_ms_screening_rules(result, ctx, meta, facts))
  }
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

# ---- 表 多点晋级筛选规则（多点鉴定体裁）----------------------------------
# 多点鉴定的规则是「每点入围比例 + 跨点出现率」两条件同时满足，
# 与单点的「位次阈值 + 分离选单株 + 倒伏排除」是完全不同的三步。
# 有效试点数 / 需入围点数取自管线实跑结果（multi_site_summary），
# 与正文结论、敏感性扫描表同源。
rpt_blk_tbl_ms_screening_rules <- function(result, ctx, meta, facts) {
  pct  <- if (!is.null(ctx$ms_pct) && is.finite(ctx$ms_pct)) ctx$ms_pct else NA_real_
  rate <- if (!is.null(ctx$ms_min_hit_rate) && is.finite(ctx$ms_min_hit_rate))
    ctx$ms_min_hit_rate else NA_real_
  need <- if (!is.null(ctx$ms_need_hits) && is.finite(ctx$ms_need_hits))
    as.integer(ctx$ms_need_hits) else NA_integer_

  # 有效试点数 = 有产量数据的试点数（未测产的点不参与排名）
  ms_sum <- result$tables$multi_site_summary
  n_valid <- if (!is.null(ms_sum) && !is.null(ms_sum$n_sites))
    as.integer(ms_sum$n_sites) else NA_integer_
  if (is.na(n_valid)) {
    ed <- result$tables$export_data
    if (!is.null(ed) && nrow(ed) > 0 && "place" %in% names(ed)) {
      n_valid <- length(unique(ed$place[!is.na(ed$MuChan)]))
    }
  }

  nm <- facts$n_material
  pn <- ctx$promoted_n
  pct_s   <- if (is.na(pct))  "—" else sprintf("%.0f%%", pct * 100)
  rate_s  <- if (is.na(rate)) "—" else sprintf("%.0f%%", rate * 100)
  valid_s <- if (is.na(n_valid)) "—" else sprintf("%d 个", n_valid)
  need_s  <- if (is.na(need))  "—" else sprintf("%d 个", need)
  pr_s    <- if (!is.na(nm) && nm > 0) sprintf("%.1f%%", pn / nm * 100) else "—"

  df <- data.frame(
    规则项 = c("晋级口径", "每点入围比例", "跨点出现率要求", "有效试点数",
             "需入围点数", "晋级判定", "晋级结果", "晋级率",
             "分离选单株", "倒伏排除"),
    管线设置 = c("「每点入围比例 × 跨点出现率」两条件同时满足",
             "每点内按亩产排名，取前 N% 入围",
             "入围点占有效试点数的最小比例",
             "有产量数据的试点数（未测产的点不计入）",
             "有效试点数 × 跨点出现率要求，向上取整",
             "两个条件同时满足才晋级",
             "晋级份数（材料口径）",
             "晋级份数 / 参试材料份数",
             "单点口径的小区级单株选择",
             "单点口径的小区级倒伏淘汰"),
    本试验取值 = c(
      if (is.na(pct) || is.na(rate)) "—"
      else sprintf("每点前 %s 且 ≥%s 的点入围", pct_s, rate_s),
      pct_s, rate_s, valid_s, need_s, "同时满足",
      sprintf("晋级 %d 份", pn), pr_s,
      "不适用（多点鉴定不做小区级单株选择）",
      "不适用（多点鉴定不做小区级淘汰）"),
    stringsAsFactors = FALSE)

  list(title = "晋级筛选规则（多点跨点稳定性口径）",
       body = rpt_md_table(df),
       note = paste0("本表由管线实际运行参数生成：每点入围比例 = ", pct_s,
                     "，跨点出现率要求 = ", rate_s,
                     "，有效试点数 = ", valid_s,
                     "（未测产的点不计入排名），需入围点数 = ", need_s, "。",
                     "⚠️ 多点鉴定按「每点入围比例 + 跨点出现率」判定晋级，",
                     "不使用单点的位次阈值，也不做小区级单株选择与倒伏淘汰。"))
}

# ---- 表 晋级材料（无/有性状分离） ------------------------------------------
rpt_split_promoted <- function(pr) {
  # 判据与 Excel 的两张晋级 sheet 共用（analysis_screening.R）
  split_promoted_by_segregation(pr)
}

rpt_promoted_table <- function(pr, with_sep, eval_map = NULL) {
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
  out$田间评价 <- rpt_lookup_field_notes(pr$stageid, eval_map)
  out
}

#' 从 A.4 田间评价表取「材料 ID → 田间评价文字」映射
#'
#' A.4 的「材料田间评价表」只列**有评价文字**的小区（drop_empty = TRUE），
#' 所以这里拿到的映射天然只覆盖有田间记录的材料；查不到就写「—」。
#'
#' @param facts 事实卡（含 a4_bundle）
#' @return named character（名字 = stageid），拿不到返回 NULL
#' @noRd
rpt_eval_map <- function(facts) {
  ev <- facts$a4_bundle$eval
  if (is.null(ev) || !is.data.frame(ev)) return(NULL)
  if (!all(c("stageid", "田间评价") %in% names(ev))) return(NULL)
  ids <- trimws(as.character(ev$stageid))
  setNames(as.character(ev[["田间评价"]]), ids)
}

#' 按 ID 取田间评价文字；查不到写「—」
#'
#' 单元格里的 ASCII 竖线会破坏 Markdown 表格，统一转义。
#'
#' @param ids 小区/材料 ID
#' @param eval_map rpt_eval_map() 的结果
#' @return 与 ids 等长的字符向量
#' @noRd
rpt_lookup_field_notes <- function(ids, eval_map) {
  ids <- trimws(as.character(ids))
  ids[is.na(ids)] <- ""
  out <- if (is.null(eval_map)) rep("", length(ids)) else unname(eval_map[ids])
  out[is.na(out)] <- ""
  out <- gsub("|", "\\|", out, fixed = TRUE)
  ifelse(nzchar(trimws(out)), out, "—")
}

rpt_blk_tbl_promoted_no_sep <- function(result, ctx, meta, facts) {
  sp <- rpt_split_promoted(result$tables$promoted)
  n <- nrow(sp$no_sep)
  list(title = sprintf("晋级材料小区产量分析——无性状分离（%d 份）", n),
       body = rpt_md_table(rpt_promoted_table(sp$no_sep, FALSE, rpt_eval_map(facts))),
       note = paste0("来自管线 promoted 表，筛选口径为两套位次同时 ≤ ",
                     ctx$screening_threshold, " 且排除严重倒伏/重倒；",
                     "本表为未记性状分离的晋级材料，共 ", n, " 份；",
                     "小区面积 ", meta$design$plot_area_m2, " m²，位次分母为 ",
                     facts$n_plots, " 个小区。",
                     "末列「田间评价」取自附录 A.4 的材料田间评价表（同小区 ID 关联），",
                     "无田间文字记录者写「—」。"))
}

rpt_blk_tbl_promoted_sep <- function(result, ctx, meta, facts) {
  sp <- rpt_split_promoted(result$tables$promoted)
  n <- nrow(sp$with_sep)
  list(title = sprintf("晋级材料小区产量分析——有性状分离、需提纯（%d 份）", n),
       body = rpt_md_table(rpt_promoted_table(sp$with_sep, TRUE, rpt_eval_map(facts))),
       note = paste0("性状分离判据：性状字段取值含「99-分离」，或备注/田间记载命中",
                     "「分离」「不纯」「提纯」；本表共 ", n, " 份；",
                     "小区面积 ", meta$design$plot_area_m2, " m²，位次分母为 ",
                     facts$n_plots, " 个小区。",
                     "末列「田间评价」取自附录 A.4 的材料田间评价表（同小区 ID 关联），",
                     "无田间文字记录者写「—」。"))
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

# ---- 表 多点晋级口径敏感性（多点鉴定体裁）--------------------------------
# ⚠️ 这个表**替代**单点口径的「晋级位次阈值敏感性」表。
#    多点筛选不用位次阈值 —— 把单点阈值扫描表放进多点报告，
#    会得出「阈值 100 → 晋级 470 份」，与本报告「晋级 4 份」的结论直接矛盾。
rpt_blk_tbl_ms_threshold_scan <- function(result, ctx, meta, facts) {
  ed <- result$tables$export_data
  if (is.null(ed) || nrow(ed) == 0) return(NULL)
  if (!exists("ms_threshold_scan", mode = "function")) return(NULL)
  base_pct  <- if (!is.null(ctx$ms_pct) && is.finite(ctx$ms_pct)) ctx$ms_pct else 0.25
  base_rate <- if (!is.null(ctx$ms_min_hit_rate) && is.finite(ctx$ms_min_hit_rate))
    ctx$ms_min_hit_rate else 0.8
  scan <- tryCatch(
    ms_threshold_scan(ed, base_pct = base_pct, base_hit_rate = base_rate),
    error = function(e) NULL)
  if (is.null(scan) || nrow(scan) == 0) return(NULL)
  list(title = "多点晋级口径敏感性（每点入围比例 × 跨点出现率要求）",
       body = rpt_md_table(scan),
       note = paste0("每个组合都对 export_data 重新实跑一遍多点筛选管线（与正文结论同源同一段代码）；",
                     "「有效试点数」= 有产量数据的试点数（未测产的点不计入）；",
                     "「需入围点数」= 有效试点数 × 跨点出现率要求，向上取整；",
                     "本报告采用的组合标「✓ 采用」。",
                     "⚠️ 本表不是单点口径的位次阈值敏感性 —— 多点筛选按「每点入围比例 + 跨点出现率」判定。"))
}

# ---- 表 跨点平均表现与增产点次（多点鉴定核心）----------------------------
rpt_blk_tbl_cross_location_avg <- function(result, ctx, meta, facts) {
  ca <- as.data.frame(result$tables$cross_location_avg)
  if (is.null(ca) || nrow(ca) == 0) return(NULL)
  ca <- ca[order(-ca$平均亩产), , drop = FALSE]
  n_all <- nrow(ca)
  top <- utils::head(ca, 15)
  df <- data.frame(
    品种 = top$品种名称,
    平均亩产 = rpt_fmt(top$平均亩产, 2),
    增产点次 = paste0(top$增产点次, "/", top$地点数),
    较平均对照 = paste0(rpt_fmt(top$较平均对照增产_pct, 2), "%"),
    生育期 = rpt_fmt(top$生育期_d, 0),
    株高 = rpt_fmt(top$株高_cm, 1),
    百粒重 = rpt_fmt(top$百粒重_g, 2),
    stringsAsFactors = FALSE
  )
  list(title = sprintf("跨点平均表现与增产点次（%d 份材料）", n_all),
       body = rpt_md_table(df),
       note = paste0("来自管线 cross_location_avg；按跨点平均亩产降序取前 ", nrow(df),
                     "（共 ", n_all, " 份材料，不含对照）。",
                     "增产点次 = 该材料较对照增产的**有效**试点数 / 该材料有产量数据的有效试点数",
                     "（未测产的点不计入分母）；",
                     "生育期、株高、百粒重为各点均值。"))
}

# ---- 表 各点亩产矩阵（品种×地点）------------------------------------------
rpt_blk_tbl_cross_site_matrix <- function(result, ctx, meta, facts) {
  cr <- as.data.frame(result$tables$cross_site_ranking)
  if (is.null(cr) || nrow(cr) == 0) return(NULL)
  # 去掉 stageid（与 name 重复）
  cr$stageid <- NULL
  top <- utils::head(cr, 15)
  # 数值列保留 1 位
  for (cc in setdiff(names(top), c("name", "排名"))) {
    top[[cc]] <- ifelse(is.na(top[[cc]]), "—", formatC(top[[cc]], format = "f", digits = 1))
  }
  names(top)[names(top) == "name"] <- "品种"
  list(title = sprintf("各点亩产矩阵（kg/亩，按跨点平均排序，前 %d 份）", nrow(top)),
       body = rpt_md_table(top),
       note = paste0("来自管线 cross_site_ranking；每行为一个品种在各试点的亩产，「—」表示该点无产量数据；",
                     "排名按各点平均亩产排序。空白点不影响其余点的统计。"))
}

# ---- 表 GGE 稳定性参数 ----------------------------------------------------
rpt_blk_tbl_gge_stability <- function(result, ctx, meta, facts) {
  gs <- result$tables$gge_stability
  if (is.null(gs)) return(NULL)
  gs <- as.data.frame(gs)
  if (nrow(gs) == 0) return(NULL)
  keep <- intersect(c("品种", "name", "平均亩产", "产量", "稳定性", "SD", "类型",
                      "Category"), names(gs))
  if (!length(keep)) keep <- names(gs)[seq_len(min(6, ncol(gs)))]
  df <- gs[, keep, drop = FALSE]
  for (cc in names(df)) {
    if (is.numeric(df[[cc]])) df[[cc]] <- formatC(df[[cc]], format = "f", digits = 2)
  }
  list(title = "GGE 稳定性参数",
       body = rpt_md_table(df),
       note = paste0("来自管线 gge_stability（metan 的 GGE 双标图分析）。",
                     "稳定性参数越小表示该品种在多点间的表现越稳定；",
                     "结合平均产量一起看：既高产又稳定的材料才是理想的广适性品种。"))
}

# ---- 表 优异组合筛选 ------------------------------------------------------
rpt_blk_tbl_cross_stats <- function(result, ctx, meta, facts) {
  if (is.null(result$tables$cross_stats)) return(NULL)
  cs <- as.data.frame(result$tables$cross_stats)
  if (nrow(cs) == 0 || !"晋级品种数" %in% names(cs)) return(NULL)
  cs <- cs[!is.na(cs$晋级品种数) & cs$晋级品种数 > 0, , drop = FALSE]
  if (nrow(cs) == 0) return(NULL)
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
  if (is.null(result$tables$parent_stats)) return(NULL)
  ps <- as.data.frame(result$tables$parent_stats)
  if (nrow(ps) == 0 || !"晋级品种数" %in% names(ps)) return(NULL)
  ps <- ps[!is.na(ps$晋级品种数) & ps$晋级品种数 > 0, , drop = FALSE]
  if (nrow(ps) == 0) return(NULL)
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
    # id_pattern 必须**按试验配**：不同试验的田间 ID 前缀长度不同
    #   · N26E001 / N26M7017  → ^[A-Z]\d{2}[A-Z]\d{3,4}$（默认）
    #   · N26CB001（商业育种）  → 前缀是 **两个** 字母，默认正则匹配不上
    #     ⇒ 硬用默认正则会「一条都提取不到」，A.4 田间评价表整段留白。
    # 配置项 field_notes_id_pattern 可覆盖；没配就按材料 ID 的实际形态自动放宽。
    id_pat <- meta$field_notes_id_pattern
    if (is.null(id_pat) || !nzchar(as.character(id_pat)[1])) {
      id_pat <- .rpt_infer_id_pattern(ed)
    }
    # ⚠️ 外部导入试验（other_trials）**没有巡田时间线 md** ⇒ md_path 为 NULL。
    #    原实现直接调 extract_timeline_notes(NULL)，报错后整个 bundle 返回 empty，
    #    A.4 三个表全部留白。这里改成：无 md 时只用数据表自带的评价字段。
    if (is.null(md_path) || !file.exists(md_path)) {
      db <- extract_db_notes(ed)
      notes <- db
      cov <- tryCatch(summarize_field_note_coverage(NULL, db, n_total = nrow(ed)),
                      error = function(e) NULL)
      ev <- build_field_eval_table(ed, notes, ck_variety = meta$design$ck_variety)
      return(list(rounds = NULL, sources = cov, eval = ev, ok = TRUE))
    }
    tl <- extract_timeline_notes(md_path, id_pattern = id_pat,
                                 corrections = FIELD_NOTE_CORRECTIONS)
    db <- extract_db_notes(ed)
    notes <- merge_field_notes(tl, db)
    cov <- summarize_field_note_coverage(tl, db, n_total = nrow(ed))
    ev <- build_field_eval_table(ed, notes, ck_variety = meta$design$ck_variety)
    # 轮次汇总：优先取 md 里的「评价轮次」表；取不到就按时间线的日期标签聚合
    rounds <- extract_rounds_summary(md_path)
    if (is.null(rounds)) rounds <- .rpt_rounds_from_timeline(tl)
    list(rounds = rounds, sources = cov, eval = ev, ok = TRUE)
  }, error = function(e) empty)
}

# ---- 按材料 ID 的实际形态推断 id_pattern ---------------------------------
#' 从 export_data 的 stageid 推出能匹配全部 ID 的正则
#'
#' 田间 ID 形如 N26E001 / N26CB001 / G25M1234：
#'   前缀字母段长度不固定（1 位或 2 位），默认正则只认 1 位。
#' 这里取「字母段最长」的那个形态来生成正则，保证不漏。
#'
#' @param ed export_data
#' @return 正则字符串
#' @noRd
.rpt_infer_id_pattern <- function(ed) {
  ids <- as.character(ed$stageid)
  ids <- ids[!is.na(ids) & nzchar(ids)]
  if (!length(ids)) return("^[A-Z]\\d{2}[A-Z]\\d{3,4}$")
  # 拆成「字母段 / 数字段 / 字母段 / 数字段」
  # ⚠️ regmatches() 返回 list，不是矩阵 ⇒ 用 do.call(rbind) 摊平再过滤
  m <- regmatches(ids, regexec("^([A-Za-z]+)(\\d+)([A-Za-z]*)(\\d+)$", ids))
  m <- m[vapply(m, length, integer(1)) == 5L]
  if (!length(m)) return("^[A-Z]\\d{2}[A-Z]\\d{3,4}$")
  mm <- do.call(rbind, m)
  pre <- mm[, 2]; mid <- mm[, 4]
  # 字母段可能有 1~2 位；数字段长度也可能不同 → 取最宽的
  pre_ch <- paste0("[A-Za-z]{", 1L, ",", max(nchar(pre)), "}")
  mid_ch <- paste0("[A-Za-z]{", 0L, ",", max(nchar(mid)), "}")
  d1 <- max(nchar(mm[, 3])); d2 <- max(nchar(mm[, 5]))
  paste0("^", pre_ch, "\\d{", 1L, ",", d1, "}", mid_ch, "\\d{", 1L, ",", d2, "}$")
}

# ---- 轮次汇总的兜底口径：按时间线的日期标签聚合 ----------------------------
#' md 里没有「评价轮次」表时，用长表时间线的 date_tag 聚合出轮次汇总
#'
#' @param tl extract_timeline_notes() 的结果（长表）
#' @return data.frame(轮次, 日期, 生育期, 评价材料数)；无数据时返回 NULL
#' @noRd
.rpt_rounds_from_timeline <- function(tl) {
  if (is.null(tl) || !is.data.frame(tl) || !nrow(tl)) return(NULL)
  if (!"date_tag" %in% names(tl)) return(NULL)
  tag <- rpt_chr(tl$date_tag)
  tag <- tag[nzchar(tag)]
  if (!length(tag)) return(NULL)
  # MM-DD → 当年 YYYY-MM-DD（巡田笔记只记月日）
  yr <- format(Sys.Date(), "%Y")
  key <- paste0(yr, "-", tag)
  agg <- tapply(unique(tl$id), key, length)
  df <- data.frame(轮次 = seq_along(agg),
                   日期 = names(agg),
                   生育期 = "—",
                   评价材料数 = as.integer(agg),
                   stringsAsFactors = FALSE)
  df[order(df$日期), , drop = FALSE]
}

rpt_blk_tbl_a4_rounds <- function(result, ctx, meta, facts) {
  b <- facts$a4_bundle
  if (is.null(b) || !isTRUE(b$ok) || is.null(b$rounds) || nrow(b$rounds) == 0)
    return(list(title = "田间评价",
                body = "本试验**无分期田间评价记录**（数据来源中评价轮次字段为空），故本表不列。",
                note = paste0("本试验为外部导入的多点鉴定数据，导入内容不含巡田时间线与分期评价，",
                              "因此无评价轮次可列。相关形态与抗性信息见 7.3 各性状分布。")))
  d <- as.data.frame(b$rounds)
  col_日期   <- if ("日期" %in% names(d)) as.character(d[["日期"]]) else as.character(d[[1]])
  col_生育期 <- if ("生育期" %in% names(d)) as.character(d[["生育期"]]) else rep("—", nrow(d))
  col_数量   <- if ("评价材料数" %in% names(d)) as.character(d[["评价材料数"]]) else rep("—", nrow(d))
  df <- data.frame(
    次序 = seq_len(nrow(d)),
    日期 = col_日期,
    生育期 = col_生育期,
    评价材料数 = col_数量,
    规模与内容 = paste0("本轮评价 ", col_数量, " 个材料（", col_生育期, "）"),
    stringsAsFactors = FALSE)
  list(title = "田间评价", body = rpt_md_table(df),
       note = paste0("来自田间调查评价时间线（report_field_notes.R 抽取「评价轮次」小节）；",
                     "评价材料数为该轮实际被评价的材料份数，未被评价的材料不进本表。"))
}

rpt_blk_tbl_a4_sources <- function(result, ctx, meta, facts) {
  b <- facts$a4_bundle
  if (is.null(b) || !isTRUE(b$ok) || is.null(b$sources))
    return(list(title = "各来源覆盖情况",
                body = "本试验**无外部田间评价来源**（无巡田笔记、无时间线文档），故不统计来源覆盖。",
                note = "外部导入的多点鉴定数据只含表格式记录，无独立的田间观察来源文档。"))
  list(title = "各来源覆盖情况", body = rpt_md_table(as.data.frame(b$sources)),
       note = "来自 report_field_notes.R 的来源覆盖统计。")
}

rpt_blk_tbl_a4_field_eval <- function(result, ctx, meta, facts) {
  b <- facts$a4_bundle
  if (is.null(b) || !isTRUE(b$ok) || is.null(b$eval))
    return(list(title = "材料田间评价表（按亩产降序）",
                body = "本试验**无逐材料田间文字评价**（田间备注与分期评价字段全为空），故本表不列。",
                note = paste0("导入数据中 TianJianBeiZhu、苗期/花期/成熟期评价、病害记载等字段",
                              "均为空列；材料表现请见 7.2 跨点表现与 7.3 性状分布。")))
  list(title = "材料田间评价表（按亩产降序）",
       body = rpt_md_table(as.data.frame(b$eval)),
       note = "来自 report_field_notes.R 的田间评价抽取。")
}

# ==============================================================================
# 图块：按图表登记表取 PNG 名
# ==============================================================================
# 图题模板。{体裁} 由 rpt_fig_title() 按 meta$体裁 / template_id 替换：
#   单点产比 → 「初级产比试验」   多点鉴定 → 「多点鉴定试验」
# ⚠️ 原实现把「大豆品种初级产比试验」写死 13 处，
#    多点鉴定报告的图题全部错叫「初级产比试验」（实测 OT-中组 PDF 图题就是错的）。
RPT_FIG_TITLES <- list(
  fig_yield_dist = "大豆品种{体裁}亩产分布",
  fig_yield_grade = "大豆品种{体裁}产量等级分布",
  fig_increase = "大豆品种{体裁}较对照增产分布（较临近对照口径）",
  fig_growth = "大豆品种{体裁}生育期分布",
  fig_scatter_growth = "大豆品种{体裁}生育期与产量相关性分析",
  fig_scatter_height = "大豆品种{体裁}株高与亩产关系",
  fig_comparison = "大豆品种{体裁}筛选前后核心农艺性状比较",
  fig_parent_plot = "大豆品种{体裁}亲本晋级表现",
  fig_radar = "大豆品种{体裁}产量前 5 材料综合性能比较（亩产、生育期、较临近对照增产率）",
  fig_quality_huase = "大豆品种{体裁}花色分布",
  fig_quality_yexing = "大豆品种{体裁}叶形分布",
  fig_quality_rongmaose = "大豆品种{体裁}茸毛色分布",
  fig_quality_jiejiaxixing = "大豆品种{体裁}结荚习性分布",
  fig_quality_daofuxing = "大豆品种{体裁}倒伏性分布",
  fig_gge_biplot = "大豆品种{体裁}GGE 双标图（品种×地点）",
  fig_gge_heatmap = "大豆品种{体裁}GGE 稳定性热图",
  fig_gge_ranking = "大豆品种{体裁}GGE 品种排名"
)

#' 按体裁取图题（把 {体裁} 换成实际体裁名）
#' @noRd
rpt_fig_title <- function(key, meta = NULL) {
  tpl <- RPT_FIG_TITLES[[key]]
  if (is.null(tpl)) return(NULL)
  genre <- if (!is.null(meta$体裁) && nzchar(as.character(meta$体裁))) {
    g <- as.character(meta$体裁)
    # 「常规-多点鉴定」→「多点鉴定试验」；「常规-初级产比」→「初级产比试验」
    tail <- sub("^[^-]+-", "", g)
    paste0(tail, "试验")
  } else if (identical(meta$template_id, "multi_site")) {
    "多点鉴定试验"
  } else {
    "初级产比试验"
  }
  gsub("{体裁}", genre, tpl, fixed = TRUE)
}

#' 构建一个图块
rpt_blk_figure <- function(result, ctx, meta, facts, block) {
  key <- block$plot_key
  reg <- RPT_PLOT_REGISTRY
  i <- match(key, reg$plot_key)
  if (is.na(i)) return(NULL)
  png_name <- reg$png_name[i]
  alt <- sub("\\.png$", "", png_name)
  img_dir <- if (!is.null(meta$images_dir)) meta$images_dir else "图表"
  title <- rpt_fig_title(block$id, meta) %||% reg$title[i]
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
