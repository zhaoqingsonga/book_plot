# ==============================================================================
# 多点鉴定专用筛选：按「每点前 N% + 跨点出现率」晋级
#
# 为什么要单独写
# --------------
# analyze_screening() 是**单点**口径：按「较临近/较平均对照位次」排名，
# 一个材料只要在**任意一个点**进了阈值就算晋级。
# 用在多点试验上会严重失真：
#   实测 黄淮多点鉴定-北组 56 份材料 × 10 点，
#   按单点位次筛 ⇒ 56 份全部晋级（晋级率 100%），因为没有一份材料在
#   所有点上都很差 —— 只要有一个点冒尖就算数。
#
# 多点鉴定的真实口径（用户 2026-10-29 确认）
# ----------------------------------------
#   ① 每个点单独排名，取前 25%（该点材料数的 25%）入围
#   ② 一个材料必须在 **≥70% 的点** 都进入前 25%，才算晋级
#   ⇒ 北组实算（min_hit_rate = 0.7）：56 份材料 → 4 份晋级
#
# ⚠️ 这两个参数**由试验配置给定**（报告配置/trials/OT-*.yaml 的
#    screening.pct / screening.min_hit_rate），代码里的 0.25 / 0.8 只是默认值。
#    当前 4 个 OT 配置统一用 pct = 0.25、min_hit_rate = 0.7。
#    报告正文里的「≥70%」必须与配置一致，不能写死在代码注释里。
#
# 这个口径衡量的是**跨点稳定性**，不是单点爆发力 —— 这正是多点鉴定的目的。
# ==============================================================================

#' 多点鉴定的核心计算（不校验输入，供内部复用）
#'
#' 把「每点排名 → 入围 → 统计入围点数 → 判定晋级」抽出来，
#' 好让 `analyze_multi_site_screening()` 与 `ms_threshold_scan()`
#' 共用同一套口径 —— 扫描表和正文结论必须来自同一段代码。
#'
#' @param df export_data（已含 name/place/产量/is_ck）
#' @param pct 每个点取前百分之几
#' @param min_hit_rate 至少要在多少比例的点入围
#' @param yield_col 产量列名
#' @return list(detail, summary, hit_sets)
#' @noRd
ms_screen_core <- function(df, pct, min_hit_rate, yield_col = "MuChan") {
  ck <- if ("is_ck" %in% names(df)) suppressWarnings(as.integer(as.character(df$is_ck))) else 0L
  ck[is.na(ck)] <- 0L
  df$.is_ck <- ck

  # 只保留有产量的记录（未测产的点整体不参与排名）
  ok <- !is.na(df[[yield_col]]) & df[[yield_col]] > 0
  dat <- df[ok, , drop = FALSE]
  if (nrow(dat) == 0) {
    return(list(detail = data.frame(), summary = list(), hit_sets = list()))
  }

  sites <- sort(unique(dat$place[!is.na(dat$place) & nzchar(dat$place)]))
  n_sites <- length(sites)

  # ---- ① 每点排名，取前 pct（对照不参与排名）----
  hit <- list()   # 点 -> 入围品种名
  ranked <- list()
  for (p in sites) {
    sub <- dat[dat$place == p & dat$.is_ck == 0L, , drop = FALSE]
    if (nrow(sub) == 0) next
    sub <- sub[order(-sub[[yield_col]]), , drop = FALSE]
    k <- max(1L, as.integer(round(nrow(sub) * pct)))
    k <- min(k, nrow(sub))
    sub$点位名次 <- seq_len(nrow(sub))
    sub$点位入围 <- sub$点位名次 <= k
    ranked[[p]] <- sub[, c("name", "place", yield_col, "点位名次", "点位入围"), drop = FALSE]
    hit[[p]] <- unique(sub$name[sub$点位入围])
  }
  if (!length(ranked)) {
    return(list(detail = data.frame(), summary = list(), hit_sets = list()))
  }
  detail <- do.call(rbind, ranked)
  rownames(detail) <- NULL

  # ---- ② 统计每个材料在多少个点入围 ----
  all_names <- unique(dat$name[dat$.is_ck == 0L])
  hit_n <- vapply(all_names, function(nm) {
    sum(vapply(hit, function(hs) nm %in% hs, logical(1)))
  }, integer(1))

  # 每点材料数 → 每点名额（用于报告注）
  site_n <- vapply(sites, function(p) {
    sum(dat$place == p & dat$.is_ck == 0L)
  }, integer(1))
  site_k <- vapply(site_n, function(n) max(1L, as.integer(round(n * pct))), integer(1))

  # 跨点平均产量（晋级排序用）
  avg <- tapply(dat[[yield_col]][dat$.is_ck == 0L],
                dat$name[dat$.is_ck == 0L], mean, na.rm = TRUE)

  need <- ceiling(n_sites * min_hit_rate)
  is_promoted <- hit_n >= need

  detail_df <- data.frame(
    材料 = all_names,
    入围点数 = as.integer(hit_n),
    总点数 = n_sites,
    入围比例 = round(as.integer(hit_n) / n_sites, 3),
    跨点平均亩产 = round(as.numeric(avg[all_names]), 2),
    晋级 = is_promoted,
    stringsAsFactors = FALSE
  )
  detail_df <- detail_df[order(-detail_df$入围点数, -detail_df$跨点平均亩产), , drop = FALSE]
  rownames(detail_df) <- NULL

  list(
    detail = detail_df,
    hit_sets = hit,
    summary = list(
      n_material   = length(all_names),
      n_sites      = n_sites,
      pct          = pct,
      min_hit_rate = min_hit_rate,
      need_hits    = need,
      n_promoted   = sum(is_promoted),
      promoted_rate = if (length(all_names)) sum(is_promoted) / length(all_names) else 0,
      site_material_n = stats::setNames(site_n, sites),
      site_quota      = stats::setNames(site_k, sites)
    )
  )
}

#' 多点鉴定筛选：每点前 pct，且必须在 min_hit_rate 比例的点上入围
#'
#' @param df export_data（含 name/place/MuChan/is_ck；已含派生位次列与否都可）
#' @param pct 每个点取前百分之几（默认 0.25）
#' @param min_hit_rate 至少要在多少比例的点入围（默认 0.8）
#' @param yield_col 产量列名（默认 MuChan）
#' @return list(promoted=data.frame, detail=data.frame, summary=list(...))
#' @export
analyze_multi_site_screening <- function(df,
                                         pct = 0.25,
                                         min_hit_rate = 0.8,
                                         yield_col = "MuChan") {
  if (is.null(df) || nrow(df) == 0) {
    return(list(promoted = data.frame(), detail = data.frame(),
                summary = list(n_material = 0L, n_sites = 0L, n_promoted = 0L)))
  }
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!all(c("name", "place") %in% names(df))) {
    stop("多点筛选需要 name 与 place 两列")
  }
  if (!yield_col %in% names(df)) stop("缺少产量列：", yield_col)

  df[[yield_col]] <- suppressWarnings(as.numeric(as.character(df[[yield_col]])))
  core <- ms_screen_core(df, pct = pct, min_hit_rate = min_hit_rate, yield_col = yield_col)

  if (!nrow(core$detail)) {
    return(list(promoted = data.frame(), detail = data.frame(),
                summary = list(n_material = 0L, n_sites = 0L, n_promoted = 0L)))
  }

  promoted_names <- core$detail$材料[core$detail$晋级]
  ok <- !is.na(df[[yield_col]]) & df[[yield_col]] > 0
  promoted <- df[ok & df$name %in% promoted_names, , drop = FALSE]
  promoted <- promoted[, setdiff(names(promoted), ".is_ck"), drop = FALSE]
  rownames(promoted) <- NULL

  list(
    promoted = promoted,
    detail   = core$detail,
    summary  = core$summary
  )
}

#' 多点晋级口径敏感性扫描（每点入围比例 × 跨点出现率要求）
#'
#' ⚠️ 这个表**替代**单点口径的「晋级位次阈值敏感性」表。
#'    多点筛选根本不用位次阈值 —— 用单点阈值扫描表放在多点报告里，
#'    会得出「阈值 100 → 晋级 470 份」这种与本报告结论完全矛盾的数。
#'
#' @param df export_data
#' @param pcts 每点入围比例候选
#' @param hit_rates 跨点出现率要求候选
#' @param base_pct 本报告实际使用的 pct（用于标记）
#' @param base_hit_rate 本报告实际使用的 min_hit_rate（用于标记）
#' @param yield_col 产量列名
#' @return data.frame：每行一个 (pct, hit_rate) 组合
#' @export
ms_threshold_scan <- function(df,
                              pcts = c(0.20, 0.25, 0.30),
                              hit_rates = c(0.50, 0.60, 0.70, 0.80, 0.90),
                              base_pct = 0.25,
                              base_hit_rate = 0.8,
                              yield_col = "MuChan") {
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if (!all(c("name", "place") %in% names(df))) {
    stop("多点筛选需要 name 与 place 两列")
  }
  if (!yield_col %in% names(df)) stop("缺少产量列：", yield_col)
  df[[yield_col]] <- suppressWarnings(as.numeric(as.character(df[[yield_col]])))

  rows <- list()
  for (p in pcts) {
    for (hr in hit_rates) {
      core <- ms_screen_core(df, pct = p, min_hit_rate = hr, yield_col = yield_col)
      if (!nrow(core$detail)) next
      s <- core$summary
      pm <- core$detail[core$detail$晋级, , drop = FALSE]
      py <- pm$跨点平均亩产
      rows[[length(rows) + 1]] <- data.frame(
        每点入围比例 = sprintf("%.0f%%", p * 100),
        跨点出现率要求 = sprintf("%.0f%%", hr * 100),
        有效试点数 = s$n_sites,
        需入围点数 = s$need_hits,
        晋级份数 = s$n_promoted,
        晋级率 = sprintf("%.1f%%", s$promoted_rate * 100),
        晋级材料跨点平均亩产 = if (length(py)) sprintf("%.2f", mean(py)) else "—",
        晋级材料亩产区间 = if (length(py))
          sprintf("%.2f–%.2f", min(py), max(py)) else "—",
        本报告口径 = if (isTRUE(all.equal(p, base_pct)) && isTRUE(all.equal(hr, base_hit_rate)))
          "✓ 采用" else "",
        stringsAsFactors = FALSE)
    }
  }
  if (!length(rows)) return(data.frame())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}
