# ==============================================================================
# 试验报告专用派生统计
# ------------------------------------------------------------------------------
# 报告需要的统计口径与管线现有输出**不完全重合**，但绝不能自己重算管线已有的数。
# 规则：
#   · 管线已产出的（yield_stats / growth_stats / increase_stats / overview /
#     screening_summary / parent_stats / cross_stats）→ 直接取用，不重算
#   · 报告额外需要的（干净口径分布、离群对照识别、淘汰原因分解、
#     田间评价覆盖率）→ 在这里算，输入只用 export_data
#
# ⚠️ 列名陷阱：export_data 用**拼音**列名（stageid / name / is_ck / MuChan /
#    JiaoPingJunDuiZhaoWeiCi ...）；yield_ranking 用**中文**列名
#    （品种名称 / 亩产 (kg) / 较临近对照位次 ...）。两者不可混用。
#
# 依赖：shared/analysis/report_rules.R
# ==============================================================================

# 报告关心的「整列全空」字段（考种 / 株高）
RPT_GRAIN_TRAIT_COLS <- c("ZhuGao", "BaiLiZhong")

# 报告关心的「田间评价」字段（三期评价）
RPT_FIELD_EVAL_COLS <- c(
  苗期   = "MiaoQiTianJianPingJia",
  花期   = "HuaQiTianJianPingJia",
  成熟期 = "ChengShuQiTianJianPingJia"
)

# 形态性状字段（用于「记录稀疏」判定）
RPT_TRAIT_COLS <- c(
  花色 = "HuaSe", 叶形 = "YeXing", 茸毛色 = "RongMaoSe",
  结荚习性 = "JieJiaXiXing", 荚熟色 = "JiaShuSe", 倒伏性 = "DaoFuXing"
)

# 淘汰原因的三类命中（对应 screening_summary$breakdown 的三个键）
RPT_ELIM_REASON_PATTERNS <- list(
  rank_fail = "位次不达标",
  separated = "分离",
  lodging   = "倒伏"
)

#' 把 export_data 的关键列规范成好用的中间表
#'
#' @param ed export_data（拼音列名）
#' @return list(df, is_ck, is_mat, has_yield)
#' @export
rpt_prepare_df <- function(ed) {
  stopifnot(is.data.frame(ed), nrow(ed) > 0)
  miss <- setdiff(c("stageid", "name", "is_ck", "MuChan"), colnames(ed))
  if (length(miss) > 0) {
    stop("export_data 缺少必需列：", paste(miss, collapse = ", "))
  }
  df <- ed
  # is_ck 在库里可能是 '1'/'1.0'/'0'/'0.0' 字符串，在内存里已是 int 0/1
  df$is_ck_n <- suppressWarnings(as.numeric(as.character(df$is_ck)))
  df$is_ck_n[is.na(df$is_ck_n)] <- 0
  df$is_ck_n <- df$is_ck_n == 1
  list(df = df,
       is_ck = df$is_ck_n,
       is_mat = !df$is_ck_n,
       has_yield = !is.na(df$MuChan))
}

#' 计算报告用的全部事实（ctx）
#'
#' @param result analysis_run() 的 result（取 tables）
#' @param meta 试验元信息 list（可空）；meta$design$n_material 存在时做一致性断言
#' @param parent_threshold 亲本口径的固定位次阈值（不随筛选阈值走）
#' @return 具名事实 list，供规则谓词与模板插值使用
#' @export
compute_report_stats <- function(result, meta = NULL, parent_threshold = 60L) {
  tb <- result$tables
  p  <- rpt_prepare_df(tb$export_data)
  df <- p$df
  is_ck <- p$is_ck
  is_mat <- p$is_mat

  ck_rows  <- df[is_ck, , drop = FALSE]
  mat_rows <- df[is_mat, , drop = FALSE]

  # ---- 对照基准 ----
  # 管线已把 1.5×IQR 判定的离群对照的 MuChan 置空（原始小区产量仍在 XiaoQuChanLiang）
  ck_valid   <- ck_rows[!is.na(ck_rows$MuChan), , drop = FALSE]
  # 离群对照 = 是对照 + 亩产为空 + 但原始小区产量有值（= 被 IQR 置空，不是没测）
  outlier_ck <- ck_rows[is.na(ck_rows$MuChan) & !is.na(ck_rows$XiaoQuChanLiang), ,
                        drop = FALSE]
  ck_mean <- if (nrow(ck_valid) > 0) round(mean(ck_valid$MuChan), 2) else NA_real_

  # ---- 缺产材料 ----
  missing_mat <- mat_rows[is.na(mat_rows$MuChan), , drop = FALSE]
  n_material  <- length(unique(mat_rows$stageid))

  # ---- 位次 ----
  max_avg_rank <- suppressWarnings(max(df$JiaoPingJunDuiZhaoWeiCi, na.rm = TRUE))
  if (!is.finite(max_avg_rank)) max_avg_rank <- NA_real_

  # ---- 产量等级分箱（材料层干净口径：剔除无产量）----
  bucket_of <- function(v) {
    ifelse(v >= 200, "ge200",
           ifelse(v >= 160, "160_199",
                  ifelse(v >= 120, "120_159", "lt120")))
  }
  mat_y <- mat_rows[!is.na(mat_rows$MuChan), , drop = FALSE]
  bk <- bucket_of(mat_y$MuChan)
  n_mat_yield <- length(bk)
  grade_counts <- c(ge200 = sum(bk == "ge200"),
                    b160_199 = sum(bk == "160_199"),
                    b120_159 = sum(bk == "120_159"),
                    lt120 = sum(bk == "lt120"))
  grade_pct <- if (n_mat_yield > 0) round(grade_counts / n_mat_yield * 100, 1) else
    setNames(rep(0, 4), names(grade_counts))

  # ---- 增产率分档（材料层，两套口径）----
  inc_tally <- function(col) {
    v <- mat_rows[[col]]
    v <- v[!is.na(v)]
    list(n = length(v), gt0 = sum(v > 0), ge0 = sum(v >= 0),
         gt5 = sum(v > 5), gt8 = sum(v > 8), gt10 = sum(v > 10))
  }
  inc_avg <- inc_tally("JiaoPingJunDuiZhaoZengChan")
  inc_lin <- inc_tally("JiaoLinJinDuiZhaoZengChan")

  # ---- 生育期 ----
  growth_mean <- if (any(!is.na(mat_rows$ShengYuQi)))
    round(mean(mat_rows$ShengYuQi, na.rm = TRUE), 1) else NA_real_
  ck_growth_mean <- if (any(!is.na(ck_rows$ShengYuQi)))
    round(mean(ck_rows$ShengYuQi, na.rm = TRUE), 1) else NA_real_

  # ---- 淘汰原因分解（权威来源 = eliminated$淘汰原因）----
  # ⚠️ 口径陷阱：对照位永远进不了晋级名单，因而以「未知」计入淘汰。
  #    所以「淘汰 N」≠ N 份材料被淘汰，必须拆成 材料数 + 对照位数。
  elim <- tb$eliminated
  elim_reasons <- if (!is.null(elim) && "淘汰原因" %in% colnames(elim))
    as.character(elim$淘汰原因) else character(0)
  elim_ck <- if (!is.null(elim) && "is_ck" %in% colnames(elim))
    suppressWarnings(as.numeric(as.character(elim$is_ck)) == 1) else logical(0)
  elim_ck[is.na(elim_ck)] <- FALSE

  hit_rank <- grepl(RPT_ELIM_REASON_PATTERNS$rank_fail, elim_reasons)
  hit_sep  <- grepl(RPT_ELIM_REASON_PATTERNS$separated, elim_reasons)
  hit_lodg <- grepl(RPT_ELIM_REASON_PATTERNS$lodging,   elim_reasons)
  n_overlap <- sum(hit_rank & hit_lodg) + sum(hit_rank & hit_sep) +
    sum(hit_sep & hit_lodg) + sum(hit_rank & hit_sep & hit_lodg)

  is_unknown <- elim_reasons == "未知"
  n_unknown        <- sum(is_unknown)
  n_unknown_ck     <- sum(is_unknown & elim_ck)
  n_unknown_mat    <- sum(is_unknown & !elim_ck)

  # ---- 整列全空 / 记录稀疏 ----
  # 「整列为空」要分两类，否则规则会为错误的理由触发：
  #   ① 室内考种字段（ZhuGao/BaiLiZhong）全空 ⇒ 依赖它的图渲染出来是空白
  #   ② 某期田间评价字段全空 ⇒ 「未记录」≠「无异常」，分母不能用小区总数
  col_all_na <- function(nm) nm %in% colnames(df) && all(is.na(df[[nm]]))
  grain_na_cols <- RPT_GRAIN_TRAIT_COLS[vapply(RPT_GRAIN_TRAIT_COLS, col_all_na, logical(1))]
  stage_eval_empty <- names(RPT_FIELD_EVAL_COLS)[
    vapply(RPT_FIELD_EVAL_COLS, col_all_na, logical(1))]
  sparse_trait_cols <- names(RPT_TRAIT_COLS)[
    vapply(RPT_TRAIT_COLS, function(nm) {
      if (!nm %in% colnames(df)) return(FALSE)
      nn <- sum(!is.na(df[[nm]]) & as.character(df[[nm]]) != "")
      nn > 0 && nn < nrow(df) * 0.1
    }, logical(1))]

  # ---- 田间评价覆盖 ----
  eval_cols <- c("TianJianBeiZhu", unname(RPT_FIELD_EVAL_COLS))
  eval_cols <- eval_cols[eval_cols %in% colnames(df)]
  has_eval <- if (length(eval_cols) > 0) {
    Reduce(`|`, lapply(eval_cols, function(nm) {
      v <- df[[nm]]; !is.na(v) & as.character(v) != ""
    }))
  } else {
    rep(FALSE, nrow(df))
  }

  # ---- 父本未知 ----
  n_unknown_parent <- if ("pa" %in% colnames(df))
    sum(is_mat & (is.na(df$pa) | grepl("未知", as.character(df$pa)))) else 0L

  # ---- 筛选摘要（直接取管线，不重算）----
  ss <- tb$screening_summary
  if (is.null(ss)) ss <- list()
  g <- function(nm, default = NA) if (!is.null(ss[[nm]])) ss[[nm]] else default

  n_reps <- if ("rp" %in% colnames(df))
    length(unique(as.character(df$rp))) else NA_integer_

  list(
    n_plots = nrow(df),
    n_material = n_material,
    n_ck = sum(is_ck),
    n_ck_valid = nrow(ck_valid),
    ck_mean = ck_mean,
    n_with_yield = sum(!is.na(df$MuChan)),
    n_na_yield = sum(is.na(df$MuChan)),
    # 产量等级图 / 增产率图的分母是**全部小区**，空值被分箱逻辑归入兜底档
    n_grade_na_rows = sum(is.na(df$MuChan)),
    n_increase_na_rows = sum(is.na(df$JiaoPingJunDuiZhaoZengChan)),
    n_missing_material = nrow(missing_mat),
    missing_material_ids = if (nrow(missing_mat) > 0)
      paste(missing_mat$stageid, collapse = "、") else "",
    outlier_ck_n = nrow(outlier_ck),
    outlier_ck_ids = if (nrow(outlier_ck) > 0)
      paste(outlier_ck$stageid, collapse = "、") else "",
    ck_position_ids = if (nrow(ck_rows) > 0)
      paste(ck_rows$stageid, collapse = "、") else "",

    max_avg_ck_rank = max_avg_rank,
    screening_threshold = g("rank_threshold_select"),
    parent_threshold = parent_threshold,

    promoted_n = g("promoted_n", 0L),
    segregated_n = g("segregated_n", 0L),
    select_plant_n = g("select_plant_n", 0L),
    eliminated_n = g("eliminated_n", 0L),
    elim_material_n = sum(!elim_ck),
    elim_ck_n = sum(elim_ck),
    elim_rank_fail = sum(hit_rank),
    elim_separated = sum(hit_sep),
    elim_lodging = sum(hit_lodg),
    elim_unknown = n_unknown,
    elim_unknown_ck = n_unknown_ck,
    elim_unknown_mat = n_unknown_mat,
    elim_overlap = n_overlap,
    screening_closed_total = g("promoted_n", 0L) + g("select_plant_n", 0L) + g("eliminated_n", 0L),

    n_no_eval = sum(!has_eval),
    grain_na_cols = grain_na_cols,
    stage_eval_empty = stage_eval_empty,
    sparse_trait_cols = sparse_trait_cols,
    n_grain_traits_na = length(grain_na_cols),
    n_unknown_parent = n_unknown_parent,
    n_reps = n_reps,
    meta_material_n = if (!is.null(meta$design$n_material))
      as.integer(meta$design$n_material) else NULL,

    n_mat_yield = n_mat_yield,
    grade_counts = grade_counts,
    grade_pct = grade_pct,
    inc_avg = inc_avg,
    inc_lin = inc_lin,
    growth_mean = growth_mean,
    ck_growth_mean = ck_growth_mean
  )
}

#' 报告用的对照基准
#'
#' @param st compute_report_stats() 的返回值
#' @return list(ck_mean, n_ck, n_ck_valid)
#' @export
rpt_ck_baseline <- function(st) {
  list(ck_mean = st$ck_mean, n_ck = st$n_ck, n_ck_valid = st$n_ck_valid)
}
