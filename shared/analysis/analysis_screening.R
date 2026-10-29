# ==============================================================================
# 品种筛选 + 筛选前后对比图
# 从参考脚本 00-main_function.R 的 screen_material_promotion() 和
# plot_selection_comparison_addck() 移植
# ==============================================================================

# 默认晋级位次阈值：取参试材料数的前 25%（粗算，够用即可）
DEFAULT_RANK_THRESHOLD_PCT <- 0.25

#' 由参试材料数算出默认晋级位次阈值
#'
#' @param n_material 参试材料数（不含对照）
#' @param pct 比例，默认 0.25
#' @return 整数位次阈值（>= 1）
#' @export
default_rank_threshold_from_n <- function(n_material,
                                          pct = DEFAULT_RANK_THRESHOLD_PCT) {
  if (is.null(n_material) || length(n_material) != 1L ||
      !is.finite(n_material) || n_material <= 0) {
    return(1L)
  }
  max(1L, as.integer(ceiling(pct * n_material)))
}

#' 统计参试材料数（对照小区不计入）
#'
#' ⚠️ 多点 / 多重复试验必须按材料去重，否则材料数会被地点数、重复数放大：
#' 同一材料在每个地点、每个重复各占一行，直接数行数会把分母放大 5~10 倍，
#' 导致「参试材料数的前 25%」这个阈值算出来远大于真实值（筛选通道形同虚设）。
#'
#' 口径：**先按「地点 × 重复」分组，每组内按材料身份去重，取各组里的最大组**。
#' - 分组键：地点（place / 地点）× 重复（rp / 重复）
#' - 材料身份：优先 stageid（阶段编号），退化时用 name（品种名称）
#' - 取「最大组」而不是「所有组求和」：因为位次是在每个 (地点, 重复) 组内各算一套，
#'   阈值要跟同一套位次可比，所以分母取「一个组里有多少个材料」。
#'
#' ⚠️ 不能改用「按品种名去重」：库里存在同名不同材料（stageid 不同、name 相同），
#'   按 name 去重会把不同材料合并成一个。
#'
#' @param df 数据框（拼音列名 is_ck / place / rp / stageid / name，
#'   或参考列名 是否对照 / 地点 / 品种名称 均可）
#' @return 整数参试材料数
#' @export
count_materials <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(0L)

  # ---- 排除对照小区 ----
  ck_col <- intersect(c("是否对照", "is_ck", "对照"), colnames(df))
  if (length(ck_col) > 0) {
    ck <- control_flag(df[[ck_col[1]]])
    ck[is.na(ck)] <- FALSE
    df <- df[!ck, , drop = FALSE]
  }
  if (nrow(df) == 0) return(0L)

  # ---- 材料身份列：stageid 优先（同名不同材料时唯一），退化到 name ----
  # 注意：adapt_to_reference() 把 stageid 映射成「阶段名称」、name 映射成「品种名称」，
  # 所以这里必须同时认这两组名字，否则会误用 name 去重（同名不同材料会被合并）。
  id_col <- intersect(c("stageid", "阶段名称", "阶段编号"), colnames(df))
  if (length(id_col) == 0) {
    id_col <- intersect(c("name", "品种名称"), colnames(df))
  }
  if (length(id_col) == 0) return(nrow(df))
  id_col <- id_col[1]

  # ---- 分组键：地点 × 重复（多点/多重复时同一材料在各组里重复出现）----
  grp_cols <- intersect(c("place", "地点", "rp", "重复"), colnames(df))

  if (length(grp_cols) == 0) {
    # 单地点单重复：直接按材料身份去重
    return(as.integer(length(unique(df[[id_col]]))))
  }

  # 按组切分，每组内按材料身份去重，取最大的一组
  key <- do.call(paste, c(lapply(grp_cols, function(cc) as.character(df[[cc]])),
                          list(sep = "\r")))
  n_by_group <- tapply(seq_len(nrow(df)), key, function(idx) {
    length(unique(df[[id_col]][idx]))
  })
  as.integer(max(n_by_group))
}

#' 由数据框算出默认晋级位次阈值（对照小区不计入参试材料）
#'
#' @param df 数据框（拼音或参考列名均可）
#' @param pct 比例，默认 0.25
#' @return 整数位次阈值（>= 1）
#' @export
default_rank_threshold <- function(df, pct = DEFAULT_RANK_THRESHOLD_PCT) {
  default_rank_threshold_from_n(count_materials(df), pct)
}

# 性状分离判据：① 性状栏取值 99-分离；② 田间备注里写了「分离 / 不纯 / 提纯」
SEPARATION_TRAITS <- list(
  list(pinyin = "HuaSe",        ref = "花色",    label = "花色"),
  list(pinyin = "YeXing",       ref = "叶形",    label = "叶形"),
  list(pinyin = "RongMaoSe",    ref = "茸毛色",  label = "茸毛色"),
  list(pinyin = "JieJiaXiXing", ref = "结荚习性", label = "结荚习性"),
  list(pinyin = "JiaShuSe",     ref = "荚熟色",  label = "荚熟色")
)
SEPARATION_MEMO_COLS <- c("田间备注", "TianJianBeiZhu", "memo")
SEPARATION_MEMO_KEYWORDS <- c("分离", "不纯", "提纯")

#' 判定每个小区的性状分离情况
#'
#' 两个判据合并：
#' ① 性状栏取值 `99-分离`（花色/叶形/茸毛色/结荚习性/荚熟色）；
#' ② 田间备注里写了「分离 / 不纯 / 提纯」等字样（人工评语也算）。
#'
#' @param df 已适配为参考列名的数据框
#' @return 字符向量，与 df 行对齐；未记分离为 NA
#' @export
detect_trait_segregation <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  out <- rep(NA_character_, nrow(df))

  trait_cols <- lapply(SEPARATION_TRAITS, function(tr) intersect(c(tr$ref, tr$pinyin), colnames(df)))
  for (i in seq_along(SEPARATION_TRAITS)) {
    cn <- trait_cols[[i]]
    if (length(cn) == 0) next
    tr <- SEPARATION_TRAITS[[i]]
    v <- as.character(df[[cn[1]]])
    hit <- !is.na(v) & grepl("99[-－— ]*分离", v)
    if (any(hit)) {
      out[hit] <- ifelse(is.na(out[hit]), tr$label, paste(out[hit], tr$label, sep = "、"))
    }
  }

  memo_col <- intersect(SEPARATION_MEMO_COLS, colnames(df))
  if (length(memo_col) > 0) {
    m <- as.character(df[[memo_col[1]]])
    for (i in seq_along(m)) {
      if (is.na(m[i])) next
      kw <- SEPARATION_MEMO_KEYWORDS[vapply(SEPARATION_MEMO_KEYWORDS,
                                            function(k) grepl(k, m[i], fixed = TRUE), logical(1))]
      if (length(kw) == 0) next
      traits <- vapply(SEPARATION_TRAITS, function(tr) grepl(tr$label, m[i], fixed = TRUE), logical(1))
      label <- paste0("备注：", paste(c(vapply(SEPARATION_TRAITS[traits],
                                               function(tr) tr$label, character(1)), kw),
                                      collapse = "·"))
      out[i] <- if (is.na(out[i])) label else paste(out[i], label, sep = "；")
    }
  }
  out
}

#' 品种筛选分析
#'
#' 根据位次阈值、倒伏性等级筛选晋级、高产分离选单株和淘汰材料，
#' 生成对比图表和雷达图。
#'
#' @param df 数据框（拼音列名）
#' @param rank_threshold_select 晋级材料位次阈值，NULL = 自动取参试材料数的前 25%
#' @param rank_threshold_plant 分离选单株位次阈值，NULL = 自动取参试材料数的前 25%
#' @param exclude_lodging 排除的倒伏性等级，NULL 表示不排除
#' @param keep_eliminated_vector 指定不参与晋级筛选的阶段名称向量，NULL 表示无
#' @param keep_not_select_vector 指定不参与分离选单株筛选的阶段名称向量，NULL 表示无
#' @param place 按地点筛选，NULL 表示全部
#' @param radar_top_n 雷达图品种数
#' @param radar_selected_names 手动指定雷达图品种
#' @return list(promoted, select_plant, eliminated, comparison_plot, radar_plot, summary)
#' @export
analyze_screening <- function(df,
                               rank_threshold_select = NULL,
                               rank_threshold_plant = NULL,
                               exclude_lodging = c("9-严重倒", "7-重倒"),
                               keep_eliminated_vector = NULL,
                               keep_not_select_vector = NULL,
                               place = NULL,
                               radar_top_n = 5,
                               radar_selected_names = NULL) {
  rdf <- adapt_to_reference(df)

  # 地点过滤
  if (!is.null(place) && "地点" %in% colnames(rdf)) {
    rdf <- rdf[rdf[["地点"]] == place, ]
  }

  # 参试材料数（对照小区不计入）
  n_material <- count_materials(rdf)

  # 默认位次阈值：参试材料数的前 25%（不传则自动算）
  if (is.null(rank_threshold_select)) {
    rank_threshold_select <- default_rank_threshold_from_n(n_material)
  }
  if (is.null(rank_threshold_plant)) {
    rank_threshold_plant <- default_rank_threshold_from_n(n_material)
  }

  # 检查必需的位次列
  rank_cols <- intersect(c("较临近对照位次", "较平均对照位次"), colnames(rdf))
  if (length(rank_cols) == 0) {
    return(list(
      promoted     = data.frame(),
      select_plant = data.frame(),
      eliminated   = data.frame(),
      comparison_plot = NULL,
      radar_plot      = NULL,
      summary      = list(total_n = 0L, promoted_n = 0L,
        select_plant_n = 0L, eliminated_n = 0L, breakdown = list()),
      message = "缺少位次筛选字段，无法进行品种筛选。"
    ))
  }

  # ===== 1. 晋级材料（非分离 + 位次达标） =====
  promoted <- rdf
  if ("阶段名称" %in% colnames(promoted)) {
    promoted <- promoted[!grepl("分离", as.character(promoted[["阶段名称"]])), ]
  }

  # 位次筛选（两个位次字段都存在时用 AND 逻辑）
  for (rc in rank_cols) {
    promoted <- promoted[!is.na(promoted[[rc]]) & promoted[[rc]] <= rank_threshold_select, ]
  }

  # 倒伏性排除
  if (!is.null(exclude_lodging) && "倒伏性" %in% colnames(promoted)) {
    for (lv in exclude_lodging) {
      promoted <- promoted[promoted[["倒伏性"]] != lv | is.na(promoted[["倒伏性"]]), ]
    }
  }

  # 手动保留（不淘汰）
  if (!is.null(keep_eliminated_vector) && "阶段名称" %in% colnames(promoted)) {
    promoted <- promoted[!promoted[["阶段名称"]] %in% keep_eliminated_vector, ]
  }

  # ===== 2. 高产分离选单株（含"分离" + 位次 < 阈值） =====
  select_plant <- rdf
  if ("阶段名称" %in% colnames(select_plant)) {
    select_plant <- select_plant[grepl("分离", as.character(select_plant[["阶段名称"]])), ]
  }

  # 位次筛选（严格小于）
  for (rc in rank_cols) {
    select_plant <- select_plant[!is.na(select_plant[[rc]]) & select_plant[[rc]] < rank_threshold_plant, ]
  }

  # 倒伏性排除
  if (!is.null(exclude_lodging) && "倒伏性" %in% colnames(select_plant)) {
    for (lv in exclude_lodging) {
      select_plant <- select_plant[select_plant[["倒伏性"]] != lv | is.na(select_plant[["倒伏性"]]), ]
    }
  }

  # 手动保留（不进入分离选单株）
  if (!is.null(keep_not_select_vector) && "阶段名称" %in% colnames(select_plant)) {
    select_plant <- select_plant[!select_plant[["阶段名称"]] %in% keep_not_select_vector, ]
  }

  # ===== 3. 淘汰材料（阶段名称不在晋级和分离材料中） =====
  promoted_stages <- if ("阶段名称" %in% colnames(promoted)) unique(promoted[["阶段名称"]]) else character(0)
  plant_stages    <- if ("阶段名称" %in% colnames(select_plant)) unique(select_plant[["阶段名称"]]) else character(0)
  retained_stages <- union(promoted_stages, plant_stages)

  eliminated <- rdf
  if ("阶段名称" %in% colnames(eliminated) && length(retained_stages) > 0) {
    eliminated <- eliminated[!eliminated[["阶段名称"]] %in% retained_stages, ]
  }

  # 计算淘汰原因
  if (nrow(eliminated) > 0) {
    eliminated$淘汰原因 <- compute_elimination_reasons(eliminated, rank_cols,
      rank_threshold_select, exclude_lodging)
  }

  # ===== 3.5 性状分离标注（性状栏 99-分离 或 田间备注写了 分离/不纯/提纯） =====
  seg_all <- detect_trait_segregation(rdf)
  add_sep_col <- function(tbl) {
    idx <- match(rownames(tbl), rownames(rdf))
    tbl$性状分离 <- ifelse(is.na(idx), NA_character_, seg_all[idx])
    tbl
  }
  promoted     <- add_sep_col(promoted)
  select_plant <- add_sep_col(select_plant)
  eliminated   <- add_sep_col(eliminated)

  # ===== 4. 筛选摘要 =====
  summary <- list(
    total_n         = nrow(rdf),
    n_material      = n_material,
    rank_threshold_select = rank_threshold_select,
    rank_threshold_plant  = rank_threshold_plant,
    promoted_n      = nrow(promoted),
    segregated_n    = sum(!is.na(promoted$性状分离)),
    select_plant_n  = nrow(select_plant),
    eliminated_n    = nrow(eliminated),
    breakdown       = list(
      rank_fail  = if (nrow(eliminated) > 0 && "淘汰原因" %in% colnames(eliminated))
        sum(grepl("位次不达标", eliminated$淘汰原因)) else 0L,
      separated  = if (nrow(eliminated) > 0 && "淘汰原因" %in% colnames(eliminated))
        sum(grepl("分离材料", eliminated$淘汰原因)) else 0L,
      lodging    = if (nrow(eliminated) > 0 && "淘汰原因" %in% colnames(eliminated))
        sum(grepl("倒伏", eliminated$淘汰原因)) else 0L
    )
  )

  # ===== 5. 转回拼音列名 =====
  promoted_py     <- adapt_to_pinyin(promoted)
  select_plant_py <- adapt_to_pinyin(select_plant)
  eliminated_py   <- adapt_to_pinyin(eliminated)

  # 亩产保留两位小数
  for (tbl in list(promoted_py, select_plant_py, eliminated_py)) {
    if ("MuChan" %in% colnames(tbl))
      tbl$MuChan <- round(tbl$MuChan, 2)
  }

  # ===== 6. 可视化 =====
  # 筛选前后对比图（晋级 vs 淘汰 vs 对照）
  comparison_plot <- tryCatch({
    plot_selection_comparison(rdf, promoted)
  }, error = function(e) NULL)

  # 雷达图
  radar_n <- min(radar_top_n, nrow(promoted))
  radar_plot <- tryCatch({
    if (nrow(promoted) > 0) {
      plot_radar_top(promoted_py, top_n = radar_n, selected_names = radar_selected_names)
    } else NULL
  }, error = function(e) NULL)

  list(
    promoted        = promoted_py,
    select_plant    = select_plant_py,
    eliminated      = eliminated_py,
    comparison_plot = comparison_plot,
    radar_plot      = radar_plot,
    summary         = summary
  )
}

#' 筛选前后对比图（小提琴 + 箱线 + 对照）
#' @keywords internal
plot_selection_comparison <- function(rdf_before, rdf_after) {
  if (!requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE)) return(NULL)

  indicators <- c("亩产_kg", "生育期_d", "株高_cm", "百粒重_g")
  indicator_labels <- c("亩产 (kg)", "生育期 (天)", "株高 (cm)", "百粒重 (g)")

  # 只使用存在且有数据的指标（整列为空的指标如未开展考种的百粒重会产生空面板）
  not_all_na <- vapply(indicators, function(ind) {
    if (!ind %in% colnames(rdf_before)) return(FALSE)
    any(!is.na(rdf_before[[ind]]))
  }, logical(1))
  exist_idx <- which(not_all_na)
  if (length(exist_idx) == 0) return(NULL)
  indicators <- indicators[exist_idx]
  indicator_labels <- indicator_labels[exist_idx]

  # 合并数据：对照 / 选前 / 选后
  comparison_data <- dplyr::bind_rows(
    # 对照
    if ("是否对照" %in% colnames(rdf_before)) {
      dplyr::filter(rdf_before, 是否对照 == 1) %>%
        dplyr::select(dplyr::all_of(indicators)) %>%
        dplyr::mutate(状态 = "对照")
    },
    # 选前（非对照）
    if ("是否对照" %in% colnames(rdf_before)) {
      dplyr::filter(rdf_before, 是否对照 != 1) %>%
        dplyr::select(dplyr::all_of(indicators)) %>%
        dplyr::mutate(状态 = "选前")
    } else {
      rdf_before %>%
        dplyr::select(dplyr::all_of(indicators)) %>%
        dplyr::mutate(状态 = "选前")
    },
    # 选后
    rdf_after %>%
      dplyr::select(dplyr::any_of(indicators)) %>%
      dplyr::mutate(状态 = "选后")
  ) %>%
    dplyr::filter(rowSums(dplyr::across(dplyr::all_of(indicators),
                                        ~ as.numeric(!is.na(.x)))) > 0)

  if (nrow(comparison_data) == 0) return(NULL)

  # 长格式
  # 注意：drop_na 必须放在 pivot_longer 之后。
  # 放在之前会按「整行任一指标为空」删记录——当某个指标（如未考种的百粒重）整列为空时，
  # 全部记录都会被删光，图就静默不生成。
  long_data <- comparison_data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(indicators),
      names_to = "指标", values_to = "数值") %>%
    tidyr::drop_na() %>%
    dplyr::mutate(指标名称 = factor(指标, levels = indicators, labels = indicator_labels))

  # 均值
  summary_data <- long_data %>%
    dplyr::group_by(状态, 指标名称) %>%
    dplyr::summarise(平均数值 = mean(数值, na.rm = TRUE), .groups = "drop")

  fill_colors <- c("对照" = "#2c7fb8", "选前" = "#1b9e77", "选后" = "#d95f02")

  ggplot2::ggplot(long_data, ggplot2::aes(x = 状态, y = 数值, fill = 状态)) +
    ggplot2::geom_violin(alpha = 0.25, trim = FALSE, color = NA) +
    ggplot2::geom_boxplot(alpha = 0.45, width = 0.18,
      outlier.shape = 21, outlier.size = 2, color = "#444",
      show.legend = FALSE) +
    ggplot2::geom_point(data = summary_data,
      ggplot2::aes(y = 平均数值),
      shape = 21, fill = "#FFD166", color = "#B22222",
      size = 3, stroke = 0.8) +
    ggplot2::geom_text(data = summary_data,
      ggplot2::aes(y = 平均数值,
        label = paste0("均值: ", round(平均数值, 1))),
      color = "#B22222", size = 3.5, vjust = -1, fontface = "bold") +
    ggplot2::facet_wrap(~指标名称, ncol = 2, scales = "free_y") +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::labs(
      title = "筛选前后核心农艺性状对比",
      subtitle = "对照 vs 选前 vs 选后",
      x = "", y = "指标数值", fill = "分组"
    ) +
    ggplot2::theme_classic() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 18, face = "bold"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 14),
      strip.text = ggplot2::element_text(size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 13),
      legend.position = "top",
      legend.text = ggplot2::element_text(size = 13)
    )
}

#' 计算淘汰原因
#' @keywords internal
compute_elimination_reasons <- function(eliminated, rank_cols, rank_threshold, exclude_lodging) {
  sapply(seq_len(nrow(eliminated)), function(i) {
    reasons <- character()

    rank_details <- character()
    for (rc in rank_cols) {
      val <- eliminated[[rc]][i]
      if (!is.na(val) && val > rank_threshold) {
        rank_details <- c(rank_details, paste0(rc, "=", val))
      }
    }
    if (length(rank_details) > 0) {
      reasons <- c(reasons, paste0("位次不达标(", paste(rank_details, collapse = ", "), ")"))
    }

    if ("阶段名称" %in% colnames(eliminated)) {
      stage <- as.character(eliminated[["阶段名称"]][i])
      if (!is.na(stage) && grepl("分离", stage)) {
        reasons <- c(reasons, "分离材料")
      }
    }

    if ("倒伏性" %in% colnames(eliminated) && length(exclude_lodging) > 0) {
      ldg <- as.character(eliminated[["倒伏性"]][i])
      if (!is.na(ldg) && ldg %in% exclude_lodging) {
        short <- gsub("^\\d+-", "", ldg)
        reasons <- c(reasons, paste0("倒伏-", short))
      }
    }

    if (length(reasons) == 0) "未知" else paste(reasons, collapse = "; ")
  })
}

#' 品种筛选控制面板 UI
#' @export
screening_controls_ui <- function(ns,
                                   rank_threshold_select = NULL,
                                   rank_threshold_plant = NULL,
                                   radar_top_n = 5,
                                   id_prefix = "",
                                   n_material = NULL) {
  # 默认阈值 = 参试材料数的前 25%（n_material 未知时退回 60）
  default_thr <- if (is.null(n_material)) 60L else default_rank_threshold_from_n(n_material)
  if (is.null(rank_threshold_select)) rank_threshold_select <- default_thr
  if (is.null(rank_threshold_plant))   rank_threshold_plant   <- default_thr
  thr_max <- max(100, as.integer(max(rank_threshold_select, rank_threshold_plant) * 2))
  fluidRow(
    column(3, sliderInput(ns(paste0(id_prefix, "scr_rank_threshold_select")),
      sprintf("晋级位次阈值（位次 ≤ N；默认 %d ≈ 前 %.0f%%）",
              default_thr, DEFAULT_RANK_THRESHOLD_PCT * 100),
      min = 1, max = thr_max, value = rank_threshold_select, step = 1, post = "")),
    column(3, sliderInput(ns(paste0(id_prefix, "scr_rank_threshold_plant")),
      sprintf("分离选单株位次阈值（位次 < N；默认 %d）", default_thr),
      min = 1, max = thr_max, value = rank_threshold_plant, step = 1, post = "")),
    column(3, numericInput(ns(paste0(id_prefix, "scr_radar_top_n")), "雷达图品种数",
      value = radar_top_n, min = 2, max = 20, step = 1)),
    column(3, checkboxGroupInput(ns(paste0(id_prefix, "scr_exclude_lodging")), "排除倒伏等级",
      choices  = c("1-不倒", "3-轻倒", "5-中倒", "7-重倒", "9-严重倒"),
      selected = c("9-严重倒", "7-重倒"),
      inline   = TRUE))
  )
}

#' 构建筛选摘要 UI
#' @export
build_screening_summary_ui <- function(summary) {
  if (is.null(summary) || summary$total_n == 0) return(NULL)
  div(class = "alert alert-info", style = "margin-bottom:15px;",
    tags$strong(sprintf("共 %d 个品种 → 晋级 %d 个 → 分离选单株 %d 个 → 淘汰 %d 个",
      summary$total_n, summary$promoted_n,
      summary$select_plant_n, summary$eliminated_n)),
    tags$br(),
    tags$small(
      "淘汰原因明细: ",
      sprintf("位次不达标: %d个 | 分离材料: %d个 | 倒伏排除: %d个",
        summary$breakdown$rank_fail,
        summary$breakdown$separated,
        summary$breakdown$lodging)
    )
  )
}
