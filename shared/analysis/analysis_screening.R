# ==============================================================================
# 品种筛选 + 筛选前后对比图
# 从参考脚本 00-main_function.R 的 screen_material_promotion() 和
# plot_selection_comparison_addck() 移植
# ==============================================================================

#' 品种筛选分析
#'
#' 根据位次阈值、倒伏性等级筛选晋级和淘汰材料，生成对比图表和雷达图。
#'
#' @param df 数据框（拼音列名）
#' @param rank_threshold 位次阈值，默认 60
#' @param exclude_lodging 排除的倒伏性等级，NULL 表示不排除
#' @return list(promoted, eliminated, comparison_plot, radar_plot)
#' @export
analyze_screening <- function(df, rank_threshold = 60,
                               exclude_lodging = c("9-严重倒", "7-重倒"),
                               radar_top_n = 5,
                               radar_selected_names = NULL) {
  rdf <- adapt_to_reference(df)

  # 检查必需的位次列
  rank_cols <- intersect(c("较临近对照位次", "较平均对照位次"), colnames(rdf))
  if (length(rank_cols) == 0) {
    return(list(
      promoted = data.frame(),
      eliminated = data.frame(),
      comparison_plot = NULL,
      radar_plot = NULL,
      message = "缺少位次筛选字段，无法进行品种筛选。"
    ))
  }

  # 非分离材料（不包含"分离"关键词）
  promoted <- rdf
  if ("阶段名称" %in% colnames(promoted)) {
    promoted <- promoted[!grepl("分离", as.character(promoted[["阶段名称"]])), ]
  }

  # 位次筛选
  for (rc in rank_cols) {
    promoted <- promoted[!is.na(promoted[[rc]]) & promoted[[rc]] <= rank_threshold, ]
  }

  # 倒伏性排除
  if (!is.null(exclude_lodging) && "倒伏性" %in% colnames(promoted)) {
    for (lv in exclude_lodging) {
      promoted <- promoted[promoted[["倒伏性"]] != lv | is.na(promoted[["倒伏性"]]), ]
    }
  }

  # 淘汰材料
  promoted_names <- if ("品种名称" %in% colnames(promoted)) promoted[["品种名称"]] else character(0)
  eliminated <- rdf
  if ("品种名称" %in% colnames(eliminated) && length(promoted_names) > 0) {
    eliminated <- eliminated[!eliminated[["品种名称"]] %in% promoted_names, ]
  }

  # 计算淘汰原因（在转拼音前，列名是中文）
  if (nrow(eliminated) > 0) {
    eliminated$淘汰原因 <- compute_elimination_reasons(eliminated, rank_cols,
      rank_threshold, exclude_lodging)
  }

  # 筛选摘要
  summary <- list(
    total_n      = nrow(rdf),
    promoted_n   = nrow(promoted),
    eliminated_n = nrow(eliminated),
    breakdown    = list(
      rank_fail = if (nrow(eliminated) > 0 && "淘汰原因" %in% colnames(eliminated))
        sum(grepl("位次不达标", eliminated$淘汰原因)) else 0L,
      separated = if (nrow(eliminated) > 0 && "淘汰原因" %in% colnames(eliminated))
        sum(grepl("分离材料", eliminated$淘汰原因)) else 0L,
      lodging   = if (nrow(eliminated) > 0 && "淘汰原因" %in% colnames(eliminated))
        sum(grepl("倒伏", eliminated$淘汰原因)) else 0L
    )
  )

  # 转回拼音列名
  promoted_py <- adapt_to_pinyin(promoted)
  eliminated_py <- adapt_to_pinyin(eliminated)

  # 亩产保留两位小数
  if ("MuChan" %in% colnames(promoted_py))
    promoted_py$MuChan <- round(promoted_py$MuChan, 2)
  if ("MuChan" %in% colnames(eliminated_py))
    eliminated_py$MuChan <- round(eliminated_py$MuChan, 2)

  # 筛选前后对比图
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

  # 只使用存在的指标
  exist_idx <- which(indicators %in% colnames(rdf_before))
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
    tidyr::drop_na()

  if (nrow(comparison_data) == 0) return(NULL)

  # 长格式
  long_data <- comparison_data %>%
    tidyr::pivot_longer(cols = dplyr::all_of(indicators),
      names_to = "指标", values_to = "数值") %>%
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
screening_controls_ui <- function(ns, rank_threshold = 60, radar_top_n = 5) {
  fluidRow(
    column(4, sliderInput(ns("scr_rank_threshold"), "位次阈值",
      min = 1, max = 100, value = rank_threshold, step = 1, post = "%")),
    column(4, numericInput(ns("scr_radar_top_n"), "雷达图品种数",
      value = radar_top_n, min = 2, max = 20, step = 1)),
    column(4, checkboxGroupInput(ns("scr_exclude_lodging"), "排除倒伏等级",
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
    tags$strong(sprintf("共 %d 个品种 → 晋级 %d 个 → 淘汰 %d 个",
      summary$total_n, summary$promoted_n, summary$eliminated_n)),
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
