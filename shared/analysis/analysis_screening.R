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
                               exclude_lodging = c("9-严重倒", "7-重倒")) {
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

  # 转回拼音列名
  promoted_py <- adapt_to_pinyin(promoted)
  eliminated_py <- adapt_to_pinyin(eliminated)

  # 筛选前后对比图
  comparison_plot <- tryCatch({
    plot_selection_comparison(rdf, promoted)
  }, error = function(e) NULL)

  # 雷达图 (Top 5)
  radar_plot <- tryCatch({
    if (nrow(promoted) > 0) {
      plot_radar_top(promoted_py, top_n = min(5, nrow(promoted)))
    } else NULL
  }, error = function(e) NULL)

  list(
    promoted        = promoted_py,
    eliminated      = eliminated_py,
    comparison_plot = comparison_plot,
    radar_plot      = radar_plot
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
