# ==============================================================================
# 亲本和组合分析
# 从参考脚本 00-elite_parents_selection.R 移植
# ==============================================================================

#' 亲本与组合分析
#'
#' @param df 数据框（拼音列名）
#' @param min_crosses 最小配组数
#' @param top_pct 晋级率分位阈值
#' @return list(parent_stats, cross_stats, parent_plot)
#' @export
analyze_parent_cross <- function(df, min_crosses = 3, top_pct = 0.65) {
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) return(NULL)

  # 检查亲本列
  if (!all(c("ma", "pa") %in% colnames(df))) return(NULL)

  has_yield <- "MuChan" %in% colnames(df) && any(!is.na(df$MuChan))
  has_promoted <- "JiaoPingJunDuiZhaoWeiCi" %in% colnames(df)

  # 筛选有效记录
  parent_df <- df %>%
    dplyr::filter(!is.na(ma) & !is.na(pa) & ma != "" & pa != "")

  if (nrow(parent_df) < min_crosses) {
    return(list(
      parent_stats = data.frame(消息 = "有效亲本组合数不足"),
      cross_stats = data.frame(),
      parent_plot = NULL
    ))
  }

  # 标记晋级状态（简化：位次<=60为晋级）
  if (has_promoted) {
    parent_df <- parent_df %>%
      dplyr::mutate(晋级状态 = ifelse(
        !is.na(JiaoPingJunDuiZhaoWeiCi) & JiaoPingJunDuiZhaoWeiCi <= 60,
        "晋级", "未晋级"))
  } else {
    parent_df <- parent_df %>%
      dplyr::mutate(晋级状态 = "未知")
  }

  # 亲本统计
  parent_stats <- dplyr::bind_rows(
    # 母本视角
    parent_df %>%
      dplyr::group_by(亲本类型 = "母本", 亲本名称 = ma) %>%
      dplyr::summarise(
        配制品种数 = dplyr::n(),
        晋级品种数 = sum(晋级状态 == "晋级"),
        晋级率 = if (has_promoted) round(晋级品种数 / 配制品种数, 3) else NA_real_,
        .groups = "drop"
      ),
    # 父本视角
    parent_df %>%
      dplyr::group_by(亲本类型 = "父本", 亲本名称 = pa) %>%
      dplyr::summarise(
        配制品种数 = dplyr::n(),
        晋级品种数 = sum(晋级状态 == "晋级"),
        晋级率 = if (has_promoted) round(晋级品种数 / 配制品种数, 3) else NA_real_,
        .groups = "drop"
      )
  ) %>%
    dplyr::filter(配制品种数 >= min_crosses) %>%
    dplyr::arrange(dplyr::desc(配制品种数))

  # 组合统计
  cross_stats <- parent_df %>%
    dplyr::mutate(亲本组合 = paste(ma, pa, sep = "×")) %>%
    dplyr::group_by(亲本组合, ma, pa) %>%
    dplyr::summarise(
      配制品种数 = dplyr::n(),
      晋级品种数 = sum(晋级状态 == "晋级"),
      晋级率 = if (has_promoted) round(晋级品种数 / 配制品种数, 3) else NA_real_,
      .groups = "drop"
    ) %>%
    dplyr::filter(配制品种数 >= max(2, min_crosses - 1)) %>%
    dplyr::arrange(dplyr::desc(配制品种数))

  # 产量（安全转换 factor/character → numeric）
  if (has_yield) {
    parent_df$MuChan <- suppressWarnings(as.numeric(as.character(parent_df$MuChan)))
    parent_yield <- parent_df %>%
      dplyr::group_by(亲本类型 = "母本", 亲本名称 = ma) %>%
      dplyr::summarise(平均亩产 = round(mean(MuChan, na.rm = TRUE), 2), .groups = "drop") %>%
      dplyr::bind_rows(
        parent_df %>%
          dplyr::group_by(亲本类型 = "父本", 亲本名称 = pa) %>%
          dplyr::summarise(平均亩产 = round(mean(MuChan, na.rm = TRUE), 2), .groups = "drop")
      )

    parent_stats <- parent_stats %>%
      dplyr::left_join(parent_yield, by = c("亲本类型", "亲本名称"))

    cross_yield <- parent_df %>%
      dplyr::mutate(亲本组合 = paste(ma, pa, sep = "×")) %>%
      dplyr::group_by(亲本组合) %>%
      dplyr::summarise(平均亩产 = round(mean(MuChan, na.rm = TRUE), 2), .groups = "drop")

    cross_stats <- cross_stats %>%
      dplyr::left_join(cross_yield, by = "亲本组合")
  }

  # 亲本散点图
  parent_plot <- NULL
  if (has_yield && has_promoted && "晋级率" %in% colnames(parent_stats)) {
    parent_plot <- tryCatch({
      ggplot2::ggplot(parent_stats,
        ggplot2::aes(x = 晋级率, y = 平均亩产, color = 亲本类型)) +
        ggplot2::geom_point(alpha = 0.6, size = 3) +
        ggplot2::geom_text(
          data = dplyr::filter(parent_stats, 晋级率 >= quantile(晋级率, top_pct, na.rm = TRUE)),
          ggplot2::aes(label = 亲本名称), vjust = -1, size = 4, fontface = "bold",
          check_overlap = TRUE) +
        ggplot2::labs(title = "亲本晋级表现", x = "晋级率", y = "平均亩产 (kg)",
          color = "亲本类型") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
          axis.text = ggplot2::element_text(size = 12),
          legend.position = "top"
        )
    }, error = function(e) NULL)
  }

  list(
    parent_stats = parent_stats,
    cross_stats  = cross_stats,
    parent_plot  = parent_plot
  )
}
