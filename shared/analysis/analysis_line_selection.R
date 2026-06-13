# ==============================================================================
# 株行分析：选择概况、选择率分布、后代行数
# ==============================================================================

#' 株行分析
#'
#' @param df 数据框（需含 sele, name, rows 列）
#' @return list(overview, sele_dist_table, progeny_table, sele_dist_plot, progeny_plot)
#' @export
analyze_line_selection <- function(df) {
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) return(NULL)

  result <- list()

  # ---- 1. 选择概况 ----
  has_sele <- "sele" %in% colnames(df) && any(!is.na(df$sele))
  has_name <- "name" %in% colnames(df)
  has_rows <- "rows" %in% colnames(df)

  overview_data <- list(
    指标 = c("总记录数", "总材料数"),
    数值 = c(nrow(df),
      if (has_name) dplyr::n_distinct(df$name, na.rm = TRUE) else nrow(df))
  )

  if (has_sele) {
    sele_vals <- as.numeric(df$sele)
    sele_vals <- sele_vals[!is.na(sele_vals)]
    overview_data$指标 <- c(overview_data$指标, "总选择数", "平均选择数")
    overview_data$数值 <- c(overview_data$数值,
      sum(sele_vals, na.rm = TRUE),
      round(mean(sele_vals, na.rm = TRUE), 2))
  }

  result$overview <- as.data.frame(overview_data, stringsAsFactors = FALSE)

  # ---- 2. 选择率分布 ----
  if (has_sele) {
    sele_dist <- df %>%
      dplyr::count(sele, name = "材料数") %>%
      dplyr::mutate(占比 = round(材料数 / sum(材料数) * 100, 1)) %>%
      dplyr::arrange(dplyr::desc(sele))

    result$sele_dist_table <- sele_dist

    result$sele_dist_plot <- tryCatch({
      ggplot2::ggplot(sele_dist,
        ggplot2::aes(x = factor(sele), y = 材料数, fill = factor(sele))) +
        ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = 材料数), vjust = -0.3, size = 4) +
        ggplot2::labs(title = "选择数分布", x = "选择数", y = "材料数") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "none"
        )
    }, error = function(e) NULL)
  }

  # ---- 3. 每材料后代行数 ----
  if (has_name && has_rows) {
    progeny <- df %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(
        后代行数 = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(后代行数)) %>%
      dplyr::slice_head(n = 20)

    result$progeny_table <- progeny

    result$progeny_plot <- tryCatch({
      ggplot2::ggplot(progeny,
        ggplot2::aes(x = stats::reorder(name, 后代行数), y = 后代行数, fill = 后代行数)) +
        ggplot2::geom_bar(stat = "identity", alpha = 0.85) +
        ggplot2::geom_text(ggplot2::aes(label = 后代行数), hjust = -0.2, size = 3.5) +
        ggplot2::scale_fill_gradient(low = "#A8E6CF", high = "#1B5E20") +
        ggplot2::labs(title = "每材料后代行数 Top 20", x = "", y = "行数") +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "none"
        )
    }, error = function(e) NULL)
  }

  result
}

#' 形态性状统计
#'
#' @param df 数据框
#' @return list(table = data.frame)
#' @export
build_morphology_stats <- function(df) {
  morph_traits <- intersect(c("ZhuGao", "DiJiaGao", "FenZhiShu",
    "ZhuJingJieShu", "DanZhuJiaShu", "BaiLiZhong"), colnames(df))

  available <- morph_traits[sapply(morph_traits, function(t) {
    any(!is.na(df[[t]]))
  })]

  if (length(available) == 0) {
    return(list(table = data.frame(消息 = "暂无形态性状数据")))
  }

  stats_df <- data.frame(
    性状 = get_trait_display_name(available),
    记录数 = sapply(available, function(t) sum(!is.na(df[[t]]))),
    均值 = sapply(available, function(t) round(mean(df[[t]], na.rm = TRUE), 2)),
    标准差 = sapply(available, function(t) round(sd(df[[t]], na.rm = TRUE), 2)),
    最小值 = sapply(available, function(t) round(min(df[[t]], na.rm = TRUE), 2)),
    最大值 = sapply(available, function(t) round(max(df[[t]], na.rm = TRUE), 2)),
    stringsAsFactors = FALSE,
    row.names = NULL
  )

  list(table = stats_df)
}
