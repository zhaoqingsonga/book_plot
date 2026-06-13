# ==============================================================================
# 群体分析：世代分布、晋级追踪、亲本组合
# ==============================================================================

#' 群体分析
#'
#' @param df 数据框（需含 f, name, ma, pa 列）
#' @return list(gen_table, gen_tracking, cross_table, gen_dist_plot, gen_tracking_plot, cross_plot)
#' @export
analyze_population <- function(df) {
  if (!requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE)) return(NULL)

  result <- list()

  # ---- 1. 世代分布 ----
  if ("f" %in% colnames(df)) {
    gen_dist <- df %>%
      dplyr::count(f, name = "记录数") %>%
      dplyr::mutate(占比 = round(记录数 / sum(记录数) * 100, 1)) %>%
      dplyr::arrange(f)

    result$gen_table <- gen_dist

    # 柱状图
    result$gen_dist_plot <- tryCatch({
      ggplot2::ggplot(gen_dist, ggplot2::aes(x = factor(f), y = 记录数, fill = factor(f))) +
        ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = paste0(记录数, "\n(", 占比, "%)")),
          vjust = -0.3, size = 4) +
        ggplot2::labs(title = "世代分布", x = "世代 (F)", y = "记录数", fill = "世代") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "none"
        )
    }, error = function(e) NULL)
  }

  # ---- 2. 世代晋级追踪 ----
  if ("f" %in% colnames(df) && "name" %in% colnames(df)) {
    gen_tracking <- df %>%
      dplyr::group_by(name) %>%
      dplyr::summarise(
        世代路径 = paste(sort(unique(as.character(f))), collapse = " → "),
        跨越世代数 = dplyr::n_distinct(f),
        出现次数 = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(跨越世代数))

    result$gen_tracking <- gen_tracking

    # 跨越世代数分布
    track_summary <- gen_tracking %>%
      dplyr::count(跨越世代数)

    result$gen_tracking_plot <- tryCatch({
      ggplot2::ggplot(track_summary,
        ggplot2::aes(x = factor(跨越世代数), y = n, fill = factor(跨越世代数))) +
        ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
        ggplot2::geom_text(ggplot2::aes(label = n), vjust = -0.3, size = 4.5) +
        ggplot2::labs(title = "世代晋级追踪", x = "跨越世代数", y = "材料数") +
        ggplot2::theme_bw() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
          legend.position = "none"
        )
    }, error = function(e) NULL)
  }

  # ---- 3. 亲本组合 Top 20 ----
  if (all(c("ma", "pa") %in% colnames(df))) {
    cross_top <- df %>%
      dplyr::filter(!is.na(ma) & !is.na(pa) & ma != "" & pa != "") %>%
      dplyr::mutate(亲本组合 = paste(ma, pa, sep = " × ")) %>%
      dplyr::count(亲本组合, sort = TRUE) %>%
      dplyr::slice_head(n = 20) %>%
      dplyr::mutate(占比 = round(n / sum(n) * 100, 1))

    # 处理 NA 亲本
    na_cross <- df %>%
      dplyr::filter(is.na(ma) | is.na(pa) | ma == "" | pa == "") %>%
      dplyr::summarise(亲本组合 = "未知亲本", n = dplyr::n(), 占比 = NA_real_)

    if (nrow(na_cross) > 0 && na_cross$n > 0) {
      cross_top <- dplyr::bind_rows(cross_top, na_cross)
    }

    result$cross_table <- cross_top

    result$cross_plot <- tryCatch({
      ggplot2::ggplot(cross_top,
        ggplot2::aes(x = stats::reorder(亲本组合, n), y = n, fill = n)) +
        ggplot2::geom_bar(stat = "identity", alpha = 0.85) +
        ggplot2::geom_text(ggplot2::aes(label = n), hjust = -0.2, size = 3.5) +
        ggplot2::scale_fill_gradient(low = "#D6EAF8", high = "#2E86C1") +
        ggplot2::labs(title = "亲本组合 Top 20", x = "", y = "材料数") +
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

#' 群体性状概览
#'
#' @param df 数据框
#' @return list(table = data.frame)
#' @export
build_population_trait_overview <- function(df) {
  traits <- intersect(c("MuChan", "ShengYuQi", "ZhuGao", "BaiLiZhong",
    "DanBai", "ZhiFang"), colnames(df))

  available <- traits[sapply(traits, function(t) any(!is.na(df[[t]])))]

  if (length(available) == 0) {
    return(list(table = data.frame(消息 = "暂无性状数据")))
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
