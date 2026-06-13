# ==============================================================================
# 质量性状分布图
# 从参考脚本 00-main_function.R 的 plot_single_trait_distribution() 移植
# ==============================================================================

#' 质量性状分布分析
#'
#' @param df 数据框（拼音列名）
#' @return list(plots = list(hua_se=ggplot, ye_xing=ggplot, ...))
#' @export
analyze_quality_traits <- function(df) {
  rdf <- adapt_to_reference(df)

  plots <- list()

  # 可展示的质量性状
  quality_traits <- list(
    花色 = "花色", 叶形 = "叶形", 结荚习性 = "结荚习性",
    倒伏性 = "倒伏性", 茸毛色 = "茸毛色", 脐色 = "脐色",
    种皮色 = "种皮色", 种皮光泽 = "种皮光泽",
    粒形 = "粒形", 荚形 = "荚形"
  )

  for (nm in names(quality_traits)) {
    ref_name <- quality_traits[[nm]]
    if (ref_name %in% colnames(rdf)) {
      vals <- rdf[[ref_name]]
      vals <- vals[!is.na(vals) & nchar(as.character(vals)) > 0]
      if (length(vals) > 0) {
        plots[[nm]] <- tryCatch({
          plot_single_trait(rdf, ref_name)
        }, error = function(e) NULL)
      }
    }
  }

  list(plots = plots)
}

#' 绘制单个质量性状分布
#' @keywords internal
plot_single_trait <- function(rdf, column) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)

  rdf[[column]] <- factor(rdf[[column]], levels = unique(rdf[[column]]))

  plot_data <- as.data.frame(table(rdf[[column]]))
  colnames(plot_data) <- c("类型", "数量")

  ggplot2::ggplot(plot_data, ggplot2::aes(x = 类型, y = 数量, fill = 类型)) +
    ggplot2::geom_col(width = 0.7, alpha = 0.85) +
    ggplot2::geom_text(ggplot2::aes(label = 数量),
      vjust = -0.3, size = 4.5, fontface = "bold") +
    ggplot2::labs(x = paste0(column, "类型"), y = "", title = "") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, size = 14),
      axis.text.y = ggplot2::element_text(size = 14),
      axis.title.x = ggplot2::element_text(size = 14, face = "bold"),
      legend.position = "none"
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))
}
