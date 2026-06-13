# ==============================================================================
# 产量核心分析：统计 + 4核心图 + 散点图 + 相关性
# 从参考脚本 00-main_function.R 移植和适配
# ==============================================================================

#' 产量核心统计分析
#'
#' @param df 数据框（需含 MuChan, ShengYuQi, is_ck 等列）
#' @return list(yield_stats, growth_stats, increase_stats, ck_mean, plots, scatter_plots, corr_plot)
#' @export
analyze_yield_core <- function(df) {
  # 列名适配
  rdf <- adapt_to_reference(df)

  # 产量统计
  yield_stats <- tryCatch({
    if ("亩产_kg" %in% colnames(rdf)) {
      rdf %>%
        dplyr::summarise(
          平均亩产 = mean(亩产_kg, na.rm = TRUE),
          产量中位数 = median(亩产_kg, na.rm = TRUE),
          最高亩产 = max(亩产_kg, na.rm = TRUE),
          最低亩产 = min(亩产_kg, na.rm = TRUE),
          产量标准差 = sd(亩产_kg, na.rm = TRUE),
          产量变异系数 = (产量标准差 / 平均亩产) * 100
        ) %>%
        round(2)
    } else NULL
  }, error = function(e) NULL)

  # 对照均值
  ck_mean <- tryCatch({
    if ("是否对照" %in% colnames(rdf) && "亩产_kg" %in% colnames(rdf)) {
      ck_data <- rdf %>% dplyr::filter(是否对照 == 1)
      if (nrow(ck_data) > 0) {
        ck_data %>%
          dplyr::summarise(dplyr::across(
            dplyr::any_of(c("亩产_kg", "生育期_d", "株高_cm", "百粒重_g")),
            ~ round(mean(.x, na.rm = TRUE), 2)
          ))
      } else NULL
    } else NULL
  }, error = function(e) NULL)

  # 生育期统计
  growth_stats <- tryCatch({
    if ("生育期_d" %in% colnames(rdf)) {
      rdf %>%
        dplyr::summarise(
          平均生育期 = mean(生育期_d, na.rm = TRUE),
          生育期中位数 = median(生育期_d, na.rm = TRUE),
          最长生育期 = max(生育期_d, na.rm = TRUE),
          最短生育期 = min(生育期_d, na.rm = TRUE)
        ) %>%
        round(1)
    } else NULL
  }, error = function(e) NULL)

  # 增产统计
  increase_stats <- tryCatch({
    inc_col <- if ("较临近对照增产_pct" %in% colnames(rdf)) "较临近对照增产_pct"
               else if ("较平均对照增产_pct" %in% colnames(rdf)) "较平均对照增产_pct"
               else NULL
    if (!is.null(inc_col)) {
      rdf %>%
        dplyr::summarise(
          平均增产率 = mean(.data[[inc_col]], na.rm = TRUE),
          最高增产率 = max(.data[[inc_col]], na.rm = TRUE),
          最大减产率 = min(.data[[inc_col]], na.rm = TRUE),
          增产品种数量 = sum(.data[[inc_col]] > 0, na.rm = TRUE),
          增产品种比例 = (增产品种数量 / dplyr::n()) * 100
        ) %>%
        round(2)
    } else NULL
  }, error = function(e) NULL)

  # ---- 可视化 ----
  plots <- list()
  scatter_plots <- list()

  # 4核心图
  if (!is.null(yield_stats) && !is.null(ck_mean)) {
    plots$yield_dist <- tryCatch({
      plot_yield_hist(rdf, yield_stats, ck_mean)
    }, error = function(e) NULL)

    plots$yield_grade_dist <- tryCatch({
      plot_yield_grade(rdf)
    }, error = function(e) NULL)

    plots$increase_dist <- tryCatch({
      plot_increase_distribution_chart(rdf)
    }, error = function(e) NULL)

    plots$growth_dist <- tryCatch({
      plot_growth_hist(rdf, growth_stats, ck_mean)
    }, error = function(e) NULL)
  }

  # 3散点图
  if ("亩产_kg" %in% colnames(rdf)) {
    for (xvar in c("生育期_d", "株高_cm", "百粒重_g")) {
      if (xvar %in% colnames(rdf)) {
        corr_val <- tryCatch({
          round(cor(rdf[["亩产_kg"]], rdf[[xvar]], use = "complete.obs"), 2)
        }, error = function(e) NA_real_)

        nm <- switch(xvar,
          "生育期_d" = "growth_vs_yield",
          "株高_cm" = "height_vs_yield",
          "百粒重_g" = "hundred_grain_vs_yield"
        )

        scatter_plots[[nm]] <- tryCatch({
          plot_trait_vs_yield(rdf, xvar, corr_val)
        }, error = function(e) NULL)
      }
    }
  }

  # 相关性矩阵
  corr_plot <- tryCatch({
    corr_vars <- intersect(c("亩产_kg", "生育期_d", "株高_cm", "百粒重_g"), colnames(rdf))
    if (length(corr_vars) >= 2) {
      plot_correlation_matrix(rdf, corr_vars)
    } else NULL
  }, error = function(e) NULL)

  list(
    yield_stats    = yield_stats,
    ck_mean        = ck_mean,
    growth_stats   = growth_stats,
    increase_stats = increase_stats,
    plots          = plots,
    scatter_plots  = scatter_plots,
    corr_plot      = corr_plot
  )
}

# ==============================================================================
# 内部绘图函数（适配自 00-main_function.R）
# ==============================================================================

#' 产量分布直方图
#' @keywords internal
plot_yield_hist <- function(rdf, yield_stats, ck_mean) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)

  hist_yield <- hist(rdf[["亩产_kg"]], breaks = 25, plot = FALSE)
  max_count <- max(hist_yield$counts)

  avg_yield <- yield_stats$平均亩产
  ck_yield <- if ("亩产_kg" %in% colnames(ck_mean)) ck_mean[["亩产_kg"]] else NA_real_

  p <- rdf %>%
    ggplot2::ggplot(ggplot2::aes(x = 亩产_kg)) +
    ggplot2::geom_histogram(bins = 25, fill = "#2E86AB", alpha = 0.7, color = "white") +
    ggplot2::geom_vline(xintercept = avg_yield,
      color = "#F18F01", linetype = "dashed", linewidth = 1.2)

  if (!is.na(ck_yield)) {
    p <- p + ggplot2::geom_vline(xintercept = ck_yield,
      color = "#C73E1D", linetype = "dashed", linewidth = 1.2)
  }

  p <- p +
    ggplot2::labs(x = "亩产 (kg)", y = "", title = "亩产分布情况") +
    ggplot2::annotate("text", x = avg_yield + diff(range(rdf[["亩产_kg"]], na.rm = TRUE)) * 0.05,
      y = max_count * 0.9, label = paste("平均:", round(avg_yield, 1), "kg"),
      color = "#F18F01", hjust = 0, size = 5) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12)
    )

  if (!is.na(ck_yield)) {
    p <- p + ggplot2::annotate("text",
      x = ck_yield + diff(range(rdf[["亩产_kg"]], na.rm = TRUE)) * 0.05,
      y = max_count * 0.8, label = paste("对照:", round(ck_yield, 1), "kg"),
      color = "#C73E1D", hjust = 0, size = 5)
  }

  p
}

#' 产量等级分布
#' @keywords internal
plot_yield_grade <- function(rdf) {
  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) return(NULL)

  rdf <- rdf %>%
    dplyr::mutate(产量等级 = dplyr::case_when(
      亩产_kg >= 200 ~ "(≥200kg)",
      亩产_kg >= 160 ~ "(160-199kg)",
      亩产_kg >= 120 ~ "(120-159kg)",
      TRUE ~ "(<120kg)"
    )) %>%
    dplyr::mutate(产量等级 = factor(产量等级,
      levels = c("(≥200kg)", "(160-199kg)", "(120-159kg)", "(<120kg)")))

  grade_counts <- rdf %>%
    dplyr::count(产量等级) %>%
    dplyr::mutate(比例_pct = (n / sum(n)) * 100)

  ggplot2::ggplot(grade_counts, ggplot2::aes(x = 产量等级, y = n, fill = 产量等级)) +
    ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = paste(n, "\n(", round(比例_pct, 1), "%)")),
      vjust = -0.3, size = 3.5) +
    ggplot2::scale_fill_manual(values = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D")) +
    ggplot2::labs(x = "产量等级", y = "", title = "产量等级分布") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 11),
      legend.position = "none"
    ) +
    ggplot2::ylim(0, max(grade_counts$n) * 1.15)
}

#' 增产分布
#' @keywords internal
plot_increase_distribution_chart <- function(rdf) {
  if (!requireNamespace("ggplot2", quietly = TRUE) || !requireNamespace("dplyr", quietly = TRUE)) return(NULL)

  inc_col <- if ("较临近对照增产_pct" %in% colnames(rdf)) "较临近对照增产_pct"
             else if ("较平均对照增产_pct" %in% colnames(rdf)) "较平均对照增产_pct"
             else return(NULL)

  rdf <- rdf %>%
    dplyr::mutate(增产等级 = dplyr::case_when(
      .data[[inc_col]] >= 10 ~ "增产(≥10%)",
      .data[[inc_col]] >= 0  ~ "增产(0-9%)",
      .data[[inc_col]] >= -10 ~ "减产(-10%~0)",
      TRUE ~ "减产(< -10%)"
    )) %>%
    dplyr::mutate(增产等级 = factor(增产等级,
      levels = c("增产(≥10%)", "增产(0-9%)", "减产(-10%~0)", "减产(< -10%)")))

  inc_counts <- rdf %>%
    dplyr::count(增产等级) %>%
    dplyr::mutate(比例_pct = (n / sum(n)) * 100)

  ggplot2::ggplot(inc_counts, ggplot2::aes(x = 增产等级, y = n, fill = 增产等级)) +
    ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = paste(n, "\n(", round(比例_pct, 1), "%)")),
      vjust = -0.3, size = 3.5) +
    ggplot2::scale_fill_manual(values = c("#6A994E", "#8B5A3C", "#F18F01", "#C73E1D")) +
    ggplot2::labs(x = "增产等级", y = "", title = "较对照增产分布") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = ggplot2::element_text(angle = 15, hjust = 1, size = 11),
      legend.position = "none"
    ) +
    ggplot2::ylim(0, max(inc_counts$n) * 1.15)
}

#' 生育期直方图
#' @keywords internal
plot_growth_hist <- function(rdf, growth_stats, ck_mean) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)

  if (!"生育期_d" %in% colnames(rdf) || is.null(growth_stats)) return(NULL)

  valid_data <- stats::na.omit(rdf[["生育期_d"]])
  if (length(valid_data) < 2) return(NULL)

  hist_growth <- hist(valid_data, breaks = 15, plot = FALSE)
  max_count <- max(hist_growth$counts)

  avg_growth <- growth_stats$平均生育期
  ck_growth <- if ("生育期_d" %in% colnames(ck_mean)) {
    ck_mean[["生育期_d"]]
  } else NA_real_

  p <- rdf %>%
    ggplot2::ggplot(ggplot2::aes(x = 生育期_d)) +
    ggplot2::geom_histogram(bins = 15, fill = "#8B5A3C", alpha = 0.7, color = "white") +
    ggplot2::geom_vline(xintercept = avg_growth,
      color = "#F18F01", linetype = "dashed", linewidth = 1.2) +
    ggplot2::labs(x = "生育期 (天)", y = "", title = "生育期分布") +
    ggplot2::annotate("text",
      x = avg_growth + diff(range(valid_data)) * 0.05,
      y = max_count * 0.9, label = paste("平均:", avg_growth, "天"),
      color = "#F18F01", hjust = 0, size = 5) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12)
    )

  if (!is.na(ck_growth)) {
    p <- p +
      ggplot2::geom_vline(xintercept = ck_growth,
        color = "#C73E1D", linetype = "dashed", linewidth = 1.2) +
      ggplot2::annotate("text",
        x = ck_growth + diff(range(valid_data)) * 0.05,
        y = max_count * 0.8, label = paste("对照:", ck_growth, "天"),
        color = "#C73E1D", hjust = 0, size = 5)
  }

  p
}

#' 性状 vs 产量散点图
#' @keywords internal
plot_trait_vs_yield <- function(rdf, x_var, corr_value = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) return(NULL)

  x_label <- gsub("_d$", " (天)", x_var)
  x_label <- gsub("_cm$", " (cm)", x_label)
  x_label <- gsub("_g$", " (g)", x_label)
  x_label <- gsub("_pct$", " (%)", x_label)

  title_label <- paste0(x_label, " 与产量关系")
  if (!is.null(corr_value) && !is.na(corr_value)) {
    title_label <- paste0(title_label, "\n相关系数: ", corr_value)
  }

  ggplot2::ggplot(rdf, ggplot2::aes(x = .data[[x_var]], y = 亩产_kg, color = 亩产_kg)) +
    ggplot2::geom_point(alpha = 0.6, size = 2) +
    ggplot2::geom_smooth(method = "lm", color = "#F18F01", se = FALSE, linewidth = 1) +
    ggplot2::scale_color_viridis_c(option = "viridis") +
    ggplot2::labs(x = x_label, y = "亩产 (kg)", title = title_label, color = "亩产") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 12, face = "bold"),
      legend.position = "right"
    )
}

#' 相关性矩阵图
#' @keywords internal
plot_correlation_matrix <- function(rdf, vars) {
  corr_matrix <- tryCatch({
    cor(rdf[, vars, drop = FALSE], use = "complete.obs") %>% round(2)
  }, error = function(e) matrix(0))

  # Always return a function (callable) for consistent API.
  # corrplot draws on the base graphics device; renderPlot handles this correctly.
  if (requireNamespace("corrplot", quietly = TRUE)) {
    function() {
      corrplot::corrplot(corr_matrix, method = "circle", type = "upper",
        order = "hclust", tl.col = "black", tl.srt = 45,
        addCoef.col = "black", number.cex = 0.8,
        title = "农艺性状与产量相关性", mar = c(0, 0, 2, 0))
    }
  } else {
    # Fallback: ggplot tile (also wrapped as callable)
    corr_df <- as.data.frame(as.table(corr_matrix))
    colnames(corr_df) <- c("Var1", "Var2", "Correlation")
    p <- ggplot2::ggplot(corr_df, ggplot2::aes(x = Var1, y = Var2, fill = Correlation)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::geom_text(ggplot2::aes(label = round(Correlation, 2)), size = 4) +
      ggplot2::scale_fill_gradient2(low = "#D73027", mid = "white", high = "#1A9850",
        midpoint = 0, limits = c(-1, 1)) +
      ggplot2::labs(title = "性状相关性矩阵", x = "", y = "") +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
      )
    function() p
  }
}
