# ==============================================================================
# 高产品种产量生育期组合图
# 基于 参考脚本/00-main_function.R:3859 generate_yield_growth_chart() 移植适配
# 用于 Shiny 分析流程：对 GGE 筛选出的高产稳定品种，生成各地点的
# 产量柱状图（平均 vs 对照）+ 生育期折线图（平均 vs 对照）双轴组合图
# ==============================================================================

# ---- 辅助：识别对照标记（兼容逻辑/数值/字符多种写法） ----
control_flag <- function(v) {
  if (is.logical(v)) return(v)
  if (is.numeric(v)) return(v == 1)
  v <- as.character(v)
  v_trim <- trimws(v)
  v_trim %in% c("1", "是", "对照", "Y", "y", "TRUE", "True", "true")
}

#' 生成高产基因型的产量-生育期双轴组合图
#'
#' @param df 原始数据框（拼音列名：place, stageid/name, MuChan, ShengYuQi, is_ck）
#' @param stable_genotypes analyze_gge() 返回的 stable_genotypes 数据框，
#'   含 stageid/name 列用于提取基因型名称
#' @return list(plots = named_list_of_ggplot, tables = named_list_of_data_frames)
#'   或 NULL（数据不足时）
generate_yield_growth_chart_shiny <- function(df, stable_genotypes) {
  # ---- 1. 输入校验 ----
  if (is.null(stable_genotypes) || nrow(stable_genotypes) == 0) return(NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)

  # 确定基因型列名
  gen_col <- if ("stageid" %in% colnames(df)) "stageid" else "name"
  if (!gen_col %in% colnames(df)) return(NULL)

  # 必要列检查
  need_cols <- c("place", gen_col, "MuChan", "ShengYuQi")
  miss <- setdiff(need_cols, colnames(df))
  if (length(miss) > 0) return(NULL)

  has_ck <- "is_ck" %in% colnames(df)

  # ---- 2. 从 stable_genotypes 提取基因型名及显示标签 ----
  # analyze_gge 返回的表格有三种可能列结构：
  #   (a) 含 stageid + name 列 — 显示为 "stageid (name)"
  #   (b) 含 name 列 — 以 name 作为标识和显示
  #   (c) 仅含 Genotype 列 — 以 Genotype 作为标识和显示
  label_map <- list()
  if ("stageid" %in% colnames(stable_genotypes)) {
    stage_names <- unique(as.character(stable_genotypes$stageid))
    if ("name" %in% colnames(stable_genotypes)) {
      # 构建 stageid → name 的一对一映射
      name_lookup <- stable_genotypes %>%
        dplyr::select(stageid, name) %>%
        dplyr::filter(!is.na(name) & nchar(name) > 0) %>%
        dplyr::distinct()
      for (i in seq_len(nrow(name_lookup))) {
        sid <- as.character(name_lookup$stageid[i])
        nm  <- as.character(name_lookup$name[i])
        label_map[[sid]] <- paste0(sid, " (", nm, ")")
      }
    }
    # 未命中映射的 stageid 原样显示
    for (sn in stage_names) {
      if (is.null(label_map[[sn]])) label_map[[sn]] <- sn
    }
  } else if ("name" %in% colnames(stable_genotypes)) {
    stage_names <- unique(as.character(stable_genotypes$name))
    for (sn in stage_names) label_map[[sn]] <- sn
  } else if ("Genotype" %in% colnames(stable_genotypes)) {
    stage_names <- unique(as.character(stable_genotypes$Genotype))
    for (sn in stage_names) label_map[[sn]] <- sn
  } else {
    return(NULL)
  }
  stage_names <- stage_names[nchar(stage_names) > 0]
  if (length(stage_names) == 0) return(NULL)

  plots_list  <- list()
  tables_list <- list()

  # ---- 3. 逐基因型生成图表 ----
  for (stage_name in stage_names) {
    display_label <- label_map[[stage_name]]
    if (is.null(display_label)) display_label <- stage_name

    # 3a. 筛选该基因型数据
    # 注意：变量名 stage_name 不能与数据框列名冲突（如 name 列），
    # 否则 dplyr data mask 会将变量解析为列名导致过滤失效
    stage_data <- df %>%
      dplyr::filter(!!rlang::sym(gen_col) == stage_name) %>%
      dplyr::group_by(place) %>%
      dplyr::summarise(
        平均亩产   = mean(MuChan, na.rm = TRUE),
        平均生育期 = mean(ShengYuQi, na.rm = TRUE),
        样本数量   = dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(平均亩产))

    if (nrow(stage_data) == 0) next

    # 3b. 对照数据（按地点汇总）
    control_data <- data.frame(
      place     = character(0),
      对照生育期 = numeric(0),
      对照亩产   = numeric(0),
      stringsAsFactors = FALSE
    )
    has_control <- FALSE

    if (has_ck) {
      ck_rows <- df[control_flag(df$is_ck), , drop = FALSE]
      if (nrow(ck_rows) > 0) {
        control_data <- ck_rows %>%
          dplyr::group_by(place) %>%
          dplyr::summarise(
            对照生育期 = mean(ShengYuQi, na.rm = TRUE),
            对照亩产   = mean(MuChan, na.rm = TRUE),
            .groups = "drop"
          )
      }
    }

    control_data_merge <- stage_data %>%
      dplyr::select(place) %>%
      dplyr::left_join(control_data, by = "place")

    has_control <- nrow(control_data) > 0 &&
      any(!is.na(control_data_merge$对照亩产))

    # 3c. 计算双Y轴比例系数
    yield_max  <- max(stage_data$平均亩产, control_data_merge$对照亩产, na.rm = TRUE)
    growth_max <- max(stage_data$平均生育期, na.rm = TRUE)
    if (has_control && any(!is.na(control_data_merge$对照生育期))) {
      growth_max <- max(growth_max, control_data_merge$对照生育期, na.rm = TRUE)
    }
    scale_factor <- if (growth_max == 0) 1 else yield_max / growth_max

    # 3d. 构建分组柱状图长表
    plot_yield_ctrl <- control_data_merge %>%
      dplyr::mutate(类型 = "对照亩产", 值 = 对照亩产) %>%
      dplyr::select(place, 类型, 值)
    plot_yield <- stage_data %>%
      dplyr::mutate(类型 = "平均亩产", 值 = 平均亩产) %>%
      dplyr::select(place, 类型, 值)
    plot_yield_all <- dplyr::bind_rows(plot_yield_ctrl, plot_yield)
    plot_yield_all <- plot_yield_all[!is.na(plot_yield_all$值), ]
    plot_yield_all$类型 <- factor(plot_yield_all$类型,
      levels = c("对照亩产", "平均亩产"))
    # 预计算标签颜色（避免在 geom_text aes 中映射触发重复 scale_color_manual）
    type_color_map <- c("对照亩产" = "#E67E22", "平均亩产" = "#2E86AB")
    plot_yield_all$label_color <- type_color_map[as.character(plot_yield_all$类型)]

    # 3e. 构建 ggplot
    p <- ggplot2::ggplot() +
      # 产量分组柱状图
      ggplot2::geom_col(
        data = plot_yield_all,
        ggplot2::aes(x = place, y = 值, fill = 类型),
        alpha = 0.8, width = 0.6,
        position = ggplot2::position_dodge(width = 0.65)
      ) +
      # 柱体底部数值标注（颜色直接指定，不走 scale mapping）
      ggplot2::geom_text(
        data = plot_yield_all,
        ggplot2::aes(x = place, y = 0,
          label = sprintf("%.1f", 值), group = 类型),
        color = plot_yield_all$label_color,
        vjust = 1.1, size = 3.2, fontface = "bold",
        position = ggplot2::position_dodge(width = 0.65),
        show.legend = FALSE
      ) +
      # 平均生育期折线 + 点
      ggplot2::geom_line(
        data = stage_data,
        ggplot2::aes(x = place, y = 平均生育期 * scale_factor,
          color = "平均生育期", group = 1),
        linewidth = 1.0
      ) +
      ggplot2::geom_point(
        data = stage_data,
        ggplot2::aes(x = place, y = 平均生育期 * scale_factor,
          color = "平均生育期"),
        size = 2.5, shape = 16, alpha = 0.9
      ) +
      # 平均生育期数值（折线上方）
      ggplot2::geom_text(
        data = stage_data,
        ggplot2::aes(x = place, y = 平均生育期 * scale_factor,
          label = sprintf("%.1f", 平均生育期)),
        vjust = -1.2, size = 3.2, color = "#27AE60", fontface = "bold"
      )

    # 对照生育期虚线 + 整数值标注
    has_ctrl_growth <- has_control &&
      any(!is.na(control_data_merge$对照生育期))
    if (has_ctrl_growth) {
      p <- p +
        ggplot2::geom_line(
          data = control_data_merge,
          mapping = ggplot2::aes(x = place, y = 对照生育期 * scale_factor,
            group = 1, color = "对照生育期"),
          linewidth = 1.0, linetype = "dashed",
          na.rm = TRUE, inherit.aes = FALSE
        ) +
        ggplot2::geom_text(
          data = control_data_merge,
          ggplot2::aes(x = place, y = 对照生育期 * scale_factor,
            label = sprintf("%.0f", 对照生育期)),
          vjust = 2.2, size = 3.2, color = "#E67E22",
          fontface = "bold", na.rm = TRUE
        )
    }

    # 3f. 双Y轴 + 主题 + 标签
    p <- p +
      ggplot2::scale_y_continuous(
        name = "平均亩产 (kg)",
        sec.axis = ggplot2::sec_axis(
          transform = ~ . / scale_factor,
          name = "平均生育期 (天)"
        ),
        limits = c(0, yield_max * 1.15 + max(20, yield_max * 0.1, na.rm = TRUE))
      ) +
      ggplot2::scale_fill_manual(
        values = c("对照亩产" = "#E67E22", "平均亩产" = "#2E86AB"),
        name = "产量",
        labels = c("对照亩产", "平均亩产")
      ) +
      ggplot2::scale_color_manual(
        values = c("平均生育期" = "#27AE60", "对照生育期" = "#E67E22"),
        name = "生育期",
        breaks = c("平均生育期", "对照生育期"),
        labels = c("平均生育期", "对照生育期")
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title       = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle    = ggplot2::element_text(hjust = 0.5, size = 14),
        axis.title.x     = ggplot2::element_text(size = 14, face = "bold"),
        axis.title.y     = ggplot2::element_text(size = 14, face = "bold"),
        axis.text.x      = ggplot2::element_text(size = 12, angle = 45, hjust = 1, vjust = 1),
        axis.text.y      = ggplot2::element_text(size = 12),
        legend.title     = ggplot2::element_text(size = 14, face = "bold"),
        legend.text      = ggplot2::element_text(size = 12),
        legend.position  = "bottom",
        legend.box       = "vertical",
        legend.margin    = ggplot2::margin(t = 8, b = 8),
        panel.grid.major.y = ggplot2::element_line(linetype = "dashed"),
        panel.grid.minor.y = ggplot2::element_blank(),
        plot.margin      = ggplot2::margin(15, 15, 15, 15, "pt"),
        plot.caption     = ggplot2::element_text(size = 10, hjust = 1)
      ) +
      ggplot2::labs(
        title    = "不同地点产量与生育期对比图",
        subtitle = paste("品种:", display_label),
        x        = "测试地点",
        y        = "平均亩产 (kg)",
        caption  = paste0(
          "数据来源: 多点测试数据 | ",
          "样本量: ", paste(unique(stage_data$样本数量), collapse = ","),
          " | 分析时间: ", Sys.time()
        )
      ) +
      ggplot2::guides(
        fill  = ggplot2::guide_legend(order = 1,
          override.aes = list(linetype = "blank")),
        color = ggplot2::guide_legend(order = 2,
          override.aes = list(fill = NA))
      )

    plots_list[[display_label]] <- p

    # 结果数据表
    result_tbl <- stage_data %>%
      dplyr::left_join(
        control_data_merge %>% dplyr::select(place, 对照亩产),
        by = "place"
      )
    tables_list[[display_label]] <- result_tbl
  }

  if (length(plots_list) == 0) return(NULL)

  list(plots = plots_list, tables = tables_list)
}
