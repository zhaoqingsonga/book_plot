# ==============================================================================
# HTML 分析报告生成
# 生成完全自包含的 HTML 文件，所有图表以 base64 图片嵌入
# ==============================================================================

# ---- 辅助：PNG → base64 ----

png_to_base64 <- function(path) {
  raw <- readBin(path, "raw", file.info(path)$size)
  paste0("data:image/png;base64,", jsonlite::base64_enc(raw))
}

# ---- 辅助：ggplot → base64 img 标签 ----

ggplot_to_img <- function(plot, width = 800, height = 500) {
  if (is.null(plot)) return(NULL)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ggplot2::ggsave(tmp, plot, width = width / 100, height = height / 100,
    dpi = 100, bg = "white", device = ragg::agg_png)
  src <- png_to_base64(tmp)
  htmltools::tags$div(class = "report-chart",
    htmltools::tags$img(src = src, width = "100%",
      style = "max-width:800px;display:block;margin:0 auto;"))
}

# ---- 辅助：基础图形 → base64 img 标签 ----

baseplot_to_img <- function(expr, width = 800, height = 500) {
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  ragg::agg_png(tmp, width = width, height = height, res = 100, bg = "white")
  tryCatch(force(expr), finally = grDevices::dev.off())
  src <- png_to_base64(tmp)
  htmltools::tags$div(class = "report-chart",
    htmltools::tags$img(src = src, width = "100%",
      style = "max-width:800px;display:block;margin:0 auto;"))
}

# ---- 辅助：data.frame → HTML 表格 ----

df_to_html_table <- function(df, max_rows = 200) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)
  n <- min(nrow(df), max_rows)
  df <- df[seq_len(n), , drop = FALSE]
  header <- htmltools::tags$thead(
    htmltools::tags$tr(lapply(colnames(df), function(nm) htmltools::tags$th(nm)))
  )
  body <- htmltools::tags$tbody(lapply(seq_len(n), function(i) {
    htmltools::tags$tr(lapply(df[i, , drop = FALSE], function(v) {
      htmltools::tags$td(if (is.numeric(v)) round(v, 2) else as.character(v))
    }))
  }))
  note <- if (nrow(df) > max_rows) {
    htmltools::tags$p(class = "text-muted small",
      paste0("（仅显示前 ", max_rows, " 行，共 ", nrow(df), " 行）"))
  }
  htmltools::tagList(htmltools::tags$table(class = "report-table", header, body), note)
}

# ---- 辅助：消息区块 ----

messages_to_html <- function(messages) {
  if (length(messages) == 0) return(NULL)
  htmltools::tags$div(lapply(messages, function(m) {
    cls <- if (grepl("^⚠️|跳过", m)) "alert alert-warning" else "alert alert-info"
    htmltools::tags$div(class = cls, m)
  }))
}

# ---- 辅助：能力列表 ----

capabilities_to_html <- function(caps) {
  htmltools::tagList(
    if (length(caps$available) > 0) htmltools::tagList(
      htmltools::tags$h5("可用分析"),
      htmltools::tags$ul(lapply(caps$available, htmltools::tags$li))
    ),
    if (length(caps$unavailable) > 0) htmltools::tagList(
      htmltools::tags$h5("不可用分析（原因）"),
      htmltools::tags$ul(class = "text-muted",
        lapply(caps$unavailable, htmltools::tags$li))
    )
  )
}

# ---- 报告 CSS ----

REPORT_CSS <- "
* { box-sizing: border-box; margin: 0; padding: 0; }
body {
  font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Microsoft YaHei', sans-serif;
  color: #333; background: #f5f7fa; line-height: 1.6;
}
.report-container { max-width: 960px; margin: 0 auto; padding: 20px; }
.report-cover {
  background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
  color: #fff; padding: 50px 40px; border-radius: 12px; margin-bottom: 24px;
  text-align: center;
}
.report-cover h1 { font-size: 28px; margin-bottom: 10px; }
.report-cover .subtitle { font-size: 14px; opacity: 0.85; }
.report-section {
  background: #fff; border-radius: 10px; padding: 28px 30px;
  margin-bottom: 20px; box-shadow: 0 1px 4px rgba(0,0,0,0.06);
}
.report-section h2 {
  font-size: 20px; color: #667eea; margin-bottom: 16px;
  padding-bottom: 10px; border-bottom: 2px solid #e8ecf1;
}
.report-section h3 { font-size: 16px; color: #555; margin: 20px 0 10px; }
.report-section h4 { font-size: 14px; color: #666; margin: 16px 0 8px; }
.report-table {
  width: 100%; border-collapse: collapse; font-size: 13px;
  margin: 10px 0 16px;
}
.report-table th {
  background: #667eea; color: #fff; padding: 8px 10px;
  text-align: left; font-weight: 600; white-space: nowrap;
}
.report-table td {
  padding: 6px 10px; border-bottom: 1px solid #e8ecf1;
}
.report-table tbody tr:hover { background: #f0f3ff; }
.report-table tbody tr:nth-child(even) { background: #f9fafb; }
.report-table tbody tr:nth-child(even):hover { background: #f0f3ff; }
.report-chart { margin: 16px 0; text-align: center; }
.report-chart img { border-radius: 6px; box-shadow: 0 1px 3px rgba(0,0,0,0.08); }
.alert { padding: 10px 14px; border-radius: 6px; margin: 6px 0; font-size: 13px; }
.alert-info { background: #e8f4fd; color: #0c5460; border: 1px solid #bee5eb; }
.alert-warning { background: #fff3cd; color: #856404; border: 1px solid #ffeaa7; }
.report-desc { background: #f8f9fa; border-left: 3px solid #667eea;
  padding: 12px 16px; margin: 10px 0; font-size: 13px;
  white-space: pre-wrap; max-height: 400px; overflow-y: auto;
}
.text-muted { color: #999; }
.small { font-size: 12px; }
.row { display: flex; flex-wrap: wrap; gap: 16px; }
.col { flex: 1; min-width: 280px; }
.report-footer {
  text-align: center; color: #999; font-size: 12px;
  padding: 20px; margin-top: 10px;
}
@media print {
  body { background: #fff; }
  .report-section { box-shadow: none; border: 1px solid #ddd; break-inside: avoid; }
  .report-cover { -webkit-print-color-adjust: exact; }
}
"

# ==============================================================================
# 各节构建函数
# ==============================================================================

#' 封面
build_report_header <- function(result) {
  trial_info <- result$trial_info
  htmltools::tags$div(class = "report-cover",
    htmltools::tags$h1("田间试验分析报告"),
    htmltools::tags$p(class = "subtitle",
      paste0("试验类型：", trial_info$label, " ｜ ",
             "地点数：", trial_info$n_places, " ｜ ",
             "重复数：", trial_info$n_reps, " ｜ ",
             "生成时间：", format(Sys.time(), "%Y-%m-%d %H:%M")))
  )
}

#' 分析信息节
build_report_info <- function(result) {
  trial_info <- result$trial_info
  caps <- result$capabilities

  htmltools::tags$div(class = "report-section",
    htmltools::tags$h2("一、分析信息"),
    htmltools::tags$p(paste("试验类型：", trial_info$label, "—", trial_info$desc)),
    if (length(result$trait_available) > 0) {
      htmltools::tags$p(paste("可用性状：",
        paste(sapply(result$trait_available, function(nm) {
          if (nm %in% names(TRAIT_DISPLAY_NAMES)) TRAIT_DISPLAY_NAMES[[nm]] else nm
        }), collapse = "、")))
    },
    capabilities_to_html(caps),
    messages_to_html(result$messages)
  )
}

#' 产量概览节
build_report_yield <- function(result) {
  if (is.null(result$tables$yield_stats)) return(NULL)

  children <- list(
    htmltools::tags$h2("二、产量概览"),
    htmltools::tags$h3("2.1 核心统计"),
    df_to_html_table(result$tables$yield_stats)
  )

  sub_idx <- 2L  # 下一小节的动态编号

  # 分地点产量统计（多地点时显示）
  if (isTRUE(result$trial_info$is_multi_site) &&
      !is.null(result$tables$per_site_yield_stats)) {
    children <- c(children, list(
      htmltools::tags$h3(sprintf("2.%d 分地点产量核心统计", sub_idx)),
      df_to_html_table(result$tables$per_site_yield_stats)
    ))
    sub_idx <- sub_idx + 1L
    if (!is.null(result$tables$per_site_growth_stats)) {
      children <- c(children, list(
        htmltools::tags$h3(sprintf("2.%d 分地点生育期统计", sub_idx)),
        df_to_html_table(result$tables$per_site_growth_stats)
      ))
      sub_idx <- sub_idx + 1L
    }
    if (!is.null(result$tables$per_site_increase_stats)) {
      children <- c(children, list(
        htmltools::tags$h3(sprintf("2.%d 分地点增产统计", sub_idx)),
        df_to_html_table(result$tables$per_site_increase_stats)
      ))
      sub_idx <- sub_idx + 1L
    }
  }

  children <- c(children, list(
    htmltools::tags$h3(sprintf("2.%d 产量与生育期分布", sub_idx))
  ))
  sub_idx <- sub_idx + 1L

  dist_plots <- list(
    "亩产分布" = result$plots$yield_dist,
    "产量等级分布" = result$plots$yield_grade,
    "增产分布" = result$plots$increase_dist,
    "生育期分布" = result$plots$growth_dist
  )
  for (nm in names(dist_plots)) {
    if (!is.null(dist_plots[[nm]])) {
      children <- c(children, list(
        htmltools::tags$h4(nm),
        ggplot_to_img(dist_plots[[nm]], 800, 380)
      ))
    }
  }

  # 分地点产量与生育期分布（多地点时显示）
  if (!is.null(result$per_site_plots)) {
    n_locs <- length(result$per_site_plots$yield_dist)
    if (n_locs > 0) {
      children <- c(children, list(
        htmltools::tags$h3(sprintf("2.%d 分地点产量与生育期分布", sub_idx))
      ))
      sub_idx <- sub_idx + 1L

      ptype_labels <- c(
        yield_dist    = "亩产分布",
        yield_grade   = "产量等级分布",
        increase_dist = "增产分布",
        growth_dist   = "生育期分布"
      )

      for (ptype in names(ptype_labels)) {
        locs <- names(result$per_site_plots[[ptype]])
        if (length(locs) == 0) next
        row_children <- list()
        for (loc in locs) {
          plot <- result$per_site_plots[[ptype]][[loc]]
          if (!is.null(plot)) {
            row_children <- c(row_children, list(
              htmltools::tags$div(class = "col",
                htmltools::tags$h5(loc, style = "text-align:center; margin-bottom:4px;"),
                ggplot_to_img(plot, 400, 300)
              )
            ))
          }
        }
        if (length(row_children) > 0) {
          children <- c(children, list(
            htmltools::tags$h4(ptype_labels[[ptype]]),
            htmltools::tags$div(class = "row", row_children)
          ))
        }
      }
    }
  }

  # 散点图
  scatter_plots <- list(
    "生育期 vs 产量" = result$plots$scatter_growth,
    "株高 vs 产量"   = result$plots$scatter_height,
    "百粒重 vs 产量"  = result$plots$scatter_grain
  )
  scatter_children <- list()
  for (nm in names(scatter_plots)) {
    if (!is.null(scatter_plots[[nm]])) {
      scatter_children <- c(scatter_children, list(
        htmltools::tags$div(class = "col", htmltools::tags$h4(nm),
          ggplot_to_img(scatter_plots[[nm]], 380, 300))
      ))
    }
  }
  if (length(scatter_children) > 0) {
    children <- c(children, list(
      htmltools::tags$h3(sprintf("2.%d 性状与产量关系", sub_idx)),
      htmltools::tags$div(class = "row", scatter_children)
    ))
    sub_idx <- sub_idx + 1L
  }

  # 相关矩阵
  if (!is.null(result$plots$corr_matrix)) {
    children <- c(children, list(
      htmltools::tags$h3(sprintf("2.%d 性状相关性", sub_idx)),
      baseplot_to_img(result$plots$corr_matrix(), 800, 420)
    ))
    sub_idx <- sub_idx + 1L
  }

  # 产量排名表
  if (!is.null(result$tables$yield_ranking)) {
    children <- c(children, list(
      htmltools::tags$h3(sprintf("2.%d 产量排名", sub_idx)),
      df_to_html_table(result$tables$yield_ranking, 50)
    ))
    sub_idx <- sub_idx + 1L
  }

  # 各地点的平均（多地点时按品种跨地点汇总）
  if (!is.null(result$tables$cross_location_avg)) {
    children <- c(children, list(
      htmltools::tags$h3(sprintf("2.%d 各地点的平均", sub_idx)),
      df_to_html_table(result$tables$cross_location_avg, 50)
    ))
    sub_idx <- sub_idx + 1L
  }

  htmltools::tags$div(class = "report-section", children)
}

#' 性状分布节
build_report_quality <- function(result) {
  qt_nms <- grep("^quality_", names(result$plots), value = TRUE)
  if (length(qt_nms) == 0) return(NULL)

  children <- list(htmltools::tags$h2("三、性状分布"))
  row_children <- list()
  for (nm in qt_nms) {
    label <- gsub("^quality_", "", nm)
    row_children <- c(row_children, list(
      htmltools::tags$div(class = "col", htmltools::tags$h4(label),
        ggplot_to_img(result$plots[[nm]], 460, 300))
    ))
  }
  children <- c(children, list(htmltools::tags$div(class = "row", row_children)))
  htmltools::tags$div(class = "report-section", children)
}

#' 品种筛选节
build_report_screening <- function(result) {
  if (is.null(result$tables$promoted)) return(NULL)

  children <- list(htmltools::tags$h2("四、品种筛选"))

  # 晋级材料表
  if (nrow(result$tables$promoted) > 0) {
    children <- c(children, list(
      htmltools::tags$h3("4.1 晋级材料"),
      df_to_html_table(result$tables$promoted, 50)
    ))
  }

  # 淘汰材料表
  if (!is.null(result$tables$eliminated) && nrow(result$tables$eliminated) > 0) {
    children <- c(children, list(
      htmltools::tags$h3("4.2 淘汰材料"),
      df_to_html_table(result$tables$eliminated, 30)
    ))
  }

  # 筛选前后对比图
  if (!is.null(result$plots$comparison)) {
    children <- c(children, list(
      htmltools::tags$h3("4.3 筛选前后性状对比"),
      ggplot_to_img(result$plots$comparison, 800, 500)
    ))
  }

  # 雷达图
  if (!is.null(result$plots$radar)) {
    rd <- result$plots$radar
    children <- c(children, list(
      htmltools::tags$h3("4.4 优良品种雷达图"),
      baseplot_to_img({
        n_varieties <- nrow(rd$data) - 2
        cols <- rainbow(n_varieties)
        fmsb::radarchart(rd$data, axistype = 1,
          title = paste0("Top ", rd$top_n, " 品种综合性能"),
          vlabels = rd$labels, vlcex = 0.8,
          pcol = cols, plwd = 2,
          cglcol = "gray80", cglty = 1, cglwd = 0.8)
        legend("topright", legend = rd$names, col = cols, lwd = 2, cex = 0.7, bg = "white")
      }, 800, 500)
    ))
  }

  # 评述
  if (!is.null(result$tables$description)) {
    children <- c(children, list(
      htmltools::tags$h3("4.5 晋级材料综合性状描述"),
      htmltools::tags$div(class = "report-desc", result$tables$description)
    ))
  }

  htmltools::tags$div(class = "report-section", children)
}

#' 亲本分析节
build_report_parent <- function(result) {
  if (is.null(result$tables$parent_stats)) return(NULL)

  children <- list(htmltools::tags$h2("五、亲本分析"))

  if (nrow(result$tables$parent_stats) > 0) {
    children <- c(children, list(
      htmltools::tags$h3("5.1 优良亲本"),
      df_to_html_table(result$tables$parent_stats, 30)
    ))
  }

  if (!is.null(result$tables$cross_stats) && nrow(result$tables$cross_stats) > 0) {
    children <- c(children, list(
      htmltools::tags$h3("5.2 优良组合"),
      df_to_html_table(result$tables$cross_stats, 30)
    ))
  }

  if (!is.null(result$plots$parent_plot)) {
    children <- c(children, list(
      htmltools::tags$h3("5.3 亲本晋级表现"),
      ggplot_to_img(result$plots$parent_plot, 800, 600)
    ))
  }

  htmltools::tags$div(class = "report-section", children)
}

#' GGE 分析节
build_report_gge <- function(result) {
  if (is.null(result$plots$gge_biplot)) return(NULL)

  children <- list(htmltools::tags$h2("六、GGE 分析"))

  gge_items <- list(
    "6.1 GGE 双标图"          = result$plots$gge_biplot,
    "6.2 稳定性 × 产量"       = result$plots$gge_stability,
    "6.3 基因型排名"          = result$plots$gge_ranking,
    "6.4 G×E 互作热图"       = result$plots$gge_heatmap
  )
  for (nm in names(gge_items)) {
    if (!is.null(gge_items[[nm]])) {
      children <- c(children, list(
        htmltools::tags$h3(nm),
        ggplot_to_img(gge_items[[nm]], 800, 500)
      ))
    }
  }

  if (!is.null(result$tables$gge_stable) && nrow(result$tables$gge_stable) > 0) {
    children <- c(children, list(
      htmltools::tags$h3("6.5 高产稳定基因型"),
      df_to_html_table(result$tables$gge_stable, 30)
    ))
  }

  if (!is.null(result$tables$gge_unstable) && nrow(result$tables$gge_unstable) > 0) {
    children <- c(children, list(
      htmltools::tags$h3("6.6 高产不稳基因型（需关注）"),
      df_to_html_table(result$tables$gge_unstable, 30)
    ))
  }

  htmltools::tags$div(class = "report-section", children)
}

#' 跨地点排名节
build_report_cross_site <- function(result) {
  if (is.null(result$tables$cross_site_ranking)) return(NULL)

  htmltools::tags$div(class = "report-section",
    htmltools::tags$h2("七、跨地点排名"),
    df_to_html_table(result$tables$cross_site_ranking, 50)
  )
}

#' 产量生育期节
build_report_yield_growth <- function(result) {
  yg_plots <- result$plots$gge_yield_growth
  if (is.null(yg_plots) || length(yg_plots) == 0) return(NULL)

  children <- list(htmltools::tags$h2("八、产量生育期"))

  stage_names <- names(yg_plots)
  for (i in seq_along(stage_names)) {
    sn <- stage_names[i]
    plot <- yg_plots[[sn]]
    if (!is.null(plot)) {
      children <- c(children, list(
        htmltools::tags$h3(paste0("8.", i, " ", sn)),
        ggplot_to_img(plot, 800, 500)
      ))
    }
  }

  htmltools::tags$div(class = "report-section", children)
}

#' 群体分析专题节
build_report_population <- function(result) {
  children <- list(htmltools::tags$h2("二、群体分析"))

  if (!is.null(result$tables$gen_dist)) {
    children <- c(children, list(
      htmltools::tags$h3("世代分布"),
      df_to_html_table(result$tables$gen_dist),
      if (!is.null(result$plots$gen_dist_chart))
        ggplot_to_img(result$plots$gen_dist_chart, 800, 400)
    ))
  }

  if (!is.null(result$tables$gen_track)) {
    children <- c(children, list(
      htmltools::tags$h3("世代追踪"),
      df_to_html_table(result$tables$gen_track),
      if (!is.null(result$plots$gen_track_chart))
        ggplot_to_img(result$plots$gen_track_chart, 800, 400)
    ))
  }

  if (!is.null(result$tables$cross_top)) {
    children <- c(children, list(
      htmltools::tags$h3("组合排名"),
      df_to_html_table(result$tables$cross_top),
      if (!is.null(result$plots$cross_chart))
        ggplot_to_img(result$plots$cross_chart, 800, 400)
    ))
  }

  if (!is.null(result$tables$trait_overview)) {
    children <- c(children, list(
      htmltools::tags$h3("性状概览"),
      df_to_html_table(result$tables$trait_overview, 50)
    ))
  }

  if (length(children) <= 1) return(NULL)  # only heading, no content
  htmltools::tags$div(class = "report-section", children)
}

#' 株行分析专题节
build_report_line_selection <- function(result) {
  children <- list(htmltools::tags$h2("二、株行分析"))

  if (!is.null(result$tables$sele_overview)) {
    children <- c(children, list(
      htmltools::tags$h3("选择概况"),
      df_to_html_table(result$tables$sele_overview),
      if (!is.null(result$plots$sele_dist_chart))
        ggplot_to_img(result$plots$sele_dist_chart, 800, 400)
    ))
  }

  if (!is.null(result$tables$sele_dist)) {
    children <- c(children, list(
      htmltools::tags$h3("选择分布"),
      df_to_html_table(result$tables$sele_dist)
    ))
  }

  if (!is.null(result$tables$progeny_top)) {
    children <- c(children, list(
      htmltools::tags$h3("优良后代"),
      df_to_html_table(result$tables$progeny_top),
      if (!is.null(result$plots$progeny_chart))
        ggplot_to_img(result$plots$progeny_chart, 800, 400)
    ))
  }

  if (!is.null(result$tables$morph_stats)) {
    children <- c(children, list(
      htmltools::tags$h3("形态统计"),
      df_to_html_table(result$tables$morph_stats, 50)
    ))
  }

  if (length(children) <= 1) return(NULL)
  htmltools::tags$div(class = "report-section", children)
}

# ==============================================================================
# 主入口
# ==============================================================================

#' 构建 HTML 分析报告并写入文件
#'
#' @param result run_analysis() 的返回值
#' @param output_path 输出文件路径（.html）
#' @export
build_html_report <- function(result, output_path) {
  if (result$type == "error") {
    html <- htmltools::tags$html(
      htmltools::tags$head(
        htmltools::tags$meta(charset = "UTF-8"),
        htmltools::tags$title("分析报告 — 错误")
      ),
      htmltools::tags$body(
        htmltools::tags$h1("分析失败"),
        htmltools::tags$p(result$messages)
      )
    )
    htmltools::save_html(html, file = output_path)
    return(invisible())
  }

  # 组装报告 body
  sections <- list(build_report_header(result), build_report_info(result))

  if (result$type == "yield_test") {
    sections <- c(sections,
      list(build_report_yield(result)),
      list(build_report_quality(result)),
      list(build_report_screening(result)),
      list(build_report_parent(result)),
      list(build_report_gge(result)),
      list(build_report_cross_site(result)),
      list(build_report_yield_growth(result))
    )
  } else if (result$type == "population") {
    sections <- c(sections, list(build_report_population(result)))
  } else if (result$type == "line_selection") {
    sections <- c(sections, list(build_report_line_selection(result)))
  }

  # 页脚
  sections <- c(sections, list(
    htmltools::tags$div(class = "report-footer",
      htmltools::tags$p(paste0("由 田间记录本生成及田间规划 自动生成 ｜ ",
        format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
    )
  ))

  # 构建完整 HTML
  html <- htmltools::tags$html(lang = "zh-CN",
    htmltools::tags$head(
      htmltools::tags$meta(charset = "UTF-8"),
      htmltools::tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
      htmltools::tags$title(paste0("分析报告 — ", result$trial_info$label)),
      htmltools::tags$style(REPORT_CSS)
    ),
    htmltools::tags$body(
      htmltools::tags$div(class = "report-container",
        Filter(Negate(is.null), sections)
      )
    )
  )

  htmltools::save_html(html, file = output_path)
  invisible(output_path)
}

# ==============================================================================
# 压缩包报告（图片 + Excel + HTML）
# ==============================================================================

#' 将 ggplot 保存为 PNG 文件
#'
#' @param plot ggplot 对象
#' @param path 输出路径
#' @param width 宽度（像素）
#' @param height 高度（像素）
#' @keywords internal
save_plot_png <- function(plot, path, width = 1200, height = 700) {
  if (is.null(plot)) return(FALSE)
  tryCatch({
    ggplot2::ggsave(path, plot, width = width / 100, height = height / 100,
      dpi = 100, bg = "white", device = ragg::agg_png)
    TRUE
  }, error = function(e) FALSE)
}

#' 将基础图形（如 fmsb::radarchart）保存为 PNG
#'
#' @param expr 绘图表达式
#' @param path 输出路径
#' @param width 宽度
#' @param height 高度
#' @keywords internal
save_baseplot_png <- function(expr, path, width = 1200, height = 700) {
  tryCatch({
    ragg::agg_png(path, width = width, height = height, res = 100, bg = "white")
    force(expr)
    grDevices::dev.off()
    TRUE
  }, error = function(e) {
    try(grDevices::dev.off(), silent = TRUE)
    FALSE
  })
}

#' 构建分析结果压缩包
#'
#' 包含：Markdown 报告、HTML 报告、Excel 报告、所有图表 PNG、关键数据 CSV
#'
#' @param result run_analysis() 的返回值
#' @param output_path 输出 zip 文件路径
# ==============================================================================
# Markdown 分析报告
# ==============================================================================

md_table <- function(df) {
  if (!is.data.frame(df) || nrow(df) == 0) return("")
  hdr <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", ncol(df)), collapse = "|"), "|")
  rows <- apply(df, 1, function(r) {
    vals <- sapply(r, function(v) if (is.na(v)) "" else as.character(v))
    paste0("| ", paste(vals, collapse = " | "), " |")
  })
  c(hdr, sep, rows)
}

build_markdown_report <- function(result, output_path, chart_dir = "图表", table_dir = "数据表") {
  if (result$type == "error") {
    writeLines(c("# 分析报告 — 错误", "", result$messages), output_path, useBytes = TRUE)
    return(invisible())
  }

  lines <- character()
  add <- function(...) lines <<- c(lines, paste0(...))
  br  <- function() lines <<- c(lines, "")
  ti <- result$trial_info; caps <- result$capabilities

  # ---- 头部 ----
  add("# 田间试验分析报告"); br()
  add(sprintf("**生成时间**: %s | **试验类型**: %s — %s",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S"), ti$label, ti$desc)); br()

  if (length(caps$available) > 0) {
    add("## 可用分析")
    for (c in caps$available) add(sprintf("- %s", c)); br() }
  if (length(result$messages) > 0) {
    add("## 提示信息")
    for (m in result$messages) add(sprintf("- %s", m)); br() }

  # =====================================================================
  # 产比试验
  # =====================================================================
  if (result$type == "yield_test") {
    if (!is.null(result$tables$yield_stats)) {
      add("## 产量概览")
      table_labels <- c(yield_stats="产量核心统计",per_site_yield_stats="分地点产量统计",
        per_site_growth_stats="分地点生育期统计",per_site_increase_stats="分地点增产统计",
        cross_location_avg="各地点的平均",yield_ranking="产量排名")
      for (nm in names(table_labels)) {
        if (!is.null(result$tables[[nm]]) && is.data.frame(result$tables[[nm]]) && nrow(result$tables[[nm]]) > 0) {
          add(sprintf("### %s", table_labels[nm])); br()
          add(md_table(if(nrow(result$tables[[nm]])>25) head(result$tables[[nm]],25) else result$tables[[nm]])); br()
        }
      }
      plot_labels <- c(yield_dist="亩产分布",yield_grade="产量等级分布",increase_dist="增产分布",
        growth_dist="生育期分布",scatter_growth="生育期vs产量",scatter_height="株高vs产量",
        scatter_grain="百粒重vs产量",comparison="筛选前后性状对比",radar="雷达图")
      for (key in names(plot_labels)) {
        if (!is.null(result$plots[[key]])) {
          add(sprintf("![%s](%s/%s.png)", plot_labels[key], chart_dir, plot_labels[key])); br()
        }
      }
      if (!is.null(result$per_site_plots)) {
        ptype_labels <- c(yield_dist="亩产分布",yield_grade="产量等级分布",increase_dist="增产分布",growth_dist="生育期分布")
        for (ptype in names(ptype_labels)) {
          for (loc in names(result$per_site_plots[[ptype]])) {
            plot <- result$per_site_plots[[ptype]][[loc]]
            if (!is.null(plot)) {
              fn <- paste0("分地点_", ptype_labels[[ptype]], "_", loc, ".png")
              add(sprintf("![分地点%s — %s](%s/%s)", ptype_labels[[ptype]], loc, chart_dir, fn)); br()
            }
          }
        }
      }
      if (!is.null(result$plots$corr_matrix))
        add(sprintf("![性状相关性](%s/性状相关性矩阵.png)", chart_dir)); br() }

    qt_nms <- grep("^quality_", names(result$plots), value = TRUE)
    if (length(qt_nms) > 0) {
      add("## 性状分布")
      for (nm in qt_nms) {
        label <- gsub("^quality_", "", nm)
        add(sprintf("![性状分布_%s](%s/性状分布_%s.png)", label, chart_dir, label))
      }; br() }

    if (!is.null(result$tables$promoted)) {
      add("## 品种筛选")
      add("### 晋级材料")
      add(md_table(head(result$tables$promoted, 25)))
      if (!is.null(result$tables$eliminated) && nrow(result$tables$eliminated) > 0) {
        add("### 淘汰材料")
        add(md_table(head(result$tables$eliminated, 30))); br()
      }
      if (!is.null(result$tables$description)) { add("### 晋级材料评述"); add(result$tables$description) }; br() }

    if (!is.null(result$tables$parent_stats)) {
      add("## 亲本分析")
      if (!is.null(result$plots$parent_plot)) add(sprintf("![亲本晋级表现](%s/亲本晋级表现.png)", chart_dir))
      add(md_table(head(result$tables$parent_stats, 20)))
      add(md_table(head(result$tables$cross_stats, 20))); br() }

    if (!is.null(result$plots$gge_biplot)) {
      add("## GGE 分析")
      for (nm in c("GGE双标图","稳定性vs产量","GxE互作热图","基因型排名"))
        add(sprintf("![%s](%s/%s.png)", nm, chart_dir, nm))
      if (!is.null(result$tables$gge_stable) && nrow(result$tables$gge_stable) > 0)
        { add("### 高产稳定基因型"); add(md_table(result$tables$gge_stable)) }
      if (!is.null(result$tables$gge_unstable) && nrow(result$tables$gge_unstable) > 0)
        { add("### 高产不稳基因型"); add(md_table(result$tables$gge_unstable)) }; br() }

    if (!is.null(result$plots$gge_yield_growth) && length(result$plots$gge_yield_growth) > 0) {
      add("## 产量生育期")
      for (sn in names(result$plots$gge_yield_growth)) {
        safe_sn <- gsub("[\\\\/:*?\"<>|() ]", "_", sn)
        add(sprintf("![产量生育期_%s](%s/产量生育期_%s.png)",
          safe_sn, chart_dir, safe_sn))
      }; br() }

    if (!is.null(result$tables$cross_site_ranking)) {
      add("## 跨地点排名")
      add(md_table(head(result$tables$cross_site_ranking, 25))); br() }

  # =====================================================================
  # 群体分析
  # =====================================================================
  } else if (result$type == "population") {
    # 世代分布
    if (!is.null(result$tables$gen_dist)) {
      add("## 世代分布")
      add(md_table(result$tables$gen_dist)); br()
      if (!is.null(result$plots$gen_dist_chart))
        add(sprintf("![世代分布](%s/世代分布.png)", chart_dir)); br() }

    # 世代追踪
    if (!is.null(result$tables$gen_track)) {
      add("## 世代追踪")
      add(md_table(result$tables$gen_track)); br()
      if (!is.null(result$plots$gen_track_chart))
        add(sprintf("![世代追踪](%s/世代追踪.png)", chart_dir)); br() }

    # 组合排名
    if (!is.null(result$tables$cross_top)) {
      add("## 组合排名")
      add(md_table(result$tables$cross_top)); br()
      if (!is.null(result$plots$cross_chart))
        add(sprintf("![组合排名](%s/组合排名.png)", chart_dir)); br() }

    # 性状概览
    if (!is.null(result$tables$trait_overview) && nrow(result$tables$trait_overview) > 0) {
      add("## 性状概览")
      add(md_table(if(nrow(result$tables$trait_overview)>25) head(result$tables$trait_overview,25) else result$tables$trait_overview)); br() }

  # =====================================================================
  # 株行分析
  # =====================================================================
  } else if (result$type == "line_selection") {
    # 选择概况
    if (!is.null(result$tables$sele_overview)) {
      add("## 选择概况")
      add(md_table(result$tables$sele_overview)); br()
      if (!is.null(result$plots$sele_dist_chart))
        add(sprintf("![选择分布](%s/选择分布.png)", chart_dir)); br() }

    # 选择分布
    if (!is.null(result$tables$sele_dist)) {
      add("## 选择分布")
      add(md_table(result$tables$sele_dist)); br() }

    # 优良后代
    if (!is.null(result$tables$progeny_top)) {
      add("## 优良后代")
      add(md_table(result$tables$progeny_top)); br()
      if (!is.null(result$plots$progeny_chart))
        add(sprintf("![优良后代](%s/优良后代.png)", chart_dir)); br() }

    # 形态统计
    if (!is.null(result$tables$morph_stats) && nrow(result$tables$morph_stats) > 0) {
      add("## 形态统计")
      add(md_table(if(nrow(result$tables$morph_stats)>25) head(result$tables$morph_stats,25) else result$tables$morph_stats)); br() }
  }

  add("---")
  add(sprintf("*由 田间记录本生成及田间规划 自动生成 ｜ %s*", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  writeLines(lines, output_path, useBytes = TRUE)
  invisible(output_path)
}

#' @export
build_analysis_zip <- function(result, output_path) {
  tmpdir <- file.path(tempdir(), paste0("analysis_", format(Sys.time(), "%Y%m%d%H%M%S")))
  dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  chart_dir <- file.path(tmpdir, "图表")
  table_dir <- file.path(tmpdir, "数据表")
  dir.create(chart_dir, showWarnings = FALSE)
  dir.create(table_dir, showWarnings = FALSE)

  plot_names <- list(
    yield_dist       = "亩产分布",
    yield_grade      = "产量等级分布",
    increase_dist    = "增产分布",
    growth_dist      = "生育期分布",
    scatter_growth   = "生育期vs产量",
    scatter_height   = "株高vs产量",
    scatter_grain    = "百粒重vs产量",
    comparison       = "筛选前后性状对比",
    parent_plot      = "亲本晋级表现",
    gge_biplot       = "GGE双标图",
    gge_stability    = "稳定性vs产量",
    gge_heatmap      = "GxE互作热图",
    gge_ranking      = "基因型排名",
    gen_dist_chart   = "世代分布",
    gen_track_chart  = "世代追踪",
    cross_chart      = "组合排名",
    sele_dist_chart  = "选择分布",
    progeny_chart    = "优良后代"
  )

  # ---- 保存 ggplot 图表 ----
  for (key in names(plot_names)) {
    plot <- result$plots[[key]]
    if (!is.null(plot) && inherits(plot, "ggplot")) {
      nm <- plot_names[[key]]
      fn <- paste0(nm, ".png")
      save_plot_png(plot, file.path(chart_dir, fn))
    }
  }

  # ---- 品质性状图 ----
  qt_nms <- grep("^quality_", names(result$plots), value = TRUE)
  for (nm in qt_nms) {
    plot <- result$plots[[nm]]
    if (!is.null(plot) && inherits(plot, "ggplot")) {
      label <- gsub("^quality_", "", nm)
      save_plot_png(plot, file.path(chart_dir, paste0("性状分布_", label, ".png")))
    }
  }

  # ---- 产量生育期图 ----
  if (!is.null(result$plots$gge_yield_growth)) {
    for (sn in names(result$plots$gge_yield_growth)) {
      plot <- result$plots$gge_yield_growth[[sn]]
      if (!is.null(plot) && inherits(plot, "ggplot")) {
        safe_sn <- gsub("[\\\\/:*?\"<>|() ]", "_", sn)
        fn <- paste0("产量生育期_", safe_sn, ".png")
        save_plot_png(plot, file.path(chart_dir, fn))
      }
    }
  }

  # ---- 分地点分布图 ----
  if (!is.null(result$per_site_plots)) {
    ptype_labels <- c(
      yield_dist    = "亩产分布",
      yield_grade   = "产量等级分布",
      increase_dist = "增产分布",
      growth_dist   = "生育期分布"
    )
    for (ptype in names(ptype_labels)) {
      for (loc in names(result$per_site_plots[[ptype]])) {
        plot <- result$per_site_plots[[ptype]][[loc]]
        if (!is.null(plot) && inherits(plot, "ggplot")) {
          fn <- paste0("分地点_", ptype_labels[[ptype]], "_", loc, ".png")
          save_plot_png(plot, file.path(chart_dir, fn))
        }
      }
    }
  }

  # ---- 雷达图（基础图形） ----
  if (!is.null(result$plots$radar)) {
    rd <- result$plots$radar
    save_baseplot_png({
      n_varieties <- nrow(rd$data) - 2
      cols <- rainbow(n_varieties)
      fmsb::radarchart(rd$data, axistype = 1,
        title = paste0("Top ", rd$top_n, " 品种综合性能"),
        vlabels = rd$labels, vlcex = 0.8,
        pcol = cols, plwd = 2,
        cglcol = "gray80", cglty = 1, cglwd = 0.8)
      legend("topright", legend = rd$names, col = cols, lwd = 2, cex = 0.7, bg = "white")
    }, file.path(chart_dir, "雷达图.png"))
  }

  # ---- 相关性矩阵（基础图形） ----
  if (!is.null(result$plots$corr_matrix)) {
    save_baseplot_png({
      result$plots$corr_matrix()
    }, file.path(chart_dir, "性状相关性矩阵.png"))
  }

  # ---- 保存数据表为 CSV ----
  # 跳过过大的 export_data 和 description（非表格）
  skip_tables <- c("export_data", "description")
  for (nm in names(result$tables)) {
    tbl <- result$tables[[nm]]
    if (nm %in% skip_tables) next
    if (is.data.frame(tbl) && nrow(tbl) > 0) {
      safe_nm <- gsub("[\\\\/:*?\"<>|]", "_", nm)
      write.csv(tbl, file.path(table_dir, paste0(safe_nm, ".csv")),
        row.names = FALSE, fileEncoding = "UTF-8")
    }
  }

  # ---- 描述文本 ----
  if (!is.null(result$tables$description) && is.character(result$tables$description)) {
    writeLines(result$tables$description,
      file.path(tmpdir, "晋级材料评述.txt"), useBytes = TRUE)
  }

  # ---- Excel 报告 ----
  xlsx_path <- file.path(tmpdir, "分析报告.xlsx")
  tryCatch({
    build_analysis_excel(result, xlsx_path)
  }, error = function(e) {
    message("Excel 报告生成失败: ", e$message)
  })

  # ---- Markdown 报告 ----
  md_path <- file.path(tmpdir, "分析报告.md")
  tryCatch({
    build_markdown_report(result, md_path, "图表", "数据表")
  }, error = function(e) {
    message("Markdown 报告生成失败: ", e$message)
  })

  # ---- HTML 报告 ----
  html_path <- file.path(tmpdir, "分析报告.html")
  tryCatch({
    build_html_report(result, html_path)
  }, error = function(e) {
    message("HTML 报告生成失败: ", e$message)
  })

  # ---- 打包 ----
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(tmpdir)
  top_entries <- list.files(".", all.files = FALSE, no.. = TRUE)
  zip::zip(output_path, top_entries, mode = "mirror")
  setwd(old_wd)

  invisible(output_path)
}
