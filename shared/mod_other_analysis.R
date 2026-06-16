# =============================================================================
# 其它试验分析弹窗
# =============================================================================

#' @param df 数据框（已清洗）
#' @param trial_name 试验名称
#' @param ns Shiny namespace function
other_analysis_show_ui <- function(df, trial_name, ns, input, output) {
  result <- run_analysis(df, "yield_test")

  ti <- result$trial_info
  caps <- result$capabilities
  tabs <- list()

  # ===== Info =====
  tabs$info <- tabPanel("分析信息", icon=icon("info-circle"), div(class="p-3",
    tags$h5(paste("试验名称：", trial_name)),
    tags$p(paste("试验类型：", ti$label, "—", ti$desc)),
    if (length(caps$available) > 0) tagList(
      tags$h6("可用分析："), tags$ul(lapply(caps$available, tags$li))),
    if (length(caps$unavailable) > 0) tagList(
      tags$h6("不可用分析："), tags$ul(class="text-muted", lapply(caps$unavailable, tags$li))),
    if (length(result$messages) > 0) lapply(result$messages, function(m) {
      div(class=if(grepl("^⚠️|跳过", m)) "alert alert-warning" else "alert alert-info",
          style="white-space:pre-wrap;", m) })))

  # ===== Yield =====
  if (!is.null(result$tables$yield_stats)) {
    yc <- tagList(
      tags$h5("产量核心统计"),
      DT::renderDataTable({DT::datatable(result$tables$yield_stats,
        options=list(pageLength=5,lengthMenu=c(5,10,15,20,25),dom='lftip'), rownames=FALSE, class="compact")}))

    if (isTRUE(ti$is_multi_site)) {
      if (!is.null(result$tables$per_site_yield_stats))
        yc <- tagList(yc, tags$hr(), tags$h5("分地点产量核心统计"),
          DT::renderDataTable({DT::datatable(result$tables$per_site_yield_stats,
            options=list(pageLength=length(ti$places),lengthMenu=c(5,10,15,20,25,50),dom='lftip'), rownames=FALSE, class="compact")}))
      if (!is.null(result$tables$per_site_growth_stats))
        yc <- tagList(yc, tags$h5("分地点生育期统计", style="margin-top:15px;"),
          DT::renderDataTable({DT::datatable(result$tables$per_site_growth_stats,
            options=list(pageLength=length(ti$places),lengthMenu=c(5,10,15,20,25,50),dom='lftip'), rownames=FALSE, class="compact")}))
      if (!is.null(result$tables$per_site_increase_stats))
        yc <- tagList(yc, tags$h5("分地点增产统计", style="margin-top:15px;"),
          DT::renderDataTable({DT::datatable(result$tables$per_site_increase_stats,
            options=list(pageLength=length(ti$places),lengthMenu=c(5,10,15,20,25,50),dom='lftip'), rownames=FALSE, class="compact")}))
    }

    yc <- tagList(yc,
      tags$hr(), tags$h5("产量与生育期分布"),
      fluidRow(column(6, if(!is.null(result$plots$yield_dist)) renderPlot({result$plots$yield_dist}, height=380)),
               column(6, if(!is.null(result$plots$yield_grade_dist)) renderPlot({result$plots$yield_grade_dist}, height=380))),
      fluidRow(column(6, if(!is.null(result$plots$increase_dist)) renderPlot({result$plots$increase_dist}, height=380)),
               column(6, if(!is.null(result$plots$growth_dist)) renderPlot({result$plots$growth_dist}, height=380))))

    if (isTRUE(ti$is_multi_site) && !is.null(result$per_site_plots)) {
      n_locs <- length(result$per_site_plots$yield_dist)
      if (n_locs > 0) {
        col_w <- if(n_locs<=2) 6L else if(n_locs==3) 4L else 3L
        ptypes <- list(yield_dist="亩产分布", yield_grade="产量等级分布",
                       increase_dist="增产分布", growth_dist="生育期分布")
        prows <- lapply(names(ptypes), function(pt) {
          locs <- names(result$per_site_plots[[pt]])
          if(length(locs)==0) return(NULL)
          fluidRow(lapply(locs, function(loc) column(col_w,
            tags$div(style="text-align:center;font-weight:bold;font-size:12px;", loc),
            renderPlot({result$per_site_plots[[pt]][[loc]]}, height=300))))})
        prows <- prows[!vapply(prows, is.null, logical(1))]
        yc <- tagList(yc, tags$hr(), tags$h5("分地点产量与生育期分布"), prows)
      }
    }

    scatter_items <- list(
      list(plot = result$plots$scatter_growth, label = "生育期-产量"),
      list(plot = result$plots$scatter_height, label = "株高-产量"),
      list(plot = result$plots$scatter_grain,   label = "百粒重-产量")
    )
    valid_scatter <- scatter_items[vapply(scatter_items, function(x) !is.null(x$plot), logical(1))]
    if (length(valid_scatter) > 0) {
      cols <- floor(12 / length(valid_scatter))
      yc <- tagList(yc, tags$hr(), tags$h5("性状与产量关系"),
        fluidRow(lapply(valid_scatter, function(x)
          column(cols, tags$div(style="text-align:center;font-weight:bold;font-size:12px;", x$label),
                 renderPlot({x$plot}, height=300)))))
    }
    if (!is.null(result$plots$corr_matrix))
      yc <- tagList(yc, tags$hr(), tags$h5("性状相关性"), renderPlot({result$plots$corr_matrix}, height=420))
    yc <- tagList(yc,
      tags$hr(), tags$h5("产量排名"),
      DT::renderDataTable({DT::datatable(result$tables$yield_ranking,
        options=list(pageLength=10,scrollX=TRUE,lengthMenu=c(5,10,15,20,25,50),dom='lftip'), rownames=FALSE, class="compact")}))
    if (!is.null(result$tables$cross_location_avg))
      yc <- tagList(yc, tags$hr(), tags$h5("各地点的平均"),
        DT::renderDataTable({DT::datatable(result$tables$cross_location_avg,
          options=list(pageLength=15,scrollX=TRUE,lengthMenu=c(5,10,15,20,25,50),dom='lftip'), rownames=FALSE, class="compact")}))

    tabs$yield <- tabPanel("产量概览", icon=icon("chart-bar"), div(class="p-3", yc))
  }

  # ===== Quality =====
  qt_nms <- grep("^quality_", names(result$plots), value=TRUE)
  if (length(qt_nms) > 0) {
    sites <- if (!is.null(result$per_site_quality)) names(result$per_site_quality) else NULL
    if (is.null(sites) || length(sites) == 0) {
      tabs$quality <- tabPanel("性状分布", icon=icon("chart-pie"), div(class="p-3",
        tags$h5("质量性状分布"),
        do.call(fluidRow, lapply(qt_nms, function(nm)
          column(6, renderPlot({result$plots[[nm]]}, height=300))))))
    } else {
      tabs$quality <- tabPanel("性状分布", icon=icon("chart-pie"), div(class="p-3",
        fluidRow(column(4, selectizeInput(ns("other_qt_site"), "选择地点",
          choices=c("全部", sites),
          selected=if("安徽宿州"%in%sites)"安徽宿州"else"全部", width="100%"))),
        uiOutput(ns("other_qt_content"))))

      output$other_qt_content <- renderUI({
        req(input$other_qt_site)
        if (input$other_qt_site == "全部") {
          do.call(fluidRow, lapply(qt_nms, function(nm)
            column(6, renderPlot({result$plots[[nm]]}, height=300))))
        } else {
          site <- input$other_qt_site
          sn <- names(result$per_site_quality[[site]])
          if (length(sn) > 0)
            do.call(fluidRow, lapply(sn, function(nm)
              column(6, renderPlot({result$per_site_quality[[site]][[nm]]}, height=300))))
          else div(class="text-muted p-3", "该地点无质量性状数据")
        }
      })
    }
  }

  # ===== Screening =====
  if (!is.null(result$tables$promoted)) {
    sc <- tagList(
      tags$h5("晋级材料"),
      DT::renderDataTable({DT::datatable(result$tables$promoted,
        options=list(pageLength=10,scrollX=TRUE,lengthMenu=c(5,10,15,20,25,50),dom='lftip'), rownames=FALSE, class="compact")}))
    if (!is.null(result$plots$comparison))
      sc <- tagList(sc, tags$hr(), tags$h5("筛选前后性状对比"), renderPlot({result$plots$comparison}, height=500))
    if (!is.null(result$plots$radar))
      sc <- tagList(sc, tags$hr(), tags$h5("优良品种雷达图"),
        renderPlot({ rd <- result$plots$radar; req(rd)
          colors <- rainbow(nrow(rd$data)-2L)
          fmsb::radarchart(rd$data, axistype=1, title=paste0("Top ", rd$top_n, " 品种综合性能"),
            vlabels=rd$labels, vlcex=0.8, pcol=colors, plwd=2, cglcol="gray80", cglty=1, cglwd=0.8)
          legend(x="bottomright", legend=rd$names, col=colors, lwd=2, cex=0.9, bty="n") }, height=500))
    if (!is.null(result$tables$description))
      sc <- tagList(sc, tags$hr(), tags$h5("晋级材料综合性状描述"),
        tags$pre(class="bg-light p-3", style="max-height:300px;overflow-y:auto;font-size:13px;", result$tables$description))
    tabs$screening <- tabPanel("品种筛选", icon=icon("filter"), div(class="p-3", sc))
  }

  # ===== Parent =====
  if (!is.null(result$tables$parent_stats)) {
    pc <- tagList(
      tags$h5("优良亲本"),
      DT::renderDataTable({DT::datatable(result$tables$parent_stats,
        options=list(pageLength=10,lengthMenu=c(5,10,15,20,25,50),dom='lftip'), rownames=FALSE, class="compact")}),
      tags$hr(), tags$h5("优良组合"),
      DT::renderDataTable({DT::datatable(result$tables$cross_stats,
        options=list(pageLength=10,lengthMenu=c(5,10,15,20,25,50),dom='lftip'), rownames=FALSE, class="compact")}))
    if (!is.null(result$plots$parent_plot))
      pc <- tagList(pc, tags$hr(), renderPlot({result$plots$parent_plot}, height=600))
    tabs$parent <- tabPanel("亲本分析", icon=icon("venus-mars"), div(class="p-3", pc))
  }

  # ===== GGE =====
  if (!is.null(result$plots$gge_biplot)) {
    gc <- tagList(
      tags$h5("GGE 双标图"), renderPlot({result$plots$gge_biplot}, height=500),
      tags$hr(), tags$h5("稳定性 × 产量"), renderPlot({result$plots$gge_stability}, height=500))
    if (!is.null(result$plots$gge_heatmap))
      gc <- tagList(gc, tags$hr(), tags$h5("G×E 互作热图"),
        renderPlot({result$plots$gge_heatmap},
          height = if (!is.null(result$plots$gge_heatmap_height)) result$plots$gge_heatmap_height else 500))
    gc <- tagList(gc, tags$hr(), tags$h5("基因型排名"), renderPlot({result$plots$gge_ranking}, height=500))
    if (!is.null(result$tables$gge_stable) && nrow(result$tables$gge_stable)>0)
      gc <- tagList(gc, tags$hr(), tags$h5("高产稳定基因型"),
        DT::renderDataTable({DT::datatable(result$tables$gge_stable,
          options=list(pageLength=10,lengthMenu=c(5,10,15,20,25,50),dom='lftip'), rownames=FALSE, class="compact")}))
    if (!is.null(result$tables$gge_unstable) && nrow(result$tables$gge_unstable)>0)
      gc <- tagList(gc, tags$hr(), tags$h5("高产不稳基因型（需关注）"),
        DT::renderDataTable({DT::datatable(result$tables$gge_unstable,
          options=list(pageLength=10,lengthMenu=c(5,10,15,20,25,50),dom='lftip'), rownames=FALSE, class="compact")}))
    tabs$gge <- tabPanel("GGE分析", icon=icon("globe"), div(class="p-3", gc))
  }

  # ===== Yield & Growth =====
  if (!is.null(result$plots$gge_yield_growth) && length(result$plots$gge_yield_growth) > 0) {
    yg_names <- names(result$plots$gge_yield_growth)
    yg_plots <- lapply(yg_names, function(nm) {
      tagList(tags$h5(nm, style="margin-top:20px;"),
              renderPlot({result$plots$gge_yield_growth[[nm]]}, height=500))
    })
    tabs$yield_growth <- tabPanel("产量与生育期", icon=icon("chart-line"), div(class="p-3", yg_plots))
  }

  # ===== Cross-site =====
  if (!is.null(result$tables$cross_site_ranking))
    tabs$cross_site <- tabPanel("跨地点排名", icon=icon("map-marked-alt"), div(class="p-3",
      tags$h5("跨地点产量排名"),
      DT::renderDataTable({DT::datatable(result$tables$cross_site_ranking,
        options=list(pageLength=10,scrollX=TRUE,lengthMenu=c(5,10,15,20,25,50),dom='lftip'), rownames=FALSE, class="compact")})))

  # ===== Export =====
  tabs$export <- tabPanel("导出", icon=icon("download"), div(class="p-3",
    tags$h5("导出分析结果"),
    actionButton(ns("other_export_btn"), "生成并下载压缩包", icon=icon("download"), class="btn-primary btn-lg")))

  list(tabs = tabs, result = result)
}
