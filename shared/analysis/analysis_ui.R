# ==============================================================================
# 通用分析模块「分析」—— Shiny 界面
#
# 顶层「分析」标签页：选试验 → 跑 run_analysis → 看结果 / 下报告。
# 核心函数在 analysis_module.R，与 scripts/analyze_trial.R 共用同一套口径，
# 报告落盘也走同一个 save_analysis_artifacts()。
# ==============================================================================

# ---- 辅助：把 Markdown 里的相对图片链接换成 base64 内嵌 ----
# 命令行产出的 分析报告.md 与「图表/」目录并列，链接是相对的；
# 单独下载 .md 时图片会断链，这里就地内嵌成 data URI。
inline_md_images <- function(md_path, chart_dir) {
  if (!file.exists(md_path)) return(invisible(FALSE))
  md <- readLines(md_path, warn = FALSE)
  pat <- "^(!\\[[^]]*\\]\\()([^)]*\\.png)(\\).*)$"

  for (i in seq_along(md)) {
    m <- regmatches(md[i], regexec(pat, md[i]))[[1]]
    if (length(m) != 4) next
    png <- file.path(chart_dir, basename(m[3]))
    if (!file.exists(png)) next
    b64 <- paste0("data:image/png;base64,",
                  jsonlite::base64_enc(readBin(png, "raw", file.info(png)$size)))
    # regexec 返回 [完整匹配, 组1, 组2, 组3]：m[2] 是 "![标题]("，m[4] 是 ")"
    md[i] <- paste0(m[2], b64, m[4])
  }
  writeLines(md, md_path, useBytes = TRUE)
  invisible(TRUE)
}

#' 通用分析界面
#'
#' @param id 模块 id
#' @export
generic_analysis_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(class = "tab-panel",
      h3(class = "panel-title", span(class = "icon", icon("chart-bar")), "分析"),
      p(class = "text-muted fb-panel-intro",
        "统一分析入口：田间记录本里的任意试验都可以在这里跑一遍完整分析管线。",
        "结果与命令行 ", tags$code("scripts/analyze_trial.R"), " 完全一致。"),

      fluidRow(
        column(4,
          div(class = "card",
            div(class = "card-header", icon("sliders"), " 选择试验"),
            div(class = "card-body",
              selectInput(ns("type_filter"), "记录本类型",
                choices = c("全部" = "all", "产比" = "yield_test",
                            "群体" = "population", "株行" = "line_selection"),
                selected = "all", width = "100%"),
              selectizeInput(ns("trial"), "试验",
                choices = NULL, width = "100%",
                options = list(placeholder = "选择要分析的试验")),
              actionButton(ns("btn_run"), "开始分析",
                icon = icon("chart-bar"), class = "btn-primary w-100"),
              hr(),
              div(class = "d-flex gap-1 flex-wrap",
                downloadButton(ns("dl_excel"), "Excel",
                  class = "btn-outline-secondary btn-sm"),
                downloadButton(ns("dl_html"), "HTML",
                  class = "btn-outline-secondary btn-sm"),
                downloadButton(ns("dl_md"), "Markdown",
                  class = "btn-outline-secondary btn-sm"),
                downloadButton(ns("dl_zip"), "ZIP 全包",
                  class = "btn-outline-secondary btn-sm"))
            ))
        ),
        column(8,
          uiOutput(ns("info_card")),
          uiOutput(ns("stats_card"))
        )
      ),

      uiOutput(ns("tables"))
    )
  )
}

#' 通用分析 server
#'
#' @param id 模块 id
#' @export
generic_analysis_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(result = NULL, trials = NULL, meta = NULL)
    db_path <- defaultDbPath()

    # ---- 试验清单 ----
    observe({
      rv$trials <- tryCatch(analysis_list_trials(db_path), error = function(e) NULL)
    })

    # ---- 按类型过滤，刷新下拉 ----
    observe({
      trials <- rv$trials
      tf <- input$type_filter
      if (is.null(trials) || nrow(trials) == 0) {
        updateSelectizeInput(session, "trial", choices = NULL, selected = NULL)
        return()
      }
      d <- trials
      if (!is.null(tf) && nzchar(tf) && tf != "all") {
        d <- d[d$module_type == tf, , drop = FALSE]
      }
      if (nrow(d) == 0) {
        updateSelectizeInput(session, "trial", choices = NULL, selected = NULL)
        return()
      }
      vals <- paste0(d$module_type, "::", d$experiment_id)
      labs <- sprintf("%s ｜ %s（%d 条）", d$type_label, d$experiment_name, d$n)
      updateSelectizeInput(session, "trial",
        choices = stats::setNames(vals, labs), selected = vals[1])
    })

    # ---- 执行分析 ----
    observeEvent(input$btn_run, {
      sel <- input$trial
      if (is.null(sel) || !nzchar(sel)) {
        showNotification("请先选择一个试验", type = "warning", duration = 4)
        return()
      }
      parts <- strsplit(sel, "::", fixed = TRUE)[[1]]
      module_type <- parts[1]
      experiment_id <- paste(parts[-1], collapse = "::")

      res <- tryCatch({
        withProgress(message = "正在执行分析管线...", value = 0.3, {
          df <- analysis_load_trial(experiment_id, module_type, db_path)
          run_analysis(df, module_type)
        })
      }, error = function(e) e)

      if (inherits(res, "error")) {
        rv$result <- NULL
        rv$meta   <- NULL
        showNotification(paste("分析失败：", conditionMessage(res)),
          type = "error", duration = 10)
        return()
      }

      rv$result <- res
      rv$meta   <- list(id = experiment_id, type = module_type,
                        label = unname(ANALYSIS_TRIAL_TYPES[[module_type]]))
      showNotification("分析完成", type = "message", duration = 3)
    })

    # ---- 分析信息 ----
    output$info_card <- renderUI({
      req(rv$result)
      ti   <- rv$result$trial_info
      cap  <- rv$result$capabilities
      msgs <- rv$result$messages

      rows <- list(
        list("试验类型", ti$label),
        list("试验信息", ti$desc),
        list("试验地点", paste(ti$places, collapse = "、")),
        list("重复数", as.character(ti$n_reps)),
        list("性状数据", if (isTRUE(rv$result$has_traits)) "有" else "无"),
        list("可用分析", paste(cap$available, collapse = "、"))
      )

      div(class = "card mb-3",
        div(class = "card-header", icon("info-circle"), " 分析信息"),
        div(class = "card-body",
          tags$table(class = "table table-sm mb-0",
            tags$tbody(lapply(rows, function(r) {
              tags$tr(
                tags$th(style = "width: 110px; font-weight: 600;", r[[1]]),
                tags$td(r[[2]]))
            }))),
          if (length(msgs) > 0) {
            tags$div(class = "mt-2 p-2 rounded",
              style = "background: var(--bs-warning-bg-subtle, #fff8e1); font-size: 0.85em;",
              tags$strong("提示："),
              tags$ul(class = "mb-0 ps-3", lapply(msgs, function(m) tags$li(m))))
          }
        ))
    })

    # ---- 产量核心统计 + 筛选摘要 ----
    output$stats_card <- renderUI({
      req(rv$result)
      t <- rv$result$tables

      stats_html <- if (!is.null(t$yield_stats) && is.data.frame(t$yield_stats)) {
        div(class = "mb-3",
          h6(class = "text-muted mb-1", "产量核心统计"),
          DT::datatable(t$yield_stats, rownames = FALSE, options = list(
            dom = "t", paging = FALSE, searching = FALSE,
            info = FALSE, ordering = FALSE)))
      } else NULL

      scr_html <- if (!is.null(t$screening_summary)) {
        s <- t$screening_summary
        div(class = "mb-3",
          h6(class = "text-muted mb-1", "品种筛选"),
          tags$p(class = "mb-1",
            tags$strong(sprintf("共 %d 个品种 → 晋级 %d 个 → 分离选单株 %d 个 → 淘汰 %d 个",
              s$total_n, s$promoted_n, s$select_plant_n, s$eliminated_n))),
          tags$small(class = "text-muted",
            sprintf("淘汰明细：位次不达标 %d ｜ 分离材料 %d ｜ 倒伏排除 %d",
              s$breakdown$rank_fail, s$breakdown$separated, s$breakdown$lodging)))
      } else NULL

      div(class = "card mb-3",
        div(class = "card-header", icon("chart-bar"), " 核心结果"),
        div(class = "card-body", stats_html, scr_html))
    })

    # ---- 明细表 ----
    output$tables <- renderUI({
      req(rv$result)
      t <- rv$result$tables

      mk <- function(tbl, label, icon_name) {
        if (is.null(tbl) || !is.data.frame(tbl) || nrow(tbl) == 0) return(NULL)
        tabPanel(label, icon = icon(icon_name),
          div(class = "card", div(class = "card-body p-0",
            DT::datatable(tbl, rownames = FALSE, options = list(
              scrollX = TRUE, pageLength = 15,
              dom = "ltip", language = list(
                lengthMenu = "显示 _MENU_ 条", info = "共 _TOTAL_ 条"))))))
      }

      tabs <- Filter(Negate(is.null), list(
        mk(t$promoted,      "晋级材料", "trophy"),
        mk(t$select_plant,  "分离选单株", "seedling"),
        mk(t$eliminated,    "淘汰材料", "xmark"),
        mk(t$overview,      "数据概览", "table"),
        mk(t$yield_ranking, "产量排名", "list-ol")
      ))
      if (length(tabs) == 0) return(NULL)

      div(class = "mt-3",
        h4(class = "panel-title", "明细"),
        do.call(tabsetPanel, tabs))
    })

    # ---- 下载：统一走共享的报告构建函数，与命令行同源 ----
    out_name <- function(ext) {
      req(rv$result, rv$meta)
      paste0(rv$meta$label, "分析_", rv$meta$id, ".", ext)
    }

    output$dl_excel <- downloadHandler(
      filename = function() out_name("xlsx"),
      content = function(file) {
        req(rv$result)
        build_analysis_excel(rv$result, file)
      })

    output$dl_html <- downloadHandler(
      filename = function() out_name("html"),
      content = function(file) {
        req(rv$result)
        build_html_report(rv$result, file)
      })

    output$dl_md <- downloadHandler(
      filename = function() out_name("md"),
      content = function(file) {
        req(rv$result)
        # 先生成图表，再把图片内嵌进 Markdown，避免单独下载时断链
        tmp <- tempfile("analysis_md_")
        dir.create(tmp, showWarnings = FALSE, recursive = TRUE)
        save_analysis_artifacts(rv$result, tmp)
        md <- file.path(tmp, "分析报告.md")
        inline_md_images(md, file.path(tmp, "图表"))
        file.copy(md, file, overwrite = TRUE)
      })

    output$dl_zip <- downloadHandler(
      filename = function() out_name("zip"),
      content = function(file) {
        req(rv$result)
        build_analysis_zip(rv$result, file)
      })
  })
}
