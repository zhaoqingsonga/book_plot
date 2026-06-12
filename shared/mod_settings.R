# =============================================================================
# 模块: 设置页面
# 功能: E智导入管理（Excel 性状数据覆盖更新）
# =============================================================================

settings_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "tab-panel",
      h3(class = "panel-title",
        span(class = "icon", icon("cog")),
        "E智导入"
      ),
      p("将 Excel 性状数据批量导入到田试记录表，覆盖更新现有数据。", class = "text-muted fb-panel-intro"),

      fluidRow(
        # ========== 左侧: 导入配置 ==========
        column(5,
          div(class = "card",
            div(class = "card-header",
              icon("upload"), " 上传数据文件"
            ),
            div(class = "card-body",
              # 文件选择
              fileInput(ns("file_excel"), "选择 Excel 文件",
                accept = c(".xlsx", ".xls"),
                buttonLabel = "浏览...",
                placeholder = "未选择文件"
              ),

              # Sheet 选择
              selectInput(ns("sheet_name"), "选择 Sheet",
                choices = c("template"),
                selected = "template",
                width = "100%"
              ),

              # 试验类型
              selectInput(ns("experiment_type"), "试验类型",
                choices = c(
                  "自动检测" = "auto",
                  "群体" = "population",
                  "株行" = "line_selection",
                  "产比" = "yield_test"
                ),
                selected = "auto",
                width = "100%"
              ),

              hr(),

              # 操作按钮
              div(class = "d-flex gap-2",
                actionButton(ns("btn_preview"), "预览数据",
                  icon = icon("eye"),
                  class = "btn-outline-primary btn-sm",
                  disabled = TRUE
                ),
                actionButton(ns("btn_import"), "开始导入",
                  icon = icon("play"),
                  class = "btn-success btn-sm",
                  disabled = TRUE
                ),
                actionButton(ns("btn_clear"), "清除",
                  icon = icon("trash"),
                  class = "btn-outline-secondary btn-sm"
                )
              )
            )
          ),

          # 导入进度
          div(class = "card mt-3",
            div(class = "card-header",
              icon("spinner"), " 导入进度"
            ),
            div(class = "card-body",
              div(id = ns("progress_container"),
                p(class = "text-muted", "选择文件后显示进度")
              ),
              verbatimTextOutput(ns("import_log")) %>% tagAppendAttributes(style = "font-size: 12px; max-height: 200px; overflow-y: auto;")
            )
          )
        ),

        # ========== 右侧: 数据预览 & 映射表 ==========
        column(7,
          div(class = "card",
            div(class = "card-header d-flex justify-content-between align-items-center",
              icon("table"), " 数据预览",
              span(class = "badge bg-info", textOutput(ns("preview_count")))
            ),
            div(class = "card-body",
              DT::dataTableOutput(ns("preview_table"))
            )
          ),

          div(class = "card mt-3",
            div(class = "card-header",
              icon("exchange-alt"), " T-code 映射表"
            ),
            div(class = "card-body",
              p(class = "text-muted small", "Excel T-code 列名 → 数据库字段名"),
              DT::dataTableOutput(ns("mapping_table"))
            )
          )
        )
      )
    )
  )
}

settings_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ---------- 反应式数据 ----------
    rv <- reactiveValues(
      file_path = NULL,
      sheet_names = character(),
      preview_data = NULL,
      import_result = NULL,
      is_importing = FALSE
    )

    # ---------- 文件上传处理 ----------
    observeEvent(input$file_excel, {
      req(input$file_excel)

      file <- input$file_excel$datapath
      rv$file_path <- file

      # 获取 Sheet 列表
      tryCatch({
        sheets <- openxlsx::getSheetNames(file)
        rv$sheet_names <- sheets

        # 更新 Sheet 选择
        updateSelectInput(session, "sheet_name",
          choices = sheets,
          selected = if ("template" %in% sheets) "template" else sheets[1]
        )

        # 启用预览按钮
        shinyjs::enable("btn_preview")

      }, error = function(e) {
        showNotification(paste("读取 Excel 失败:", e$message), type = "error")
        rv$file_path <- NULL
      })
    })

    # ---------- Sheet 选择变化 ----------
    observeEvent(input$sheet_name, {
      # 当 Sheet 变化时，清空预览
      rv$preview_data <- NULL
      shinyjs::disable("btn_import")
    })

    # ---------- 预览数据 ----------
    observeEvent(input$btn_preview, {
      req(rv$file_path)

      tryCatch({
        # 读取并预览数据
        preview_df <- previewTraitsExcel(rv$file_path, input$sheet_name, n = 20)
        rv$preview_data <- preview_df

        # 启用导入按钮
        shinyjs::enable("btn_import")

        # 更新预览计数
        output$preview_count <- renderText({
          nrow(preview_df)
        })

      }, error = function(e) {
        showNotification(paste("预览失败:", e$message), type = "error")
        rv$preview_data <- NULL
      })
    })

    # ---------- 预览表格 ----------
    output$preview_table <- DT::renderDataTable({
      req(rv$preview_data)

      df <- rv$preview_data

      # 格式化显示
      DT::datatable(df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = "rtip",
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          )
        ),
        class = "compact stripe"
      )
    })

    # ---------- T-code 映射表 ----------
    output$mapping_table <- DT::renderDataTable({
      mapping_df <- getTCodeMappingTable()

      DT::datatable(mapping_df,
        options = list(
          pageLength = 15,
          scrollX = TRUE,
          dom = "rtip"
        ),
        class = "compact stripe"
      )
    })

    # ---------- 开始导入 ----------
    observeEvent(input$btn_import, {
      req(rv$file_path)
      req(!rv$is_importing)

      rv$is_importing <- TRUE
      rv$import_result <- NULL

      # 显示进度
      output$import_log <- renderText("")

      tryCatch({
        withProgress(message = "导入中...", value = 0, {
          result <- importTraitsFromExcel(
            file = rv$file_path,
            experiment_type = input$experiment_type,
            sheet = input$sheet_name,
            verbose = TRUE
          )

          rv$import_result <- result

          # 更新日志
          log_text <- paste0(
            "=== 导入完成 ===\n",
            "时间: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n",
            "总记录数: ", result$total, "\n",
            "更新: ", result$updated, "\n",
            "新增: ", result$inserted, "\n",
            "跳过: ", result$skipped, "\n",
            if (length(result$errors) > 0) {
              paste0("错误: ", length(result$errors), "\n")
            } else {
              ""
            }
          )

          output$import_log <- renderText(log_text)

          # 显示成功通知
          showNotification(
            paste0("导入完成！更新 ", result$updated, " 行，新增 ", result$inserted, " 行"),
            type = "message",
            duration = 5
          )
        })

      }, error = function(e) {
        output$import_log <- renderText(paste0("错误: ", e$message))
        showNotification(paste("导入失败:", e$message), type = "error")
      })

      rv$is_importing <- FALSE
    })

    # ---------- 清除 ----------
    observeEvent(input$btn_clear, {
      rv$file_path <- NULL
      rv$preview_data <- NULL
      rv$import_result <- NULL

      # 重置文件输入
      shinyjs::reset("file_excel")
      updateSelectInput(session, "sheet_name", choices = "template", selected = "template")

      # 禁用按钮
      shinyjs::disable("btn_preview")
      shinyjs::disable("btn_import")

      # 清空预览（preview_data <- NULL 后，renderDataTable 中 req() 自动阻断）
      output$preview_count <- renderText("0")
      output$import_log <- renderText("")
    })

    # ---------- 返回结果 ----------
    return(reactive(rv$import_result))
  })
}