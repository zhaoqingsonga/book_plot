# =============================================================================
# 模块: 群体记录本
# 功能: 处理群体数据(F1-F7)，生成群体种植记录本
# 流程: 上传 -> 维护 -> 生成记录本
# =============================================================================

population_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      id = ns("pop_tabs"),

      # === 1. 上传群体数据 ===
      tabPanel("上传数据",
        value = "upload",
        icon = icon("upload"),

        div(class = "tab-panel",
          h3(class = "panel-title",
            span(class = "icon", icon("upload")),
            "上传群体材料清单"
          ),
          p(
            strong("必填字段："),
            span(style = "font-size: 11px; color: #666; margin-left: 10px;",
                 code("name"), " - 材料名称，",
                 code("next_stage"), " - 下一阶段，",
                 code("f"), " - 世代（如 F1, F2, F3...），",
                 code("new_rows"), " - 种植行数"
            ),
            style = "font-size: 12px; color: #dc3545; margin-bottom: 5px;"
          ),
     

          fluidRow(
            column(4,
              div(class = "sidebar-panel",
                textInput(ns("exp_name"), "试验名称", value = "",
                  placeholder = "如: 2025宿州群体试验", width = "100%"
                ),
                fileInput(ns("file"), "选择Excel文件",
                  accept = c(".xlsx", ".xls"),
                  buttonLabel = icon("folder-open"),
                  placeholder = "未选择文件",
                  width = "100%"
                ),
                selectInput(ns("sheet"), "选择工作表", choices = NULL, width = "100%"),
                div(class = "button-group",
                  actionButton(ns("btn_preview"), "预览", icon = icon("eye"), class = "btn-info"),
                  actionButton(ns("btn_save"), "保存", icon = icon("save"), class = "btn-primary")
                ),

                div(class = "status-box", id = ns("status"),
                  icon("info-circle"), " 请上传或选择文件..."
                )
              )
            ),

            column(8,
              div(class = "card",
                div(class = "card-header",
                  icon("table"), " 数据预览"
                ),
                DT::dataTableOutput(ns("preview_table"))
              ),

              div(class = "card",
                div(class = "card-header",
                  icon("chart-bar"), " 数据统计"
                ),
                verbatimTextOutput(ns("stats"))
              ),

              div(class = "card",
                div(class = "record-list",
                  DT::dataTableOutput(ns("record_list"))
                )
              ),

              div(class = "card",
                div(class = "card-header",
                  icon("info-circle"), " 选中记录详情"
                ),
                DT::dataTableOutput(ns("detail_table"))
              )
            )
          )
        )
      ),

      # === 2. 生成群体记录本 ===
      tabPanel("生成记录",
        value = "generate",
        icon = icon("cog"),

        div(class = "tab-panel",
          h3(class = "panel-title",
            span(class = "icon", icon("cog")),
            "生成群体记录本"
          ),
          p("选择试验并配置 planting 参数后生成 Excel 记录本。", class = "text-muted fb-panel-intro"),

          fluidRow(
            column(4,
              div(class = "sidebar-panel",
                # === 试验选择（不折叠）===
                h5(icon("database"), " 选择试验"),
                selectInput(ns("select_exp"), "", choices = NULL, width = "100%"),
                p("注：只处理new_rows>0的记录", style = "color: red; font-size: 12px;"),

                # === 折叠面板：种植参数 ===
                accordion(
                  accordion_panel(
                    "种植参数",
                    textInput(ns("ck"), "对照品种", value = "", width = "100%"),
                    p("注：填写后会在最后添加一行对照，空格分隔多个", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    numericInput(ns("interval"), "对照间隔数", value = NULL, min = 1, width = "100%"),
                    p("每隔N行插入一行对照", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    numericInput(ns("ck_rows"), "对照行数", value = 4, min = 1, width = "100%"),
                    p("每个对照品种的种植行数", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    numericInput(ns("rp"), "重复数", value = 1, min = 1, width = "100%"),
                    p("1重复=顺序；2-3重复=随机", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    numericInput(ns("digits"), "编号位数", value = 3, min = 1, width = "100%"),
                    p("材料编号的数字位数", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    textInput(ns("prefix"), "材料前缀", value = "N25F2P", width = "100%"),
                    p("材料编号的前缀", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    textInput(ns("location"), "试验地点", value = "安徽宿州", width = "100%"),
                    p("空格分隔多个地点", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    checkboxInput(ns("ckfixed"), "对照固定", value = TRUE),
                    p("固定则按间隔插入；不固定则随机插入", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    numericInput(ns("startN"), "起始编号", value = 1, min = 1, width = "100%"),
                    p("fieldid起始编号", class = "text-muted", style = "font-size: 12px; margin-top: -3px;")
                  ),
                  # === 折叠面板：世代筛选 ===
                  accordion_panel(
                    "世代筛选",
                    numericInput(ns("min_f"), "最小世代", value = 0, min = 0, max = 7, width = "100%"),
                    numericInput(ns("max_f"), "最大世代", value = 6, min = 1, max = 7, width = "100%")
                  ),
                  open = FALSE  # 默认折叠
                ),

                div(class = "status-box", id = ns("gen_status"),
                  icon("arrow-left"), " 请选择试验记录..."
                )
              )
            ),

            column(8,
              div(class = "card",
                div(class = "card-header",
                  icon("eye"), " 材料预览"
                ),
                DT::dataTableOutput(ns("material_preview"))
              ),

              div(class = "stats-grid",
                div(class = "stat-item",
                  div(class = "stat-value", textOutput(ns("stat_count"))),
                  div(class = "stat-label", "材料数量")
                ),
                div(class = "stat-item",
                  div(class = "stat-value", textOutput(ns("stat_f_range"))),
                  div(class = "stat-label", "世代范围")
                ),
                div(class = "stat-item",
                  div(class = "stat-value", textOutput(ns("stat_rows_sum"))),
                  div(class = "stat-label", "总行数")
                )
              ),

              div(class = "card",
                div(class = "card-header",
                  icon("play-circle"), " 生成操作"
                ),
                div(class = "button-group",
                  actionButton(ns("btn_generate"), "生成记录本", icon = icon("cog"), class = "btn-primary"),
                  tags$div(
                    style = "display: none;",
                    downloadButton(ns("btn_download"), "下载记录本", icon = icon("download"), class = "btn-success")
                  )
                ),
                div(class = "result-box", id = ns("gen_result"),
                  "生成结果将显示在这里..."
                )
              ),

                              div(class = "card",
                div(class = "card-header",
                  icon("clipboard-list"), " 田试记录（已生成）"
                ),


                selectInput(ns("view_exp"), "选择试验", choices = NULL, width = "100%"),
                div(class = "button-group",
                  actionButton(ns("btn_view_refresh"), "刷新", icon = icon("refresh"), class = "btn-info btn-sm"),
                  downloadButton(ns("btn_view_download"), "下载", class = "btn-success btn-sm"),
                  actionButton(ns("btn_view_delete"), "删除", icon = icon("trash"), class = "btn-danger btn-sm"),
                  downloadButton(ns("btn_view_download_all"), "下载全部", class = "btn-warning btn-sm")
                ),
                DT::dataTableOutput(ns("view_table"))
              )
            )
          )
        )
      ),

    )
  )
}

population_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv <- reactiveValues(
      raw_data = NULL,
      records = NULL,
      selected_exp = NULL,
      pending_delete_exp = NULL,
      materials = NULL,
      planted_data = NULL,
      output_data = NULL,
      view_data = NULL,
      view_exp_name = NULL,
      view_exp_sel = NULL
    )

    fields <- FIELD_VIEW_COLS
    db_path <- defaultDbPath()

    # ========== 上传数据选项卡 ==========

    observeEvent(input$file, {
      tryCatch({
        sheets <- getSheetNames(input$file$datapath)
        # 默认选择 "planting" 工作表（如果存在）
        selected <- if ("planting" %in% sheets) "planting" else sheets[1]
        updateSelectInput(session, "sheet", choices = sheets, selected = selected)

        # 自动提取文件名作为试验名称（去掉扩展名）
        file_name <- input$file$name
        exp_name <- gsub("\\.(xlsx|xls)$", "", file_name, ignore.case = TRUE)
        updateTextInput(session, "exp_name", value = exp_name)
      }, error = function(e) {
        showNotification(paste("读取失败:", e$message), type = "error")
      })
    })

    observeEvent(input$btn_preview, {
      if (is.null(input$file)) {
        showNotification("请上传文件", type = "warning")
        return()
      }

      tryCatch({
        data <- read.xlsx(input$file$datapath, sheet = input$sheet, colNames = TRUE)
        rv$raw_data <- data

        # 检查必填字段（get_population需要: name, next_stage, f, new_rows）
        required_fields <- c("name", "next_stage", "f", "new_rows")
        missing_fields <- setdiff(required_fields, names(data))

        if (length(missing_fields) > 0) {
          # 弹出字段映射对话框
          showModal(modalDialog(
            title = "字段映射",
            p(strong("以下必填字段缺失，请选择Excel中对应的列进行映射：")),
            uiOutput(ns("field_mapping_ui")),
            easyClose = FALSE,
            footer = tagList(
              actionButton(ns("btn_confirm_mapping"), "确认映射", class = "btn-primary"),
              actionButton(ns("btn_cancel_mapping"), "取消", class = "btn-default")
            )
          ))

          # 保存当前数据和缺失字段信息
          rv$pending_data <- data
          rv$missing_fields <- missing_fields
          rv$all_columns <- c("不映射（留空）", names(data))
        } else {
          shinyjs::html(ns("status"), paste("已加载", nrow(rv$raw_data), "行数据"))
        }
      }, error = function(e) {
        showNotification(paste("读取失败:", e$message), type = "error")
      })
    })

    # 字段映射对话框内容
    output$field_mapping_ui <- renderUI({
      req(rv$missing_fields)

      field_labels <- list(
        name = "材料名称 *",
        next_stage = "下一阶段 *",
        f = "世代 *",
        new_rows = "种植行数 *"
      )

      mapping_list <- lapply(rv$missing_fields, function(field) {
        label <- field_labels[[field]]
        if (is.null(label)) label <- paste0(field, " *")

        fluidRow(
          column(4, p(strong(label))),
          column(8,
            selectInput(ns(paste0("map_", field)),
              label = NULL,
              choices = rv$all_columns,
              selected = rv$all_columns[1],
              width = "100%"
            )
          )
        )
      })

      tagList(mapping_list)
    })

    # 确认映射
    observeEvent(input$btn_confirm_mapping, {
      req(rv$pending_data, rv$missing_fields)

      data <- rv$pending_data
      mapping_success <- TRUE

      # 处理每个缺失字段
      for (field in rv$missing_fields) {
        input_id <- paste0("map_", field)
        selected <- input[[input_id]]

        if (!is.null(selected) && selected != "不映射（留空）") {
          # 重命名列
          names(data)[names(data) == selected] <- field
        } else {
          # 如果必填字段没有映射，报错
          showNotification(paste("字段", field, "必须映射到Excel中的列"), type = "error")
          mapping_success <- FALSE
          break
        }
      }

      if (mapping_success) {
        rv$raw_data <- data
        rv$pending_data <- NULL
        rv$missing_fields <- NULL
        removeModal()
        shinyjs::html(ns("status"), paste("已加载", nrow(rv$raw_data), "行数据"))
        showNotification("字段映射成功", type = "message")
      }
    })

    # 取消映射
    observeEvent(input$btn_cancel_mapping, {
      rv$pending_data <- NULL
      rv$missing_fields <- NULL
      rv$raw_data <- NULL
      removeModal()
      showNotification("已取消上传", type = "warning")
    })

    output$preview_table <- DT::renderDataTable({
      req(rv$raw_data)
      rv$raw_data
    }, options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip'))

    output$stats <- renderPrint({
      req(rv$raw_data)
      data <- rv$raw_data
      cat("数据行数:", nrow(data), "\n")
      if ("f" %in% names(data)) {
        cat("\n世代分布:\n")
        print(table(data$f, useNA = "ifany"))
      }
      if ("new_rows" %in% names(data)) {
        cat("\n总行数:", sum(data$new_rows, na.rm = TRUE))
      } else if ("实际种行数" %in% names(data)) {
        cat("\n总行数:", sum(data$实际种行数, na.rm = TRUE))
      }
    })

    observeEvent(input$btn_save, {
      req(rv$raw_data)

      exp_name <- input$exp_name
      if (!nzchar(exp_name)) {
        exp_name <- paste0("群体试验_", format(Sys.time(), "%Y%m%d%H%M%S"))
      }

      tryCatch({
        result <- savePopulationRecord(
          experiment_name = exp_name,
          materials_df = rv$raw_data,
          db_path = db_path
        )

        shinyjs::html(ns("status"), paste("已保存:", result$experiment_id))
        showNotification(paste("保存成功! 共", result$record_count, "条记录"), type = "message")

        # 刷新记录列表
        rv$records <- listPopulationRecords(db_path = db_path)
        # 构建分组choices
        generated <- rv$records[rv$records$has_generated == 1, ]
        not_generated <- rv$records[rv$records$has_generated == 0, ]
        choices <- c(
          "已生成" = if (nrow(generated) > 0) setNames(generated$experiment_id, generated$experiment_name) else character(0),
          "未生成" = if (nrow(not_generated) > 0) setNames(not_generated$experiment_id, not_generated$experiment_name) else character(0)
        )
        updateSelectInput(session, "select_exp", choices = choices)

      }, error = function(e) {
        showNotification(paste("保存失败:", e$message), type = "error")
      })
    })

    # ========== 维护记录选项卡 ==========

    observe({
      rv$records <- listPopulationRecords(db_path = db_path)
    })

    output$record_list <- DT::renderDataTable({
      req(rv$records)

      df <- rv$records
      df$has_generated <- ifelse(df$has_generated == 1, "已生成", "未生成")
      df$created_at <- substr(df$created_at, 1, 19)
      df$操作 <- fb_record_list_delete_buttons(df$experiment_id, ns)

      cols <- c("experiment_id", "experiment_name", "total_rows", "has_generated", "created_at", "操作")
      DT::datatable(df[, cols],
        selection = "single",
        escape = setdiff(cols, "操作"),
        options = list(pageLength = 10, dom = 'frtip'),
        class = "compact stripe hover"
      )
    })

    observeEvent(input$record_list_rows_selected, {
      selected_row <- input$record_list_rows_selected
      if (length(selected_row) > 0) {
        exp_id <- rv$records$experiment_id[selected_row]
        rv$selected_exp <- exp_id
        rv$materials <- getPopulationMaterials(exp_id, db_path = db_path)
      }
    })

    output$detail_table <- DT::renderDataTable({
      req(rv$materials)
      rv$materials
    }, options = list(pageLength = 10, scrollX = TRUE))

    observeEvent(input$delete_record_row, {
      req(input$delete_record_row$experiment_id)
      rv$pending_delete_exp <- as.character(input$delete_record_row$experiment_id)
      showModal(modalDialog(
        title = "确认删除",
        paste0("确定要删除该试验记录吗？此操作不可恢复。"),
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("btn_confirm_delete_yes"), "确定删除", class = "btn-danger"),
          actionButton(ns("btn_confirm_delete_no"), "取消", class = "btn-default")
        )
      ))
    })

    observeEvent(input$btn_confirm_delete_yes, {
      removeModal()
      tryCatch({
        deletePopulationRecord(rv$pending_delete_exp, db_path = db_path)
        rv$records <- listPopulationRecords(db_path = db_path)
        if (identical(rv$selected_exp, rv$pending_delete_exp)) {
          rv$selected_exp <- NULL
          rv$materials <- NULL
        }
        rv$pending_delete_exp <- NULL
        showNotification("删除成功", type = "message")
      }, error = function(e) {
        rv$pending_delete_exp <- NULL
        showNotification(paste("删除失败:", e$message), type = "error")
      })
    })

    observeEvent(input$btn_confirm_delete_no, {
      rv$pending_delete_exp <- NULL
      removeModal()
    })

    # ========== 田试记录查看（维护记录标签页内）==========
    observe({
      records <- listPopulationRecords(db_path = db_path)
      generated <- records[records$has_generated == 1, ]
      if (nrow(generated) > 0) {
        choices <- setNames(generated$experiment_id, generated$experiment_name)
        updateSelectInput(session, "view_exp", choices = choices)
      } else {
        updateSelectInput(session, "view_exp", choices = NULL)
      }
    })

    observeEvent(input$view_exp, {
      req(input$view_exp)
      tryCatch({
        rv$view_data <- getPopulationFieldRecord(input$view_exp, db_path = db_path)
        rv$view_exp_name <- input$view_exp
      }, error = function(e) {
        showNotification(paste("读取失败:", e$message), type = "error")
        rv$view_data <- NULL
      })
    })

    observeEvent(input$btn_view_refresh, {
      if (!is.null(input$view_exp) && input$view_exp != "") {
        tryCatch({
          rv$view_data <- getPopulationFieldRecord(input$view_exp, db_path = db_path)
          showNotification("已刷新", type = "message")
        }, error = function(e) {
          showNotification(paste("刷新失败:", e$message), type = "error")
        })
      }
    })

    output$view_table <- renderFieldRecordTable(reactive(rv$view_data))

    output$btn_view_download <- downloadHandler(
      filename = function() {
        experiment_id <- NULL
        if (!is.null(input$view_exp) && nzchar(trimws(as.character(input$view_exp)))) {
          experiment_id <- input$view_exp
        } else if (!is.null(rv$view_exp_name) && nzchar(trimws(as.character(rv$view_exp_name)))) {
          experiment_id <- rv$view_exp_name
        }

        exp_name <- getExperimentFilenameLabel(
          records = rv$records,
          experiment_id = experiment_id,
          default_name = "population_field"
        )
        paste0("群体田试记录_", exp_name, ".xlsx")
      },
      content = function(file) {
        req(rv$view_data)
        openxlsx::write.xlsx(rv$view_data, file, overwrite = TRUE)
      }
    )

    # 下载全部已生成记录
    output$btn_view_download_all <- downloadHandler(
      filename = function() {
        exp_name <- getExperimentFilenameLabel(
          records = rv$records,
          experiment_id = input$view_exp,
          default_name = "all"
        )
        paste0("群体田试记录_全部_", exp_name, ".xlsx")
      },
      content = function(file) {
        all_data <- getAllPopulationFieldRecords(db_path = db_path)
        if (length(all_data) == 0) {
          showNotification("没有已生成的记录", type = "warning")
          return(NULL)
        }
        combined_data <- dplyr::bind_rows(all_data)
        openxlsx::write.xlsx(combined_data, file, overwrite = TRUE)
      }
    )

    # 删除田试记录
    observeEvent(input$btn_view_delete, {
      req(input$view_exp)
      exp_id <- input$view_exp
      showModal(modalDialog(
        title = "确认删除",
        paste0("确定要删除该田试记录吗？此操作不可恢复。"),
        easyClose = FALSE,
        footer = tagList(
          actionButton(ns("btn_confirm_view_delete_yes"), "确定删除", class = "btn-danger"),
          actionButton(ns("btn_confirm_view_delete_no"), "取消", class = "btn-default")
        )
      ))
    })

    observeEvent(input$btn_confirm_view_delete_yes, {
      removeModal()
      tryCatch({
        exp_id <- input$view_exp
        deletePopulationFieldRecord(exp_id, db_path = db_path)
        resetPopulationGenerated(exp_id, db_path = db_path)
        rv$view_data <- NULL
        rv$view_exp_name <- NULL

        # 刷新田试记录下拉列表
        records <- listPopulationRecords(db_path = db_path)
        generated <- records[records$has_generated == 1, ]
        if (nrow(generated) > 0) {
          choices <- setNames(generated$experiment_id, generated$experiment_name)
          updateSelectInput(session, "view_exp", choices = choices, selected = character(0))
        } else {
          updateSelectInput(session, "view_exp", choices = NULL, selected = character(0))
        }

        # 刷新生成记录本页面的下拉列表
        rv$records <- records
        generated2 <- records[records$has_generated == 1, ]
        not_generated2 <- records[records$has_generated == 0, ]
        choices2 <- c(
          "已生成" = if (nrow(generated2) > 0) setNames(generated2$experiment_id, generated2$experiment_name) else character(0),
          "未生成" = if (nrow(not_generated2) > 0) setNames(not_generated2$experiment_id, not_generated2$experiment_name) else character(0)
        )
        updateSelectInput(session, "select_exp", choices = choices2)

        showNotification("删除成功", type = "message")
      }, error = function(e) {
        showNotification(paste("删除失败:", e$message), type = "error")
      })
    })

    observeEvent(input$btn_confirm_view_delete_no, {
      removeModal()
    })

    # ========== 生成记录本选项卡 ==========

    observe({
      rv$records <- listPopulationRecords(db_path = db_path)
      # 构建分组choices
      updateSelectInput(session, "select_exp", choices = buildGeneratedChoices(rv$records))
    })

    observeEvent(input$select_exp, {
      req(input$select_exp)
      rv$selected_exp <- input$select_exp
      rv$materials <- getPopulationMaterials(input$select_exp, db_path = db_path)

      # 重置田间参数到默认值
      updateTextInput(session, "ck", value = "")
      updateNumericInput(session, "interval", value = NULL)
      updateNumericInput(session, "ck_rows", value = 4)
      updateNumericInput(session, "rp", value = 1)
      updateNumericInput(session, "digits", value = 3)
      updateTextInput(session, "prefix", value = "N25F2P")
      updateTextInput(session, "location", value = "安徽宿州")
      updateCheckboxInput(session, "ckfixed", value = TRUE)
      updateNumericInput(session, "startN", value = 1)
      updateNumericInput(session, "min_f", value = 0)
      updateNumericInput(session, "max_f", value = 6)
    })

    output$material_preview <- DT::renderDataTable({
      req(rv$materials)
      rv$materials
    }, options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip'))

    output$stat_count <- renderText({
      req(rv$materials)
      nrow(rv$materials)
    })

    output$stat_f_range <- renderText({
      req(rv$materials)
      data <- rv$materials
      if ("f" %in% names(data)) {
        paste(range(data$f, na.rm = TRUE), collapse = " - ")
      } else ""
    })

    output$stat_rows_sum <- renderText({
      req(rv$materials)
      data <- rv$materials
      if ("rows" %in% names(data)) sum(data$rows, na.rm = TRUE) else ""
    })

    observeEvent(input$btn_generate, {
      req(rv$selected_exp, rv$materials)

      # 检查是否已生成
      exp_record <- rv$records[rv$records$experiment_id == rv$selected_exp, ]
      is_regenerated <- nrow(exp_record) > 0 && exp_record$has_generated == 1

      if (is_regenerated) {
        # 弹出确认对话框
        showModal(modalDialog(
          title = "确认覆盖",
          paste0("该记录已生成过记录本，重新生成会覆盖原有数据。\n\n是否继续？"),
          easyClose = FALSE,
          footer = tagList(
            actionButton(ns("btn_confirm_generate_yes"), "确定覆盖", class = "btn-primary"),
            actionButton(ns("btn_confirm_generate_no"), "取消", class = "btn-default")
          )
        ))
        return()
      }

      # 执行生成逻辑
      doGenerate()
    })

    # 确认覆盖时的处理
    observeEvent(input$btn_confirm_generate_yes, {
      removeModal()

      # 先删除旧记录
      tryCatch({
        deletePopulationFieldRecord(rv$selected_exp, db_path = db_path)
      }, error = function(e) {
        # 如果删除失败（记录不存在），继续生成
        print("删除旧记录失败或记录不存在:", e$message)
      })

      # 执行生成
      doGenerate()
    })

    observeEvent(input$btn_confirm_generate_no, {
      removeModal()
      showNotification("已取消", type = "warning")
    })

    # 生成逻辑主函数
    doGenerate <- function() {
      # 获取当前选中的试验记录
      exp_record <- rv$records[rv$records$experiment_id == rv$selected_exp, ]
      exp_name_val <- if (nrow(exp_record) > 0) exp_record$experiment_name else rv$selected_exp

      tryCatch({

        # 从rv$materials获取数据
        mydata <- rv$materials

        # 移除数据库特有的列
        db_cols <- DB_MATERIAL_COLS
        mydata <- mydata[, !names(mydata) %in% db_cols, drop = FALSE]

        # 确保f列是数值型
        if ("f" %in% names(mydata)) {
          mydata$f <- as.integer(mydata$f)
        }

        # 添加next_stage列（get_population需要）
        # 如果stageid包含"F"则next_stage设为"群体"
        if (!"next_stage" %in% names(mydata)) {
          mydata$next_stage <- ifelse(grepl("F", as.character(mydata$stageid)), "群体", mydata$stageid)
        }

        # 添加必要的列（get_population需要）
        if (!"mapa" %in% names(mydata)) {
          mydata$mapa <- paste(mydata$ma, mydata$pa, sep = " x ")
        }
        # 强制重置 process 和 path，避免遗留值干扰 planting()
        mydata$process <- mydata$name
        mydata$path <- mydata$name

        # 清理可能干扰 planting() 的遗留列
        # 这些列应该是 planting() 的输出，不应该存在于输入数据中
        cols_to_remove <- c("rows", "line_number", "is_ck", "fieldid", ".actual_rows")
        existing_cols_to_remove <- cols_to_remove[cols_to_remove %in% names(mydata)]
        if (length(existing_cols_to_remove) > 0) {
          mydata <- mydata[, !names(mydata) %in% existing_cols_to_remove, drop = FALSE]
        }

        # 按世代筛选
        if (!is.null(input$min_f) && "f" %in% names(mydata)) {
          mydata <- mydata[mydata$f >= input$min_f & mydata$f <= input$max_f, ]
        }

        # 确保ma和pa是字符型（不替换NA为"未知"，保持原始值以便combination_matrix正确处理）
        mydata$ma <- as.character(mydata$ma)
        mydata$pa <- as.character(mydata$pa)

        # 获取群体的实际种植行数（new_rows列）
        # 用向量保存，直接按位置匹配（get_population保持顺序）
        rows_col <- "new_rows"
        .new_rows_vec <- rep(1, nrow(mydata))  # 默认1
        if (rows_col %in% names(mydata)) {
          .new_rows_vec <- as.numeric(mydata[[rows_col]])
          .new_rows_vec[is.na(.new_rows_vec) | .new_rows_vec <= 0] <- 1
        }

        # 双重淘汰：只选 new_rows > 0 的记录
        if (rows_col %in% names(mydata)) {
          new_rows_vals <- as.numeric(mydata[[rows_col]])
          n_before <- nrow(mydata)
          valid_mask <- !is.na(new_rows_vals) & new_rows_vals > 0
          mydata <- mydata[valid_mask, ]
          .new_rows_vec <- .new_rows_vec[valid_mask]
          n_after <- nrow(mydata)
          if (n_before > n_after) {
            shinyjs::html(ns("gen_status"), paste("已过滤", n_before - n_after, "条new_rows<=0的记录，保留", n_after, "条"))
          }
        }

        # 若筛选后无可用材料，提前终止并给出可读提示
        if (nrow(mydata) == 0) {
          stop("筛选后无可生成材料：请检查世代范围(min_f/max_f)与new_rows是否大于0")
        }

        # 预先建立原始材料名 -> new_rows 映射
        # get_population() 后 source 通常对应筛选前的 name，用于恢复实际种植行数
        pre_name_to_rows <- setNames(.new_rows_vec, as.character(mydata$name))

        # 添加seeds列（如果不存在）
        if (!"seeds" %in% names(mydata)) {
          mydata$seeds <- 0
        }

        # 调用get_population进行数据升级升级
        shinyjs::html(ns("gen_status"), "正在调用get_population...")

        # 验证必要列是否存在
        required_cols <- c("ma", "pa", "stageid", "name")
        missing_cols <- setdiff(required_cols, names(mydata))
        if (length(missing_cols) > 0) {
          stop(paste("缺少必要列:", paste(missing_cols, collapse = ", ")))
        }

        # 验证ma和pa不是空的
        if (all(is.na(mydata$ma)) || all(nchar(mydata$ma) == 0)) {
          stop("母本(ma)列全部为空，请检查数据")
        }
        if (all(is.na(mydata$pa)) || all(nchar(mydata$pa) == 0)) {
          stop("父本(pa)列全部为空，请检查数据")
        }

        print("=== get_population input ===")
        print(str(mydata))
        print(head(mydata))
        print(names(mydata))
        mydata <- soyplant::get_population(mydata)

        # 确保f列是整数（get_population可能改变其类型）
        if ("f" %in% names(mydata)) {
          mydata$f <- as.integer(mydata$f)
        }

        # 检查get_population返回值
        if (is.character(mydata) && grepl("No selected population", mydata)) {
          stop("get_population未找到有效的群体数据，请检查f世代列和stageid列是否存在且有效")
        }
        if (!is.data.frame(mydata) || nrow(mydata) == 0) {
          stop(paste("get_population返回无效数据:", paste(capture.output(str(mydata)), collapse = "\n")))
        }

        # 调试：检查get_population返回的数据
        print("=== get_population result ===")
        print(str(mydata))
        print(head(mydata))

        # 按 source 恢复实际 new_rows（不依赖 get_population() 前后行数一致）
        source_key <- as.character(mydata$source)
        mydata$.actual_rows <- as.numeric(pre_name_to_rows[source_key])
        # 若 source 匹配不到，兜底用 name 再尝试一次
        missing_rows_idx <- is.na(mydata$.actual_rows)
        if (any(missing_rows_idx)) {
          mydata$.actual_rows[missing_rows_idx] <- as.numeric(pre_name_to_rows[as.character(mydata$name[missing_rows_idx])])
        }
        mydata$.actual_rows[is.na(mydata$.actual_rows) | mydata$.actual_rows <= 0] <- 1

        # 创建材料行数映射（优先 source，其次 name，再其次 code）
        # 某些数据在 planting() 后 source 可能为空或类型不稳定，需多键兜底
        source_to_rows <- setNames(mydata$.actual_rows, as.character(mydata$source))
        name_to_rows <- setNames(mydata$.actual_rows, as.character(mydata$name))
        code_to_rows <- if ("code" %in% names(mydata)) {
          setNames(mydata$.actual_rows, as.character(mydata$code))
        } else {
          numeric(0)
        }

        shinyjs::html(ns("gen_status"), paste("get_population完成，返回", nrow(mydata), "行"))

        # 处理对照品种和间隔
        ck_value <- parseCkInput(input$ck)
        # 如果没有填对照品种，间隔数设为999（不起作用）
        interval_val <- if (is.null(ck_value) || !nzchar(input$interval)) INTERVAL_DISABLED else input$interval

        # 多地点处理
        location_vec <- if (input$location == "") character(0) else strsplit(trimws(input$location), " +")[[1]]

        if (length(location_vec) == 0) {
          stop("请输入试验地点")
        }

        # 循环处理每个地点
        all_planted <- list()
        for (i in seq_along(location_vec)) {
          planted_loc <- mydata %>% planting(
            interval = interval_val,
            s_prefix = input$prefix,
            place = location_vec[i], rp = input$rp,
            digits = input$digits, ck = ck_value,
            ckfixed = input$ckfixed, restartfid = TRUE, startN = input$startN
          )

          # 调试：检查 planting 返回的 is_ck 和 source
          print(paste0("=== location ", i, " planting is_ck ==="))
          print(table(planted_loc$is_ck, useNA = "ifany"))
          print(paste0("source NA count: ", sum(is.na(planted_loc$source))))

          # 从mydata合并额外字段到planted
          # 保存原始 is_ck，merge 会覆盖它
          original_is_ck <- planted_loc$is_ck
          extra_cols <- setdiff(names(mydata), names(planted_loc))
          if (length(extra_cols) > 0) {
            if ("code" %in% names(planted_loc) && "code" %in% names(mydata)) {
              merge_keys <- c("code", "ma", "pa")
              merge_keys <- merge_keys[merge_keys %in% names(planted_loc) & merge_keys %in% names(mydata)]
              if (length(merge_keys) > 0) {
                planted_loc <- merge(planted_loc, mydata[, c(merge_keys, extra_cols), drop = FALSE],
                                 by = merge_keys, all.x = TRUE, sort = FALSE)
              }
            }
          }
          # 恢复原始 is_ck
          planted_loc$is_ck <- original_is_ck

          # 重新计算 rows 和 line_number：一次遍历同时处理
          # 对照行判断：is_ck == 1 且 source 是 NA
          # 材料行判断：否则（is_ck == 0 或 is_ck == 1 但 source 有值）
          ck_rows_val <- if (is.null(input$ck_rows) || is.na(input$ck_rows)) 1 else input$ck_rows
          is_ck_vec <- if ("is_ck" %in% names(planted_loc)) planted_loc$is_ck else integer(nrow(planted_loc))
          if (!is.numeric(is_ck_vec)) is_ck_vec <- as.integer(is_ck_vec)
          is_ck_vec[is.na(is_ck_vec)] <- 0L
          source_vec <- if ("source" %in% names(planted_loc)) as.character(planted_loc$source) else rep(NA_character_, nrow(planted_loc))
          name_vec <- if ("name" %in% names(planted_loc)) as.character(planted_loc$name) else rep(NA_character_, nrow(planted_loc))
          code_vec <- if ("code" %in% names(planted_loc)) as.character(planted_loc$code) else rep(NA_character_, nrow(planted_loc))

          pos <- 1
          for (idx in seq_len(nrow(planted_loc))) {
            is_ck_i <- is_ck_vec[idx]
            src <- source_vec[idx]
            # 对照行：is_ck == 1 且 source 是 NA
            # 材料行：否则（包括 is_ck == 0 或 is_ck == 1 但 source 有值）
            if (is_ck_i == 1L && is.na(src)) {
              # 对照行：用 ck_rows_val
              actual_rows_i <- ck_rows_val
            } else {
              # 材料行：按 source -> name -> code 的优先级查找 new_rows
              actual_rows_i <- NA_real_
              src_chr <- if (!is.na(src) && nzchar(src)) as.character(src) else NA_character_
              if (!is.na(src_chr) && src_chr %in% names(source_to_rows)) {
                actual_rows_i <- as.numeric(source_to_rows[[src_chr]])
              }
              if (is.na(actual_rows_i)) {
                nm_chr <- name_vec[idx]
                if (!is.na(nm_chr) && nzchar(nm_chr) && nm_chr %in% names(name_to_rows)) {
                  actual_rows_i <- as.numeric(name_to_rows[[nm_chr]])
                }
              }
              if (is.na(actual_rows_i)) {
                cd_chr <- code_vec[idx]
                if (!is.na(cd_chr) && nzchar(cd_chr) && cd_chr %in% names(code_to_rows)) {
                  actual_rows_i <- as.numeric(code_to_rows[[cd_chr]])
                }
              }
              if (is.na(actual_rows_i)) actual_rows_i <- 1
            }
            planted_loc$rows[idx] <- actual_rows_i
            planted_loc$line_number[idx] <- paste0(pos, "-", pos + actual_rows_i - 1)
            pos <- pos + actual_rows_i
          }
          all_planted[[i]] <- planted_loc
          # 间隔1.2秒确保fieldid不同
          if (i < length(location_vec)) Sys.sleep(FIELDID_DELAY_SECONDS)
        }
        planted <- dplyr::bind_rows(all_planted)

        # 调试：检查planting返回的数据
        print("=== planting result ===")
        print(str(planted))

        # 确保 is_ck 是整数（避免保存到数据库时出现小数点）
        if ("is_ck" %in% names(planted)) {
          planted$is_ck <- as.integer(planted$is_ck)
        }

        rv$planted_data <- planted
        rv$output_data <- list(
          origin = mydata,
          planting = planted,
          myview = planted[c(fields, "ma", "pa")],
          combi_matrix = combination_matrix(mydata)
        )

        # 标记为已生成
        markPopulationGenerated(rv$selected_exp, db_path = db_path)

        # 添加88个性状列
        planted <- addTraitColumns(planted)

        # 保存到田试记录表（planting + 性状）
        savePopulationFieldRecord(
          experiment_id = rv$selected_exp,
          experiment_name = exp_name_val,
          planting_df = planted,
          db_path = db_path
        )

        rv$records <- listPopulationRecords(db_path = db_path)

        # 自动刷新田试记录
        records <- listPopulationRecords(db_path = db_path)
        generated <- records[records$has_generated == 1, ]
        if (nrow(generated) > 0) {
          choices <- setNames(generated$experiment_id, generated$experiment_name)
          updateSelectInput(session, "view_exp", choices = choices, selected = rv$selected_exp)
          rv$view_data <- getPopulationFieldRecord(rv$selected_exp, db_path = db_path)
          rv$view_exp_name <- rv$selected_exp
        }

        shinyjs::html(ns("gen_result"), paste(
          "生成成功!<br>",
          "原始:", nrow(mydata), "行<br>",
          "种植:", nrow(planted), "行<br>",
          "正在准备下载记录本"
        ))
        showNotification("群体记录本生成成功!", type = "message")
        session$sendCustomMessage("experiments_module_refresh", list())
        session$sendCustomMessage("auto_download_when_ready", list(
          id = ns("btn_download"),
          failInputId = ns("download_ready_timeout"),
          maxAttempts = 40,
          intervalMs = 250
        ))

      }, error = function(e) {
        print("========================================")
        print("ERROR in population generation:")
        print(e)
        print("========================================")

        # 解析错误信息，转换为用户可理解的中文
        err_msg <- e$message
        user_msg <- switch(err_msg,
          # get_population 错误
          if (grepl("No selected population", err_msg, ignore.case = TRUE)) {
            "未找到有效的群体数据，请检查：\n1. 数据中是否包含f世代列\n2. stageid列是否包含F1/F2等世代标识\n3. 母本(ma)和父本(pa)列是否有数据"
          } else if (grepl("缺少必要列", err_msg)) {
            gsub("缺少必要列:", "缺少必要列：\n", paste0("缺少必要列：", gsub(", ", "\n", err_msg)))
          } else if (grepl("母本.*为空", err_msg)) {
            "母本(ma)列数据为空，请检查Excel文件中的母本列"
          } else if (grepl("父本.*为空", err_msg)) {
            "父本(pa)列数据为空，请检查Excel文件中的父本列"
          } else if (grepl("get_population返回无效数据", err_msg)) {
            "数据处理失败，请检查数据格式是否正确"
          } else if (grepl("筛选后无可生成材料", err_msg)) {
            paste0(
              "筛选后没有可生成的材料，请检查：\n",
              "1. 世代范围设置（最小世代/最大世代）是否覆盖该试验数据\n",
              "2. 该试验的new_rows是否都为0或空值"
            )
          } else {
            # 通用错误
            paste0("生成失败：", err_msg)
          }
        )

        # 尝试在UI中显示错误信息
        tryCatch({
          shinyjs::html(ns("gen_result"), paste0(
            '<span style="color: red;">生成失败</span><br>',
            '<pre style="text-align: left; font-size: 12px;">',
            gsub("\n", "<br>", user_msg),
            '</pre>'
          ))
        }, error = function(e2) {
          print("shinyjs::html also failed:", e2$message)
        })
        showNotification(user_msg, type = "error", duration = 10)
      })
    }

    output$btn_download <- downloadHandler(
      filename = function() {
        exp_name <- getExperimentFilenameLabel(
          records = rv$records,
          experiment_id = rv$selected_exp,
          default_name = "population"
        )
        paste0("群体记录本_", exp_name, ".xlsx")
      },
      content = function(file) {
        req(rv$output_data)
        soyplant::savewb(
          origin = rv$output_data$origin,
          planting = rv$output_data$planting,
          myview = rv$output_data$myview,
          combi_matrix = rv$output_data$combi_matrix,
          filename = file,
          overwrite = TRUE
        )
      }
    )
    outputOptions(output, "btn_download", suspendWhenHidden = FALSE)

    observeEvent(input$download_ready_timeout, {
      showNotification("记录本已生成，但自动下载未触发，请刷新页面后重试。", type = "warning", duration = 8)
    })
  })
}
