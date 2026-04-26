# =============================================================================
# 模块: 株行记录本
# 功能: 从种植计划中选择单株，生成株行种植记录本
# 流程: 上传 -> 维护 -> 生成记录本
# =============================================================================

line_selection_ui <- function(id) {
  ns <- NS(id)

  tagList(
    tabsetPanel(
      id = ns("line_tabs"),

      # === 1. 上传株行数据 ===
      tabPanel("上传数据",
        value = "upload",
        icon = icon("upload"),

        div(class = "tab-panel",
          h3(class = "panel-title",
            span(class = "icon", icon("upload")),
            "上传株行材料清单"
          ),
          p(
            span(strong("必填字段："), style = "color: #dc3545;"),
            span(
              code("name"), " - 材料名称； ",
              code("sele"), " - 选择数（需 > 0）； ",
              code("f"), " - 世代",
              style = "font-size: 11px; color: #666; margin-left: 8px;"
            ),
            style = "font-size: 12px; margin-bottom: 5px;"
          ),
     

          fluidRow(
            column(4,
              div(class = "sidebar-panel",
                textInput(ns("exp_name"), "试验名称",
                  placeholder = "如: 2025宿州株行试验",
                  width = "100%"
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
                DT::dataTableOutput(ns("record_list"))
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

      # === 2. 生成株行记录本 ===
      tabPanel("生成记录",
        value = "generate",
        icon = icon("cog"),

        div(class = "tab-panel",
          h3(class = "panel-title",
            span(class = "icon", icon("cog")),
            "生成株行记录本"
          ),
          p("选择试验并配置 planting 参数后生成 Excel 记录本。", class = "text-muted fb-panel-intro"),

          fluidRow(
            column(4,
              div(class = "sidebar-panel",
                # === 试验选择（不折叠）===
                h5(icon("database"), " 选择试验"),
                selectInput(ns("select_exp"), "", choices = NULL, width = "100%"),

                # === 折叠面板：种植参数 ===
                accordion(
                  accordion_panel(
                    "种植参数",
                    textInput(ns("ck"), "对照品种", value = "", width = "100%"),
                    p("注：填写后会在最后添加一行对照，空格分隔多个", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    numericInput(ns("interval"), "对照间隔数", value = NULL, min = 1, width = "100%"),
                    p("每隔N行插入一行对照", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    numericInput(ns("rp"), "重复数", value = 1, min = 1, width = "100%"),
                    p("1重复=顺序；2-3重复=随机", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    numericInput(ns("digits"), "编号位数", value = 4, min = 1, width = "100%"),
                    p("材料编号的数字位数", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    numericInput(ns("rows"), "材料种植行数", value = 1, min = 1, width = "100%"),
                    p("每个材料的种植行数", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    textInput(ns("prefix"), "材料前缀", value = "N25bL", width = "100%"),
                    p("材料编号的前缀", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    textInput(ns("location"), "试验地点", value = "安徽宿州", width = "100%"),
                    p("空格分隔多个地点", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    checkboxInput(ns("ckfixed"), "对照固定", value = TRUE),
                    p("固定则按间隔插入；不固定则随机插入", class = "text-muted", style = "font-size: 12px; margin-top: -3px;"),
                    numericInput(ns("startN"), "起始编号", value = 1, min = 1, width = "100%"),
                    p("fieldid起始编号", class = "text-muted", style = "font-size: 12px; margin-top: -3px;")
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
                  div(class = "stat-value", textOutput(ns("stat_sele_sum"))),
                  div(class = "stat-label", "总选择数")
                ),
                div(class = "stat-item",
                  div(class = "stat-value", textOutput(ns("stat_sele_avg"))),
                  div(class = "stat-label", "平均选择数")
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
                p("查看该试验已生成的田试记录（planting数据+88个性状）", class = "text-muted"),

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

line_selection_server <- function(id) {
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
        # 清理列名（去除首尾空格）
        names(data) <- trimws(names(data))

        # 检查必填字段（get_plant/get_line需要: name,Sele,f）
        required_fields <- c("name", "sele", "f")
        missing_fields <- setdiff(required_fields, tolower(names(data)))

        # 同时检查大小写不敏感的情况
        if (length(missing_fields) > 0) {
          # 检查是否有大小写不同的同名列
          col_names_lower <- tolower(names(data))
          for (field in required_fields) {
            idx <- which(col_names_lower == field)
            if (length(idx) > 0 && names(data)[idx[1]] != field) {
              names(data)[idx[1]] <- field
              missing_fields <- setdiff(required_fields, tolower(names(data)))
            }
          }
        }

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
          # 处理sele列
          data <- processSeleColumn(data)
          rv$raw_data <- data
          shinyjs::html(ns("status"), paste("已加载", nrow(rv$raw_data), "行数据"))
        }
      }, error = function(e) {
        showNotification(paste("读取失败:", e$message), type = "error")
      })
    })

    # 处理sele列的辅助函数
    processSeleColumn <- function(data) {
      target_idx <- which(tolower(names(data)) == "sele")
      if (length(target_idx) > 0) {
        target_col <- names(data)[target_idx[1]]
        if (target_col != "sele") {
          names(data)[target_idx[1]] <- "sele"
          showNotification(paste("已将列'", target_col, "'重命名为'sele'"), type = "message")
        }
      } else {
        select_idx <- which(tolower(names(data)) %in% c("选择数", "选择"))
        if (length(select_idx) > 0) {
          target_col <- names(data)[select_idx[1]]
          data[["sele"]] <- data[[target_col]]
          data[[target_col]] <- NULL
          showNotification(paste("已将列'", target_col, "'重命名为'sele'"), type = "message")
        } else {
          data$sele <- 0
          showNotification("未找到sele(选择数)列，已创建值为0的列", type = "warning")
        }
      }
      data
    }

    # 字段映射对话框内容
    output$field_mapping_ui <- renderUI({
      req(rv$missing_fields)

      field_labels <- list(
        name = "材料名称 *",
        Sele = "选择数 *",
        f = "世代 *"
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

      for (field in rv$missing_fields) {
        input_id <- paste0("map_", field)
        selected <- input[[input_id]]

        if (!is.null(selected) && selected != "不映射（留空）") {
          names(data)[names(data) == selected] <- field
        } else {
          showNotification(paste("字段", field, "必须映射到Excel中的列"), type = "error")
          mapping_success <- FALSE
          break
        }
      }

      if (mapping_success) {
        # 处理sele列
        data <- processSeleColumn(data)
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
      if ("sele" %in% names(data)) {
        cat("总选择数:", sum(data$sele, na.rm = TRUE), "\n")
        cat("平均选择数:", round(mean(data$sele, na.rm = TRUE), 2))
      }
    })

    observeEvent(input$btn_save, {
      req(rv$raw_data)

      exp_name <- input$exp_name
      if (!nzchar(exp_name)) {
        exp_name <- paste0("株行试验_", format(Sys.time(), "%Y%m%d%H%M%S"))
      }

      tryCatch({
        result <- saveLineSelectionRecord(
          experiment_name = exp_name,
          materials_df = rv$raw_data,
          db_path = db_path
        )

        shinyjs::html(ns("status"), paste("已保存:", result$experiment_id))
        showNotification(paste("保存成功! 共", result$record_count, "条记录"), type = "message")

        rv$records <- listLineSelectionRecords(db_path = db_path)
        # 构建分组choices
        updateSelectInput(
          session,
          "select_exp",
          choices = buildGeneratedChoices(rv$records),
          selected = rv$selected_exp
        )

      }, error = function(e) {
        showNotification(paste("保存失败:", e$message), type = "error")
      })
    })

    # ========== 维护记录选项卡 ==========

    observe({
      rv$records <- listLineSelectionRecords(db_path = db_path)
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
        rv$materials <- getLineSelectionMaterials(exp_id, db_path = db_path)
      }
    })

    output$detail_table <- DT::renderDataTable({
      req(rv$materials)
      rv$materials
    }, options = list(pageLength = 10, scrollX = TRUE), class = "compact")

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
        deleteLineSelectionRecord(rv$pending_delete_exp, db_path = db_path)
        rv$records <- listLineSelectionRecords(db_path = db_path)
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
      records <- listLineSelectionRecords(db_path = db_path)
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
        rv$view_data <- getLineSelectionFieldRecord(input$view_exp, db_path = db_path)
        rv$view_exp_name <- input$view_exp
      }, error = function(e) {
        showNotification(paste("读取失败:", e$message), type = "error")
        rv$view_data <- NULL
      })
    })

    observeEvent(input$btn_view_refresh, {
      if (!is.null(input$view_exp) && input$view_exp != "") {
        tryCatch({
          rv$view_data <- getLineSelectionFieldRecord(input$view_exp, db_path = db_path)
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
          default_name = "line_selection_field"
        )
        paste0("株行田试记录_", exp_name, ".xlsx")
      },
      content = function(file) {
        req(rv$view_data)
        openxlsx::write.xlsx(rv$view_data, file, overwrite = TRUE)
      }
    )

    # 下载全部已生成记录
    output$btn_view_download_all <- downloadHandler(
      filename = function() {
        experiment_id <- NULL
        if (!is.null(input$view_exp) && nzchar(trimws(as.character(input$view_exp)))) {
          experiment_id <- input$view_exp
        }

        exp_name <- getExperimentFilenameLabel(
          records = rv$records,
          experiment_id = experiment_id,
          default_name = "all"
        )
        paste0("株行田试记录_全部_", exp_name, ".xlsx")
      },
      content = function(file) {
        all_data <- getAllLineSelectionFieldRecords(db_path = db_path)
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
        deleteLineSelectionFieldRecord(exp_id, db_path = db_path)
        resetLineSelectionGenerated(exp_id, db_path = db_path)
        rv$view_data <- NULL
        rv$view_exp_name <- NULL

        # 刷新田试记录下拉列表
        records <- listLineSelectionRecords(db_path = db_path)
        generated <- records[records$has_generated == 1, ]
        if (nrow(generated) > 0) {
          choices <- setNames(generated$experiment_id, generated$experiment_name)
          updateSelectInput(session, "view_exp", choices = choices, selected = character(0))
        } else {
          updateSelectInput(session, "view_exp", choices = NULL, selected = character(0))
        }

        # 刷新生成记录本页面的下拉列表
        rv$records <- records
        updateSelectInput(
          session,
          "select_exp",
          choices = buildGeneratedChoices(records),
          selected = rv$selected_exp
        )

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
      rv$records <- listLineSelectionRecords(db_path = db_path)
      # 构建分组choices
      updateSelectInput(
        session,
        "select_exp",
        choices = buildGeneratedChoices(rv$records),
        selected = rv$selected_exp
      )
    })

    observeEvent(input$select_exp, {
      req(input$select_exp)
      rv$selected_exp <- input$select_exp
      rv$materials <- getLineSelectionMaterials(input$select_exp, db_path = db_path)

      # 重置田间参数到默认值
      updateTextInput(session, "ck", value = "")
      updateNumericInput(session, "interval", value = NULL)
      updateNumericInput(session, "rp", value = 1)
      updateNumericInput(session, "digits", value = 4)
      updateNumericInput(session, "rows", value = 1)
      updateTextInput(session, "prefix", value = "N25bL")
      updateTextInput(session, "location", value = "安徽宿州")
      updateCheckboxInput(session, "ckfixed", value = TRUE)
      updateNumericInput(session, "startN", value = 1)
    })

    output$material_preview <- DT::renderDataTable({
      req(rv$materials)
      rv$materials
    }, options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip'))

    output$stat_count <- renderText({
      req(rv$materials)
      nrow(rv$materials)
    })

    output$stat_sele_sum <- renderText({
      req(rv$materials)
      if ("sele" %in% names(rv$materials)) {
        sum(rv$materials$sele, na.rm = TRUE)
      } else 0
    })

    output$stat_sele_avg <- renderText({
      req(rv$materials)
      if ("sele" %in% names(rv$materials)) {
        round(mean(rv$materials$sele, na.rm = TRUE), 2)
      } else 0
    })

    output$gen_stats <- renderPrint({
      req(rv$materials)
      data <- rv$materials
      cat("材料数量:", nrow(data), "\n")
      if ("sele" %in% names(data)) {
        cat("总选择数:", sum(data$sele, na.rm = TRUE), "\n")
        cat("平均选择数:", round(mean(data$sele, na.rm = TRUE), 2))
      }
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
        deleteLineSelectionFieldRecord(rv$selected_exp, db_path = db_path)
      }, error = function(e) {
        message("删除旧记录失败或记录不存在: ", e$message)
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
        origin <- as.data.frame(rv$materials, stringsAsFactors = FALSE)

        # 标准化列名（确保大写Sele转为小写sele）
        names(origin) <- tolower(names(origin))
        names(origin) <- gsub("^sele$", "sele", names(origin))

        # 移除数据库特有的列
        db_cols <- DB_MATERIAL_COLS
        origin <- origin[, !names(origin) %in% db_cols, drop = FALSE]

        # 确保sele列是数值型
        if ("sele" %in% names(origin)) {
          origin$sele <- as.numeric(origin$sele)
        }

        # 筛选 sele > 0
        if ("sele" %in% names(origin)) {
          origin <- origin[origin$sele > 0, ]
        }

        # 检查筛选后是否有数据
        if (nrow(origin) == 0) {
          stop("筛选后没有数据，请检查sele列的值是否大于0")
        }

        origin$ma[is.na(origin$ma)] <- "未知"
        origin$pa[is.na(origin$pa)] <- "未知"

        # 确保ma和pa是字符型
        origin$ma <- as.character(origin$ma)
        origin$pa <- as.character(origin$pa)

        # 确保f列是数值型
        if ("f" %in% names(origin)) {
          origin$f <- as.integer(origin$f)
        } else {
          origin$f <- 1
        }

        # 添加必要的列（get_plant需要）
        if (!"path" %in% names(origin)) {
          origin$path <- origin$name
        }
        if (!"process" %in% names(origin)) {
          origin$process <- origin$name
        }

        mydata <- origin |> soyplant::get_plant() |> soyplant::get_line()

        if (nrow(mydata) == 0) {
          stop("get_line返回0行数据，请检查数据格式是否正确")
        }

        if (!is.data.frame(mydata)) {
          stop(paste("get_line返回类型错误:", class(mydata)[1]))
        }

        # 处理对照品种和间隔
        ck_value <- if (nzchar(input$ck)) {
          unlist(strsplit(trimws(input$ck), " +"))
        } else {
          NULL
        }
        # 如果没有填对照品种，间隔数设为9999（不起作用）
        interval_val <- if (is.null(ck_value) || !nzchar(input$interval)) 999 else input$interval

        # 多地点处理
        location_vec <- if (input$location == "") character(0) else strsplit(trimws(input$location), " +")[[1]]

        if (length(location_vec) == 0) {
          stop("请输入试验地点")
        }

        # 循环处理每个地点
        all_planted <- list()
        for (i in seq_along(location_vec)) {
          planted_loc <- mydata %>% planting(
            interval = interval_val, s_prefix = input$prefix,
            place = location_vec[i], rp = input$rp,
            digits = input$digits, ck = ck_value, rows = input$rows,
            ckfixed = input$ckfixed, restartfid = TRUE, startN = input$startN
          )

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

          all_planted[[i]] <- planted_loc
          # 间隔1.2秒确保fieldid不同
          if (i < length(location_vec)) Sys.sleep(FIELDID_DELAY_SECONDS)
        }
        planted <- dplyr::bind_rows(all_planted)

        if (!is.data.frame(planted)) {
          stop(paste("planting返回类型错误:", class(planted)[1]))
        }
        if (nrow(planted) == 0) {
          stop("planting返回0行数据")
        }

        rv$planted_data <- planted

        # 确保planted中sele列是小写（处理上游产生的Sele或大小写变体）
        if (any(tolower(names(planted)) == "sele")) {
          idx <- which(tolower(names(planted)) == "sele")[1]
          if (names(planted)[idx] != "sele") {
            names(planted)[idx] <- "sele"
          }
        }

        myview_cols <- intersect(c(fields, "ma", "pa", "sele", "former_fieldid", "former_stageid", "source"), names(planted))
        rv$output_data <- list(
          origin = origin,
          material = mydata,
          planting = planted,
          myview = planted[, myview_cols, drop = FALSE],
          combi_matrix = combination_matrix(mydata)
        )

        markLineSelectionGenerated(rv$selected_exp, db_path = db_path)

        # 添加88个性状列
        planted <- addTraitColumns(planted)

        # 保存到田试记录表（planting + 性状）
        saveLineSelectionFieldRecord(
          experiment_id = rv$selected_exp,
          experiment_name = exp_name_val,
          planting_df = planted,
          db_path = db_path
        )

        rv$records <- listLineSelectionRecords(db_path = db_path)

        # 自动刷新田试记录
        records <- listLineSelectionRecords(db_path = db_path)
        generated <- records[records$has_generated == 1, ]
        if (nrow(generated) > 0) {
          choices <- setNames(generated$experiment_id, generated$experiment_name)
          updateSelectInput(session, "view_exp", choices = choices, selected = rv$selected_exp)
          rv$view_data <- getLineSelectionFieldRecord(rv$selected_exp, db_path = db_path)
          rv$view_exp_name <- rv$selected_exp
        }

        shinyjs::html(ns("gen_result"), paste(
          "生成成功!<br>",
          "原始:", nrow(origin), "行<br>",
          "材料:", nrow(mydata), "行<br>",
          "种植:", nrow(planted), "行<br>",
          "正在准备下载记录本"
        ))
        showNotification("株行记录本生成成功!", type = "message")
        session$sendCustomMessage("experiments_module_refresh", list(id = "exp_mod-experiments_module_refresh"))
        session$sendCustomMessage("auto_download_when_ready", list(
          id = ns("btn_download"),
          failInputId = ns("download_ready_timeout"),
          maxAttempts = 40,
          intervalMs = 250
        ))

      }, error = function(e) {
        message("ERROR in line_selection generation: ", e$message)

        # 解析错误信息，转换为用户可理解的中文
        err_msg <- e$message
        user_msg <- switch(err_msg,
          if (grepl("get_plant|get_line", err_msg, ignore.case = TRUE)) {
            "数据处理失败，请检查数据中是否包含必要的列（ma, pa, name等）"
          } else if (grepl("缺少必要列", err_msg)) {
            paste0("缺少必要列：", gsub(", ", "、", err_msg))
          } else {
            paste0("生成失败：", err_msg)
          }
        )

        tryCatch({
          shinyjs::html(ns("gen_result"), paste0(
            '<span style="color: red;">生成失败</span><br>',
            '<pre style="text-align: left; font-size: 12px;">',
            gsub("\n", "<br>", user_msg),
            '</pre>'
          ))
        }, error = function(e2) {
          message("shinyjs::html also failed: ", e2$message)
        })
        showNotification(user_msg, type = "error", duration = 10)
      })
    }

    output$btn_download <- downloadHandler(
      filename = function() {
        records <- listLineSelectionRecords(db_path = db_path)
        experiment_id <- NULL
        if (!is.null(rv$selected_exp) && nzchar(trimws(as.character(rv$selected_exp)))) {
          experiment_id <- rv$selected_exp
        } else if (!is.null(input$select_exp) && nzchar(trimws(as.character(input$select_exp)))) {
          experiment_id <- input$select_exp
        }

        exp_name <- getExperimentFilenameLabel(
          records = records,
          experiment_id = experiment_id,
          default_name = "line_selection"
        )
        paste0("株行记录本_", exp_name, ".xlsx")
      },
      content = function(file) {
        req(rv$output_data)
        soyplant::savewb(
          origin = rv$output_data$origin,
          material = rv$output_data$material,
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
