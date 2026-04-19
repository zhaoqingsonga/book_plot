buildDesignplotServer <- function(input, output) {

  # ===========================================================================
  # 共享辅助函数
  # ===========================================================================

  # 计算田间布局图的度量指标
  computeLayoutPlotMetrics <- function(plant_matrix) {
    data_cols <- ncol(plant_matrix) - STAT_COL_COUNT
    rowno_idx <- match("排数", colnames(plant_matrix))
    interval_idx <- match("间隔", colnames(plant_matrix))
    rowno_vec <- if (!is.na(rowno_idx)) suppressWarnings(as.numeric(plant_matrix[, rowno_idx])) else seq_len(nrow(plant_matrix))
    unique_rows <- sort(unique(rowno_vec[!is.na(rowno_vec)]))
    total_rows <- if (length(unique_rows) > 0) length(unique_rows) else nrow(plant_matrix)
    min_row_id <- if (length(unique_rows) > 0) min(unique_rows) else 1
    max_row_id <- if (length(unique_rows) > 0) max(unique_rows) else total_rows

    row_meter <- 0.5
    rank_meter <- 1
    if (!is.na(interval_idx)) {
      interval_vec <- suppressWarnings(as.numeric(plant_matrix[, interval_idx]))
      valid_interval <- interval_vec[!is.na(interval_vec) & interval_vec > 0]
      if (length(valid_interval) > 0) rank_meter <- mean(valid_interval)
    }
    aspect_ratio <- rank_meter / row_meter

    list(
      data_cols = data_cols,
      total_rows = total_rows,
      min_row_id = min_row_id,
      max_row_id = max_row_id,
      total_cols = data_cols,
      aspect_ratio = aspect_ratio,
      rowno_vec = rowno_vec
    )
  }

  # 构建田间布局 ggplot 对象（output 与 download 共用）
  buildFieldLayoutGgplot <- function(layout_df, plant_matrix, simplified_name, metrics) {
    normalizeSupplementLabel <- function(x) {
      x <- trimws(as.character(x))
      x <- sub("^补种\\s*[:：-]\\s*", "", x)
      ifelse(nzchar(x), paste0(x, "（补种）"), "补种")
    }

    fill_values <- stats::setNames(as.character(layout_df$color), as.character(layout_df$name))
    fill_values <- fill_values[!duplicated(names(fill_values))]
    fill_values <- fill_values[!is.na(fill_values) & fill_values != "NA"]

    exp_df <- layout_df[layout_df$type == "实验", , drop = FALSE]
    special_df <- layout_df[layout_df$type == "特殊区域", , drop = FALSE]
    supplement_df <- layout_df[layout_df$type == "补种", , drop = FALSE]
    if (nrow(supplement_df) > 0) {
      supplement_df$supplement_label <- normalizeSupplementLabel(supplement_df$name)
      supplement_stats <- stats::aggregate(
        x = list(area_cells = supplement_df$rows * supplement_df$cols),
        by = list(supplement_label = supplement_df$supplement_label),
        FUN = sum
      )
      supplement_stats$supplement_label <- as.character(supplement_stats$supplement_label)
      supplement_stats$display_label <- paste0(supplement_stats$supplement_label, "（", supplement_stats$area_cells, "格）")
      label_map <- stats::setNames(supplement_stats$display_label, supplement_stats$supplement_label)
      supplement_df$supplement_display <- unname(label_map[supplement_df$supplement_label])
    }

    p <- ggplot() +
      geom_rect(data = exp_df,
                mapping = aes(xmin = start_col - 0.5, xmax = end_col + 0.5,
                              ymin = start_row - 0.5, ymax = end_row + 0.5,
                              fill = name),
                color = "white", linewidth = 0.5, alpha = 0.9) +
      geom_rect(data = special_df,
                mapping = aes(xmin = start_col - 0.5, xmax = end_col + 0.5,
                              ymin = start_row - 0.5, ymax = end_row + 0.5,
                              fill = name),
                color = "white", linewidth = 0.5, alpha = 0.35) +
      scale_fill_manual(values = fill_values, breaks = names(fill_values), name = "区域")

    if (nrow(supplement_df) > 0) {
      supplement_breaks <- unique(as.character(supplement_df$supplement_display))
      supplement_colors <- stats::setNames(
        grDevices::hcl.colors(length(supplement_breaks), palette = "Dark 3"),
        supplement_breaks
      )
      p <- p +
        geom_rect(data = supplement_df,
                  mapping = aes(xmin = start_col - 0.5, xmax = end_col + 0.5,
                                ymin = start_row - 0.5, ymax = end_row + 0.5,
                                color = supplement_display),
                  fill = NA, linetype = "22", linewidth = 1.0) +
        scale_color_manual(values = supplement_colors, breaks = supplement_breaks, name = "补种材料")
    }

    p +
      scale_x_continuous(name = "行", breaks = seq(0, metrics$total_cols, by = max(1, floor(metrics$total_cols / 10))),
                         limits = c(0, metrics$total_cols + 1)) +
      scale_y_continuous(name = "排", breaks = seq(0, metrics$max_row_id + 2, by = 2),
                         limits = c(0, metrics$max_row_id + 0.5), expand = c(0, 0)) +
      coord_fixed(ratio = metrics$aspect_ratio) +
      (if (nrow(supplement_df) > 0) {
        guides(fill = guide_legend(ncol = 2, byrow = TRUE), color = guide_legend(order = 2))
      } else {
        guides(fill = guide_legend(ncol = 2, byrow = TRUE))
      }) +
      theme_minimal() +
      theme(
        panel.grid.major = element_line(color = "gray80", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13, face = "bold"),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold")
      ) +
      ggtitle(paste0("田间布局总览 - ", simplified_name, " (", metrics$total_rows, "排×", metrics$total_cols, "行)"))
  }

  # ===========================================================================
  # 状态变量
  # ===========================================================================
  latest_plan_id <- reactiveVal(NULL)
  sqlite_db_path <- defaultSqlitePath()

  # ---- 细粒度刷新触发器 ----
  experimentsTrigger <- reactiveVal(0L)
  recordsTrigger <- reactiveVal(0L)
  sowTrigger <- reactiveVal(0L)
  plantTableTrigger <- reactiveVal(0L)
  fieldModelTrigger <- reactiveVal(0L)

  pendingFieldModelSelection <- reactiveVal(NULL)
  pendingSqliteExperimentSelection <- reactiveVal(NULL)
  fieldModelInitialized <- reactiveVal(FALSE)
  planningInputsHydrated <- reactiveVal(FALSE)
  autoPersistLastPlanSig <- reactiveVal(NA_character_)
  autoPersistLastAssignSig <- reactiveVal(NA_character_)
  lastValidDataset <- reactiveVal(NULL)
  fieldLayoutCache <- reactiveVal(list(key = NA_character_, data = NULL))
  pendingDeleteExpRowId <- reactiveVal(NULL)
  isPlantingInProgress <- reactiveVal(FALSE)
  # 执行种植撤销：内存中保留最近 2 次操作前的快照（刷新页面后清空）
  plantingUndoStack <- reactiveVal(list())
  # 各 Tab 共用的当前种植表（*.plant 表名）；四个下拉框由此同步
  activePlantTableName <- reactiveVal(NA_character_)
  unifiedPlantSelectBusy <- reactiveVal(FALSE)

  # ===========================================================================
  # 元数据与默认值
  # ===========================================================================
  buildPersistenceMeta <- reactive({
    list(
      source_param_file = NA_character_,
      field_length = NA_real_,
      field_layout = input$get_water_columns,
      bridge_layout = input$bridges,
      row_gap = input$w,
      group_rows = input$subg,
      plant_start_pos = input$experimentPlantStartPos,
      plant_end_pos = input$experimentPlantEndPos,
      design_from_left = as.logical(input$design_from_left),
      plant_from_left = as.logical(input$plant_from_left)
    )
  })

  currentExperimentName <- reactive({
    defaultExperimentName(param_file = NULL, provided_name = NULL)
  })

  # ===========================================================================
  # 地块模型：确保默认 + 返回数据（避免重复查询）
  # ===========================================================================
  ensureDefaultFieldModel <- function() {
    models <- listFieldModels(sqlite_db_path)
    if (!is.data.frame(models) || nrow(models) == 0 || !("常规地块1" %in% as.character(models$field_name))) {
      saveFieldModel(
        "常规地块1",
        list(
          field_len = 200, no_plant = "", field_layout = "w/8/w",
          strip_width = "10,6/3,10", protect_strip = "",
          cross_path_width = 1, row_gap = 0.5, group_rows = 1,
          plant_start_pos = "1,1", plant_end_pos = "",
          plan_left = TRUE, plant_left = TRUE
        ),
        sqlite_db_path
      )
    }
    # 返回模型数据，避免调用方重复查询
    getFieldModel("常规地块1", sqlite_db_path)
  }

  # ===========================================================================
  # 初始化地块模型
  # ===========================================================================
  observe({
    if (isTRUE(fieldModelInitialized())) return()
    model <- ensureDefaultFieldModel()
    names_vec <- as.character(listFieldModels(sqlite_db_path)$field_name)
    if (length(names_vec) == 0) return()
    selected <- if ("常规地块1" %in% names_vec) "常规地块1" else names_vec[1]
    choices <- setNames(names_vec, names_vec)
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "field_model_select", choices = choices, selected = selected)
    if (!is.null(model)) applyModelToPlanningInputs(model)
    planningInputsHydrated(TRUE)
    fieldModelInitialized(TRUE)
    fieldModelTrigger(fieldModelTrigger() + 1)
  })

  # ===========================================================================
  # 地块模型选择 -> 加载参数
  # ===========================================================================
  observeEvent(input$field_model_select, {
    req(nzchar(trimws(input$field_model_select)))
    tryCatch({
      model <- getFieldModel(input$field_model_select, sqlite_db_path)
      if (!is.null(model)) applyModelToPlanningInputs(model)
    }, error = function(e) {
      showNotification(paste0("地块加载失败: ", e$message), type = "error")
    })
    planningInputsHydrated(TRUE)
  })

  # ===========================================================================
  # 观察者：地块模型列表同步
  # ===========================================================================
  observe({
    plantTableTrigger()
    names_vec <- fieldModelNames()
    if (length(names_vec) == 0) return()
    tables_df <- plantTables()
    created_fields <- if (is.data.frame(tables_df) && nrow(tables_df) > 0) {
      as.character(tables_df$field_name)
    } else {
      character()
    }
    labels <- ifelse(names_vec %in% created_fields,
                     paste0(names_vec, "（已创建）"),
                     names_vec)
    choices <- setNames(names_vec, labels)
    pending <- pendingFieldModelSelection()
    current <- shiny::isolate(input$field_model_select)
    selected <- if (!is.null(pending) && nzchar(trimws(pending)) && pending %in% names_vec) {
      pending
    } else if (!is.null(current) && nzchar(trimws(current)) && current %in% names_vec) {
      current
    } else if ("常规地块1" %in% names_vec) {
      "常规地块1"
    } else {
      names_vec[1]
    }
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "field_model_select", choices = choices, selected = selected)
    if (!is.null(pending) && identical(selected, pending)) pendingFieldModelSelection(NULL)
  })

  # ===========================================================================
  # 统一「当前种植地块」：种植展示 / 种植试验 / 播种列表 / 田间布局图 四下拉同步
  # ===========================================================================
  pushUnifiedPlantSelects <- function(plant_table_name, tables_df, sow_df) {
    session <- getDefaultReactiveDomain()
    unifiedPlantSelectBusy(TRUE)
    on.exit(unifiedPlantSelectBusy(FALSE), add = TRUE)
    if (!is.data.frame(tables_df) || nrow(tables_df) == 0) {
      updateSelectInput(session, "plant_table_select", choices = c("暂无地块" = ""), selected = "")
      updateSelectInput(session, "experimentPlantTableSelect", choices = c("暂无地块" = ""), selected = "")
      updateSelectInput(session, "layout_field_select", choices = c("暂无地块" = ""), selected = "")
      updateSelectInput(session, "sow_table_select", choices = c("暂无播种表" = ""), selected = "")
      return(invisible(NULL))
    }
    valid_plants <- as.character(tables_df$table_name)
    p <- trimws(as.character(plant_table_name))
    if (!nzchar(p) || !(p %in% valid_plants)) p <- valid_plants[1]
    plant_choices <- stats::setNames(valid_plants, valid_plants)
    updateSelectInput(session, "plant_table_select", choices = plant_choices, selected = p)
    updateSelectInput(session, "experimentPlantTableSelect", choices = plant_choices, selected = p)
    field_name <- {
      row_hit <- match(p, tables_df$table_name)
      if (!is.na(row_hit)) as.character(tables_df$field_name[row_hit]) else sub("\\.plant$", "", p)
    }
    layout_choices <- stats::setNames(as.character(tables_df$field_name), as.character(tables_df$field_name))
    updateSelectInput(session, "layout_field_select", choices = layout_choices, selected = field_name)
    if (is.data.frame(sow_df) && nrow(sow_df) > 0) {
      sow_choices <- stats::setNames(as.character(sow_df$table_name), as.character(sow_df$field_name))
      sow_match <- as.character(sow_df$table_name)[as.character(sow_df$field_name) == field_name]
      sow_sel <- if (length(sow_match) >= 1L) sow_match[1] else as.character(sow_df$table_name[1])
      updateSelectInput(session, "sow_table_select", choices = sow_choices, selected = sow_sel)
    } else {
      updateSelectInput(session, "sow_table_select", choices = c("暂无播种表" = ""), selected = "")
    }
    invisible(NULL)
  }

  setActivePlantTable <- function(plant_table_name) {
    tables_df <- shiny::isolate(plantTables())
    sow_df <- shiny::isolate(sowTables())
    if (!is.data.frame(tables_df) || nrow(tables_df) == 0) {
      activePlantTableName(NA_character_)
      pushUnifiedPlantSelects("", tables_df, sow_df)
      return(invisible(NULL))
    }
    valid_plants <- as.character(tables_df$table_name)
    p <- trimws(as.character(plant_table_name))
    if (!nzchar(p) || !(p %in% valid_plants)) p <- valid_plants[1]
    activePlantTableName(p)
    pushUnifiedPlantSelects(p, tables_df, sow_df)
    invisible(NULL)
  }

  observe({
    tables_df <- plantTables()
    sow_df <- sowTables()
    if (!is.data.frame(tables_df) || nrow(tables_df) == 0) {
      activePlantTableName(NA_character_)
      pushUnifiedPlantSelects("", tables_df, sow_df)
      return(invisible(NULL))
    }
    valid_plants <- as.character(tables_df$table_name)
    cur <- shiny::isolate(activePlantTableName())
    cur <- trimws(as.character(cur))
    p <- if (nzchar(cur) && cur %in% valid_plants) cur else valid_plants[1]
    activePlantTableName(p)
    pushUnifiedPlantSelects(p, tables_df, sow_df)
  })

  observeEvent(input$plant_table_select, {
    if (isTRUE(shiny::isolate(unifiedPlantSelectBusy()))) return(invisible(NULL))
    v <- trimws(input$plant_table_select)
    if (!nzchar(v)) return(invisible(NULL))
    setActivePlantTable(v)
  }, ignoreInit = TRUE)

  observeEvent(input$experimentPlantTableSelect, {
    if (isTRUE(shiny::isolate(unifiedPlantSelectBusy()))) return(invisible(NULL))
    v <- trimws(input$experimentPlantTableSelect)
    if (!nzchar(v)) return(invisible(NULL))
    setActivePlantTable(v)
  }, ignoreInit = TRUE)

  observeEvent(input$layout_field_select, {
    if (isTRUE(shiny::isolate(unifiedPlantSelectBusy()))) return(invisible(NULL))
    f <- trimws(input$layout_field_select)
    if (!nzchar(f)) return(invisible(NULL))
    setActivePlantTable(createPlantTableName(f))
  }, ignoreInit = TRUE)

  observeEvent(input$sow_table_select, {
    if (isTRUE(shiny::isolate(unifiedPlantSelectBusy()))) return(invisible(NULL))
    s <- trimws(input$sow_table_select)
    if (!nzchar(s)) return(invisible(NULL))
    f <- sub("\\.sow$", "", s)
    if (!nzchar(f)) return(invisible(NULL))
    setActivePlantTable(createPlantTableName(f))
  }, ignoreInit = TRUE)

  # ===========================================================================
  # 观察者：试验下拉框
  # ===========================================================================
  observe({
    exp_df <- experimentOptions()
    if (!is.data.frame(exp_df) || nrow(exp_df) == 0) {
      pendingSqliteExperimentSelection(NULL)
      updateSelectInput(session = getDefaultReactiveDomain(), inputId = "sqliteFilterExperimentId", choices = c("全部试验" = ""), selected = "")
      return()
    }
    planted_ids <- getPlantedExperimentIds(sqlite_db_path)
    planted_ids <- as.character(planted_ids)
    nm <- trimws(as.character(exp_df$experiment_name))
    eid <- trimws(as.character(exp_df$experiment_id))
    stat_lbl <- ifelse(eid %in% planted_ids, "✓已种", "○未种")
    name_dup <- stats::ave(seq_len(nrow(exp_df)), nm, FUN = length)
    name_dup <- as.integer(name_dup) > 1L
    # 下拉框展示以试验名为准；仅当名称重复时附带 ID 以免混淆
    labels <- paste0(nm, ifelse(name_dup, paste0(" [", eid, "]"), ""), " ", stat_lbl)
    choices <- stats::setNames(eid, labels)
    filter_choices <- c("全部试验" = "", choices)
    choice_ids <- unname(filter_choices)
    pending <- pendingSqliteExperimentSelection()
    pending_id <- if (!is.null(pending)) trimws(as.character(pending)) else ""
    # 勿直接依赖 input$sqliteFilterExperimentId：否则 updateSelectInput 后立刻清空 pending 会再跑一次本 observe，
    # 此时客户端尚未回传新值，input 仍为旧值，会把选中项错误重置为「全部试验」。
    current_filter <- trimws(shiny::isolate(input$sqliteFilterExperimentId))
    selected_filter <- if (nzchar(pending_id) && pending_id %in% choice_ids) {
      pending_id
    } else if (nzchar(current_filter) && current_filter %in% choice_ids) {
      current_filter
    } else {
      ""
    }
    updateSelectInput(session = getDefaultReactiveDomain(), inputId = "sqliteFilterExperimentId", choices = filter_choices, selected = selected_filter)
  })

  # pending 仅在客户端 input 已与目标 ID 一致后再清除，避免与上方 observe 形成「抢跑」重置下拉框
  observe({
    p <- pendingSqliteExperimentSelection()
    if (is.null(p) || !nzchar(trimws(as.character(p)))) {
      return(invisible(NULL))
    }
    target <- trimws(as.character(p))
    cur <- trimws(as.character(input$sqliteFilterExperimentId))
    if (nzchar(cur) && identical(cur, target)) {
      pendingSqliteExperimentSelection(NULL)
    }
  })

  # ===========================================================================
  # 细粒度触发器助手函数
  # ===========================================================================
  refreshExperiments <- function() experimentsTrigger(experimentsTrigger() + 1L)
  refreshRecords <- function() recordsTrigger(recordsTrigger() + 1L)
  refreshSow <- function() {
    sowTrigger(sowTrigger() + 1L)
    recordsTrigger(recordsTrigger() + 1L)  # 播种表依赖记录
  }
  refreshAllSqlite <- function() {
    experimentsTrigger(experimentsTrigger() + 1L)
    recordsTrigger(recordsTrigger() + 1L)
    sowTrigger(sowTrigger() + 1L)
  }

  # ===========================================================================
  # 刷新播种列表
  # ===========================================================================
  observeEvent(input$refreshSowList, {
    plantTableTrigger(plantTableTrigger() + 1)
    refreshSow()
    showNotification("播种列表已刷新", type = "message")
  })

  # ===========================================================================
  # 地块模型操作
  # ===========================================================================
  observeEvent(input$saveFieldModel, {
    tryCatch({
      saveFieldModel(currentFieldModelName(), currentFieldModelData(), sqlite_db_path)
      showNotification("地块模型保存成功", type = "message")
      fieldModelTrigger(fieldModelTrigger() + 1)
      experimentsTrigger(experimentsTrigger() + 1L)
    }, error = function(e) {
      showNotification(paste0("地块模型保存失败: ", e$message), type = "error")
    })
  })

  observeEvent(input$generatePlantField, {
    target_name <- createPlantTableName(currentFieldModelName())
    showModal(modalDialog(
      title = "确认生成种植地块",
      tags$p(paste0("将把当前规划写入【", target_name, "】。"), style = "margin:0 0 6px 0;"),
      tags$p("同时会重置该地块的防重复锁（允许同一试验再次种植）。", style = "margin:0;color:#b91c1c;"),
      footer = tagList(modalButton("取消"), actionButton("confirmGeneratePlantField", "确认生成", class = "btn-warning")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirmGeneratePlantField, {
    tryCatch({
      removeModal()
      save_result <- savePlantTable(field_name = currentFieldModelName(), plan_matrix = datasetInput(), db_path = sqlite_db_path)
      clearExperimentPlantRunByTable(save_result$table_name, sqlite_db_path)
      clearSowTableByPlantTable(save_result$table_name, sqlite_db_path)
      plantTableTrigger(plantTableTrigger() + 1)
      refreshSow()
      setActivePlantTable(save_result$table_name)
      showNotification(paste0("已生成种植地块：", save_result$table_name, "；防重复锁与播种列表已同步重置"), type = "message")
    }, error = function(e) {
      showNotification(paste0("生成种植地块失败: ", e$message), type = "error")
    })
  })

  observeEvent(input$deleteFieldModel, {
    target_name <- currentFieldModelName()
    showModal(modalDialog(
      title = "确认删除",
      tags$p(paste0("确定要删除地块参数「", target_name, "」吗？删除后不可恢复。"), style = "margin:0;"),
      tags$p("若已通过「创建地块」生成种植表，须先点击「删除地块」删除所创建的地块后，才能删除本参数。", style = "margin:10px 0 0 0;color:#b45309;font-size:13px;line-height:1.5;"),
      footer = tagList(modalButton("取消"), actionButton("confirmDeleteFieldModel", "确认删除", class = "btn-danger")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirmDeleteFieldModel, {
    tryCatch({
      removeModal()
      target_name <- currentFieldModelName()
      plant_table_name <- createPlantTableName(target_name)
      tables_df <- listPlantTables(sqlite_db_path)
      if (is.data.frame(tables_df) && nrow(tables_df) > 0 &&
          plant_table_name %in% tables_df$table_name) {
        showNotification(paste0("参数「", target_name, "」已创建地块，请先删除地块后再删除参数"), type = "warning")
        return(invisible(NULL))
      }
      deleteFieldModel(target_name, sqlite_db_path)
      showNotification("参数删除成功", type = "message")
      fieldModelTrigger(fieldModelTrigger() + 1)
      refreshAllSqlite()
    }, error = function(e) {
      showNotification(paste0("参数删除失败: ", e$message), type = "error")
    })
  })

  observeEvent(input$deletePlantField, {
    target_name <- currentFieldModelName()
    plant_table_name <- createPlantTableName(target_name)

    # 检查地块表是否存在
    tables_df <- listPlantTables(sqlite_db_path)
    if (!is.data.frame(tables_df) || nrow(tables_df) == 0 ||
        !(plant_table_name %in% tables_df$table_name)) {
      showNotification("该地块尚未创建，无需删除", type = "warning")
      return()
    }

    showModal(modalDialog(
      title = "确认删除地块",
      tags$div(
        tags$p(paste0("确定要删除地块「", target_name, "」吗？"), style = "margin:0 0 6px 0;"),
        tags$p("此操作将删除：", style = "margin:0 0 4px 0;font-weight:600;"),
        tags$ul(style = "margin:0 0 6px 0;padding-left:20px;",
          tags$li("种植地块表数据"),
          tags$li("相关的试验种植记录"),
          tags$li("相关的播种列表")
        ),
        tags$p("删除后不可恢复！", style = "margin:0;color:#dc2626;font-weight:600;")
      ),
      footer = tagList(modalButton("取消"), actionButton("confirmDeletePlantField", "确认删除", class = "btn-danger")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirmDeletePlantField, {
    tryCatch({
      removeModal()
      deletePlantTable(currentFieldModelName(), sqlite_db_path)
      showNotification("种植地块删除成功", type = "message")
      plantTableTrigger(plantTableTrigger() + 1)
      refreshAllSqlite()
    }, error = function(e) {
      showNotification(paste0("种植地块删除失败: ", e$message), type = "error")
    })
  })

  observeEvent(input$addFieldModel, {
    tryCatch({
      base_name <- currentFieldModelName()
      if (!nzchar(base_name)) base_name <- "新地块"
      suggested_name <- paste0(base_name, "_", format(Sys.time(), "%H%M%S"))
      showModal(modalDialog(
        title = "增加地块模板",
        textInput("new_field_model_name", "地块模板名称", value = suggested_name, width = "100%"),
        tags$p("支持自定义名称；名称不能为空且不能与已有地块重名。", style = "margin:0;color:#6b7280;font-size:12px;"),
        footer = tagList(modalButton("取消"), actionButton("confirmAddFieldModel", "确认增加", class = "btn-success")),
        easyClose = TRUE
      ))
    }, error = function(e) {
      showNotification(paste0("增加地块模板失败: ", e$message), type = "error")
    })
  })

  observeEvent(input$confirmAddFieldModel, {
    tryCatch({
      req(input$new_field_model_name)
      new_name <- trimws(input$new_field_model_name)
      if (!nzchar(new_name)) {
        showNotification("地块名称不能为空", type = "error")
        return(invisible(NULL))
      }
      if (grepl(";", new_name, fixed = TRUE) || grepl("[\r\n\t]", new_name)) {
        showNotification("地块模板名称包含非法字符", type = "error")
        return(invisible(NULL))
      }
      names_vec <- as.character(listFieldModels(sqlite_db_path)$field_name)
      if (new_name %in% names_vec) {
        showNotification("地块名称已存在，请换一个名称", type = "error")
        return(invisible(NULL))
      }
      saveFieldModel(new_name, currentFieldModelData(), sqlite_db_path)
      removeModal()
      pendingFieldModelSelection(new_name)
      fieldModelTrigger(fieldModelTrigger() + 1)
      experimentsTrigger(experimentsTrigger() + 1L)
      showNotification("新地块模板已创建并选中，可继续调整后点击「保存」", type = "message")
    }, error = function(e) {
      showNotification(paste0("增加地块模板失败: ", e$message), type = "error")
    })
  })

  # ===========================================================================
  # 试验导入
  # ===========================================================================
  observeEvent(input$experimentImportFile, {
    req(input$experimentImportFile)
    upload_name <- input$experimentImportFile$name
    default_name <- tools::file_path_sans_ext(basename(upload_name))
    updateTextInput(session = getDefaultReactiveDomain(), inputId = "experimentImportName", value = default_name)
  }, ignoreInit = TRUE)

  observeEvent(input$importExperimentPlanting, {
    req(input$experimentImportFile)
    tryCatch({
      import_result <- importExperimentFromPlantingExcel(
        excel_path = input$experimentImportFile$datapath,
        experiment_name = input$experimentImportName,
        db_path = sqlite_db_path,
        sheet = "planting"
      )
      imported_id <- trimws(import_result$experiment_id)
      if (!nzchar(imported_id)) stop("导入后未获取到试验ID")

      experiments_all <- readTableFromSqlite("experiments", sqlite_db_path)
      records_all <- readTableFromSqlite("experiment_records", sqlite_db_path)
      exp_rows <- if (is.data.frame(experiments_all) && nrow(experiments_all) > 0 && "experiment_id" %in% names(experiments_all)) {
        experiments_all[as.character(experiments_all$experiment_id) == imported_id, , drop = FALSE]
      } else data.frame()
      rec_rows <- if (is.data.frame(records_all) && nrow(records_all) > 0 && "experiment_id" %in% names(records_all)) {
        records_all[as.character(records_all$experiment_id) == imported_id, , drop = FALSE]
      } else data.frame()
      if (nrow(exp_rows) < 1 || nrow(rec_rows) < 1) stop("导入校验失败：未找到对应试验或记录")

      # 先记下待选试验，再刷新；避免 refresh 触发的 observe 在 input 尚未更新时把选中项重置为「全部」
      pendingSqliteExperimentSelection(imported_id)
      refreshAllSqlite()
      showNotification(paste0("试验导入成功：ID=", imported_id, "，记录数=", nrow(rec_rows), "，汇总行数=", as.numeric(exp_rows$total_rows[1])), type = "message")
    }, error = function(e) {
      showNotification(paste0("导入试验失败: ", e$message), type = "error")
    })
  })

  # ===========================================================================
  # 删除试验
  # ===========================================================================
  observeEvent(input$deleteExperimentBtn, {
    exp_id <- trimws(input$sqliteFilterExperimentId)
    if (!nzchar(exp_id)) {
      showNotification("请先选择要删除的 experiment_id", type = "error")
      return(invisible(NULL))
    }
    showModal(modalDialog(
      title = "确认删除试验",
      tags$p(paste0("确定要删除试验ID「", exp_id, "」及其全部记录吗？该操作不可恢复。"), style = "margin:0;"),
      footer = tagList(modalButton("取消"), actionButton("confirmDeleteExperimentBtn", "确认删除", class = "btn-danger")),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirmDeleteExperimentBtn, {
    tryCatch({
      exp_id <- trimws(input$sqliteFilterExperimentId)
      if (!nzchar(exp_id)) stop("experiment_id 不能为空")
      removeModal()
      affected <- deleteExperiment(exp_id, sqlite_db_path)
      if (isTRUE(affected < 1)) stop("未找到对应试验ID")
      refreshAllSqlite()
      showNotification("试验及其记录已删除", type = "message")
    }, error = function(e) {
      showNotification(paste0("删除试验失败: ", e$message), type = "error")
    })
  })

  # ---- 行内删除按钮（DT 每行右侧的删除按钮）----
  observeEvent(input$delete_exp_row, {
    row_data <- input$delete_exp_row
    if (is.null(row_data) || !is.list(row_data)) return(invisible(NULL))
    exp_id <- as.character(row_data$id)
    if (!nzchar(exp_id)) return(invisible(NULL))
    pendingDeleteExpRowId(exp_id)
    showModal(modalDialog(
      title = "确认删除试验",
      tags$p(paste0("确定要删除试验ID「", exp_id, "」及其全部记录吗？该操作不可恢复。"), style = "margin:0;"),
      footer = tagList(
        modalButton("取消"),
        actionButton("confirmDeleteExpRowBtn", "确认删除", class = "btn-danger")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$confirmDeleteExpRowBtn, {
    tryCatch({
      removeModal()
      exp_id_to_del <- pendingDeleteExpRowId()
      if (!nzchar(exp_id_to_del)) stop("experiment_id 不能为空")
      affected <- deleteExperiment(exp_id_to_del, sqlite_db_path)
      if (isTRUE(affected < 1)) stop("未找到对应试验ID")
      pendingDeleteExpRowId(NULL)
      refreshAllSqlite()
      showNotification(paste0("试验「", exp_id_to_del, "」及其记录已删除"), type = "message")
    }, error = function(e) {
      pendingDeleteExpRowId(NULL)
      showNotification(paste0("删除试验失败: ", e$message), type = "error")
    })
  })

  # ===========================================================================
  # 规划计算
  # ===========================================================================
  planningCoordinateRange <- reactive({
    total_cols <- w_c()$total_cols
    total_rows <- nrow(datasetInput())
    tryCatch(
      parsePlantingCoordinateRange(input$experimentPlantStartPos, input$experimentPlantEndPos, total_rows, total_cols),
      error = function(e) validate(need(FALSE, paste0("种植位置设置错误：", e$message)))
    )
  })

  w_c <- reactive({
    tryCatch(
      getWaterLane(input$get_water_columns),
      error = function(e) validate(need(FALSE, paste0("田间布局格式错误：", e$message)))
    )
  })

  datasetInput <- reactive({
    fieldModelTrigger()
    req(isTRUE(planningInputsHydrated()))
    tryCatch({
      validate(
        need(nchar(input$get_water_columns) > 0, "请输入田间布局（如 w/8/w）"),
        need(nchar(input$bridges) > 0, "请输入各条宽度（如 10,6/3,10）"),
        need(!is.na(input$ww) && input$ww >= 0, "横向观察道宽度须为非负数"),
        need(!is.na(input$w) && input$w >= 0, "材料间隔距离须为非负数"),
        need(!is.na(input$subg) && input$subg >= 1, "分组大小须为正整数")
      )
      blocks <- length(modibridges(input$bridges))
      y <- w_c()$total_cols
      ww <- input$ww
      w <- input$w
      subg <- input$subg
      bridges <- modibridges(input$bridges)
      protected_columns <- w_c()$protect_cols
      parsed_params <- parseDesignInputParams(input$protected_blocks, input$p_a)
      protected_blocks <- parsed_params$protected_blocks
      water_columns <- w_c()$water_cols
      lane_columns <- w_c()$lane_cols
      p_a <- parsed_params$p_a
      design_from_left <- as.logical(input$design_from_left)
      plant_from_left <- as.logical(input$plant_from_left)
      result <- designPlot(
        blocks, y, bridges, ww, w,
        protected_columns, protected_blocks,
        water_columns, lane_columns, p_a,
        design_from_left, plant_from_left, subg
      )
      lastValidDataset(result)
      result
    }, error = function(e) {
      cached <- lastValidDataset()
      if (is.matrix(cached) && nrow(cached) > 0 && ncol(cached) > 0) {
        return(cached)
      }
      validate(need(FALSE, e$message))
    })
  })

  datasetSelected <- reactive({ selectedCol(datasetInput()) })
  datasetStats <- reactive({ getSta(datasetInput()) })
  seedList <- reactive({ req(input$file1); makelist(contents()) })
  datasetInputDebounced <- shiny::debounce(datasetInput, 400)
  # plantedDataDebounced 在 plantedData 定义后设置（见下方）

  # ===========================================================================
  # 辅助函数
  # ===========================================================================
  applyModelToPlanningInputs <- function(model) {
    if (is.null(model)) return(invisible(NULL))
    if (!is.na(model$no_plant)) updateTextInput(inputId = "p_a", value = as.character(model$no_plant))
    if (!is.na(model$field_layout)) updateTextInput(inputId = "get_water_columns", value = as.character(model$field_layout))
    if (!is.na(model$strip_width)) updateTextInput(inputId = "bridges", value = as.character(model$strip_width))
    if (!is.na(model$protect_strip)) updateTextInput(inputId = "protected_blocks", value = as.character(model$protect_strip))
    if (!is.na(model$cross_path_width)) updateNumericInput(inputId = "ww", value = as.numeric(model$cross_path_width))
    if (!is.na(model$row_gap)) updateNumericInput(inputId = "w", value = as.numeric(model$row_gap))
    if (!is.na(model$group_rows)) updateNumericInput(inputId = "subg", value = as.numeric(model$group_rows))
    start_pos <- model$plant_start_pos
    end_pos <- model$plant_end_pos
    # 兼容旧数据字段
    if ((is.null(start_pos) || is.na(start_pos) || !nzchar(trimws(as.character(start_pos)))) &&
        !is.null(model$plant_start_row) && !is.na(model$plant_start_row) &&
        !is.null(model$plant_start_col) && !is.na(model$plant_start_col)) {
      start_pos <- paste0(as.integer(model$plant_start_row), ",", as.integer(model$plant_start_col))
    }
    if ((is.null(end_pos) || is.na(end_pos)) && !is.null(model$plant_end_col) && !is.na(model$plant_end_col)) {
      end_pos <- paste0(",", as.integer(model$plant_end_col))
    }
    if (!is.null(start_pos) && !is.na(start_pos)) updateTextInput(inputId = "experimentPlantStartPos", value = as.character(start_pos))
    if (!is.null(end_pos) && !is.na(end_pos)) updateTextInput(inputId = "experimentPlantEndPos", value = as.character(end_pos))
    updateRadioButtons(inputId = "design_from_left", selected = ifelse(!is.na(model$plan_left), as.logical(model$plan_left), TRUE))
    updateRadioButtons(inputId = "plant_from_left", selected = ifelse(!is.na(model$plant_left), as.logical(model$plant_left), TRUE))
    invisible(NULL)
  }

  currentFieldModelData <- reactive({
    list(
      field_len = NA_real_, no_plant = input$p_a,
      field_layout = input$get_water_columns, strip_width = input$bridges,
      protect_strip = input$protected_blocks, cross_path_width = input$ww,
      row_gap = input$w, group_rows = input$subg,
      plant_start_pos = input$experimentPlantStartPos, plant_end_pos = input$experimentPlantEndPos,
      plan_left = as.logical(input$design_from_left),
      plant_left = as.logical(input$plant_from_left)
    )
  })

  currentFieldModelName <- reactive({
    selected <- input$field_model_select
    if (is.null(selected)) return("常规地块1")
    selected <- trimws(selected)
    if (!nzchar(selected)) return("常规地块1")
    selected
  })

  autoPersistPlanId <- reactive({
    model_name <- currentFieldModelName()
    if (is.null(model_name) || !nzchar(trimws(model_name))) model_name <- "default"
    safe_model <- gsub("[^A-Za-z0-9_-]+", "_", enc2utf8(model_name))
    safe_model <- trimws(safe_model)
    if (!nzchar(safe_model)) safe_model <- "default"
    exp_name <- currentExperimentName()
    safe_exp <- gsub("[^A-Za-z0-9_-]+", "_", enc2utf8(exp_name))
    safe_exp <- trimws(safe_exp)
    if (!nzchar(safe_exp)) safe_exp <- "default"
    paste0("auto_plan_", safe_model, "_", safe_exp)
  })

  matrixSignature <- function(mat, data_cols = NULL) {
    if (is.null(mat) || !is.matrix(mat) || nrow(mat) == 0 || ncol(mat) == 0) return("empty")
    if (is.null(data_cols) || is.na(data_cols)) data_cols <- max(1, ncol(mat) - STAT_COL_COUNT)
    data_cols <- min(max(1, as.integer(data_cols)), ncol(mat))
    data_part <- mat[, seq_len(data_cols), drop = FALSE]
    data_num <- suppressWarnings(matrix(as.numeric(data_part), nrow = nrow(data_part), ncol = ncol(data_part)))
    num_sum <- sum(data_num, na.rm = TRUE)
    pos_cnt <- sum(!is.na(data_num) & data_num > 0)
    paste0("r", nrow(mat), "_c", ncol(mat), "_d", data_cols, "_s", format(num_sum, scientific = FALSE), "_p", pos_cnt)
  }

  countPlantingTargets <- function(field, start_row = 1, start_col = 1, end_row = NULL, end_col = NULL) {
    if (is.null(field) || !is.matrix(field) || ncol(field) <= STAT_COL_COUNT) return(0L)
    data_cols <- ncol(field) - STAT_COL_COUNT
    rowno_idx <- ncol(field) - STAT_COL_OFFSET_ROWNO
    rowno_vec <- suppressWarnings(as.numeric(field[, rowno_idx]))
    valid_rows <- rowno_vec[!is.na(rowno_vec)]
    max_row <- if (length(valid_rows) > 0) max(valid_rows) else nrow(field)
    if (is.null(end_row)) end_row <- max_row
    if (is.null(end_col)) end_col <- data_cols

    remaining <- 0L
    for (r in seq_len(nrow(field))) {
      row_no <- suppressWarnings(as.numeric(field[r, rowno_idx]))
      if (is.na(row_no) || row_no < start_row || row_no > end_row) next
      for (c in seq_len(data_cols)) {
        if (c < start_col || c > end_col) next
        cell_text <- trimws(as.character(field[r, c]))
        if (grepl("^[0-9]+$", cell_text)) remaining <- remaining + 1L
      }
    }
    as.integer(remaining)
  }

  countSeedDemand <- function(seed_df) {
    if (!is.data.frame(seed_df) || nrow(seed_df) == 0 || !("re" %in% names(seed_df))) return(0L)
    re_vec <- suppressWarnings(as.integer(seed_df$re))
    re_vec[is.na(re_vec) | re_vec < 0] <- 0L
    as.integer(sum(re_vec))
  }

  getPlantingCapacityStatus <- function(base_matrix, parsed_range, seed_df, exp_id = NULL, table_name = NULL) {
    seed_demand <- countSeedDemand(seed_df)
    target_capacity <- countPlantingTargets(base_matrix, parsed_range$start_row, parsed_range$start_col, parsed_range$end_row, parsed_range$end_col)
    list(
      ok = isTRUE(seed_demand <= target_capacity),
      demand = seed_demand,
      capacity = target_capacity,
      overflow = max(0L, seed_demand - target_capacity),
      exp_id = exp_id,
      table_name = table_name
    )
  }

  splitConnectedBoxes <- function(mask_matrix, row_ids) {
    nr <- nrow(mask_matrix)
    nc <- ncol(mask_matrix)
    if (is.null(nr) || is.null(nc) || nr == 0 || nc == 0) return(list())
    visited <- matrix(FALSE, nrow = nr, ncol = nc)
    boxes <- list()
    bi <- 1L
    dirs <- matrix(c(1, 0, -1, 0, 0, 1, 0, -1), ncol = 2, byrow = TRUE)

    for (r in seq_len(nr)) {
      for (c in seq_len(nc)) {
        if (!isTRUE(mask_matrix[r, c]) || isTRUE(visited[r, c])) next
        q_r <- c(r)
        q_c <- c(c)
        q_head <- 1L
        visited[r, c] <- TRUE
        comp_r <- integer(0)
        comp_c <- integer(0)

        while (q_head <= length(q_r)) {
          cr <- q_r[q_head]
          cc <- q_c[q_head]
          q_head <- q_head + 1L
          comp_r <- c(comp_r, cr)
          comp_c <- c(comp_c, cc)
          for (di in seq_len(nrow(dirs))) {
            nr2 <- cr + dirs[di, 1]
            nc2 <- cc + dirs[di, 2]
            if (nr2 < 1 || nr2 > nr || nc2 < 1 || nc2 > nc) next
            if (!isTRUE(mask_matrix[nr2, nc2]) || isTRUE(visited[nr2, nc2])) next
            visited[nr2, nc2] <- TRUE
            q_r <- c(q_r, nr2)
            q_c <- c(q_c, nc2)
          }
        }

        row_vals <- row_ids[unique(comp_r)]
        row_vals <- row_vals[!is.na(row_vals)]
        if (length(row_vals) == 0) next
        boxes[[bi]] <- list(
          start_row = min(row_vals),
          end_row = max(row_vals),
          start_col = min(comp_c),
          end_col = max(comp_c)
        )
        bi <- bi + 1L
      }
    }
    boxes
  }

  fieldModelNames <- reactive({
    fieldModelTrigger()
    models <- listFieldModels(sqlite_db_path)
    if (!is.data.frame(models) || nrow(models) == 0) return(character())
    as.character(models$field_name)
  })

  plantTables <- reactive({
    plantTableTrigger()
    listPlantTables(sqlite_db_path)
  })

  sowTables <- reactive({
    sowTrigger()
    listSowTables(sqlite_db_path)
  })

  selectedPlantTableName <- reactive({
    plantTableTrigger()
    ap <- activePlantTableName()
    if (length(ap) != 1L || is.na(ap)) return(NA_character_)
    ap <- trimws(as.character(ap))
    if (!nzchar(ap)) NA_character_ else ap
  })

  selectedExperimentPlantTableName <- reactive({
    selectedPlantTableName()
  })

  selectedLayoutFieldName <- reactive({
    plantTableTrigger()
    p <- selectedPlantTableName()
    if (is.na(p) || !nzchar(p)) return(NA_character_)
    sub("\\.plant$", "", p)
  })

  selectedSowTableName <- reactive({
    sowTrigger()
    plantTableTrigger()
    field <- selectedLayoutFieldName()
    if (is.na(field) || !nzchar(field)) return(NA_character_)
    sow_df <- sowTables()
    if (!is.data.frame(sow_df) || nrow(sow_df) == 0) return(NA_character_)
    sm <- as.character(sow_df$table_name[as.character(sow_df$field_name) == field])
    if (length(sm) >= 1L) return(sm[1])
    as.character(sow_df$table_name[1])
  })

  selectedExperimentPlantBaseMatrix <- reactive({
    plantTableTrigger()
    selected_name <- selectedExperimentPlantTableName()
    if (is.na(selected_name)) return(selectedPlantBaseMatrix())
    readPlantTable(selected_name, sqlite_db_path)
  })

  selectedPlantBaseMatrix <- reactive({
    plantTableTrigger()
    selected_name <- selectedPlantTableName()
    if (is.na(selected_name)) return(datasetInput())
    readPlantTable(selected_name, sqlite_db_path)
  })

  experimentOptions <- reactive({
    experimentsTrigger()
    recordsTrigger()
    plantTableTrigger()
    exp_df <- readTableFromSqlite("experiments", sqlite_db_path)
    if (!is.data.frame(exp_df) || nrow(exp_df) == 0) return(data.frame())
    exp_df
  })

  experimentSeedData <- reactive({
    exp_id <- trimws(input$sqliteFilterExperimentId)
    validate(need(nzchar(exp_id), "请先选择试验名称"))
    records <- readTableFromSqlite("experiment_records", sqlite_db_path)
    validate(need(is.data.frame(records) && nrow(records) > 0, "暂无试验记录"))
    records <- records[as.character(records$experiment_id) == exp_id, , drop = FALSE]
    validate(need(nrow(records) > 0, "当前试验没有可种植记录"))
    records
  })

  experimentSeedList <- reactive({ makelist(experimentSeedListRaw()) })

  experimentSeedListRaw <- reactive({
    records <- experimentSeedData()
    validate(need("stageid" %in% names(records), "试验记录缺少 stageid 列"))
    stage_vec <- as.character(records$stageid)
    validate(need(all(!is.na(stage_vec) & nzchar(trimws(stage_vec))), "试验记录中的 stageid 不能为空"))
    seed_df <- data.frame(
      num = as.character(records$id),
      name = stage_vec,
      re = suppressWarnings(as.integer(records$rows)),
      stringsAsFactors = FALSE
    )
    validate(need(all(!is.na(seed_df$re) & seed_df$re > 0), "试验记录中的 rows 必须为正整数"))
    seed_df
  })

  experimentPlantedState <- reactiveVal(NULL)

  experimentFillPreview <- reactive({
    plantTableTrigger()
    pt <- selectedPlantTableName()
    table_name <- if (is.na(pt)) "" else trimws(as.character(pt))
    material <- trimws(input$experimentFillMaterial)
    if (!nzchar(table_name)) {
      return(list(level = "info", runnable = FALSE, message = "ℹ️ 补种摘要：请选择种植地块", parsed = NULL, base_matrix = NULL, data_cols = NA_integer_))
    }
    if (!nzchar(material)) {
      return(list(level = "neutral", runnable = FALSE, message = "ℹ️ 补种摘要：请输入补种材料名称", parsed = NULL, base_matrix = NULL, data_cols = NA_integer_))
    }
    base_matrix <- tryCatch(selectedExperimentPlantBaseMatrix(), error = function(e) NULL)
    if (is.null(base_matrix) || !is.matrix(base_matrix) || ncol(base_matrix) <= STAT_COL_COUNT) {
      return(list(level = "neutral", runnable = FALSE,
                  message = paste0("ℹ️ 补种摘要：地块=", table_name, "（地块数据不可用）"),
                  parsed = NULL, base_matrix = NULL, data_cols = NA_integer_))
    }
    data_cols <- ncol(base_matrix) - STAT_COL_COUNT
    parsed <- tryCatch(
      parsePlantingCoordinateRange(input$experimentPlantStartPos, input$experimentPlantEndPos, nrow(base_matrix), data_cols),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      return(list(level = "error", runnable = FALSE,
                  message = paste0("⛔ 补种范围无效：可选列范围 1-", data_cols),
                  parsed = NULL, base_matrix = base_matrix, data_cols = data_cols))
    }
    fill_res <- tryCatch(
      fillUnplantedWithMaterial(base_matrix, material, parsed$start_row, parsed$start_col, parsed$end_row, parsed$end_col),
      error = function(e) NULL
    )
    if (is.null(fill_res)) {
      return(list(level = "error", runnable = FALSE, message = "⛔ 补种预估失败，请检查输入",
                  parsed = parsed, base_matrix = base_matrix, data_cols = data_cols))
    }
    msg <- paste0("✅ 补种预估：将在行 ", parsed$start_row, "-", parsed$end_row, "、列 ", parsed$start_col, "-", parsed$end_col,
                  " 内，将 ", fill_res$filled_count, " 个未种位补为『", material, "』。")
    list(level = "ok", runnable = TRUE, message = msg, parsed = parsed, base_matrix = base_matrix, data_cols = data_cols, fill_res = fill_res)
  })

  output$experimentFillSummaryUi <- renderUI({
    state <- experimentFillPreview()
    style <- switch(state$level,
                    "info" = "background:#eef2ff;border:1px solid #c7d2fe;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#3730a3;",
                    "neutral" = "background:#f8fafc;border:1px solid #dbe4ee;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#475569;",
                    "error" = "background:#fef2f2;border:1px solid #fca5a5;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#991b1b;",
                    "ok" = "background:#ecfdf5;border:1px solid #86efac;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#166534;",
                    "background:#f8fafc;border:1px solid #dbe4ee;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#475569;")
    tags$div(style = style, state$message)
  })

  # ---- 试验种植页面顶部状态摘要 ----
  experimentStatusSummary <- reactive({
    experimentsTrigger()
    recordsTrigger()
    plantTableTrigger()
    exp_df <- readTableFromSqlite("experiments", sqlite_db_path)
    rec_df <- readTableFromSqlite("experiment_records", sqlite_db_path)
    exp_count <- if (is.data.frame(exp_df) && nrow(exp_df) > 0) nrow(exp_df) else 0L
    rec_count <- if (is.data.frame(rec_df) && nrow(rec_df) > 0) nrow(rec_df) else 0L
    selected_exp <- trimws(input$sqliteFilterExperimentId)
    row_sum_col <- if (is.data.frame(rec_df) && nrow(rec_df) > 0) {
      if ("new_rows" %in% names(rec_df)) "new_rows" else if ("rows" %in% names(rec_df)) "rows" else NA_character_
    } else NA_character_
    # 与「当前筛选」无关：累计全部试验在记录表中的行数
    total_rows_sum <- if (!is.na(row_sum_col)) {
      sum(suppressWarnings(as.numeric(rec_df[[row_sum_col]])), na.rm = TRUE)
    } else {
      0
    }
    selected_exp_display <- if (!nzchar(selected_exp)) {
      "全部"
    } else if (is.data.frame(exp_df) && nrow(exp_df) > 0 &&
               "experiment_id" %in% names(exp_df) && "experiment_name" %in% names(exp_df)) {
      eid_col <- trimws(as.character(exp_df$experiment_id))
      hit <- exp_df[eid_col == trimws(selected_exp), , drop = FALSE]
      if (nrow(hit) >= 1L) {
        nm <- trimws(as.character(hit$experiment_name[1]))
        if (nzchar(nm)) nm else selected_exp
      } else {
        selected_exp
      }
    } else {
      selected_exp
    }
    pt <- selectedPlantTableName()
    selected_plant_display <- if (!is.na(pt) && nzchar(trimws(as.character(pt)))) trimws(as.character(pt)) else "未选"
    hint <- if (!nzchar(selected_exp)) {
      "📋 请先在「种植配置」中选择种植地块与试验，或导入新试验"
    } else {
      "✅ 已筛选，可查看下方数据表或执行种植操作"
    }
    list(exp_count = exp_count, rec_count = rec_count, total_rows_sum = total_rows_sum,
         selected_exp = selected_exp_display, selected_plant = selected_plant_display,
         hint = hint)
  })

  output$experimentStatusSummaryUi <- renderUI({
    s <- experimentStatusSummary()
    tags$div(
      style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:12px;padding:12px 16px;margin-bottom:14px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
      fluidRow(
        column(2, tags$div(style = "text-align:center;",
          tags$span(as.character(s$exp_count), style = "font-size:28px;font-weight:700;color:#3b82f6;"),
          tags$br(),
          tags$span("试验数", style = "font-size:12px;color:#6b7280;")
        )),
        column(2, tags$div(style = "text-align:center;",
          tags$span(as.character(s$rec_count), style = "font-size:28px;font-weight:700;color:#3b82f6;"),
          tags$br(),
          tags$span("记录数", style = "font-size:12px;color:#6b7280;")
        )),
        column(2, tags$div(style = "text-align:center;",
          tags$span(as.character(s$total_rows_sum), style = "font-size:28px;font-weight:700;color:#3b82f6;"),
          tags$br(),
          tags$span("总行数（全部试验）", style = "font-size:12px;color:#6b7280;")
        )),
        column(3, tags$div(style = "text-align:center;",
          tags$span(s$selected_exp, style = "font-size:14px;font-weight:600;color:#1f2937;"),
          tags$br(),
          tags$span("当前筛选", style = "font-size:12px;color:#6b7280;")
        )),
        column(3, tags$div(style = "text-align:center;",
          tags$span(s$selected_plant, style = "font-size:14px;font-weight:600;color:#1f2937;"),
          tags$br(),
          tags$span("种植地块", style = "font-size:12px;color:#6b7280;")
        ))
      ),
      tags$div(style = "margin-top:8px;padding-top:8px;border-top:1px solid #e5e7eb;",
        tags$span(s$hint, style = "font-size:13px;color:#4b5563;")
      )
    )
  })

  observeEvent(input$runExperimentFillUnplanted, {
    tryCatch({
      state <- experimentFillPreview()
      validate(need(isTRUE(state$runnable), state$message))
      fill_res <- state$fill_res
      selected_plant_table <- selectedExperimentPlantTableName()
      validate(need(!is.na(selected_plant_table) && nzchar(selected_plant_table), "请先选择要写入的种植地块"))

      updated_matrix <- fill_res$matrix
      savePlantTable(field_name = sub("\\.plant$", "", selected_plant_table), plan_matrix = updated_matrix, db_path = sqlite_db_path)
      sow_result <- saveSowTable(
        field_name = sub("\\.plant$", "", selected_plant_table),
        sow_data = buildSowTable(field_name = sub("\\.plant$", "", selected_plant_table), base_matrix = state$base_matrix, planted_matrix = updated_matrix),
        db_path = sqlite_db_path
      )
      experimentPlantedState(list(table_name = selected_plant_table, matrix = updated_matrix))
      plantTableTrigger(plantTableTrigger() + 1)
      refreshSow()
      setActivePlantTable(selected_plant_table)
      showNotification(paste0("区域补种完成：已补 ", fill_res$filled_count, " 个未种位；结果写入 ", selected_plant_table, " 与 ", sow_result$table_name), type = "message")
    }, error = function(e) {
      showNotification(paste0("区域补种失败: ", e$message), type = "error")
    })
  })

  experimentPlantValidation <- reactive({
    plantTableTrigger()
    exp_id <- trimws(input$sqliteFilterExperimentId)
    pt <- selectedPlantTableName()
    table_name <- if (is.na(pt)) "" else trimws(as.character(pt))
    if (!nzchar(exp_id) || !nzchar(table_name)) {
      return(list(level = "info", runnable = FALSE, message = "ℹ️ 执行摘要：请选择试验与种植地块"))
    }
    base_matrix <- tryCatch(selectedExperimentPlantBaseMatrix(), error = function(e) NULL)
    if (is.null(base_matrix) || !is.matrix(base_matrix) || ncol(base_matrix) <= STAT_COL_COUNT) {
      return(list(level = "neutral", runnable = FALSE,
                  message = paste0("ℹ️ 执行摘要：试验=", exp_id, "；地块=", table_name, "（地块数据不可用，暂不校验范围）")))
    }
    total_rows <- nrow(base_matrix)
    data_cols <- ncol(base_matrix) - STAT_COL_COUNT
    parsed <- tryCatch(
      parsePlantingCoordinateRange(input$experimentPlantStartPos, input$experimentPlantEndPos, total_rows, data_cols),
      error = function(e) NULL
    )
    if (is.null(parsed)) {
      return(list(level = "error", runnable = FALSE,
                  message = paste0("⛔ 范围无效：试验=", exp_id, "；地块=", table_name, "；当前列范围设置不合法（可选列范围 1-", data_cols, "）")))
    }
    start_row <- parsed$start_row
    end_row <- parsed$end_row
    start_col <- parsed$start_col
    end_col <- parsed$end_col
    capacity_status <- tryCatch(
      getPlantingCapacityStatus(base_matrix, parsed, experimentSeedListRaw(), exp_id = exp_id, table_name = table_name),
      error = function(e) NULL
    )
    if (is.null(capacity_status)) {
      return(list(level = "error", runnable = FALSE,
                  message = paste0("⛔ 容量校验失败：试验=", exp_id, "；地块=", table_name, "。请检查试验记录中的 rows 是否为有效正整数。")))
    }
    if (!isTRUE(capacity_status$ok)) {
      return(list(level = "error", runnable = FALSE,
                  message = paste0("⛔ 种植容量不足：可种 ", capacity_status$capacity, " 行；需种 ", capacity_status$demand, " 行；超出 ", capacity_status$overflow, " 行。")))
    }
    if (isTRUE(input$allowExperimentReplant)) {
      return(list(level = "warn", runnable = TRUE,
                  message = paste0("⚠️ 覆盖模式：试验=", exp_id, "；地块=", table_name, "；行 ", start_row, "-", end_row, "；列 ", start_col, "-", end_col, "。将执行重复种植。")))
    }
    list(level = "ok", runnable = TRUE,
         message = paste0("✅ 可执行：试验=", exp_id, "；地块=", table_name, "；行 ", start_row, "-", end_row, "；列 ", start_col, "-", end_col))
  })

  output$experimentPlantSummaryUi <- renderUI({
    state <- tryCatch(experimentPlantValidation(), error = function(e) NULL)
    req(state)
    style <- switch(state$level,
                    "info" = "background:#eef2ff;border:1px solid #c7d2fe;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#3730a3;",
                    "neutral" = "background:#f8fafc;border:1px solid #dbe4ee;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#475569;",
                    "error" = "background:#fef2f2;border:1px solid #fca5a5;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#991b1b;",
                    "warn" = "background:#fffbeb;border:1px solid #fcd34d;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#92400e;",
                    "ok" = "background:#ecfdf5;border:1px solid #86efac;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#166534;",
                    "background:#f8fafc;border:1px solid #dbe4ee;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#475569;")
    tags$div(style = style, state$message)
  })

  output$runExperimentPlantingUi <- renderUI({
    if (isTRUE(isPlantingInProgress())) {
      actionButton("runExperimentPlanting", "执行中...", class = "btn-default", width = "100%", disabled = "disabled")
    } else {
      state <- tryCatch(experimentPlantValidation(), error = function(e) NULL)
      req(state)
      if (isTRUE(state$runnable)) {
        actionButton("runExperimentPlanting", "执行试验种植", class = "btn-primary", width = "100%")
      } else {
        actionButton("runExperimentPlanting", "执行试验种植（先选试验或修正设置）", class = "btn-default", width = "100%", disabled = "disabled")
      }
    }
  })

  runExperimentPlantingImpl <- function(overwrite_mode = FALSE) {
    selected_plant_table <- selectedExperimentPlantTableName()
    validate(need(!is.na(selected_plant_table) && nzchar(selected_plant_table), "请先选择要写入的种植地块"))
    exp_id <- trimws(input$sqliteFilterExperimentId)
    validate(need(nzchar(exp_id), "请先选择试验名称"))
    if (!isTRUE(overwrite_mode) && !isTRUE(input$allowExperimentReplant) &&
        isTRUE(hasExperimentPlantRun(exp_id, selected_plant_table, sqlite_db_path))) {
      stop("该试验已在当前地块执行过种植。若确需重种，请勾选「允许覆盖重种」后再执行。")
    }
    base_matrix <- selectedExperimentPlantBaseMatrix()
    validate(need(is.matrix(base_matrix) && ncol(base_matrix) > 0, "请选择有效的种植地块"))
    data_cols <- ncol(base_matrix) - STAT_COL_COUNT
    validate(need(data_cols > 0, "种植地块列数不合法"))

    parsed <- parsePlantingCoordinateRange(input$experimentPlantStartPos, input$experimentPlantEndPos, nrow(base_matrix), data_cols)
    capacity_status <- getPlantingCapacityStatus(base_matrix, parsed, experimentSeedListRaw(), exp_id = exp_id, table_name = selected_plant_table)
    if (!isTRUE(capacity_status$ok)) {
      stop(paste0("种植容量不足：可种 ", capacity_status$capacity, " 行；需种 ", capacity_status$demand, " 行；超出 ", capacity_status$overflow, " 行。请缩小种子量或扩大种植范围。"))
    }
    stable_plan_id <- autoPersistPlanId()
    ck <- capturePlantingUndoCheckpoint(
      plant_table_name = selected_plant_table,
      experiment_id = exp_id,
      plan_id = stable_plan_id,
      field_name = sub("\\.plant$", "", selected_plant_table),
      db_path = sqlite_db_path
    )

    planted <- plant(base_matrix, experimentSeedList(),
                     start_row = parsed$start_row, start_col = parsed$start_col,
                     end_row = parsed$end_row, end_col = parsed$end_col)
    experimentPlantedState(list(table_name = selected_plant_table, matrix = planted))

    sow_result <- saveSowTable(
      field_name = sub("\\.plant$", "", selected_plant_table),
      sow_data = buildSowTable(field_name = sub("\\.plant$", "", selected_plant_table), base_matrix = base_matrix, planted_matrix = planted),
      db_path = sqlite_db_path
    )
    savePlantTable(field_name = sub("\\.plant$", "", selected_plant_table), plan_matrix = planted, db_path = sqlite_db_path)
    plantTableTrigger(plantTableTrigger() + 1)
    setActivePlantTable(selected_plant_table)

    exp_df <- experimentOptions()
    exp_name <- if (is.data.frame(exp_df) && nrow(exp_df) > 0 && exp_id %in% as.character(exp_df$experiment_id)) {
      as.character(exp_df$experiment_name[match(exp_id, as.character(exp_df$experiment_id))])
    } else {
      currentExperimentName()
    }
    plan_result <- savePlanToSqlite(plan_matrix = base_matrix, experiment_name = exp_name, db_path = sqlite_db_path, plan_id = stable_plan_id, metadata = buildPersistenceMeta())
    latest_plan_id(plan_result$plan_id)
    saveAssignmentsToSqlite(plan_id = plan_result$plan_id, planted_matrix = planted, plan_matrix = base_matrix, experiment_name = exp_name, db_path = sqlite_db_path)
    saveExperimentPlantRun(experiment_id = exp_id, plant_table_name = selected_plant_table, sow_table_name = sow_result$table_name,
                           plan_id = plan_result$plan_id, db_path = sqlite_db_path,
                           overwrite = isTRUE(overwrite_mode) || isTRUE(input$allowExperimentReplant))
    refreshAllSqlite()
    stk <- plantingUndoStack()
    stk <- c(stk, list(ck))
    if (length(stk) > 2L) stk <- utils::tail(stk, 2)
    plantingUndoStack(stk)
    if (isTRUE(overwrite_mode)) {
      showNotification(paste0("试验种植完成（覆盖重种），结果已写入：", selected_plant_table, "；", sow_result$table_name, "；plan_id: ", plan_result$plan_id), type = "message")
    } else {
      showNotification(paste0("试验种植完成，结果已写入：", selected_plant_table, "；", sow_result$table_name, "；plan_id: ", plan_result$plan_id), type = "message")
    }
  }

  observeEvent(input$runExperimentPlanting, {
    tryCatch({
      if (isTRUE(input$allowExperimentReplant)) {
        showModal(modalDialog(
          title = "确认覆盖重种",
          tags$p("你已启用「允许覆盖重种」。", style = "margin:0;color:#b91c1c;"),
          footer = tagList(modalButton("取消"), actionButton("confirmRunExperimentPlanting", "确认执行", class = "btn-danger")),
          easyClose = TRUE
        ))
      } else {
        isPlantingInProgress(TRUE)
        runExperimentPlantingImpl(FALSE)
      }
    }, error = function(e) {
      showNotification(paste0("试验种植失败: ", e$message), type = "error")
    }, finally = {
      isPlantingInProgress(FALSE)
    })
  })

  observeEvent(input$confirmRunExperimentPlanting, {
    tryCatch({
      removeModal()
      isPlantingInProgress(TRUE)
      runExperimentPlantingImpl(TRUE)
    }, error = function(e) {
      showNotification(paste0("试验种植失败: ", e$message), type = "error")
    }, finally = {
      isPlantingInProgress(FALSE)
    })
  })

  output$undoExperimentPlantingUi <- renderUI({
    n <- length(plantingUndoStack())
    lbl <- if (n > 0L) {
      sprintf("撤销上一步种植（还可 %d 次）", n)
    } else {
      "撤销上一步种植（当前无可撤）"
    }
    cls <- if (n > 0L) "btn-warning" else "btn-default"
    actionButton("undoExperimentPlanting", lbl, class = cls, width = "100%")
  })

  observeEvent(input$undoExperimentPlanting, {
    stk <- plantingUndoStack()
    if (length(stk) == 0L) {
      showNotification("没有可撤销的种植操作（仅保留最近两次，且刷新页面后会清空）", type = "warning")
      return(invisible(NULL))
    }
    ck <- stk[[length(stk)]]
    tryCatch({
      restorePlantingUndoCheckpoint(ck, sqlite_db_path)
      plantingUndoStack(stk[-length(stk)])
      experimentPlantedState(list(table_name = ck$plant_table_name, matrix = ck$plant_matrix))
      if (is.data.frame(ck$plan_run) && nrow(ck$plan_run) > 0L && nzchar(trimws(as.character(ck$plan_id)))) {
        latest_plan_id(as.character(ck$plan_id[1]))
      } else {
        latest_plan_id(NULL)
      }
      eid_undo <- trimws(as.character(ck$experiment_id))
      if (nzchar(eid_undo)) pendingSqliteExperimentSelection(eid_undo)
      plantTableTrigger(plantTableTrigger() + 1L)
      refreshAllSqlite()
      setActivePlantTable(ck$plant_table_name)
      showNotification("已撤销上一步「执行试验种植」", type = "message")
    }, error = function(e) {
      showNotification(paste0("撤销失败: ", e$message), type = "error")
    })
  })

  plantedData <- reactive({
    req(input$file1)
    coord <- planningCoordinateRange()
    plant(
      selectedPlantBaseMatrix(),
      seedList(),
      start_row = coord$start_row,
      start_col = coord$start_col,
      end_row = coord$end_row,
      end_col = coord$end_col
    )
  })
  plantedDataDebounced <- shiny::debounce(plantedData, 400)

  currentExperimentPlantedMatrix <- reactive({
    planted_state <- experimentPlantedState()
    selected_table <- selectedPlantTableName()
    if (is.list(planted_state) && identical(planted_state$table_name, selected_table) && is.matrix(planted_state$matrix) && ncol(planted_state$matrix) > 0) {
      return(planted_state$matrix)
    }
    NULL
  })

  currentPlantedMatrix <- reactive({
    experiment_matrix <- currentExperimentPlantedMatrix()
    if (is.matrix(experiment_matrix) && ncol(experiment_matrix) > 0) return(experiment_matrix)
    plantedData()
  })

  currentPlantPreviewMatrix <- reactive({
    experiment_matrix <- currentExperimentPlantedMatrix()
    if (is.matrix(experiment_matrix) && ncol(experiment_matrix) > 0) return(experiment_matrix)
    selectedPlantBaseMatrix()
  })

  recordsLatestMap <- reactive({
    recordsTrigger()
    records <- readTableFromSqlite("experiment_records", sqlite_db_path)
    if (!is.data.frame(records) || nrow(records) == 0 || !all(c("stageid", "name", "former_stageid", "source") %in% names(records))) return(NULL)
    stage_vec <- trimws(as.character(records$stageid))
    valid_idx <- !is.na(stage_vec) & nzchar(stage_vec)
    records <- records[valid_idx, , drop = FALSE]
    if (nrow(records) == 0) return(NULL)
    if ("created_at" %in% names(records)) {
      created <- as.character(records$created_at)
      records <- records[order(created, decreasing = TRUE), , drop = FALSE]
    }
    stage_vec <- trimws(as.character(records$stageid))
    keep <- !duplicated(stage_vec)
    records <- records[keep, c("stageid", "name", "former_stageid", "source"), drop = FALSE]
    rownames(records) <- trimws(as.character(records$stageid))
    records
  })

  currentSowData <- reactive({
    sowTrigger()
    selected_table <- selectedSowTableName()
    validate(need(!is.na(selected_table) && nzchar(trimws(as.character(selected_table))), "请先选择播种地块"))
    selected_table <- trimws(as.character(selected_table))
    sow_df <- readSowTable(selected_table, sqlite_db_path)
    validate(need(is.data.frame(sow_df) && nrow(sow_df) > 0, paste0("暂无播种表：", selected_table)))

    records <- recordsLatestMap()
    if (is.data.frame(records) && nrow(records) > 0 && "ID" %in% names(sow_df)) {
      id_trim <- trimws(as.character(sow_df$ID))
      match_idx <- match(id_trim, rownames(records))
      sow_df$name <- as.character(records$name[match_idx])
      sow_df$former_stageid <- as.character(records$former_stageid[match_idx])
      sow_df$source <- as.character(records$source[match_idx])
      base_cols <- names(sow_df)
      id_pos <- match("ID", base_cols)
      extra_cols <- c("name", "former_stageid", "source")
      if (!is.na(id_pos)) {
        ordered <- c(base_cols[seq_len(id_pos)], extra_cols, setdiff(base_cols, c(base_cols[seq_len(id_pos)], extra_cols)))
        sow_df <- sow_df[, ordered, drop = FALSE]
      }
    }
    sow_df
  })

  output$sowIntegrityUi <- renderUI({
    sowTrigger()
    plantTableTrigger()
    st <- selectedSowTableName()
    selected_table <- if (!is.na(st)) trimws(as.character(st)) else ""
    if (!nzchar(selected_table)) {
      return(tags$div(style = "background:#eef2ff;border:1px solid #c7d2fe;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#3730a3;", "ℹ️ 完整性检查：请先选择播种地块"))
    }
    sow_df <- readSowTable(selected_table, sqlite_db_path)
    if (!is.data.frame(sow_df) || nrow(sow_df) == 0) {
      return(tags$div(style = "background:#f8fafc;border:1px solid #dbe4ee;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#475569;", "ℹ️ 完整性检查：当前地块暂无播种数据"))
    }
    plant_table <- sub("\\.sow$", ".plant", selected_table)
    base_matrix <- tryCatch(readPlantTable(plant_table, sqlite_db_path), error = function(e) NULL)
    if (is.null(base_matrix) || !is.matrix(base_matrix) || ncol(base_matrix) <= STAT_COL_COUNT) {
      return(tags$div(style = "background:#f8fafc;border:1px solid #dbe4ee;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#475569;",
                      paste0("ℹ️ 完整性检查：播种条目 ", nrow(sow_df), " 条（无法读取对应地块，跳过比对）")))
    }
    progress <- computeSowProgress(base_matrix, sow_df)
    planted_count <- progress$planted_count
    unplanted_count <- progress$placeholder_count
    total_count <- planted_count + unplanted_count
    tags$div(
      style = "background:#f8fafc;border:1px solid #dbe4ee;border-radius:8px;padding:8px 10px;margin-bottom:8px;color:#334155;",
      paste0("总行数 ", total_count, " 行；已种 ", planted_count, " 行；未种 ", unplanted_count, " 行（未种定义：ID 为纯数字）。")
    )
  })

  # ===========================================================================
  # 自动持久化（debounce 保护）
  # ===========================================================================
  observeEvent(datasetInputDebounced(), {
    tryCatch({
      plan_mat <- datasetInputDebounced()
      plan_sig <- paste0(autoPersistPlanId(), "_", currentExperimentName(), "_", matrixSignature(plan_mat, ncol(plan_mat) - STAT_COL_COUNT))
      if (identical(plan_sig, autoPersistLastPlanSig())) return(invisible(NULL))
      plan_result <- savePlanToSqlite(
        plan_matrix = plan_mat,
        experiment_name = currentExperimentName(),
        db_path = sqlite_db_path,
        plan_id = autoPersistPlanId(),
        metadata = buildPersistenceMeta()
      )
      autoPersistLastPlanSig(plan_sig)
      latest_plan_id(plan_result$plan_id)
    }, error = function(e) {
      showNotification(paste0("SQLite 保存规划失败: ", e$message), type = "error")
    })
  }, ignoreInit = TRUE)

  observeEvent(plantedDataDebounced(), {
    req(latest_plan_id())
    tryCatch({
      planted_mat <- plantedDataDebounced()
      assign_sig <- paste0(latest_plan_id(), "_", matrixSignature(planted_mat, ncol(planted_mat) - STAT_COL_COUNT))
      if (identical(assign_sig, autoPersistLastAssignSig())) return(invisible(NULL))
      saveAssignmentsToSqlite(plan_id = latest_plan_id(), planted_matrix = planted_mat, plan_matrix = selectedPlantBaseMatrix(),
                               experiment_name = currentExperimentName(), db_path = sqlite_db_path)
      autoPersistLastAssignSig(assign_sig)
    }, error = function(e) {
      showNotification(paste0("SQLite 保存种植分配失败: ", e$message), type = "error")
    })
  }, ignoreInit = TRUE)

  observe({
    experimentsTrigger()
    db_latest_plan_id <- getLatestPlanId(sqlite_db_path)
    if (!is.na(db_latest_plan_id)) latest_plan_id(db_latest_plan_id)
  })

  # ===========================================================================
  # SQLite 数据表 reactives（使用细粒度触发器）
  # ===========================================================================
  sqliteExperiments <- reactive({
    experimentsTrigger()
    experiments <- readTableFromSqlite("experiments", sqlite_db_path)
    stripExperimentTsCols <- function(df) {
      if (!is.data.frame(df)) return(df)
      drop_ts <- intersect(c("created_at", "updated_at"), names(df))
      if (length(drop_ts) > 0) df[, setdiff(names(df), drop_ts), drop = FALSE] else df
    }
    if (!is.data.frame(experiments) || nrow(experiments) == 0) return(stripExperimentTsCols(experiments))
    filter_id <- trimws(input$sqliteFilterExperimentId)
    if (nzchar(filter_id) && "experiment_id" %in% names(experiments)) {
      experiments <- experiments[as.character(experiments$experiment_id) == filter_id, , drop = FALSE]
    }
    # 追加状态列和操作列（HTML，escape=FALSE 时渲染）
    n <- nrow(experiments)
    if (n > 0) {
      exp_ids <- as.character(experiments$experiment_id)
      # 状态列：已种（绿）+ 地块名，或未种（灰）
      status_col <- sapply(exp_ids, function(eid) {
        planted <- getPlantedTablesForExperiment(eid, sqlite_db_path)
        if (length(planted) > 0) {
          tables_str <- paste(planted, collapse = ", ")
          sprintf('<span style="background:#ecfdf5;border:1px solid #86efac;color:#166534;padding:2px 8px;border-radius:12px;font-size:12px;font-weight:600;">已种</span><br><span style="font-size:11px;color:#6b7280;">%s</span>', tables_str)
        } else {
          '<span style="background:#f3f4f6;border:1px solid #d1d5db;color:#6b7280;padding:2px 8px;border-radius:12px;font-size:12px;font-weight:600;">未种</span>'
        }
      })
      # 操作列
      btn_col <- sapply(seq_len(n), function(i) {
        sprintf('<button class="btn btn-danger btn-xs" onclick="Shiny.setInputValue(\'delete_exp_row\', {id:\'%s\',i:%d}, {priority:\'event\'})">删除</button>', exp_ids[i], i)
      })
      experiments$状态 <- status_col
      experiments$操作 <- btn_col
    }
    stripExperimentTsCols(experiments)
  })

  sqliteExperimentRecords <- reactive({
    recordsTrigger()
    records <- readTableFromSqlite("experiment_records", sqlite_db_path)
    if (!is.data.frame(records) || nrow(records) == 0) return(records)
    filter_id <- trimws(input$sqliteFilterExperimentId)
    if (nzchar(filter_id) && "experiment_id" %in% names(records)) {
      records <- records[as.character(records$experiment_id) == filter_id, , drop = FALSE]
    }
    records
  })

  # ===========================================================================
  # 下载处理器
  # ===========================================================================
  output$simpleDownloadData <- downloadHandler(filename = function() { "simpleDesignplot.xlsx" },
                                               content = function(file) { writeFormattedXlsx(datasetSelected(), file) })
  output$plantPreviewDownloadData <- downloadHandler(
    filename = function() { "plantPreviewDesignplot.xlsx" },
    content = function(file) {
      preview_data <- currentPlantPreviewMatrix()
      preview_numeric <- !any(grepl("\\|", as.character(preview_data[, 1:max(1, ncol(preview_data) - STAT_COL_COUNT), drop = FALSE])))
      if (isTRUE(preview_numeric)) {
        writeFormattedXlsx(preview_data, file, highlight_positive = TRUE)
      } else {
        writeFormattedXlsx(preview_data, file, ref_data = selectedPlantBaseMatrix(), color_planted = FALSE)
      }
    }
  )

  # ===========================================================================
  # 输出渲染
  # ===========================================================================
  output$selected_var <- renderText({
    current_data <- datasetInput()
    if (!("总长" %in% colnames(current_data))) validate(need(FALSE, "结果数据缺少「总长」列，请检查统计列配置"))
    planed <- current_data[1, "总长"]
    paste("已规划总长", planed, "米。", sep = "")
  })
  output$simpleMydata <- DT::renderDataTable(
    DT::datatable({ datasetSelected() }, options = list(pageLength = 50, lengthMenu = DT_PAGE_MENU)) %>%
      formatStyle(1:(ncol(datasetSelected()) - STAT_COL_COUNT), backgroundColor = styleInterval(COLOR_BREAKS, COLOR_VALUES))
  )
  output$sta <- DT::renderDataTable(DT::datatable({ datasetStats() }, options = list(pageLength = 5, lengthMenu = DT_PAGE_MENU)))
  contents <- reactive({ req(input$file1); read.xlsx(input$file1$datapath, 1) })
  output$contents <- DT::renderDataTable(DT::datatable({ contents() }, options = list(pageLength = 5, lengthMenu = c(5, 10, 100, 1000, 10000))))
  output$mydf <- DT::renderDataTable(DT::datatable({ currentSowData() }, options = list(pageLength = 5, lengthMenu = c(5, 15, 30, 100, 1000))))
  output$selectedPlantPlotPreview <- DT::renderDataTable({
    preview_mat <- currentPlantPreviewMatrix()
    validate(need(!is.null(preview_mat) && is.matrix(preview_mat) && ncol(preview_mat) > 0, "暂无可预览的种植地块"))
    DT::datatable(preview_mat, options = list(pageLength = 100, lengthMenu = c(20, 50, 100, 200), scrollX = TRUE, autoWidth = TRUE, dom = "lfrtip"), class = "compact stripe hover") %>%
      formatStyle(columns = 1:ncol(preview_mat), `text-align` = "center") %>%
      formatStyle(1:(ncol(preview_mat) - STAT_COL_COUNT), backgroundColor = styleInterval(COLOR_BREAKS, COLOR_VALUES))
  })
  output$experimentSeedRecords <- DT::renderDataTable(DT::datatable(experimentSeedData(), options = list(pageLength = 20, scrollX = TRUE)))
  sqliteExperimentsForExport <- reactive({
    experimentsTrigger()
    experiments <- readTableFromSqlite("experiments", sqlite_db_path)
    if (!is.data.frame(experiments) || nrow(experiments) == 0) return(experiments)
    filter_id <- trimws(input$sqliteFilterExperimentId)
    if (nzchar(filter_id) && "experiment_id" %in% names(experiments)) {
      experiments <- experiments[as.character(experiments$experiment_id) == filter_id, , drop = FALSE]
    }
    experiments
  })

  output$sqliteExperiments <- DT::renderDataTable(
    DT::datatable(sqliteExperiments(), options = list(pageLength = 20, scrollX = TRUE), escape = FALSE)
  )
  output$sqliteExperimentRecords <- DT::renderDataTable(DT::datatable(sqliteExperimentRecords(), options = list(pageLength = 10, scrollX = TRUE)))
  output$downloadExperimentsCsv <- downloadHandler(filename = function() { "experiments.csv" },
                                                    content = function(file) { write.csv(sqliteExperimentsForExport(), file, row.names = FALSE, fileEncoding = "UTF-8") })
  output$downloadExperimentRecordsCsv <- downloadHandler(filename = function() { "experiment_records.csv" },
                                                        content = function(file) { write.csv(sqliteExperimentRecords(), file, row.names = FALSE, fileEncoding = "UTF-8") })
  output$downloadSowListXlsx <- downloadHandler(
    filename = function() {
      st <- selectedSowTableName()
      selected_table <- if (!is.na(st)) trimws(as.character(st)) else ""
      base_name <- if (nzchar(selected_table)) sub("\\.sow$", "", selected_table) else "sow_list"
      paste0(base_name, "_sow_list.xlsx")
    },
    content = function(file) {
      sow_df <- currentSowData()
      writeFormattedXlsx(sow_df, file)
    }
  )

  # ===========================================================================
  # 田间布局图
  # ===========================================================================
  fieldLayoutErrorState <- reactiveVal(NULL)

  getFullTableName <- function(name_or_table) {
    value <- trimws(name_or_table)
    if (!nzchar(value)) return("")
    tables_df <- listPlantTables(sqlite_db_path)
    if (!is.data.frame(tables_df) || nrow(tables_df) == 0) return("")
    table_names <- as.character(tables_df$table_name)
    field_names <- as.character(tables_df$field_name)
    if (value %in% table_names) return(value)
    idx <- match(value, field_names)
    if (!is.na(idx)) return(table_names[idx])
    if (grepl("\\.plant$", value)) return(value)
    paste0(value, ".plant")
  }

  fieldLayoutCacheKey <- reactive({
    plantTableTrigger()
    sowTrigger()
    sn <- selectedLayoutFieldName()
    simplified_name <- if (!is.na(sn)) trimws(as.character(sn)) else ""
    if (!nzchar(simplified_name)) return("empty")
    selected_table <- getFullTableName(simplified_name)
    paste0(selected_table, "|", plantTableTrigger(), "|", sowTrigger())
  })

  # ---- 田间布局数据（纯函数，无副作用）----
  fieldLayoutData <- reactive({
    cache_key <- fieldLayoutCacheKey()
    cached <- fieldLayoutCache()
    if (is.list(cached) && !is.na(cached$key) && identical(cached$key, cache_key) && is.data.frame(cached$data)) {
      return(cached$data)
    }

    sn <- selectedLayoutFieldName()
    simplified_name <- if (!is.na(sn)) trimws(as.character(sn)) else ""
    validate(need(nzchar(simplified_name), "请先选择地块"))

    tryCatch({
      selected_table <- getFullTableName(simplified_name)
      plant_matrix <- readPlantTable(selected_table, sqlite_db_path)
      validate(need(is.matrix(plant_matrix) && nrow(plant_matrix) > 0, "该地块暂无种植数据"))

      data_cols <- ncol(plant_matrix) - STAT_COL_COUNT
      if (data_cols <= 0) return(emptyLayoutDf())
      plant_data <- plant_matrix[, 1:data_cols, drop = FALSE]
      rowno_idx <- match("排数", colnames(plant_matrix))
      rowno_vec <- if (!is.na(rowno_idx)) suppressWarnings(as.numeric(plant_matrix[, rowno_idx])) else seq_len(nrow(plant_matrix))

      row_id_from_idx <- function(idx_vec) {
        ids <- unique(rowno_vec[idx_vec])
        ids <- ids[!is.na(ids)]
        if (length(ids) == 0) return(numeric(0))
        sort(as.numeric(ids))
      }

      plant_data_num <- matrix(suppressWarnings(as.numeric(plant_data)), nrow = nrow(plant_data), ncol = data_cols)

      con <- connectDesignplotDb(sqlite_db_path)
      on.exit(DBI::dbDisconnect(con), add = TRUE)

      plant_runs <- DBI::dbGetQuery(con, "SELECT DISTINCT experiment_id FROM experiment_plant_runs WHERE plant_table_name = ?", params = list(selected_table))
      if (!is.data.frame(plant_runs) || nrow(plant_runs) == 0) {
        plant_runs <- DBI::dbGetQuery(con, "SELECT DISTINCT experiment_id FROM experiment_records")
      }

      special_areas <- list(
        list(code = -77,  name = "水沟",         color = "#20B2AA"),
        list(code = -111, name = "纵向观察道",    color = "#FF8C00"),
        list(code = -9,   name = "纵向保护行",    color = "#008000"),
        list(code = -99,  name = "横向保护行",    color = "#2E8B57"),
        list(code = -8,   name = "不能种植",      color = "#FF0000")
      )

      layout_info <- list()

      # 识别实验区域
      exp_records <- DBI::dbGetQuery(con, "SELECT experiment_id, stageid FROM experiment_records")
      exp_names_df <- DBI::dbGetQuery(con, "SELECT experiment_id, experiment_name FROM experiments")
      stage_set <- character(0)

      if (is.data.frame(exp_records) && nrow(exp_records) > 0) {
        valid_exp_ids <- if (is.data.frame(plant_runs) && nrow(plant_runs) > 0) as.character(plant_runs$experiment_id) else character()
        if (length(valid_exp_ids) > 0) {
          exp_records <- exp_records[as.character(exp_records$experiment_id) %in% valid_exp_ids, , drop = FALSE]
        }

        stage_to_exp <- data.frame(
          stageid = trimws(as.character(exp_records$stageid)),
          experiment_id = as.character(exp_records$experiment_id),
          stringsAsFactors = FALSE
        )
        stage_to_exp <- stage_to_exp[!is.na(stage_to_exp$stageid) & nzchar(stage_to_exp$stageid), , drop = FALSE]
        stage_set <- unique(as.character(stage_to_exp$stageid))

        if (nrow(stage_to_exp) > 0) {
          exp_name_map <- stats::setNames(as.character(exp_names_df$experiment_name), as.character(exp_names_df$experiment_id))
          exp_cell_map <- vector("list", 0)

          for (r in seq_len(nrow(plant_data))) {
            for (c in seq_len(ncol(plant_data))) {
              parsed <- tryCatch(parseAssignmentCell(plant_data[r, c]), error = function(e) NULL)
              if (is.null(parsed)) next
              stage_name <- trimws(as.character(parsed$material_name))
              hit <- stage_to_exp$experiment_id[stage_to_exp$stageid == stage_name]
              if (length(hit) == 0) next
              exp_id <- as.character(hit[1])
              exp_name_val <- as.character(exp_name_map[exp_id])
              key <- if (!is.na(exp_name_val) && nzchar(trimws(exp_name_val))) exp_name_val else exp_id
              if (is.null(exp_cell_map[[key]])) exp_cell_map[[key]] <- matrix(FALSE, nrow = nrow(plant_data), ncol = ncol(plant_data))
              exp_cell_map[[key]][r, c] <- TRUE
            }
          }

          for (exp_name in names(exp_cell_map)) {
            is_exp_material <- exp_cell_map[[exp_name]]
            if (!any(is_exp_material, na.rm = TRUE)) next
            boxes <- splitConnectedBoxes(is_exp_material, rowno_vec)
            for (b in boxes) {
              layout_info[[length(layout_info) + 1]] <- data.frame(
                type = "实验", name = exp_name,
                start_row = b$start_row, end_row = b$end_row,
                start_col = b$start_col, end_col = b$end_col,
                rows = b$end_row - b$start_row + 1,
                cols = b$end_col - b$start_col + 1,
                color = NA, stringsAsFactors = FALSE
              )
            }
          }
        }
      }

      # 识别补种材料
      supplement_cell_map <- vector("list", 0)
      for (r in seq_len(nrow(plant_data))) {
        for (c in seq_len(ncol(plant_data))) {
          parsed <- tryCatch(parseAssignmentCell(plant_data[r, c]), error = function(e) NULL)
          if (is.null(parsed)) next
          mat_name <- trimws(as.character(parsed$material_name))
          if (!nzchar(mat_name)) next
          if (mat_name %in% stage_set) next
          key <- paste0("补种:", mat_name)
          if (is.null(supplement_cell_map[[key]])) supplement_cell_map[[key]] <- matrix(FALSE, nrow = nrow(plant_data), ncol = ncol(plant_data))
          supplement_cell_map[[key]][r, c] <- TRUE
        }
      }

      for (sup_name in names(supplement_cell_map)) {
        is_sup <- supplement_cell_map[[sup_name]]
        if (!any(is_sup, na.rm = TRUE)) next
        boxes <- splitConnectedBoxes(is_sup, rowno_vec)
        for (b in boxes) {
          layout_info[[length(layout_info) + 1]] <- data.frame(
            type = "补种", name = sup_name,
            start_row = b$start_row, end_row = b$end_row,
            start_col = b$start_col, end_col = b$end_col,
            rows = b$end_row - b$start_row + 1,
            cols = b$end_col - b$start_col + 1,
            color = "#111827", stringsAsFactors = FALSE
          )
        }
      }

      # 识别特殊区域
      for (sa in special_areas) {
        is_special <- plant_data_num == sa$code
        if (any(is_special, na.rm = TRUE)) {
          rows_idx <- which(rowSums(is_special, na.rm = TRUE) > 0)
          cols_idx <- which(colSums(is_special, na.rm = TRUE) > 0)
          if (length(rows_idx) > 0 && length(cols_idx) > 0) {
            row_ids <- row_id_from_idx(rows_idx)
            if (length(row_ids) == 0) next
            layout_info[[length(layout_info) + 1]] <- data.frame(
              type = "特殊区域", name = sa$name,
              start_row = min(row_ids), end_row = max(row_ids),
              start_col = min(cols_idx), end_col = max(cols_idx),
              rows = length(row_ids), cols = length(cols_idx),
              color = sa$color, stringsAsFactors = FALSE
            )
          }
        }
      }

      if (length(layout_info) > 0) {
        result <- do.call(rbind, layout_info)
        exp_names <- unique(as.character(result$name[result$type == "实验"]))
        if (length(exp_names) > 0) {
          exp_colors <- scales::hue_pal(l = 55, c = 150)(length(exp_names))
          exp_color_map <- stats::setNames(exp_colors, exp_names)
          exp_idx <- which(result$type == "实验")
          result$color[exp_idx] <- exp_color_map[as.character(result$name[exp_idx])]
        }
        fieldLayoutErrorState(NULL)
        # 缓存写入移至 observe（见下方）
        return(result)
      } else {
        fieldLayoutErrorState(NULL)
        return(emptyLayoutDf())
      }
    }, error = function(e) {
      fieldLayoutErrorState(paste0("田间布局图数据解析失败：", e$message))
      return(emptyLayoutDf())
    })
  })

  # ---- 空布局 DataFrame 工厂 ----
  emptyLayoutDf <- function() {
    data.frame(
      type = character(), name = character(),
      start_row = numeric(), end_row = numeric(),
      start_col = numeric(), end_col = numeric(),
      rows = numeric(), cols = numeric(),
      color = character(), stringsAsFactors = FALSE
    )
  }

  # ---- observe：缓存写入（副作用分离）----
  observe({
    cache_key <- fieldLayoutCacheKey()
    cached <- fieldLayoutCache()
    if (is.list(cached) && !is.na(cached$key) && identical(cached$key, cache_key)) return()
    data <- fieldLayoutData()
    fieldLayoutCache(list(key = cache_key, data = data))
  })

  # ---- 田间布局图错误展示 ----
  output$fieldLayoutError <- renderUI({
    msg <- fieldLayoutErrorState()
    if (is.null(msg) || !nzchar(trimws(msg))) return(NULL)
    tags$div(
      style = "background:#fef2f2;border:1px solid #fca5a5;border-radius:8px;padding:8px 10px;margin:8px 0;color:#991b1b;",
      paste0("⛔ ", msg)
    )
  })

  # ---- 田间布局图绘制（使用共享函数）----
  output$fieldLayoutPlot <- renderPlot({
    fieldLayoutErrorState(NULL)
    layout_df <- fieldLayoutData()
    validate(need(nrow(layout_df) > 0, "暂无已种植的实验数据"))

    sn <- selectedLayoutFieldName()
    simplified_name <- if (!is.na(sn)) trimws(as.character(sn)) else ""
    selected_table <- getFullTableName(simplified_name)
    plant_matrix <- readPlantTable(selected_table, sqlite_db_path)
    metrics <- computeLayoutPlotMetrics(plant_matrix)

    p <- buildFieldLayoutGgplot(layout_df, plant_matrix, simplified_name, metrics)
    print(p)
  })

  # ---- 田间布局图下载（使用共享函数）----
  output$downloadFieldLayoutPlot <- downloadHandler(
    filename = function() {
      sn <- selectedLayoutFieldName()
      selected <- if (!is.na(sn)) trimws(as.character(sn)) else ""
      base_name <- if (nzchar(selected)) selected else "field_layout"
      paste0(base_name, "_layout.png")
    },
    content = function(file) {
      layout_df <- fieldLayoutData()
      validate(need(nrow(layout_df) > 0, "暂无可导出的布局图数据"))

      sn <- selectedLayoutFieldName()
      simplified_name <- if (!is.na(sn)) trimws(as.character(sn)) else ""
      selected_table <- getFullTableName(simplified_name)
      plant_matrix <- readPlantTable(selected_table, sqlite_db_path)
      metrics <- computeLayoutPlotMetrics(plant_matrix)

      p <- buildFieldLayoutGgplot(layout_df, plant_matrix, simplified_name, metrics)
      ggplot2::ggsave(filename = file, plot = p, width = 14, height = 9, dpi = 150)
    }
  )
}
