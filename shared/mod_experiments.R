# =============================================================================
# 模块: 试验管理
# 功能: 两级面板UI - 左侧试验列表 + 右侧田间记录明细
# 数据源: population_field_records, line_selection_field_records, yield_test_field_records
# =============================================================================

experiments_ui <- function(id) {
  ns <- NS(id)

  tagList(
    div(class = "tab-panel",
      h3(class = "panel-title",
        span(class = "icon", icon("flask")),
        "田间记录管理"
      ),
      p("选择左侧试验查看田间记录明细，支持按类型筛选。", class = "text-muted fb-panel-intro"),

      fluidRow(
        # ========== 左侧: 试验列表 ==========
        column(4,
          div(class = "sidebar-panel",
            h5(icon("list"), " 试验列表"),

            # 年份筛选
            selectInput(ns("filter_year"), "年份",
              choices = c("全部" = ""),
              selected = as.character(Sys.Date()[[1]]), width = "100%"
            ),

            # 类型筛选
            selectInput(ns("filter_type"), "记录类型",
              choices = c("全部" = "", "群体" = "population", "株行" = "line_selection", "产比" = "yield_test"),
              selected = "", width = "100%"
            ),

            # 地点筛选
            selectInput(ns("filter_location"), "地点",
              choices = c("全部" = ""),
              selected = "安徽宿州", width = "100%"
            ),

            # 隐藏的点击处理器
            textInput(ns("experiment_list_click"), "", value = ""),
            tags$script(HTML(sprintf("
              $(document).on('click', '.exp-item', function() {
                var expId = $(this).attr('data-exp-id');
                if (expId && window.Shiny) {
                  Shiny.setInputValue('%s', expId, {priority: 'event'});
                }
              });
            ", ns("experiment_list_click")))),

            # 搜索
            textInput(ns("filter_search"), "搜索",
              placeholder = "输入试验名称或ID...", width = "100%"
            ),

            # 统计信息
            div(class = "stats-row",
              span(class = "stat-badge", textOutput(ns("exp_count"))),
              span(class = "stat-text", "个试验")
            ),

            # 刷新和重置按钮
            div(class = "button-group mt-3",
              actionButton(ns("btn_reset"), "重置", icon = icon("refresh"), class = "btn-outline-secondary btn-sm w-50"),
              actionButton(ns("btn_refresh"), "刷新", icon = icon("sync"), class = "btn-primary btn-sm w-50")
            ),

            # 导入到种植试验按钮
            div(class = "button-group mt-3",
              actionButton(ns("btn_import_to_designplot"), "导入田间种植", icon = icon("upload"), class = "btn-warning btn-sm w-50"),
              actionButton(ns("btn_import_all_to_designplot"), "导入全部", icon = icon("upload"), class = "btn-warning btn-sm w-50")
            ),
            p("将已生成的田试记录导入到种植试验模块", class = "text-muted", style = "font-size: 11px; margin-top: 4px;")
          ),

          # 试验列表（按类型分组显示）
          div(class = "exp-list-container",
            uiOutput(ns("experiment_list_ui"))
          )
        ),

        # ========== 右侧: 田间记录明细 ==========
        column(8,
          div(class = "card",
            div(class = "card-header d-flex justify-content-between align-items-center",
              div(
                icon("file-alt"), " 田间记录明细 ",
                span(class = "badge bg-info ms-2", textOutput(ns("record_count")))
              ),
              actionButton(ns("btn_delete_exp"), "删除", icon = icon("trash"),
                          class = "btn btn-sm btn-danger", style = "display:none;")
            ),
            DT::dataTableOutput(ns("field_records_table"))
          ),

          # 试验信息摘要
          div(class = "card mt-3",
            div(class = "card-header",
              icon("info-circle"), " 试验摘要"
            ),
            uiOutput(ns("exp_summary_ui"))
          )
        )
      )
    )
  )
}

experiments_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    normalize_field_records <- function(records) {
      if (is.null(records) || nrow(records) == 0) {
        return(records)
      }

      if (!"stage" %in% names(records) && "stageid" %in% names(records)) {
        records$stage <- records$stageid
      }

      if (!"place" %in% names(records)) {
        records$place <- NA_character_
      }

      records
    }

    # --- 反应式数据 ---
    db_path <- reactive({ defaultDbPath() })
    refresh_version <- reactiveVal(0)

    # 类型映射
    type_map <- c(
      "population" = "群体",
      "line_selection" = "株行",
      "yield_test" = "产比"
    )

    source_table_map <- c(
      "population" = "population_field_records",
      "line_selection" = "line_selection_field_records",
      "yield_test" = "yield_test_field_records"
    )

    record_table_map <- c(
      "population" = "population_records",
      "line_selection" = "line_selection_records",
      "yield_test" = "yield_test_records"
    )

    extract_year <- function(x) {
      if (is.null(x)) {
        return(rep(NA_character_, 0))
      }

      x <- as.character(x)
      year <- substr(x, 1, 4)
      year[!grepl("^[0-9]{4}$", year)] <- NA_character_
      year
    }

    split_place_values <- function(x) {
      if (is.null(x) || length(x) == 0) {
        return(character(0))
      }

      values <- unlist(strsplit(as.character(x), ",", fixed = TRUE), use.names = FALSE)
      values <- trimws(values)
      unique(values[nzchar(values)])
    }

    place_matches_filter <- function(place_value, location_filter) {
      if (is.null(location_filter) || !nzchar(trimws(location_filter))) {
        return(TRUE)
      }

      location_filter <- trimws(location_filter)
      if (is.null(place_value) || length(place_value) == 0 || is.na(place_value)) {
        return(FALSE)
      }

      location_filter %in% split_place_values(place_value)
    }

    summarize_import_overwrite <- function(experiment_ids, location_filter = NULL) {
      exp_ids <- trimws(as.character(experiment_ids))
      exp_ids <- exp_ids[nzchar(exp_ids)]
      if (length(exp_ids) == 0) {
        return(list(items = list(), overwrite_count = 0L, planted_count = 0L))
      }

      items <- lapply(exp_ids, function(exp_id) {
        dp_id <- paste0("DP_", exp_id)
        existed <- tryCatch(designplotExperimentExists(dp_id, db_path()), error = function(e) FALSE)
        plant_runs <- if (existed) {
          tryCatch(getExperimentPlantRuns(dp_id, db_path()), error = function(e) data.frame())
        } else {
          data.frame()
        }
        if (!is.data.frame(plant_runs)) plant_runs <- data.frame()
        planted_tables <- if (nrow(plant_runs) > 0 && "plant_table_name" %in% names(plant_runs)) {
          unique(trimws(as.character(plant_runs$plant_table_name)))
        } else {
          character(0)
        }
        planted_tables <- planted_tables[nzchar(planted_tables)]

        list(
          source_experiment_id = exp_id,
          designplot_experiment_id = dp_id,
          exists = existed,
          planted = length(planted_tables) > 0,
          planted_tables = planted_tables,
          location_filter = location_filter %||% ""
        )
      })

      overwrite_count <- sum(vapply(items, function(item) isTRUE(item$exists), logical(1)))
      planted_count <- sum(vapply(items, function(item) isTRUE(item$planted), logical(1)))
      list(items = items, overwrite_count = overwrite_count, planted_count = planted_count)
    }

    build_import_warning_ui <- function(summary_info, batch = FALSE) {
      items <- summary_info$items %||% list()
      overwrite_items <- Filter(function(item) isTRUE(item$exists), items)
      planted_items <- Filter(function(item) isTRUE(item$planted), overwrite_items)
      location_values <- unique(trimws(vapply(items, function(item) item$location_filter %||% "", character(1))))
      location_values <- location_values[nzchar(location_values)]

      tagList(
        tags$p(
          if (batch) {
            "检测到本次导入包含已存在的种植试验记录；继续后会覆盖原试验。"
          } else {
            "检测到当前试验已存在于种植试验中；继续后会覆盖原试验。"
          },
          style = "margin-bottom:8px;font-weight:600;color:#92400e;"
        ),
        if (length(location_values) > 0) {
          tags$p(sprintf("当前按地点过滤：%s", paste(location_values, collapse = ", ")),
                 style = "margin-bottom:8px;color:#6b7280;")
        },
        if (length(overwrite_items) > 0) {
          tags$div(
            tags$strong(sprintf("将覆盖 %d 个试验：", length(overwrite_items))),
            tags$ul(
              lapply(overwrite_items, function(item) {
                tags$li(
                  sprintf("%s → %s", item$source_experiment_id, item$designplot_experiment_id),
                  if (isTRUE(item$planted)) {
                    tags$span(
                      sprintf("（已种植地块：%s）", paste(item$planted_tables, collapse = ", ")),
                      style = "color:#b91c1c;"
                    )
                  }
                )
              })
            )
          )
        },
        if (length(planted_items) > 0) {
          tags$p(
            "这些试验原来已经种过。覆盖导入后会自动重置为“未种植”，并清掉相关种植运行记录；如需继续使用，必须重新生成所种地块。",
            style = "margin:8px 0 0 0;color:#b91c1c;font-weight:600;line-height:1.6;"
          )
        } else {
          tags$p(
            "继续后会覆盖原试验记录。若后续需要种植，请重新执行种植生成。",
            style = "margin:8px 0 0 0;color:#6b7280;line-height:1.6;"
          )
        }
      )
    }

    perform_single_import <- function(exp, location_filter = NULL) {
      result <- importExperimentToDesignplot(
        source_experiment_id = exp$experiment_id,
        source_type = exp$experiment_type,
        location_filter = location_filter,
        db_path = db_path()
      )

      if (isTRUE(result$success)) {
        showNotification(result$message, type = "message", duration = 8)
        session$sendCustomMessage('refresh_designplot_experiments', list())
      } else {
        showNotification(result$message, type = "error")
      }
      invisible(result)
    }

    perform_batch_import <- function(location_filter = NULL) {
      result <- importAllExperimentsToDesignplot(location_filter = location_filter, db_path = db_path())
      showNotification(result$message, type = "message", duration = 8)
      session$sendCustomMessage('refresh_designplot_experiments', list())
      invisible(result)
    }

    load_experiments <- reactive({
      req(refresh_version())
      con <- connectDb(db_path())
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      initDb(con)

      type_val <- input$filter_type

      # 确定要查询哪些类型
      types <- if (is.null(type_val) || nzchar(type_val) == 0) {
        c("population", "line_selection", "yield_test")
      } else {
        type_val
      }

      all_exps <- list()

      for (t in types) {
        rec_tbl <- record_table_map[t]
        field_tbl <- source_table_map[t]

        q <- sprintf(
          "SELECT r.experiment_id, r.experiment_name, r.total_rows, r.has_generated,
                  r.generated_at, r.created_at, r.experiment_id as fieldid,
                  '%s' as experiment_type,
                  (SELECT GROUP_CONCAT(DISTINCT TRIM(f.place))
                   FROM %s f
                   WHERE f.experiment_id = r.experiment_id
                     AND f.place IS NOT NULL
                     AND TRIM(f.place) != '') as place
           FROM %s r
           WHERE r.has_generated = 1",
          t, field_tbl, rec_tbl
        )
        exps <- DBI::dbGetQuery(con, q)

        if (nrow(exps) > 0) {
          exps$year <- extract_year(dplyr::coalesce(exps$generated_at, exps$created_at))

          for (i in seq_len(nrow(exps))) {
            exp_id <- exps$experiment_id[i]
            field_data <- DBI::dbGetQuery(con,
              sprintf("SELECT fieldid FROM %s WHERE experiment_id = ? LIMIT 1", field_tbl),
              params = list(exp_id)
            )
            if (nrow(field_data) > 0) {
              exps$fieldid[i] <- field_data$fieldid[1]
            }
          }
          all_exps[[t]] <- exps
        }
      }

      if (length(all_exps) == 0) {
        return(data.frame(
          experiment_id = character(),
          experiment_name = character(),
          total_rows = numeric(),
          experiment_type = character(),
          fieldid = character(),
          generated_at = character(),
          created_at = character(),
          year = character(),
          place = character(),
          stringsAsFactors = FALSE
        ))
      }

      do.call(rbind, all_exps)
    })

    observeEvent(load_experiments(), {
      all_years <- sort(unique(stats::na.omit(load_experiments()$year)), decreasing = TRUE)
      selected_year <- isolate(input$filter_year)
      selected_year <- if (!is.null(selected_year) && selected_year %in% all_years) selected_year else ""

      updateSelectInput(
        session,
        "filter_year",
        choices = c("全部" = "", stats::setNames(all_years, all_years)),
        selected = selected_year
      )

      # 更新地点下拉框
      all_locs <- sort(unique(split_place_values(load_experiments()$place)))
      selected_loc <- isolate(input$filter_location)
      selected_loc <- if (!is.null(selected_loc) && selected_loc %in% all_locs) selected_loc else ""

      updateSelectInput(
        session,
        "filter_location",
        choices = c("全部" = "", stats::setNames(all_locs, all_locs)),
        selected = selected_loc
      )
    }, ignoreNULL = FALSE)

    # --- 获取试验列表（带分组） ---
    experiments_list <- reactive({
      combined <- load_experiments()
      year_val <- input$filter_year
      search_val <- input$filter_search
      loc_val <- input$filter_location

      if (!is.null(year_val) && nzchar(year_val)) {
        combined <- combined[combined$year == year_val, , drop = FALSE]
      }

      if (!is.null(loc_val) && nzchar(loc_val)) {
        matches <- vapply(combined$place, place_matches_filter, logical(1), location_filter = loc_val)
        combined <- combined[matches, , drop = FALSE]
      }

      # 搜索筛选
      if (!is.null(search_val) && nzchar(search_val)) {
        pattern <- tolower(search_val)
        combined <- combined[
          grepl(pattern, tolower(combined$experiment_id)) |
          grepl(pattern, tolower(combined$experiment_name)),
        ]
      }

      combined
    })

    # --- 当前选中的试验 ---
    selected_experiment <- reactiveVal(NULL)

    # --- 输出: 试验计数 ---
    output$exp_count <- renderText({
      nrow(experiments_list())
    })

    # --- 输出: 记录计数 ---
    output$record_count <- renderText({
      exp <- selected_experiment()
      if (is.null(exp)) return("0")
      nrow(exp$field_records %||% data.frame())
    })

    # --- 输出: 试验列表UI（分组） ---
    output$experiment_list_ui <- renderUI({
      exps <- experiments_list()

      if (nrow(exps) == 0) {
        return(div(class = "empty-message",
          icon("inbox"), " 暂无试验记录"
        ))
      }

      # 按类型分组
      types_order <- c("population", "line_selection", "yield_test")
      type_names <- type_map[types_order]

      group_blocks <- lapply(types_order, function(t) {
        group_exps <- exps[exps$experiment_type == t, ]
        if (nrow(group_exps) == 0) return(NULL)

        type_name <- type_map[t]

        exp_items <- lapply(1:nrow(group_exps), function(i) {
          row <- group_exps[i, ]
          is_selected <- !is.null(selected_experiment()) &&
                         selected_experiment()$experiment_id == row$experiment_id
          place_values <- split_place_values(row$place)

          btn_class <- if (is_selected) "exp-item selected" else "exp-item"

          div(class = btn_class, `data-exp-id` = row$experiment_id,
            div(class = "exp-item-header",
              span(class = "exp-name", row$experiment_name),
              span(class = "exp-badge", type_name)
            ),
            div(class = "exp-item-meta",
              sprintf("ID: %s | 行数: %.0f", row$experiment_id, row$total_rows %||% 0)
            ),
            if (length(place_values) > 0) {
              div(class = "exp-item-locations",
                span(class = "exp-location-label", "地点"),
                lapply(place_values, function(place) {
                  span(class = "exp-location-tag", place)
                })
              )
            } else {
              div(class = "exp-item-locations is-empty",
                span(class = "exp-location-label", "地点"),
                span(class = "exp-location-empty", "未设置")
              )
            }
          )
        })

        tagList(
          div(class = "exp-group-header",
            icon(get_type_icon(t)), " ", type_name,
            span(class = "exp-group-count", nrow(group_exps))
          ),
          div(class = "exp-group-items", exp_items)
        )
      })

      tagList(group_blocks)
    })

    # --- 试验列表点击事件 ---
    observeEvent(input$experiment_list_click, {
      exp_id <- input$experiment_list_click
      if (is.null(exp_id) || exp_id == "") return()

      exps <- experiments_list()
      exp_row <- exps[exps$experiment_id == exp_id, ]
      if (nrow(exp_row) == 0) return()

      # 加载田间记录明细
      con <- connectDb(db_path())
      on.exit(DBI::dbDisconnect(con), add = TRUE)
      initDb(con)

      field_tbl <- source_table_map[exp_row$experiment_type]
      field_records <- DBI::dbGetQuery(con,
        sprintf("SELECT * FROM %s WHERE experiment_id = ? ORDER BY rowid", field_tbl),
        params = list(exp_id)
      )
      field_records <- normalize_field_records(field_records)

      exp_data <- as.list(exp_row[1, , drop = FALSE])
      exp_data$field_records <- field_records
      selected_experiment(exp_data)

      # 显示删除按钮
      session$sendCustomMessage("toggle_delete_btn", list(show = TRUE))
    })

    # --- 输出: 田间记录表格 ---
    output$field_records_table <- DT::renderDataTable({
      exp <- selected_experiment()
      if (is.null(exp) || is.null(exp$field_records) || nrow(exp$field_records) == 0) {
        return(NULL)
      }

      records <- exp$field_records

      # 显示全部列（不重新造轮子，直接复用三张田间记录表的完整字段）
      display_df <- records

      # 列名中文映射（保留映射，未知列直接用原名）
      col_names <- c(
        "record_id" = "记录ID",
        "experiment_id" = "试验ID",
        "experiment_name" = "试验名称",
        "fieldid" = "田试ID",
        "id" = "材料ID",
        "user" = "用户",
        "stageid" = "阶段ID",
        "name" = "材料名称",
        "ma" = "母本",
        "pa" = "父本",
        "mapa" = "母父本",
        "memo" = "备注",
        "stage" = "阶段",
        "next_stage" = "下一阶段",
        "f" = "F值",
        "sele" = "选择",
        "process" = "处理",
        "path" = "路径",
        "source" = "来源",
        "former_fieldid" = "原田试ID",
        "former_stageid" = "原阶段ID",
        "code" = "编号",
        "rp" = "重复",
        "treatment" = "处理",
        "place" = "地点",
        "rows" = "行数",
        "line_number" = "行号",
        "is_ck" = "对照",
        "XiaoQuShiShouMianJi" = "小区收获面积",
        "XiaoQuChanLiang" = "小区产量",
        "HanShuiLiang" = "含水量",
        "MuChan" = "亩产",
        "BoZhongQi" = "播种期",
        "ChuMiaoQi" = "出苗期",
        "ChuMiaoLiangFou" = "出苗率",
        "MiaoQiTianJianPingJia" = "苗期田间评价",
        "KaiHuaQi" = "开花期",
        "HuaSe" = "花色",
        "HuaQiTianJianPingJia" = "花期田间评价",
        "YeXing" = "叶形",
        "RongMaoSe" = "茸毛色",
        "ShengZhangXiXing" = "生长习性",
        "JieJiaXiXing" = "结荚习性",
        "DaoFuXing" = "倒伏性",
        "ZaoShuaiXing" = "早衰性",
        "ZhuXing" = "株型",
        "LuoYeXing" = "落叶性",
        "LieJiaXing" = "裂荚性",
        "ChengShuQi" = "成熟期",
        "HuoGanChengShu" = "活秆成熟",
        "ChengShuQiTianJianPingJia" = "成熟期田间评价",
        "ShouHuoQi" = "收获期",
        "XiaoQuShouHuoZhuShu" = "小区收获株数",
        "ShengYuQi" = "剩余器",
        "TianJianBeiZhu" = "田间备注",
        "HuaYeBingDuBing" = "花叶病毒病",
        "NiJingDianZhongFuBing" = "茎点早伏病",
        "ShuangMeiBing" = "霜霉病",
        "HuiBanBing" = "灰斑病",
        "XiJunXingBanDianBing" = "细菌性斑点病",
        "XiuBing" = "锈病",
        "GenFuBing" = "根腐病",
        "BaoNangXianChongBing" = "孢囊线虫病",
        "QiTaBingHai" = "其他病害",
        "DouGanHeiQianYing" = "豆杆黑潜蝇",
        "DouJiaMing" = "豆荚冥",
        "YaChong" = "蚜虫",
        "ShiYeXingHaiChong" = "食叶性害虫",
        "KaoZhongZhuShu" = "考中株数",
        "ZhuGao" = "株高",
        "DiJiaGao" = "底荚高",
        "FenZhiShu" = "分枝数",
        "ZhuJingJieShu" = "主茎节数",
        "JiaXing" = "荚型",
        "JiaShuSe" = "荚熟色",
        "YouXiaoJia" = "有效荚",
        "WuXiaoJia" = "无效荚",
        "DanZhuJiaShu" = "单株荚数",
        "DanZhuLiShu" = "单株粒数",
        "DanZhuLiZhong" = "单株粒重",
        "MeiJiaLiShu" = "每荚粒数",
        "LiXing" = "粒形",
        "ZhongPiSe" = "种皮色",
        "QiSe" = "脐色",
        "ZiYeSe" = "子叶色",
        "ZhongPiGuangZe" = "种皮光泽",
        "BaiLiZhong" = "百粒重",
        "WanHaoLiLv" = "完好率",
        "PoSuiLiLv" = "破碎率",
        "BingLiLv" = "病粒率",
        "ZiBanLiLv" = "紫斑率",
        "HeBanLiLv" = "褐斑率",
        "ShuangMeiLiLv" = "霜霉粒率",
        "HuiBanLiLv" = "灰斑粒率",
        "ChongShiLiLv" = "虫蚀粒率",
        "ZiLiPingJia" = "子粒评价",
        "DanBai" = "蛋白",
        "ZhiFang" = "脂肪",
        "DanZhiHe" = "蛋白合计",
        "CaoGanLinKangXing" = "草甘磷抗性",
        "ShiZhiJianCe" = "试纸检测",
        "HanJiYin" = "含揪因",
        "BoZhongPenShu" = "播种盆数",
        "BoZhongLiShu" = "播种粒数",
        "ChuMiaoShu" = "出苗数",
        "ChuMiaoLiShu" = "出苗粒数",
        "NaiYanXing" = "耐盐性",
        "NaiHanXing" = "耐旱性",
        "ShiHuaQi" = "始花期",
        "ZaJiaoHuaShu" = "杂角花数",
        "ChengHuoJiaShu" = "成活荚数",
        "ZhaJiaoliShu" = "炸角粒数",
        "ChuShuQi" = "出数期",
        "WanShuQi" = "完熟期",
        "HuiFuLv" = "恢复率",
        "SSRBuHeGeWeiDian" = "SSR不合格微点",
        "created_at" = "创建时间"
      )
      safe_names <- unname(col_names[colnames(display_df)])
      safe_names[is.na(safe_names)] <- colnames(display_df)[is.na(safe_names)]
      colnames(display_df) <- safe_names

      # 格式化时间
      if ("创建时间" %in% names(display_df)) {
        display_df$创建时间 <- format(as.POSIXct(display_df$创建时间), "%Y-%m-%d %H:%M")
      }

      DT::datatable(
        display_df,
        rownames = FALSE,
        escape = FALSE,
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 20, 50, 100),
          scrollX = TRUE,
          scrollY = TRUE,
          dom = "Bfrtip",
          order = list(list(ncol(display_df) - 1, "desc")),
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          )
        )
      )
    })

    # --- 输出: 试验摘要 ---
    output$exp_summary_ui <- renderUI({
      exp <- selected_experiment()
      if (is.null(exp)) {
        return(div(class = "empty-message",
          icon("hand-point-right"), " 请从左侧选择试验查看详情"
        ))
      }

      type_name <- type_map[exp$experiment_type]
      records <- exp$field_records %||% data.frame()

      tagList(
        fluidRow(
          column(3,
            div(class = "summary-item",
              h6("试验名称"),
              p(exp$experiment_name %||% "-")
            )
          ),
          column(3,
            div(class = "summary-item",
              h6("试验类型"),
              p(type_name)
            )
          ),
          column(3,
            div(class = "summary-item",
              h6("试验ID"),
              p(exp$experiment_id %||% "-")
            )
          ),
          column(3,
            div(class = "summary-item",
              h6("总行数"),
              p(format(nrow(records), big.mark = ","))
            )
          )
        ),
        if (nrow(records) > 0) {
          fluidRow(
            column(4,
              div(class = "summary-item",
                h6("地点"),
                p(unique(records$place) %>% na.omit() %>% paste(collapse = ", ") %||% "-")
              )
            ),
            column(4,
              div(class = "summary-item",
                h6("阶段"),
                p(unique(records$stage) %>% na.omit() %>% paste(collapse = ", ") %||% "-")
              )
            ),
            column(4,
              div(class = "summary-item",
                h6("创建时间"),
                p(format(as.POSIXct(exp$generated_at), "%Y-%m-%d %H:%M") %||% "-")
              )
            )
          )
        }
      )
    })

    # --- 删除按钮点击 ---
    observeEvent(input$btn_delete_exp, {
      exp <- selected_experiment()
      if (is.null(exp) || is.null(exp$experiment_id)) return()

      showModal(modalDialog(
        title = tagList(icon("exclamation-triangle"), "确认删除"),
        p("删除田间记录将同时删除所有关联的性状数据，此操作不可恢复。"),
        strong(sprintf("%s (%s)", exp$experiment_name, exp$experiment_id),
               class = "text-danger"),
        p("是否确定删除？", class = "text-muted"),
        easyClose = FALSE,
        footer = tagList(
          modalButton("取消"),
          actionButton(ns("btn_confirm_delete"), "确认删除",
                      icon = icon("trash"), class = "btn-danger")
        )
      ))
    })

    # --- 确认删除 ---
    observeEvent(input$btn_confirm_delete, {
      exp <- selected_experiment()
      if (!is.null(exp) && !is.null(exp$experiment_id)) {
        tryCatch({
          con <- connectDb(db_path())
          on.exit(DBI::dbDisconnect(con), add = TRUE)
          initDb(con)

          exp_id <- exp$experiment_id
          field_tbl <- source_table_map[exp$experiment_type]

          # 删除田间记录
          DBI::dbExecute(con,
            sprintf("DELETE FROM %s WHERE experiment_id = ?", field_tbl),
            params = list(exp_id)
          )

          # 更新记录状态
          rec_tbl <- record_table_map[exp$experiment_type]
          DBI::dbExecute(con,
            sprintf("UPDATE %s SET has_generated = 0 WHERE experiment_id = ?", rec_tbl),
            params = list(exp_id)
          )

          # 删除关联性状数据
          DBI::dbExecute(con,
            "DELETE FROM traits_survey WHERE experiment_id = ?",
            params = list(exp_id)
          )

          showNotification("删除成功", type = "message")
          selected_experiment(NULL)
          session$sendCustomMessage("toggle_delete_btn", list(show = FALSE))
          refresh_version(refresh_version() + 1)

        }, error = function(e) {
          showNotification(paste("删除失败:", e$message), type = "error")
        })
      }
      removeModal()
    })

    # --- 刷新 ---
    observeEvent(input$btn_refresh, {
      refresh_version(refresh_version() + 1)
      showNotification("已刷新", type = "message", duration = 1)
    })

    # --- 重置 ---
    observeEvent(input$btn_reset, {
      updateSelectInput(session, "filter_year", selected = as.character(Sys.Date()[[1]]))
      updateSelectInput(session, "filter_type", selected = "")
      updateSelectInput(session, "filter_location", selected = "安徽宿州")
      updateTextInput(session, "filter_search", value = "")
      selected_experiment(NULL)
      session$sendCustomMessage("toggle_delete_btn", list(show = FALSE))
      refresh_version(refresh_version() + 1)
    })

    # --- 导入到种植试验 ---
    observeEvent(input$btn_import_to_designplot, {
      exp <- selected_experiment()
      if (is.null(exp) || is.null(exp$experiment_id)) {
        showNotification("请先从左侧选择一个试验", type = "warning")
        return()
      }

      tryCatch({
        loc_filter <- input$filter_location
        loc_filter <- if (is.null(loc_filter) || !nzchar(trimws(loc_filter))) NULL else trimws(loc_filter)
        summary_info <- summarize_import_overwrite(exp$experiment_id, loc_filter)
        if (summary_info$overwrite_count > 0) {
          showModal(modalDialog(
            title = tagList(icon("exclamation-triangle"), "确认覆盖导入"),
            build_import_warning_ui(summary_info, batch = FALSE),
            easyClose = TRUE,
            footer = tagList(
              modalButton("取消"),
              actionButton(ns("confirm_import_to_designplot"), "继续覆盖导入", class = "btn-warning")
            )
          ))
        } else {
          perform_single_import(exp, loc_filter)
        }
      }, error = function(e) {
        showNotification(paste("导入失败:", e$message), type = "error")
      })
    })

    observeEvent(input$confirm_import_to_designplot, {
      exp <- selected_experiment()
      if (is.null(exp) || is.null(exp$experiment_id)) {
        removeModal()
        showNotification("请先从左侧选择一个试验", type = "warning")
        return()
      }

      tryCatch({
        removeModal()
        loc_filter <- input$filter_location
        loc_filter <- if (is.null(loc_filter) || !nzchar(trimws(loc_filter))) NULL else trimws(loc_filter)
        perform_single_import(exp, loc_filter)
      }, error = function(e) {
        showNotification(paste("导入失败:", e$message), type = "error")
      })
    })

    # --- 导入全部试验（按当前地点筛选）---
    observeEvent(input$btn_import_all_to_designplot, {
      tryCatch({
        loc_filter <- input$filter_location
        loc_filter <- if (is.null(loc_filter) || !nzchar(trimws(loc_filter))) NULL else trimws(loc_filter)

        target_ids <- experiments_list()$experiment_id %||% character(0)
        summary_info <- summarize_import_overwrite(target_ids, loc_filter)
        if (summary_info$overwrite_count > 0) {
          showModal(modalDialog(
            title = tagList(icon("exclamation-triangle"), "确认批量覆盖导入"),
            build_import_warning_ui(summary_info, batch = TRUE),
            easyClose = TRUE,
            footer = tagList(
              modalButton("取消"),
              actionButton(ns("confirm_import_all_to_designplot"), "继续批量覆盖导入", class = "btn-warning")
            )
          ))
        } else {
          perform_batch_import(loc_filter)
        }
      }, error = function(e) {
        showNotification(paste("批量导入失败:", e$message), type = "error")
      })
    })

    observeEvent(input$confirm_import_all_to_designplot, {
      tryCatch({
        removeModal()
        loc_filter <- input$filter_location
        loc_filter <- if (is.null(loc_filter) || !nzchar(trimws(loc_filter))) NULL else trimws(loc_filter)
        perform_batch_import(loc_filter)
      }, error = function(e) {
        showNotification(paste("批量导入失败:", e$message), type = "error")
      })
    })

    # --- 初始化 ---
    observe({
      refresh_version(1)
    })

    # --- 返回刷新和重置按钮ID（供外部调用）---
    list(
      btn_refresh = reactive(input$btn_refresh),
      btn_reset = reactive(input$btn_reset)
    )
  })
}

# 辅助函数：获取类型图标
get_type_icon <- function(type) {
  switch(type,
    "population" = "users",
    "line_selection" = "th-list",
    "yield_test" = "chart-bar",
    "file"
  )
}

# 辅助函数：空值替换
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}
