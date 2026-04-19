buildDesignplotUI <- function(){
  fluidPage(
    navbarPage("田间设计",
               tabPanel("田间规划",
                        sidebarLayout(
                          sidebarPanel(
                            tags$div(
                              style = "background:#f8fafc;border:1px solid #dbe4ee;border-radius:10px;padding:12px 12px 8px 12px;margin-bottom:10px;",
                              tags$div("地块管理", style = "font-size:16px;font-weight:600;color:#1f2937;margin-bottom:8px;"),
                              selectInput("field_model_select", "当前参数", choices = c("常规地块1" = "常规地块1"), selected = "常规地块1", width = "100%"),
                              fluidRow(
                                column(4, actionButton("saveFieldModel", "保存参数", class = "btn-primary", width = "100%")),
                                column(4, actionButton("addFieldModel", "增加参数", class = "btn-success", width = "100%")),
                                column(4, actionButton("deleteFieldModel", "删除参数", class = "btn-danger", width = "100%"))
                              ),
                              tags$div(style = "height:8px;"),
                              fluidRow(
                                column(6, actionButton("generatePlantField", "创建地块", class = "btn-info", width = "100%")),
                                column(6, actionButton("deletePlantField", "删除地块", class = "btn-danger", width = "100%"))
                              ),
                              tags$div("提示：设计表会实时更新；\"保存\"仅保存地块模板参数，\"创建地块\"才会写入可用于种植的地块表。删除参数前请先删除所创建的地块（点击「删除地块」）。", style = "font-size:12px;color:#6b7280;margin-top:8px;line-height:1.6;")
                            ),
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:10px;padding:10px 12px;margin-bottom:10px;",
                              tags$div("基础参数", style = "font-size:15px;font-weight:600;color:#1f2937;margin-bottom:6px;"),
                              textInput("get_water_columns", h5("行设计"), value="", placeholder = "w/8/w"),
                              tags$p("w=水沟；r=纵向观察道；p=保护行；斜杠间数字=水沟间行数", style = "margin-top:-6px;color:#6b7280;font-size:12px;line-height:1.6;"),
                              textInput("bridges", h5("排设计"), value="", placeholder = "10,6/3,10"),
                              tags$p("格式：长度(m) 或 长度(m)/重复", style = "margin-top:-6px;color:#6b7280;font-size:12px;")
                            ),
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:10px;padding:10px 12px;margin-bottom:10px;",
                              tags$div("观察道与材料分组", style = "font-size:15px;font-weight:600;color:#1f2937;margin-bottom:6px;"),
                              numericInput("ww", h5("横向观察道宽(m)"), value = NA, min = 0),
                              numericInput("w", h5("材料间隔(m)"), value = NA, min = 0),
                              numericInput("subg", h5("组内不隔断行数"), value = NA, min = 1)
                            ),
                            tags$details(
                              style = "background:#f8fafc;border:1px dashed #cbd5e1;border-radius:8px;padding:8px 10px;",
                              tags$summary(tags$span("▶ 高级参数（点击展开/收起）", style = "font-weight:600;color:#1f2937;cursor:pointer;")),
                              tags$div(style = "margin-top:8px;",
                                textInput("p_a", h5("不可种植区"), value="", placeholder = "23,25,3,5,30,40,7,10"),
                                textInput("protected_blocks", h5("横向保护行"), value="", placeholder = "留空或0=无；示例：1,3"),
                                tags$div(style = "height:10px;"),
                                tags$div("种植方向", style = "font-size:14px;font-weight:600;color:#1f2937;margin-bottom:4px;"),
                                radioButtons("design_from_left", h5("地块编号方向"), choices = list("从左" = TRUE, "从右" = FALSE), selected = TRUE, inline = TRUE),
                                radioButtons("plant_from_left", h5("材料排种方向"), choices = list("从左" = TRUE, "从右" = FALSE), selected = TRUE, inline = TRUE),
                                tags$div(style = "height:10px;"),
                                tags$details(
                                  style = "background:#ffffff;border:1px dashed #d1d5db;border-radius:6px;padding:6px 8px;",
                                  tags$summary(tags$span("▶ 编码图例（点击展开/收起）", style = "font-weight:600;color:#374151;cursor:pointer;")),
                                  tags$div(style = "margin-top:6px;",
                                    p("表中",
                                      span("正整数", style = "background-color:yellow"),
                                      "=可种植；",
                                      span("-77", style = "background-color:lightseagreen"),
                                      "=水沟；",
                                      span("-11", style = "background-color:gray"),
                                      "=横向观察道；",
                                      span("-111", style = "background-color:darkorange"),
                                      "=纵向观察道；",
                                      span("-1", style = "background-color:silver"),
                                      "=间隔；",
                                      span("-9", style = "background-color:green"),
                                      "=纵向保护行；",
                                      span("-99", style = "background-color:seagreen"),
                                      "=横向保护行；",
                                      span("-8", style = "background-color:red"),
                                      "=禁种；",
                                      span("-6", style = "background-color:limegreen"),
                                      "=分组补充行。")
                                  )
                                )
                              )
                            )
                          , width = 3),
                          mainPanel(
                            h4("设计简表"), textOutput("selected_var"), DT::dataTableOutput("simpleMydata"), downloadButton("simpleDownloadData", "下载设计简表（xlsx）"),
                            h4("行数统计"), DT::dataTableOutput("sta")
                          , width = 9)
                        )
               ),
               tabPanel("种植试验",
                        uiOutput("experimentStatusSummaryUi"),
                        tags$div(
                          style = "background:#eff6ff;border:1px solid #bfdbfe;border-radius:10px;padding:10px 14px;margin-bottom:14px;color:#1e3a5f;font-size:13px;line-height:1.55;",
                          tags$strong("建议顺序："),
                          "导入试验 → 选种植地块 → 选试验与种植范围 → 执行种植 →（可选）区域补种"
                        ),
                        fluidRow(
                          column(6,
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-left:4px solid #3b82f6;border-radius:10px;padding:12px;margin-bottom:12px;",
                              fluidRow(
                                column(1, tags$span("1", style = "background:#3b82f6;color:#fff;border-radius:50%;width:22px;height:22px;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:13px;")),
                                column(11, tags$span("导入试验", style = "font-size:15px;font-weight:600;color:#1f2937;margin-left:6px;"))
                              ),
                              tags$p("上传含 planting 工作表的 Excel 文件以导入试验。", style = "margin:4px 0 8px 0;color:#6b7280;font-size:12px;"),
                              fileInput("experimentImportFile", "上传试验Excel（含 planting 工作表）", multiple = FALSE,
                                        accept = c(".xlsx", ".xls", "application/vnd.ms-excel", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")),
                              textInput("experimentImportName", "试验名称（默认=上传文件名）", value = ""),
                              actionButton("importExperimentPlanting", "导入试验", class = "btn-primary", width = "100%")
                            )
                          ),
                          column(6,
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:10px;padding:12px;margin-bottom:12px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
                              fluidRow(
                                column(1, tags$span("2", style = "background:#10b981;color:#fff;border-radius:50%;width:22px;height:22px;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:13px;")),
                                column(11, tags$span("种植配置（先选地块，再选试验与范围）", style = "font-size:15px;font-weight:600;color:#1f2937;margin-left:6px;"))
                              ),
                              tags$div(style = "height:4px;"),
                              selectInput("experimentPlantTableSelect", "选择种植地块", choices = c("暂无地块" = ""), selected = ""),
                              selectInput("sqliteFilterExperimentId", "选择试验", choices = c("全部试验" = ""), selected = "", width = "100%"),
                              textInput("experimentPlantStartPos", "种植起始位置（排,行）", value = "1,1", placeholder = "示例：1,1 / 1, / ,3 / 1"),
                              textInput("experimentPlantEndPos", "种植终止位置（排,行；留空=最后）", value = "", placeholder = "示例：10,8 / 10, / ,8 / 10"),
                              uiOutput("experimentPlantSummaryUi")
                            )
                          )
                        ),
                        fluidRow(
                          column(12,
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-left:4px solid #2563eb;border-radius:12px;padding:14px;margin-bottom:12px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
                              fluidRow(
                                column(1, tags$span("3", style = "background:#2563eb;color:#fff;border-radius:50%;width:22px;height:22px;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:13px;")),
                                column(11, tags$span("执行种植", style = "font-size:16px;font-weight:600;color:#1f2937;margin-left:6px;"))
                              ),
                              tags$div(style = "height:8px;"),
                              checkboxInput("allowExperimentReplant", "允许覆盖重种（默认关闭）", value = FALSE),
                              tags$p("默认会阻止同一 experiment_id 在同一地块重复种植，避免误操作。", style = "margin:0 0 8px 0;color:#6b7280;font-size:12px;line-height:1.6;"),
                              uiOutput("runExperimentPlantingUi"),
                              tags$div(style = "height:8px;"),
                              tags$p("最近两次「执行试验种植」可在下方撤销（仅本会话有效，刷新页面后记录清空）。", style = "margin:0 0 6px 0;color:#6b7280;font-size:12px;line-height:1.55;"),
                              uiOutput("undoExperimentPlantingUi")
                            )
                          )
                        ),
                        fluidRow(
                          column(12,
                            tags$div(
                              style = "background:#fffbeb;border:1px solid #fcd34d;border-left:4px solid #f59e0b;border-radius:10px;padding:12px;margin-bottom:12px;",
                              fluidRow(
                                column(1, tags$span("4", style = "background:#f59e0b;color:#fff;border-radius:50%;width:22px;height:22px;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:13px;")),
                                column(11, tags$span("区域补种（可选）", style = "font-size:15px;font-weight:600;color:#1f2937;margin-left:6px;"))
                              ),
                              tags$div(style = "height:4px;"),
                              textInput("experimentFillMaterial", "区域补种材料（仅填未种位）", value = "", placeholder = "示例：补种材料A"),
                              uiOutput("experimentFillSummaryUi"),
                              actionButton("runExperimentFillUnplanted", "执行区域补种（仅未种）", class = "btn-warning", width = "100%")
                            )
                          )
                        ),
                        fluidRow(
                          column(6,
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:12px;padding:14px;margin-bottom:12px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
                              fluidRow(
                                column(12, tags$span("试验名称表", style = "font-size:16px;font-weight:600;color:#1f2937;"))
                              ),
                              tags$div(style = "height:4px;"),
                              DT::dataTableOutput("sqliteExperiments"),
                              downloadButton("downloadExperimentsCsv", "下载试验名称表（csv）")
                            )
                          ),
                          column(6,
                            tags$div(
                              style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:12px;padding:14px;margin-bottom:12px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
                              h4("试验记录表", style = "margin-top:0;margin-bottom:10px;"),
                              DT::dataTableOutput("sqliteExperimentRecords"),
                              downloadButton("downloadExperimentRecordsCsv", "下载试验记录表（csv）")
                            )
                          )
                        )
               ),
               tabPanel("种植展示",
                        fluidRow(column(12,
                                        tags$div(style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:12px;padding:14px 14px 10px 14px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
                                                 h4("种植地块预览", style = "margin-top:0;margin-bottom:10px;"),
                                                 tags$p("与「种植试验」「播种列表」「田间布局图」共用当前地块，任一处切换会同步。", style = "font-size:12px;color:#6b7280;margin:0 0 8px 0;line-height:1.5;"),
                                                 selectInput("plant_table_select", "选择种植地块", choices = c("暂无地块" = ""), selected = "", width = "100%"),
                                                 DT::dataTableOutput("selectedPlantPlotPreview"),
                                                 downloadButton("plantPreviewDownloadData", "下载种植展示（xlsx）")
                                        )))),
               tabPanel("播种列表",
                        mainPanel(
                          h4("播种列表"),
                          tags$p("播种表随当前地块自动对齐；与种植展示、田间布局图所选地块一致。", style = "font-size:12px;color:#6b7280;margin:0 0 8px 0;line-height:1.5;"),
                          actionButton("refreshSowList", "刷新播种列表", class = "btn-default"),
                          tags$div(style = "height:8px;"),
                          selectInput("sow_table_select", "选择播种地块", choices = c("暂无播种表" = ""), selected = "", width = "100%"),
                          uiOutput("sowIntegrityUi"),
                          DT::dataTableOutput("mydf"),
                          downloadButton("downloadSowListXlsx", "下载播种列表（xlsx）")
                        )),
               tabPanel("田间布局图",
                        fluidRow(column(12,
                                        tags$div(style = "background:#ffffff;border:1px solid #dbe4ee;border-radius:12px;padding:14px 14px 10px 14px;box-shadow:0 2px 8px rgba(15,23,42,0.06);",
                                                  h4("田间布局总览", style = "margin-top:0;margin-bottom:10px;"),
                                                  tags$p("布局图地块与种植展示、播种列表共用；切换即切换全局当前地块。", style = "font-size:12px;color:#6b7280;margin:0 0 8px 0;line-height:1.5;"),
                                                  selectInput("layout_field_select", "选择地块", choices = c("暂无地块" = ""), selected = "", width = "100%"),
                                                  uiOutput("fieldLayoutError"),
                                                   plotOutput("fieldLayoutPlot", height = "600px"),
                                                   downloadButton("downloadFieldLayoutPlot", "下载布局图（PNG）")
                                         ))))
    )
  )
}
