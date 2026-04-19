# =============================================================================
# 田间记录本生成器 - Shiny App
# 基于soyplant库的田间试验规划工具
# 支持三种记录本类型：群体、株行、产比
# 流程：上传 -> 维护 -> 生成记录本
# =============================================================================

library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(dplyr)
library(openxlsx)
library(soyplant)
library(ggplot2)

# 加载辅助函数
source("shared/helpers.R")
source("shared/db_persistence.R")

# 加载模块
source("shared/mod_experiments.R")
source("shared/mod_line_selection.R")
source("shared/mod_population.R")
source("shared/mod_yield_test.R")

# 加载designplot模块
source("shared/designplot/constants.R")
source("shared/designplot/sqlite_persistence.R")
source("shared/designplot/parsers.R")
source("shared/designplot/core_design.R")
source("shared/designplot/app_ui.R", local = TRUE)
source("shared/designplot/app_server.R", local = TRUE)

# Bootstrap 5 主题（主色、圆角、字体与自定义样式中的 var(--bs-*) 对齐）
fb_theme <- bs_theme(
  version = 5,
  primary = "#667eea",
  `font-family-sans-serif` = "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Microsoft YaHei', sans-serif",
  `font-family-base` = "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Microsoft YaHei', sans-serif",
  `border-radius` = "0.5rem",
  `enable-rounded` = "true"
)

# === UI 定义 ===

ui <- navbarPage(
  title = "田间记录本生成器",
  id = "main_nav",
  theme = fb_theme,

  header = tags$head(
    includeCSS("www/styles.css"),
    includeScript("www/custom-handlers.js")
  ),

  # === 1. 群体记录本 ===
  tabPanel("群体记录本",
    icon = icon("dna"),
    population_ui("pop_mod")
  ),

  # === 2. 株行记录本 ===
  tabPanel("株行记录本",
    icon = icon("leaf"),
    line_selection_ui("line_mod")
  ),

  # === 3. 产比记录本 ===
  tabPanel("产比记录本",
    icon = icon("chart-bar"),
    yield_test_ui("yield_mod")
  ),

  # === 4. 试验管理 ===
  tabPanel(
    "试验管理",
    icon = icon("flask"),
    experiments_ui("exp_mod")
  ),

  # === 5. 田间种植 ===
  tabPanel(
    "🌱 田间种植",
    icon = icon("seedling"),
    buildDesignplotUI()
  ),

  # === 关于 ===
  tabPanel("关于",
    icon = icon("info-circle"),
    fluidPage(
      style = "max-width: 800px; margin: auto; padding: 20px;",
      h3("田间记录本生成器 v2.0"),
      p("基于soyplant库的田间试验规划工具，支持群体、株行、产比三种记录本生成。"),
      hr(),
      h4("数据库说明"),
      p("所有试验记录保存在 data/field_book.sqlite 数据库中，包括："),
      tags$ul(
        tags$li("群体记录表 (population_records)"),
        tags$li("株行记录表 (line_selection_records)"),
        tags$li("产比记录表 (yield_test_records)")
      ),
      hr(),
      p("依赖包: shiny, DT, dplyr, openxlsx, soyplant, DBI, RSQLite"),
      p("开发者: zhaoqingsonga/soyplant")
    )
  )
)

# === Server 定义 ===

server <- function(input, output, session) {
  experiments_server("exp_mod")
  population_server("pop_mod")
  line_selection_server("line_mod")
  yield_test_server("yield_mod")
  buildDesignplotServer(input, output)
}

# === 启动应用 ===
shinyApp(ui = ui, server = server)
