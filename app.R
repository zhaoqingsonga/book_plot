# =============================================================================
# 田间记录本生成及田间规划 - Shiny App
# 基于soyplant库的田间试验规划工具
# 支持三种记录本类型：群体、株行、产比
# 流程：上传 -> 维护 -> 生成记录本
# =============================================================================

options(shiny.maxRequestSize = 1024 * 1024 * 1024)

library(shiny)
library(bslib)
library(shinyjs)
library(DT)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(soyplant)
library(DBI)
library(RSQLite)

# 加载辅助函数
source("shared/helpers.R")
source("shared/db_persistence.R")

# 加载模块
source("shared/mod_experiments.R")
source("shared/mod_line_selection.R")
source("shared/mod_population.R")
source("shared/mod_yield_test.R")
source("shared/mod_settings.R")

# 加载E智导入功能
source("shared/import_traits.R")

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
  title = div(icon("seedling"), "田间记录本生成及田间规划"),
  id = "main_nav",
  theme = fb_theme,

  header = tags$head(
    includeCSS("www/styles.css"),
    includeScript("www/custom-handlers.js"),
    tags$link(rel = "icon", href = "soybean.svg", type = "image/svg+xml")
  ),

  useShinyjs(),

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
    icon = icon("balance-scale"),
    yield_test_ui("yield_mod")
  ),

  # === 4. 试验管理 ===
  tabPanel(
    "试验管理",
    icon = icon("clipboard-list"),
    experiments_ui("exp_mod")
  ),

  # === 5. 田间种植 ===
  tabPanel(
    "田间种植",
    icon = icon("seedling"),
    buildDesignplotUI()
  ),

  # === 6. E智导入 ===
  tabPanel(
    "E智导入",
    icon = icon("upload"),
    settings_ui("settings_mod")
  ),

  # === 关于 ===
  tabPanel("关于",
    icon = icon("info-circle"),
    fluidPage(
      # ---- Hero Banner ----
      div(class = "about-hero",
        div(class = "about-hero-content",
          div(class = "about-hero-icon", icon("seedling")),
          h2("田间记录本生成及田间规划"),
          p(class = "about-hero-version", "v2.0"),
          p(class = "about-hero-desc",
            "基于 soyplant 库的田间试验规划工具，一站式完成群体、株行、产比三种记录本的生成与管理。"
          )
        )
      ),

      # ---- Feature Stats Row ----
      div(class = "about-features",
        div(class = "about-feature-card",
          div(class = "about-feature-icon", icon("dna")),
          div(class = "about-feature-label", "群体记录本"),
          div(class = "about-feature-desc", "F1-F7 群体数据管理")
        ),
        div(class = "about-feature-card",
          div(class = "about-feature-icon", icon("leaf")),
          div(class = "about-feature-label", "株行记录本"),
          div(class = "about-feature-desc", "单株选择与行号规划")
        ),
        div(class = "about-feature-card",
          div(class = "about-feature-icon", icon("balance-scale")),
          div(class = "about-feature-label", "产比记录本"),
          div(class = "about-feature-desc", "杂交组合与产量比较")
        ),
        div(class = "about-feature-card",
          div(class = "about-feature-icon", icon("seedling")),
          div(class = "about-feature-label", "田间种植图"),
          div(class = "about-feature-desc", "可视化编辑种植布局")
        )
      ),

      # ---- Database Info Card ----
      div(class = "card",
        div(class = "card-header", icon("database"), " 数据库说明"),
        p("所有试验记录保存在 ", tags$code("data/field_book.sqlite"), " 数据库中，核心表结构如下："),
        div(class = "about-db-grid",
          div(class = "about-db-item",
            div(class = "about-db-item-name", "population_records"),
            div(class = "about-db-item-desc", "群体记录 — 世代升级与种植计划")
          ),
          div(class = "about-db-item",
            div(class = "about-db-item-name", "line_selection_records"),
            div(class = "about-db-item-desc", "株行记录 — 单株选择与行号分配")
          ),
          div(class = "about-db-item",
            div(class = "about-db-item-name", "yield_test_records"),
            div(class = "about-db-item-desc", "产比记录 — 杂交组合与产量数据")
          ),
          div(class = "about-db-item",
            div(class = "about-db-item-name", "experiments"),
            div(class = "about-db-item-desc", "试验基本信息 — 名称、类型与状态")
          )
        )
      ),

      # ---- Dependencies & Credits Card ----
      div(class = "card",
        div(class = "card-header", icon("code"), " 技术栈与致谢"),
        div(class = "about-tech-row",
          div(class = "about-tech-section",
            h5("核心依赖"),
            tags$span(class = "about-tech-tag", "shiny"),
            tags$span(class = "about-tech-tag", "DT"),
            tags$span(class = "about-tech-tag", "dplyr"),
            tags$span(class = "about-tech-tag", "openxlsx"),
            tags$span(class = "about-tech-tag", "ggplot2"),
            tags$span(class = "about-tech-tag", "soyplant"),
            tags$span(class = "about-tech-tag", "DBI"),
            tags$span(class = "about-tech-tag", "RSQLite"),
            tags$span(class = "about-tech-tag", "bslib")
          ),
          div(class = "about-tech-section",
            h5("开发者"),
            p(class = "about-credit",
              icon("user-pen"), " 赵青松",
              br(),
              tags$a(href = "https://github.com/zhaoqingsonga/soyplant",
                     target = "_blank",
                     icon("github"), " soyplant R 包")
            )
          )
        ),
        hr(),
        p(style = "text-align: center; color: #888; font-size: 0.85em; margin: 0;",
          "© 2024 田间记录本生成及田间规划 — 为作物育种田间试验而生")
      )
    )
  )
)

# === Server 定义 ===

server <- function(input, output, session) {
  experiments_server("exp_mod")
  population_server("pop_mod")
  line_selection_server("line_mod")
  yield_test_server("yield_mod")
  settings_server("settings_mod")
  buildDesignplotServer(input, output, session)

  # 中转：mod_experiments 导入成功后向 designplot 发送的 refresh 消息，
  # 由于 session 隔离，mod_experiments 的 session 无法直接刷新 designplot 的 UI。
  # 因此让主 session 监听 JS 全局设置的 designplot_refresh input，
  # 然后通过 designplot server 暴露在 userData 中的函数来触发刷新。
  observeEvent(input$designplot_refresh, {
    fn <- session$userData$designplot_refresh_fn
    if (is.function(fn)) {
      fn()
    }
  })

  # --- 数据库导出（downloadHandler） ---
  # 已移除（关于页数据库管理功能）

  # --- 数据库导入 ---
  # 已移除（关于页数据库管理功能）
}

# === 启动应用 ===
shinyApp(ui = ui, server = server)
