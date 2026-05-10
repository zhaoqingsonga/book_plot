# =============================================================================
# 田间记录本生成器 - Shiny App
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
    "🌱 田间种植",
    icon = icon("seedling"),
    buildDesignplotUI()
  ),

  # === 关于 ===
  tabPanel("关于",
    icon = icon("info-circle"),
    fluidPage(
      # ---- Hero Banner ----
      div(class = "about-hero",
        div(class = "about-hero-content",
          div(class = "about-hero-icon", "🌾"),
          h2("田间记录本生成器"),
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

      # ---- Database Management Card ----
      div(class = "card",
        div(class = "card-header", icon("wrench"), " 数据库管理"),
        p("导出或导入 SQLite 数据库（.sql 格式），便于数据备份与迁移。"),
        div(class = "about-mgmt-row",
          div(class = "about-mgmt-action",
            div(class = "about-mgmt-label", icon("download"), " 导出数据库"),
            p(class = "about-mgmt-hint", "将全部表结构及数据导出为 .sql 文件"),
            downloadButton("export_sql", "导出数据库文件",
              icon = icon("file-export"), class = "btn-primary")
          ),
          div(class = "about-mgmt-divider"),
          div(class = "about-mgmt-action",
            div(class = "about-mgmt-label", icon("upload"), " 导入数据库"),
            p(class = "about-mgmt-hint", "从 .sql 文件恢复数据（将覆盖现有数据）"),
            fileInput("import_sql", NULL,
              accept = ".sql", buttonLabel = "选择 .sql 文件"),
            conditionalPanel(
              condition = "input.import_sql != null",
              actionButton("confirm_import", "确认导入（将覆盖现有数据！）",
                icon = icon("triangle-exclamation"), class = "btn-danger")
            )
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
          "© 2024 田间记录本生成器 — 为作物育种田间试验而生")
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
  output$export_sql <- downloadHandler(
    filename = function() {
      paste0("field_book_export_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".sql")
    },
    content = function(file) {
      db_path <- normalizePath("data/field_book.sqlite", mustWork = FALSE)

      # 优先使用 sqlite3 CLI（速度快，470K 行数据秒级导出）
      sqlite3_bin <- Sys.which("sqlite3")
      if (nzchar(sqlite3_bin)) {
        result <- system2(sqlite3_bin, c(shQuote(db_path), ".dump"),
                         stdout = file, stderr = NULL)
        if (result == 0) return()
      }

      # 降级方案：基于 R 的批量导出（无 sqlite3 CLI 时使用）
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con))

      tables <- dbListTables(con)
      f <- file(file, open = "w", encoding = "UTF-8")
      on.exit(close(f), add = TRUE)

      writeLines(c("-- SQLite Database Export",
                   paste("-- Generated at:", Sys.time()),
                   "-- This file can be imported back via the About page.",
                   ""), f)

      for (tbl in tables) {
        writeLines(paste0("DROP TABLE IF EXISTS `", tbl, "`;"), f)

        create_stmt <- dbGetQuery(con,
          paste0("SELECT sql FROM sqlite_master WHERE type='table' AND name='", tbl, "'"))
        if (nrow(create_stmt) > 0 && !is.na(create_stmt$sql[1])) {
          writeLines(c(create_stmt$sql[1], ""), f)
        }

        df <- dbReadTable(con, tbl)
        if (nrow(df) > 0) {
          writeLines("BEGIN TRANSACTION;", f)
          fields <- paste0("`", names(df), "`", collapse = ", ")
          batch_size <- 500L

          for (start_idx in seq(1L, nrow(df), by = batch_size)) {
            end_idx <- min(start_idx + batch_size - 1L, nrow(df))
            batch <- df[start_idx:end_idx, , drop = FALSE]

            value_rows <- apply(batch, 1L, function(row) {
              vals <- mapply(function(val, col_name) {
                if (is.na(val)) return("NULL")
                cls <- class(val)
                if (is.numeric(val) || "integer64" %in% cls) return(as.character(val))
                paste0("'", gsub("'", "''", as.character(val)), "'")
              }, row, names(row), SIMPLIFY = TRUE, USE.NAMES = FALSE)
              paste0("(", paste(vals, collapse = ", "), ")")
            })

            insert_stmt <- paste0("INSERT INTO `", tbl, "` (", fields, ") VALUES\n",
                                 paste(value_rows, collapse = ",\n"), ";")
            writeLines(insert_stmt, f)
          }

          writeLines(c("COMMIT;", ""), f)
        }
      }
    }
  )

  # --- 数据库导入 ---
  observeEvent(input$confirm_import, {
    req(input$import_sql)
    tryCatch({
      db_path <- normalizePath("data/field_book.sqlite", mustWork = FALSE)
      sql_path <- input$import_sql$datapath

      # 优先使用 sqlite3 CLI（正确处理含分号的 JSON 字段）
      sqlite3_bin <- Sys.which("sqlite3")
      if (nzchar(sqlite3_bin)) {
        import_cmd <- paste0(".read ", shQuote(sql_path))
        result <- system2(sqlite3_bin, c(shQuote(db_path)),
                         input = import_cmd, stdout = TRUE, stderr = TRUE,
                         wait = TRUE)
        # 判断成功：stdout 为空（character(0)）且 exit code 为 0
        if (identical(result, character(0)) && is.null(attr(result, "status"))) {
          showNotification("数据库导入成功！页面将刷新...", type = "message")
          Sys.sleep(1)
          shinyjs::js$refresh()
          return()
        }
        # 如果 CLI 失败，打印错误信息，fall through 到 R 方案
        if (length(result) > 0) {
          warning(paste("sqlite3 CLI 导入失败:", paste(result, collapse = "\n")))
        }
      }

      # 降级方案：R 批量导入（对含分号的 JSON 字段会出错，仅作备用）
      con <- dbConnect(RSQLite::SQLite(), db_path)
      on.exit(dbDisconnect(con))
      sql_content <- readLines(sql_path, warn = FALSE)
      sql_content <- paste(sql_content, collapse = "\n")
      statements <- unlist(strsplit(sql_content, ";"))
      statements <- trimws(statements[which(nchar(trimws(statements)) > 0)])
      for (stmt in statements) {
        stmt <- trimws(stmt)
        if (nchar(stmt) == 0) next
        tryCatch({
          dbExecute(con, stmt)
        }, error = function(e) {
          warning(paste("Failed statement:", substr(stmt, 1, 100), "Error:", e$message))
        })
      }
      showNotification("数据库导入成功！页面将刷新...", type = "message")
      Sys.sleep(1)
      shinyjs::js$refresh()
    }, error = function(e) {
      showNotification(paste("导入失败:", e$message), type = "error")
    })
  })
}

# === 启动应用 ===
shinyApp(ui = ui, server = server)
