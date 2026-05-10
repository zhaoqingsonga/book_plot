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
      h4("数据库管理"),
      p("导出或导入 SQLite 数据库（.sql 文件）："),
       fluidRow(
         column(6,
           downloadButton("export_sql", "导出数据库",
             icon = icon("download"), class = "btn-primary"),
          ),
        column(6,
          fileInput("import_sql", "导入数据库 (.sql)",
            accept = ".sql", buttonLabel = "选择文件")
        )
      ),
      conditionalPanel(
        condition = "input.import_sql != null",
        actionButton("confirm_import", "确认导入（将覆盖现有数据！）",
          icon = icon("warning"), class = "btn-danger")
      ),
      hr(),
      h4("依赖包"),
      p("依赖包: shiny, DT, dplyr, openxlsx, soyplant, DBI, RSQLite"),
      p("开发者: 赵青松"),
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
      con <- dbConnect(RSQLite::SQLite(), "data/field_book.sqlite")
      on.exit(dbDisconnect(con))

      sql_content <- readLines(input$import_sql$datapath, warn = FALSE)
      sql_content <- paste(sql_content, collapse = "\n")

      # 分割每条语句
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
