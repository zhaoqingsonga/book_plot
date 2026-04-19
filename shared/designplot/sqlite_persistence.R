defaultSqlitePath <- function() {
  data_dir <- file.path("data")
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(data_dir, "field_book.sqlite")
}

# ---- 连接数据库（带 WAL 和超时配置）----
connectDesignplotDb <- function(db_path = defaultSqlitePath()) {
  # 每次连接新数据库时重置 schema 版本缓存，避免跨库污染
  .schema_version_cache <<- NULL
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

# ---- 数据库 schema 版本常量 ----
SCHEMA_VERSION <- 2L

# ---- 获取/设置 schema 版本（内存缓存）----
.schema_version_cache <- NULL

getSchemaVersion <- function(con) {
  # 每次都重新读取，避免跨数据库缓存污染
  if (!"db_meta" %in% DBI::dbListTables(con)) {
    DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS db_meta (key TEXT PRIMARY KEY, value TEXT)")
    DBI::dbExecute(con, "INSERT OR IGNORE INTO db_meta (key, value) VALUES ('schema_version', '0')")
  }
  ver <- DBI::dbGetQuery(con, "SELECT value FROM db_meta WHERE key = 'schema_version'")$value
  ver <- as.integer(ver[1])
  if (length(ver) == 0 || is.na(ver)) ver <- 0L
  .schema_version_cache <<- ver
  ver
}

setSchemaVersion <- function(con, ver) {
  DBI::dbExecute(con, "INSERT OR REPLACE INTO db_meta (key, value) VALUES ('schema_version', ?)", params = list(as.character(ver)))
  .schema_version_cache <<- ver
}

# ---- 初始化数据库（带版本化迁移）----
initDesignplotDb <- function(con) {
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")

  current_ver <- getSchemaVersion(con)

  # ---- Schema v1：创建所有基础表 ----
  if (current_ver < 1L) {
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS field_models (
        field_model_id INTEGER PRIMARY KEY AUTOINCREMENT,
        field_name TEXT NOT NULL UNIQUE,
        field_len REAL,
        no_plant TEXT,
        field_layout TEXT,
        strip_width TEXT,
        protect_strip TEXT,
        cross_path_width REAL,
        row_gap REAL,
        group_rows INTEGER,
        plant_start_pos TEXT,
        plant_end_pos TEXT,
        plant_start_row INTEGER,
        plant_start_col INTEGER,
        plant_end_col INTEGER,
        plan_left INTEGER,
        plant_left INTEGER,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL
      )
    ")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS experiments (
        experiment_id TEXT PRIMARY KEY,
        experiment_name TEXT NOT NULL,
        total_rows REAL,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL
      )
    ")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS experiment_plant_runs (
        run_id INTEGER PRIMARY KEY AUTOINCREMENT,
        experiment_id TEXT NOT NULL,
        plant_table_name TEXT NOT NULL,
        sow_table_name TEXT,
        plan_id TEXT,
        created_at TEXT NOT NULL,
        updated_at TEXT NOT NULL,
        FOREIGN KEY(experiment_id) REFERENCES experiments(experiment_id) ON DELETE CASCADE,
        UNIQUE(experiment_id, plant_table_name)
      )
    ")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS experiment_records (
        record_id INTEGER PRIMARY KEY AUTOINCREMENT,
        experiment_id TEXT NOT NULL,
        fieldid TEXT,
        id TEXT,
        stageid TEXT,
        name TEXT,
        former_stageid TEXT,
        source TEXT,
        code TEXT,
        rp TEXT,
        rows REAL,
        line_number TEXT,
        created_at TEXT NOT NULL,
        FOREIGN KEY(experiment_id) REFERENCES experiments(experiment_id) ON DELETE CASCADE
      )
    ")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS plan_runs (
        plan_id TEXT PRIMARY KEY,
        experiment_name TEXT NOT NULL,
        source_param_file TEXT,
        field_length REAL,
        field_layout TEXT,
        bridge_layout TEXT,
        row_gap REAL,
        group_rows INTEGER,
        design_from_left INTEGER,
        plant_from_left INTEGER,
        created_at TEXT NOT NULL
      )
    ")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS plan_slots (
        slot_id INTEGER PRIMARY KEY AUTOINCREMENT,
        plan_id TEXT NOT NULL,
        seq_no INTEGER NOT NULL,
        field_row_index INTEGER NOT NULL,
        field_row_no INTEGER,
        field_col_no INTEGER NOT NULL,
        row_length REAL,
        total_length REAL,
        interval_width REAL,
        created_at TEXT NOT NULL,
        FOREIGN KEY(plan_id) REFERENCES plan_runs(plan_id) ON DELETE CASCADE,
        UNIQUE(plan_id, seq_no)
      )
    ")
    DBI::dbExecute(con, "
      CREATE TABLE IF NOT EXISTS plant_assignments (
        assignment_id INTEGER PRIMARY KEY AUTOINCREMENT,
        plan_id TEXT NOT NULL,
        seq_no INTEGER NOT NULL,
        experiment_name TEXT NOT NULL,
        material_name TEXT NOT NULL,
        material_subrow_no INTEGER,
        field_row_no INTEGER,
        field_col_no INTEGER,
        created_at TEXT NOT NULL,
        FOREIGN KEY(plan_id) REFERENCES plan_runs(plan_id) ON DELETE CASCADE,
        UNIQUE(plan_id, seq_no)
      )
    ")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_plan_slots_plan_row_col ON plan_slots(plan_id, field_row_no, field_col_no)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_assignments_plan_material ON plant_assignments(plan_id, material_name)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_field_models_name ON field_models(field_name)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_experiments_name ON experiments(experiment_name)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_experiment_records_expid ON experiment_records(experiment_id)")
    DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_experiment_plant_runs_expid ON experiment_plant_runs(experiment_id)")
    setSchemaVersion(con, 1L)
    current_ver <- 1L
  }

  # ---- Schema v2：迁移旧 experiment_records + 补充 field_models 字段 ----
  if (current_ver < 2L) {
    # 迁移 experiment_records
    if ("experiment_records" %in% DBI::dbListTables(con)) {
      info <- DBI::dbGetQuery(con, "PRAGMA table_info(experiment_records)")
      if (is.data.frame(info) && nrow(info) > 0) {
        existing_names <- tolower(as.character(info$name))
        line_idx <- which(existing_names == "line_number")
        line_type <- if (length(line_idx) == 1) toupper(trimws(as.character(info$type[line_idx]))) else ""
        name_pos <- match("name", existing_names)
        former_pos <- match("former_stageid", existing_names)
        source_pos <- match("source", existing_names)

        needs_migration <- !identical(line_type, "TEXT") ||
          is.na(former_pos) || is.na(source_pos) || is.na(name_pos) ||
          former_pos != (name_pos + 1L) || source_pos != (name_pos + 2L)

        if (isTRUE(needs_migration)) {
          source_expr <- function(col_name) {
            if (col_name %in% existing_names) quoteSqliteIdentifier(col_name) else "NULL"
          }

          DBI::dbWithTransaction(con, {
            DBI::dbExecute(con, "DROP TABLE IF EXISTS experiment_records_new")
            DBI::dbExecute(con, "
              CREATE TABLE experiment_records_new (
                record_id INTEGER PRIMARY KEY AUTOINCREMENT,
                experiment_id TEXT NOT NULL,
                fieldid TEXT,
                id TEXT,
                stageid TEXT,
                name TEXT,
                former_stageid TEXT,
                source TEXT,
                code TEXT,
                rp TEXT,
                rows REAL,
                line_number TEXT,
                created_at TEXT NOT NULL,
                FOREIGN KEY(experiment_id) REFERENCES experiments(experiment_id) ON DELETE CASCADE
              )
            ")
            insert_sql <- paste0(
              "INSERT INTO experiment_records_new(record_id, experiment_id, fieldid, id, stageid, name, former_stageid, source, code, rp, rows, line_number, created_at) ",
              "SELECT ",
              source_expr("record_id"), ", ", source_expr("experiment_id"), ", ", source_expr("fieldid"), ", ",
              source_expr("id"), ", ", source_expr("stageid"), ", ", source_expr("name"), ", ",
              source_expr("former_stageid"), ", ", source_expr("source"), ", ", source_expr("code"), ", ",
              source_expr("rp"), ", ", source_expr("rows"), ", ",
              "CAST(", source_expr("line_number"), " AS TEXT), ",
              "COALESCE(", source_expr("created_at"), ", strftime('%Y-%m-%d %H:%M:%S', 'now'))",
              " FROM experiment_records"
            )
            DBI::dbExecute(con, insert_sql)
            DBI::dbExecute(con, "DROP TABLE experiment_records")
            DBI::dbExecute(con, "ALTER TABLE experiment_records_new RENAME TO experiment_records")
          })
          DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_experiment_records_expid ON experiment_records(experiment_id)")
        }
      }
    }

    # 补充 field_models 坐标字段
    if ("field_models" %in% DBI::dbListTables(con)) {
      fm_info <- DBI::dbGetQuery(con, "PRAGMA table_info(field_models)")
      if (is.data.frame(fm_info) && nrow(fm_info) > 0) {
        fm_names <- tolower(as.character(fm_info$name))
        if (!("plant_start_pos" %in% fm_names)) {
          DBI::dbExecute(con, "ALTER TABLE field_models ADD COLUMN plant_start_pos TEXT")
        }
        if (!("plant_end_pos" %in% fm_names)) {
          DBI::dbExecute(con, "ALTER TABLE field_models ADD COLUMN plant_end_pos TEXT")
        }
      }
    }
    setSchemaVersion(con, 2L)
  }
}

# ---- 通用工具 ----
quoteSqliteIdentifier <- function(x) {
  paste0('"', gsub('"', '""', as.character(x), fixed = TRUE), '"')
}

createPlanId <- function(experiment_name, plan_matrix) {
  safe_name <- gsub("[^A-Za-z0-9_-]+", "_", enc2utf8(experiment_name))
  safe_name <- trimws(safe_name)
  if (!nzchar(safe_name)) safe_name <- "experiment"
  plan_hash <- abs(sum(as.integer(plan_matrix), na.rm = TRUE))
  paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_", safe_name, "_", plan_hash)
}

createExperimentId <- function(experiment_name, total_rows, attempt = 1L) {
  ts_part <- format(Sys.time(), "%Y%m%d%H%M%S")
  rows_part <- as.integer(round(as.numeric(total_rows)))
  if (is.na(rows_part) || rows_part < 0) rows_part <- 0L
  if (isTRUE(as.integer(attempt) <= 1L)) {
    return(paste0("exp_", ts_part, "_", rows_part))
  }
  paste0("exp_", ts_part, "_", rows_part, "_", as.integer(attempt))
}

defaultExperimentName <- function(param_file = NULL, provided_name = NULL) {
  if (!is.null(provided_name) && nzchar(trimws(provided_name))) return(trimws(provided_name))
  if (!is.null(param_file) && nzchar(trimws(param_file))) return(tools::file_path_sans_ext(basename(param_file)))
  paste0("experiment_", format(Sys.time(), "%Y%m%d_%H%M%S"))
}

createPlantTableName <- function(field_name) {
  name <- trimws(as.character(field_name))
  if (!nzchar(name)) stop("地块名不能为空")
  if (grepl(";", name, fixed = TRUE) || grepl("[\r\n\t]", name)) stop("地块名包含非法字符")
  paste0(name, ".plant")
}

createSowTableName <- function(field_name) {
  name <- trimws(as.character(field_name))
  if (!nzchar(name)) stop("地块名不能为空")
  if (grepl(";", name, fixed = TRUE) || grepl("[\r\n\t]", name)) stop("地块名包含非法字符")
  paste0(name, ".sow")
}

# ---- 读取 planting Sheet 并规范化 ----
normalizePlantingDataFrame <- function(planting_df) {
  if (!is.data.frame(planting_df) || nrow(planting_df) == 0) stop("planting 表不能为空")

  required_cols <- c("fieldid", "id", "stageid", "name", "source", "code", "rp", "rows", "line_number")
  optional_cols <- c("former_stageid")
  target_cols <- c(required_cols, optional_cols)
  src_names <- names(planting_df)
  src_map <- stats::setNames(src_names, tolower(trimws(src_names)))
  missing_cols <- required_cols[!(required_cols %in% names(src_map))]
  if (length(missing_cols) > 0) stop(paste0("planting 表缺少字段: ", paste(missing_cols, collapse = ", ")))

  normalized <- data.frame(row.names = seq_len(nrow(planting_df)), stringsAsFactors = FALSE)
  for (col in target_cols) {
    if (col %in% names(src_map)) {
      normalized[[col]] <- planting_df[[src_map[[col]]]]
    } else {
      normalized[[col]] <- rep("", nrow(planting_df))
    }
  }

  normalized$fieldid <- as.character(normalized$fieldid)
  normalized$id <- as.character(normalized$id)
  normalized$stageid <- as.character(normalized$stageid)
  normalized$name <- as.character(normalized$name)
  normalized$former_stageid <- as.character(normalized$former_stageid)
  normalized$source <- as.character(normalized$source)
  normalized$code <- as.character(normalized$code)
  normalized$rp <- as.character(normalized$rp)
  normalized$rows <- suppressWarnings(as.numeric(normalized$rows))
  normalized$line_number <- as.character(normalized$line_number)

  if (any(is.na(normalized$rows))) stop("planting 表中的 rows 存在非数字值")
  if (any(!nzchar(trimws(normalized$line_number)) | is.na(normalized$line_number))) stop("planting 表中的 line_number 不能为空")

  normalized <- normalized[order(as.character(normalized$fieldid), na.last = TRUE), , drop = FALSE]
  rownames(normalized) <- NULL
  normalized
}

readPlantingSheetFromExcel <- function(excel_path, sheet = "planting") {
  if (is.null(excel_path) || !nzchar(trimws(excel_path)) || !file.exists(excel_path)) stop("Excel 文件不存在")
  sheet_names <- openxlsx::getSheetNames(excel_path)
  target_sheet <- sheet_names[tolower(sheet_names) == tolower(trimws(sheet))][1]
  if (is.na(target_sheet)) stop(paste0("Excel 中不存在工作表: ", sheet))
  planting_df <- openxlsx::read.xlsx(excel_path, sheet = target_sheet)
  normalizePlantingDataFrame(planting_df)
}

generateUniqueExperimentId <- function(con, experiment_name, total_rows) {
  for (i in seq_len(20)) {
    candidate <- createExperimentId(experiment_name, total_rows, attempt = i)
    existed <- DBI::dbGetQuery(con, "SELECT 1 AS hit FROM experiments WHERE experiment_id = ? LIMIT 1", params = list(candidate))
    if (!is.data.frame(existed) || nrow(existed) == 0) return(candidate)
  }
  stop("无法生成唯一试验ID，请重试")
}

# ---- 试验与记录操作 ----
saveExperimentWithRecords <- function(experiment_name, planting_df, db_path = defaultSqlitePath(), experiment_id = NULL) {
  if (is.null(experiment_name) || !nzchar(trimws(experiment_name))) stop("试验名称不能为空")
  exp_name <- trimws(experiment_name)
  normalized <- normalizePlantingDataFrame(planting_df)
  total_rows <- sum(normalized$rows, na.rm = TRUE)

  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  exp_id <- if (is.null(experiment_id) || !nzchar(trimws(experiment_id))) {
    generateUniqueExperimentId(con, exp_name, total_rows)
  } else {
    trimws(experiment_id)
  }

  DBI::dbWithTransaction(con, {
    existed <- DBI::dbGetQuery(con, "SELECT experiment_id FROM experiments WHERE experiment_id = ? LIMIT 1", params = list(exp_id))
    if (!is.data.frame(existed) || nrow(existed) == 0) {
      DBI::dbExecute(con,
        "INSERT INTO experiments(experiment_id, experiment_name, total_rows, created_at, updated_at) VALUES(?, ?, ?, ?, ?)",
        params = list(exp_id, exp_name, total_rows, now, now))
    } else {
      DBI::dbExecute(con,
        "UPDATE experiments SET experiment_name = ?, total_rows = ?, updated_at = ? WHERE experiment_id = ?",
        params = list(exp_name, total_rows, now, exp_id))
    }

    DBI::dbExecute(con, "DELETE FROM experiment_records WHERE experiment_id = ?", params = list(exp_id))
    normalized$experiment_id <- exp_id
    normalized$created_at <- now
    DBI::dbWriteTable(con, "experiment_records",
      normalized[, c("experiment_id", "fieldid", "id", "stageid", "name", "former_stageid", "source", "code", "rp", "rows", "line_number", "created_at")],
      append = TRUE)
  })

  invisible(list(experiment_id = as.character(exp_id), experiment_name = exp_name, total_rows = total_rows,
                 record_count = nrow(normalized), db_path = db_path))
}

importExperimentFromPlantingExcel <- function(excel_path, experiment_name = NULL, db_path = defaultSqlitePath(), sheet = "planting") {
  planting_df <- readPlantingSheetFromExcel(excel_path, sheet = sheet)
  exp_name <- defaultExperimentName(param_file = excel_path, provided_name = experiment_name)
  result <- saveExperimentWithRecords(experiment_name = exp_name, planting_df = planting_df, db_path = db_path, experiment_id = NULL)

  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  latest <- DBI::dbGetQuery(con,
    "SELECT experiment_id, experiment_name, total_rows FROM experiments WHERE experiment_name = ? ORDER BY created_at DESC, experiment_id DESC LIMIT 1",
    params = list(exp_name))
  if (is.data.frame(latest) && nrow(latest) > 0) {
    result$experiment_id <- as.character(latest$experiment_id[1])
    result$total_rows <- as.numeric(latest$total_rows[1])
  }
  invisible(result)
}

renameExperiment <- function(experiment_id, new_experiment_name, db_path = defaultSqlitePath()) {
  if (is.null(experiment_id) || !nzchar(trimws(experiment_id))) stop("试验ID不能为空")
  if (is.null(new_experiment_name) || !nzchar(trimws(new_experiment_name))) stop("试验名称不能为空")
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  updated <- DBI::dbExecute(con,
    "UPDATE experiments SET experiment_name = ?, updated_at = ? WHERE experiment_id = ?",
    params = list(trimws(new_experiment_name), now, trimws(experiment_id)))
  invisible(updated)
}

hasExperimentPlantRun <- function(experiment_id, plant_table_name, db_path = defaultSqlitePath()) {
  exp_id <- trimws(as.character(experiment_id))
  table_name <- trimws(as.character(plant_table_name))
  if (!nzchar(exp_id) || !nzchar(table_name)) return(FALSE)
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  existed <- DBI::dbGetQuery(con,
    "SELECT 1 AS hit FROM experiment_plant_runs WHERE experiment_id = ? AND plant_table_name = ? LIMIT 1",
    params = list(exp_id, table_name))
  is.data.frame(existed) && nrow(existed) > 0
}

# 获取所有已有种植记录的试验ID列表
getPlantedExperimentIds <- function(db_path = defaultSqlitePath()) {
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  df <- DBI::dbGetQuery(con, "SELECT DISTINCT experiment_id FROM experiment_plant_runs")
  if (is.data.frame(df) && nrow(df) > 0) as.character(df$experiment_id) else character(0)
}

# 获取某试验已种植的地块列表（返回字符向量）
getPlantedTablesForExperiment <- function(experiment_id, db_path = defaultSqlitePath()) {
  exp_id <- trimws(as.character(experiment_id))
  if (!nzchar(exp_id)) return(character(0))
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  df <- DBI::dbGetQuery(con,
    "SELECT DISTINCT plant_table_name FROM experiment_plant_runs WHERE experiment_id = ?",
    params = list(exp_id))
  if (is.data.frame(df) && nrow(df) > 0) df$plant_table_name else character(0)
}

saveExperimentPlantRun <- function(experiment_id, plant_table_name, sow_table_name = NULL, plan_id = NULL,
                                   db_path = defaultSqlitePath(), overwrite = FALSE) {
  exp_id <- trimws(as.character(experiment_id))
  table_name <- trimws(as.character(plant_table_name))
  if (!nzchar(exp_id)) stop("experiment_id 不能为空")
  if (!nzchar(table_name)) stop("plant_table_name 不能为空")

  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  DBI::dbWithTransaction(con, {
    existed <- DBI::dbGetQuery(con,
      "SELECT run_id FROM experiment_plant_runs WHERE experiment_id = ? AND plant_table_name = ? LIMIT 1",
      params = list(exp_id, table_name))
    if (is.data.frame(existed) && nrow(existed) > 0) {
      if (!isTRUE(overwrite)) stop("该试验已在当前地块执行过种植；如需重种请启用覆盖重种")
      DBI::dbExecute(con,
        "UPDATE experiment_plant_runs SET sow_table_name = ?, plan_id = ?, updated_at = ? WHERE experiment_id = ? AND plant_table_name = ?",
        params = list(
          if (!is.null(sow_table_name) && nzchar(trimws(as.character(sow_table_name)))) trimws(as.character(sow_table_name)) else NA_character_,
          if (!is.null(plan_id) && nzchar(trimws(as.character(plan_id)))) trimws(as.character(plan_id)) else NA_character_,
          now, exp_id, table_name))
      return(invisible("updated"))
    }
    DBI::dbExecute(con,
      "INSERT INTO experiment_plant_runs(experiment_id, plant_table_name, sow_table_name, plan_id, created_at, updated_at) VALUES(?, ?, ?, ?, ?, ?)",
      params = list(
        exp_id, table_name,
        if (!is.null(sow_table_name) && nzchar(trimws(as.character(sow_table_name)))) trimws(as.character(sow_table_name)) else NA_character_,
        if (!is.null(plan_id) && nzchar(trimws(as.character(plan_id)))) trimws(as.character(plan_id)) else NA_character_,
        now, now))
  })
  invisible("inserted")
}

clearExperimentPlantRunByTable <- function(plant_table_name, db_path = defaultSqlitePath()) {
  table_name <- trimws(as.character(plant_table_name))
  if (!nzchar(table_name)) return(invisible(0L))
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  deleted <- DBI::dbExecute(con, "DELETE FROM experiment_plant_runs WHERE plant_table_name = ?", params = list(table_name))
  invisible(as.integer(deleted))
}

clearSowTableByPlantTable <- function(plant_table_name, db_path = defaultSqlitePath()) {
  table_name <- trimws(as.character(plant_table_name))
  if (!nzchar(table_name)) return(invisible(FALSE))
  field_name <- sub("\\.plant$", "", table_name)
  sow_table <- createSowTableName(field_name)
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  tables <- DBI::dbListTables(con)
  if (!(sow_table %in% tables)) return(invisible(FALSE))
  DBI::dbRemoveTable(con, DBI::Id(table = sow_table))
  invisible(TRUE)
}

deleteExperiment <- function(experiment_id, db_path = defaultSqlitePath()) {
  if (is.null(experiment_id) || !nzchar(trimws(experiment_id))) stop("试验ID不能为空")
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  deleted <- DBI::dbExecute(con, "DELETE FROM experiments WHERE experiment_id = ?", params = list(trimws(experiment_id)))
  invisible(deleted)
}

# ---- 地块表操作 ----
savePlantTable <- function(field_name, plan_matrix, db_path = defaultSqlitePath()) {
  if (is.null(plan_matrix) || !is.matrix(plan_matrix)) stop("地块数据必须为矩阵")
  plant_table <- createPlantTableName(field_name)
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  plant_df <- as.data.frame(plan_matrix, stringsAsFactors = FALSE)
  plant_df$`row_index` <- seq_len(nrow(plant_df))
  plant_df <- plant_df[, c("row_index", setdiff(names(plant_df), "row_index")), drop = FALSE]
  DBI::dbWriteTable(con, DBI::Id(table = plant_table), plant_df, overwrite = TRUE)
  invisible(list(table_name = plant_table, row_count = nrow(plant_df), col_count = ncol(plant_df), db_path = db_path))
}

listPlantTables <- function(db_path = defaultSqlitePath()) {
  if (!file.exists(db_path)) return(data.frame(table_name = character(), field_name = character(), stringsAsFactors = FALSE))
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  names_df <- DBI::dbGetQuery(con,
    "SELECT name FROM sqlite_master WHERE type='table' AND name LIKE '%.plant' ORDER BY name")
  if (!is.data.frame(names_df) || nrow(names_df) == 0) {
    return(data.frame(table_name = character(), field_name = character(), stringsAsFactors = FALSE))
  }
  table_name <- as.character(names_df$name)
  field_name <- sub("\\.plant$", "", table_name)
  data.frame(table_name = table_name, field_name = field_name, stringsAsFactors = FALSE)
}

readPlantTable <- function(table_name, db_path = defaultSqlitePath()) {
  if (is.null(table_name) || !nzchar(trimws(table_name))) return(matrix(numeric(0), nrow = 0, ncol = 0))
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  target_name <- trimws(table_name)
  tables <- DBI::dbListTables(con)
  if (!(target_name %in% tables)) stop(paste0("种植地块不存在: ", target_name))
  df <- DBI::dbReadTable(con, DBI::Id(table = target_name))
  row_index_col <- names(df)[tolower(names(df)) %in% c("row_index", "__row_index", "x__row_index")]
  if (length(row_index_col) > 0) {
    idx_col <- row_index_col[1]
    df <- df[order(suppressWarnings(as.numeric(df[[idx_col]]))), setdiff(names(df), idx_col), drop = FALSE]
  }
  as.matrix(df)
}

deletePlantTable <- function(field_name, db_path = defaultSqlitePath()) {
  if (is.null(field_name) || !nzchar(trimws(field_name))) stop("地块名不能为空")
  plant_table <- createPlantTableName(field_name)
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)

  # 检查表是否存在
  tables <- DBI::dbListTables(con)
  if (!(plant_table %in% tables)) {
    stop(paste0("种植地块表不存在: ", plant_table))
  }

  # 删除地块表
  DBI::dbExecute(con, sprintf("DROP TABLE `%s`", plant_table))

  # 清理相关的种植运行记录
  clearExperimentPlantRunByTable(plant_table, db_path)

  # 清理相关的播种表
  clearSowTableByPlantTable(plant_table, db_path)

  invisible(TRUE)
}

# ---- 播种表（优化：向量化构建）----
buildSowTable <- function(field_name, base_matrix, planted_matrix = base_matrix) {
  if (is.null(field_name) || !nzchar(trimws(field_name))) stop("地块名不能为空")
  if (is.null(base_matrix) || !is.matrix(base_matrix)) stop("基础地块矩阵必须为矩阵")
  if (is.null(planted_matrix) || !is.matrix(planted_matrix)) stop("种植矩阵必须为矩阵")

  data_cols <- ncol(base_matrix) - STAT_COL_COUNT
  if (is.na(data_cols) || data_cols <= 0) stop("种植矩阵列数不合法")

  row_no_idx <- match("排数", colnames(base_matrix))
  if (is.na(row_no_idx)) row_no_idx <- ncol(base_matrix) - STAT_COL_OFFSET_ROWNO
  interval_idx <- match("间隔", colnames(base_matrix))
  if (is.na(interval_idx)) interval_idx <- data_cols + 1L

  # 向量化：提取行号和间隔
  field_row_nos <- suppressWarnings(as.numeric(base_matrix[, row_no_idx]))
  interval_widths <- suppressWarnings(as.numeric(base_matrix[, interval_idx]))
  field_row_nos[is.na(field_row_nos)] <- seq_len(length(field_row_nos))[is.na(field_row_nos)]
  interval_widths[is.na(interval_widths)] <- 0

  # 预判有多少个有效位置（避免多次 realloc）
  base_sub <- base_matrix[, seq_len(data_cols), drop = FALSE]
  seq_nos_mat <- suppressWarnings(as.numeric(base_sub))
  if (is.null(dim(seq_nos_mat))) dim(seq_nos_mat) <- c(nrow(base_sub), ncol(base_sub))
  is_valid_seq <- !is.na(seq_nos_mat) & seq_nos_mat > 0
  planted_mat <- planted_matrix[, seq_len(data_cols), drop = FALSE]
  is_parsed <- vapply(as.vector(planted_mat), function(cell) {
    !is.null(tryCatch(parseAssignmentCell(cell), error = function(e) NULL))
  }, logical(1))
  dim(is_parsed) <- c(nrow(planted_mat), ncol(planted_mat))
  is_target <- is_valid_seq | is_parsed

  total_cells <- sum(is_target)
  if (total_cells == 0L) {
    return(data.frame(
      Location = character(), ID = character(), Y = integer(), X = integer(),
      XiaoQuChangDu = numeric(), GuoDaoKuanDu = numeric(), HangJv = numeric(), XiaoQuLiShu = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  # 预分配向量
  Location <- rep(as.character(field_name), total_cells)
  ID <- character(total_cells)
  Y <- integer(total_cells)
  X <- integer(total_cells)
  XiaoQuChangDu <- numeric(total_cells)
  GuoDaoKuanDu <- rep(100, total_cells)
  HangJv <- rep(40, total_cells)
  XiaoQuLiShu <- numeric(total_cells)

  pos <- 1L
  for (row_idx in seq_len(nrow(base_matrix))) {
    for (col_idx in seq_len(data_cols)) {
      if (!is_target[row_idx, col_idx]) next

      seq_no <- seq_nos_mat[row_idx, col_idx]
      parsed <- tryCatch(parseAssignmentCell(planted_mat[row_idx, col_idx]), error = function(e) NULL)

      ID[pos] <- if (!is.null(parsed)) as.character(parsed$material_name) else as.character(as.integer(seq_no))
      Y[pos] <- as.integer(field_row_nos[row_idx])
      X[pos] <- as.integer(col_idx)
      int_v <- as.numeric(interval_widths[row_idx])
      XiaoQuChangDu[pos] <- int_v * 100
      XiaoQuLiShu[pos] <- int_v * 13
      pos <- pos + 1L
    }
  }

  data.frame(
    Location = Location, ID = ID, Y = Y, X = X,
    XiaoQuChangDu = XiaoQuChangDu, GuoDaoKuanDu = GuoDaoKuanDu,
    HangJv = HangJv, XiaoQuLiShu = XiaoQuLiShu,
    stringsAsFactors = FALSE,
    check.rows = FALSE
  )
}

saveSowTable <- function(field_name, sow_data, db_path = defaultSqlitePath()) {
  if (!is.data.frame(sow_data)) stop("播种表数据必须为 data.frame")
  sow_table <- createSowTableName(field_name)
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  DBI::dbWriteTable(con, DBI::Id(table = sow_table), sow_data, overwrite = TRUE)
  invisible(list(table_name = sow_table, row_count = nrow(sow_data), col_count = ncol(sow_data), db_path = db_path))
}

listSowTables <- function(db_path = defaultSqlitePath()) {
  if (!file.exists(db_path)) return(data.frame(table_name = character(), field_name = character(), stringsAsFactors = FALSE))
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  names_df <- DBI::dbGetQuery(con, "SELECT name FROM sqlite_master WHERE type='table' AND name LIKE '%.sow' ORDER BY name")
  if (!is.data.frame(names_df) || nrow(names_df) == 0) {
    return(data.frame(table_name = character(), field_name = character(), stringsAsFactors = FALSE))
  }
  table_name <- as.character(names_df$name)
  field_name <- sub("\\.sow$", "", table_name)
  data.frame(table_name = table_name, field_name = field_name, stringsAsFactors = FALSE)
}

readSowTable <- function(table_name, db_path = defaultSqlitePath()) {
  if (is.null(table_name) || !nzchar(trimws(table_name))) return(data.frame())
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  target_name <- trimws(table_name)
  tables <- DBI::dbListTables(con)
  if (!(target_name %in% tables)) return(data.frame())
  DBI::dbReadTable(con, DBI::Id(table = target_name))
}

computeSowProgress <- function(base_matrix, sow_df) {
  expected_count <- 0L
  if (is.matrix(base_matrix) && ncol(base_matrix) > STAT_COL_COUNT) {
    data_cols <- ncol(base_matrix) - STAT_COL_COUNT
    seq_nos <- suppressWarnings(as.numeric(base_matrix[, seq_len(data_cols)]))
    expected_count <- sum(!is.na(seq_nos) & seq_nos > 0, na.rm = TRUE)
  }

  if (!is.data.frame(sow_df) || nrow(sow_df) == 0) {
    return(list(expected_count = expected_count, planted_count = 0L, placeholder_count = 0L, status = "empty"))
  }

  id_vec <- if ("ID" %in% names(sow_df)) as.character(sow_df$ID) else rep(NA_character_, nrow(sow_df))
  id_trim <- trimws(id_vec)
  is_numeric_id <- grepl("^[0-9]+$", id_trim)
  planted_count <- sum(!is.na(id_trim) & nzchar(id_trim) & !is_numeric_id)
  placeholder_count <- sum(!is.na(id_trim) & nzchar(id_trim) & is_numeric_id)

  status <- if (planted_count == expected_count) "ok" else if (planted_count < expected_count) "under" else "over"

  list(expected_count = as.integer(expected_count), planted_count = as.integer(planted_count),
       placeholder_count = as.integer(placeholder_count), status = status)
}

# ---- 地块模型（优化：统一 UPSERT）----
listFieldModels <- function(db_path = defaultSqlitePath()) {
  models <- readTableFromSqlite("field_models", db_path)
  if (!is.data.frame(models) || nrow(models) == 0) return(data.frame())
  models[order(as.character(models$field_name)), , drop = FALSE]
}

getFieldModel <- function(field_name, db_path = defaultSqlitePath()) {
  if (is.null(field_name) || !nzchar(trimws(field_name))) return(NULL)
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  df <- DBI::dbGetQuery(con, "SELECT * FROM field_models WHERE field_name = ? LIMIT 1", params = list(trimws(field_name)))
  if (!is.data.frame(df) || nrow(df) == 0) return(NULL)
  as.list(df[1, , drop = FALSE])
}

.saveFieldModelImpl <- function(con, model_name, model_data, now) {
  # 检查是否存在
  existing <- DBI::dbGetQuery(con, "SELECT field_model_id, created_at FROM field_models WHERE field_name = ? LIMIT 1", params = list(model_name))
  is_update <- is.data.frame(existing) && nrow(existing) > 0

  if (is_update) {
    # UPDATE（直接绑定命名参数）
    DBI::dbExecute(con,
      "UPDATE field_models SET field_len = ?, no_plant = ?, field_layout = ?, strip_width = ?, protect_strip = ?, cross_path_width = ?, row_gap = ?, group_rows = ?, plant_start_pos = ?, plant_end_pos = ?, plant_start_row = ?, plant_start_col = ?, plant_end_col = ?, plan_left = ?, plant_left = ?, updated_at = ? WHERE field_name = ?",
      params = list(
        model_data$field_len %||% NA_real_,
        model_data$no_plant %||% NA_character_,
        model_data$field_layout %||% NA_character_,
        model_data$strip_width %||% NA_character_,
        model_data$protect_strip %||% NA_character_,
        model_data$cross_path_width %||% NA_real_,
        model_data$row_gap %||% NA_real_,
        model_data$group_rows %||% NA_integer_,
        model_data$plant_start_pos %||% NA_character_,
        model_data$plant_end_pos %||% NA_character_,
        model_data$plant_start_row %||% NA_integer_,
        model_data$plant_start_col %||% NA_integer_,
        model_data$plant_end_col %||% NA_integer_,
        as.integer(isTRUE(model_data$plan_left)),
        as.integer(isTRUE(model_data$plant_left)),
        now,
        model_name
      ))
  } else {
    # INSERT
    DBI::dbExecute(con,
      "INSERT INTO field_models(field_name, field_len, no_plant, field_layout, strip_width, protect_strip, cross_path_width, row_gap, group_rows, plant_start_pos, plant_end_pos, plant_start_row, plant_start_col, plant_end_col, plan_left, plant_left, created_at, updated_at) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(
        model_name,
        model_data$field_len %||% NA_real_,
        model_data$no_plant %||% NA_character_,
        model_data$field_layout %||% NA_character_,
        model_data$strip_width %||% NA_character_,
        model_data$protect_strip %||% NA_character_,
        model_data$cross_path_width %||% NA_real_,
        model_data$row_gap %||% NA_real_,
        model_data$group_rows %||% NA_integer_,
        model_data$plant_start_pos %||% NA_character_,
        model_data$plant_end_pos %||% NA_character_,
        model_data$plant_start_row %||% NA_integer_,
        model_data$plant_start_col %||% NA_integer_,
        model_data$plant_end_col %||% NA_integer_,
        as.integer(isTRUE(model_data$plan_left)),
        as.integer(isTRUE(model_data$plant_left)),
        now, now
      ))
  }
}

saveFieldModel <- function(field_name, model_data, db_path = defaultSqlitePath()) {
  if (is.null(field_name) || !nzchar(trimws(field_name))) stop("地块模型名称不能为空")
  model_name <- trimws(field_name)
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbWithTransaction(con, .saveFieldModelImpl(con, model_name, model_data, now))
  invisible(getFieldModel(model_name, db_path))
}

deleteFieldModel <- function(field_name, db_path = defaultSqlitePath()) {
  if (is.null(field_name) || !nzchar(trimws(field_name))) stop("地块模型名称不能为空")
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  deleted <- DBI::dbExecute(con, "DELETE FROM field_models WHERE field_name = ?", params = list(trimws(field_name)))
  invisible(deleted)
}

# ---- 规划与分配 ----
extractPlanSlots <- function(plan_matrix) {
  data_cols <- ncol(plan_matrix) - STAT_COL_COUNT
  data_part <- plan_matrix[, 1:data_cols, drop = FALSE]
  numeric_part <- suppressWarnings(matrix(as.numeric(data_part), nrow = nrow(data_part), ncol = ncol(data_part)))
  positions <- which(!is.na(numeric_part) & numeric_part > 0, arr.ind = TRUE)
  if (nrow(positions) == 0) {
    return(data.frame(
      seq_no = integer(), field_row_index = integer(), field_row_no = integer(), field_col_no = integer(),
      row_length = numeric(), total_length = numeric(), interval_width = numeric(), stringsAsFactors = FALSE
    ))
  }
  seq_no <- integer(nrow(positions))
  field_row_index <- integer(nrow(positions))
  field_row_no <- numeric(nrow(positions))
  field_col_no <- integer(nrow(positions))
  row_length <- numeric(nrow(positions))
  total_length <- numeric(nrow(positions))
  interval_width <- numeric(nrow(positions))
  stat_offset_1 <- ncol(plan_matrix) - STAT_COL_OFFSET_ROWNO
  stat_offset_2 <- ncol(plan_matrix) - STAT_COL_OFFSET_INTERVAL
  stat_offset_3 <- ncol(plan_matrix) - STAT_COL_OFFSET_BLOCKNO
  stat_offset_4 <- ncol(plan_matrix) - STAT_COL_COUNT

  for (i in seq_len(nrow(positions))) {
    row_idx <- positions[i, 1]
    col_idx <- positions[i, 2]
    seq_no[i] <- as.integer(numeric_part[row_idx, col_idx])
    field_row_index[i] <- row_idx
    field_row_no[i] <- suppressWarnings(as.numeric(plan_matrix[row_idx, stat_offset_1]))
    field_col_no[i] <- col_idx
    row_length[i] <- suppressWarnings(as.numeric(plan_matrix[row_idx, stat_offset_2]))
    total_length[i] <- suppressWarnings(as.numeric(plan_matrix[row_idx, stat_offset_3]))
    interval_width[i] <- suppressWarnings(as.numeric(plan_matrix[row_idx, stat_offset_4]))
  }
  data.frame(seq_no = seq_no, field_row_index = field_row_index, field_row_no = field_row_no, field_col_no = field_col_no,
             row_length = row_length, total_length = total_length, interval_width = interval_width, stringsAsFactors = FALSE)
}

parseAssignmentCell <- function(cell_value) {
  parts <- strsplit(as.character(cell_value), "\\|", fixed = FALSE)[[1]]
  if (length(parts) != 3) return(NULL)
  list(
    material_name = parts[1],
    material_subrow_no = suppressWarnings(as.integer(parts[2])),
    seq_no = suppressWarnings(as.integer(parts[3]))
  )
}

extractPlantAssignments <- function(planted_matrix, plan_matrix, experiment_name) {
  data_cols <- ncol(plan_matrix) - STAT_COL_COUNT
  result <- list()
  idx <- 1L
  stat_offset_rowno <- ncol(plan_matrix) - STAT_COL_OFFSET_ROWNO
  for (row_idx in seq_len(nrow(planted_matrix))) {
    for (col_idx in seq_len(data_cols)) {
      parsed <- parseAssignmentCell(planted_matrix[row_idx, col_idx])
      if (is.null(parsed)) next
      if (is.na(parsed$seq_no) || parsed$seq_no <= 0) next
      result[[idx]] <- data.frame(
        seq_no = parsed$seq_no, experiment_name = experiment_name, material_name = parsed$material_name,
        material_subrow_no = parsed$material_subrow_no,
        field_row_no = suppressWarnings(as.numeric(plan_matrix[row_idx, stat_offset_rowno])),
        field_col_no = col_idx, stringsAsFactors = FALSE
      )
      idx <- idx + 1L
    }
  }
  if (length(result) == 0) {
    return(data.frame(
      seq_no = integer(), experiment_name = character(), material_name = character(),
      material_subrow_no = integer(), field_row_no = integer(), field_col_no = integer(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, result)
}

savePlanToSqlite <- function(plan_matrix, experiment_name, db_path = defaultSqlitePath(), plan_id = NULL, metadata = list()) {
  if (is.null(plan_id) || !nzchar(trimws(plan_id))) plan_id <- createPlanId(experiment_name, plan_matrix)
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  slots <- extractPlanSlots(plan_matrix)
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM plant_assignments WHERE plan_id = ?", params = list(plan_id))
    DBI::dbExecute(con, "DELETE FROM plan_slots WHERE plan_id = ?", params = list(plan_id))
    DBI::dbExecute(con, "DELETE FROM plan_runs WHERE plan_id = ?", params = list(plan_id))
    DBI::dbExecute(con,
      "INSERT INTO plan_runs(plan_id, experiment_name, source_param_file, field_length, field_layout, bridge_layout, row_gap, group_rows, design_from_left, plant_from_left, created_at) VALUES(?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(plan_id, experiment_name, metadata$source_param_file %||% NA_character_,
                    metadata$field_length %||% NA_real_, metadata$field_layout %||% NA_character_,
                    metadata$bridge_layout %||% NA_character_, metadata$row_gap %||% NA_real_,
                    metadata$group_rows %||% NA_integer_,
                    as.integer(isTRUE(metadata$design_from_left)), as.integer(isTRUE(metadata$plant_from_left)), created_at))
    if (nrow(slots) > 0) {
      slots$plan_id <- plan_id
      slots$created_at <- created_at
      DBI::dbWriteTable(con, "plan_slots",
        slots[, c("plan_id", "seq_no", "field_row_index", "field_row_no", "field_col_no", "row_length", "total_length", "interval_width", "created_at")],
        append = TRUE)
    }
  })
  invisible(list(plan_id = plan_id, slot_count = nrow(slots), db_path = db_path))
}

saveAssignmentsToSqlite <- function(plan_id, planted_matrix, plan_matrix, experiment_name, db_path = defaultSqlitePath()) {
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  assignments <- extractPlantAssignments(planted_matrix, plan_matrix, experiment_name)
  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM plant_assignments WHERE plan_id = ?", params = list(plan_id))
    if (nrow(assignments) > 0) {
      assignments$plan_id <- plan_id
      assignments$created_at <- created_at
      DBI::dbWriteTable(con, "plant_assignments",
        assignments[, c("plan_id", "seq_no", "experiment_name", "material_name", "material_subrow_no", "field_row_no", "field_col_no", "created_at")],
        append = TRUE)
    }
  })
  invisible(list(plan_id = plan_id, assignment_count = nrow(assignments), db_path = db_path))
}

readTableFromSqlite <- function(table_name, db_path = defaultSqlitePath()) {
  if (!file.exists(db_path)) return(data.frame())
  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)
  if (!(table_name %in% DBI::dbListTables(con))) return(data.frame())
  DBI::dbReadTable(con, table_name)
}

getLatestPlanId <- function(db_path = defaultSqlitePath()) {
  plan_runs <- readTableFromSqlite("plan_runs", db_path)
  if (!is.data.frame(plan_runs) || nrow(plan_runs) == 0 || !all(c("plan_id", "created_at") %in% names(plan_runs))) return(NA_character_)
  plan_runs <- plan_runs[order(as.character(plan_runs$created_at), as.character(plan_runs$plan_id)), , drop = FALSE]
  as.character(plan_runs$plan_id[nrow(plan_runs)])
}

filterSqliteTable <- function(data, plan_id = NULL, experiment_name = NULL, keep_latest = FALSE) {
  if (!is.data.frame(data) || nrow(data) == 0) return(data)
  result <- data
  if (isTRUE(keep_latest) && "plan_id" %in% names(result)) {
    latest_id <- tail(unique(as.character(result$plan_id)), 1)
    result <- result[as.character(result$plan_id) == latest_id, , drop = FALSE]
  }
  if (!is.null(plan_id) && nzchar(trimws(plan_id)) && "plan_id" %in% names(result)) {
    result <- result[as.character(result$plan_id) == trimws(plan_id), , drop = FALSE]
  }
  if (!is.null(experiment_name) && nzchar(trimws(experiment_name)) && "experiment_name" %in% names(result)) {
    result <- result[grepl(trimws(experiment_name), as.character(result$experiment_name), fixed = TRUE), , drop = FALSE]
  }
  result
}

persistPlanAndAssignments <- function(plan_matrix, planted_matrix = NULL, experiment_name,
                                      db_path = defaultSqlitePath(), plan_id = NULL, metadata = list()) {
  plan_result <- savePlanToSqlite(plan_matrix = plan_matrix, experiment_name = experiment_name,
                                   db_path = db_path, plan_id = plan_id, metadata = metadata)
  if (!is.null(planted_matrix)) {
    assignment_result <- saveAssignmentsToSqlite(plan_id = plan_result$plan_id, planted_matrix = planted_matrix,
                                                 plan_matrix = plan_matrix, experiment_name = experiment_name, db_path = db_path)
    return(invisible(c(plan_result, assignment_result["assignment_count"])))
  }
  invisible(plan_result)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

# ---- 执行种植：撤销栈快照（内存中保留最近 N 步，由 Shiny 侧维护列表长度）----
capturePlantingUndoCheckpoint <- function(plant_table_name, experiment_id, plan_id, field_name,
                                          db_path = defaultSqlitePath()) {
  plant_table_name <- trimws(as.character(plant_table_name))
  experiment_id <- trimws(as.character(experiment_id))
  plan_id <- trimws(as.character(plan_id))
  field_name <- trimws(as.character(field_name))
  if (!nzchar(plant_table_name)) stop("plant_table_name 不能为空")
  if (!nzchar(experiment_id)) stop("experiment_id 不能为空")
  if (!nzchar(plan_id)) stop("plan_id 不能为空")
  if (!nzchar(field_name)) stop("field_name 不能为空")

  plant_matrix <- tryCatch(
    readPlantTable(plant_table_name, db_path),
    error = function(e) stop(paste0("读取种植表失败: ", e$message))
  )
  sow_name <- createSowTableName(field_name)
  sow_df <- readSowTable(sow_name, db_path)
  if (!is.data.frame(sow_df)) sow_df <- data.frame()

  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)

  plan_run <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM plan_runs WHERE plan_id = ?", params = list(plan_id)),
    error = function(e) data.frame()
  )
  if (!is.data.frame(plan_run)) plan_run <- data.frame()
  plan_slots <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM plan_slots WHERE plan_id = ? ORDER BY slot_id", params = list(plan_id)),
    error = function(e) data.frame()
  )
  if (!is.data.frame(plan_slots)) plan_slots <- data.frame()
  plant_assignments <- tryCatch(
    DBI::dbGetQuery(con, "SELECT * FROM plant_assignments WHERE plan_id = ? ORDER BY assignment_id", params = list(plan_id)),
    error = function(e) data.frame()
  )
  if (!is.data.frame(plant_assignments)) plant_assignments <- data.frame()

  epr <- tryCatch(
    DBI::dbGetQuery(con,
      "SELECT * FROM experiment_plant_runs WHERE experiment_id = ? AND plant_table_name = ?",
      params = list(experiment_id, plant_table_name)),
    error = function(e) data.frame()
  )
  if (!is.data.frame(epr)) epr <- data.frame()

  list(
    version = 1L,
    plant_table_name = plant_table_name,
    field_name = field_name,
    sow_table_name = sow_name,
    experiment_id = experiment_id,
    plan_id = plan_id,
    plant_matrix = plant_matrix,
    sow_df = sow_df,
    plan_run = plan_run,
    plan_slots = plan_slots,
    plant_assignments = plant_assignments,
    experiment_plant_run = epr
  )
}

restorePlantingUndoCheckpoint <- function(checkpoint, db_path = defaultSqlitePath()) {
  if (!is.list(checkpoint) || !identical(checkpoint$version, 1L)) stop("无效的快照")
  plant_table_name <- trimws(as.character(checkpoint$plant_table_name))
  field_name <- trimws(as.character(checkpoint$field_name))
  plan_id <- trimws(as.character(checkpoint$plan_id))
  experiment_id <- trimws(as.character(checkpoint$experiment_id))

  savePlantTable(field_name, checkpoint$plant_matrix, db_path = db_path)
  sow_df <- checkpoint$sow_df
  if (!is.data.frame(sow_df)) sow_df <- data.frame()
  saveSowTable(field_name, sow_df, db_path = db_path)

  con <- connectDesignplotDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDesignplotDb(con)

  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con, "DELETE FROM plant_assignments WHERE plan_id = ?", params = list(plan_id))
    DBI::dbExecute(con, "DELETE FROM plan_slots WHERE plan_id = ?", params = list(plan_id))
    DBI::dbExecute(con, "DELETE FROM plan_runs WHERE plan_id = ?", params = list(plan_id))

    pr <- checkpoint$plan_run
    if (is.data.frame(pr) && nrow(pr) > 0) {
      DBI::dbExecute(con,
        paste0(
          "INSERT INTO plan_runs(plan_id, experiment_name, source_param_file, field_length, field_layout, ",
          "bridge_layout, row_gap, group_rows, design_from_left, plant_from_left, created_at) ",
          "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        ),
        params = list(
          as.character(pr$plan_id[1]),
          as.character(pr$experiment_name[1]),
          if ("source_param_file" %in% names(pr)) as.character(pr$source_param_file[1]) else NA_character_,
          suppressWarnings(as.numeric(if ("field_length" %in% names(pr)) pr$field_length[1] else NA_real_)),
          if ("field_layout" %in% names(pr)) as.character(pr$field_layout[1]) else NA_character_,
          if ("bridge_layout" %in% names(pr)) as.character(pr$bridge_layout[1]) else NA_character_,
          suppressWarnings(as.numeric(if ("row_gap" %in% names(pr)) pr$row_gap[1] else NA_real_)),
          suppressWarnings(as.integer(if ("group_rows" %in% names(pr)) pr$group_rows[1] else NA_integer_)),
          suppressWarnings(as.integer(if ("design_from_left" %in% names(pr)) pr$design_from_left[1] else NA_integer_)),
          suppressWarnings(as.integer(if ("plant_from_left" %in% names(pr)) pr$plant_from_left[1] else NA_integer_)),
          as.character(pr$created_at[1])
        ))
    }

    ps <- checkpoint$plan_slots
    if (is.data.frame(ps) && nrow(ps) > 0) {
      slot_cols <- c("plan_id", "seq_no", "field_row_index", "field_row_no", "field_col_no", "row_length", "total_length", "interval_width", "created_at")
      slot_cols <- intersect(slot_cols, names(ps))
      if (length(slot_cols) > 0) {
        ps_out <- ps[, slot_cols, drop = FALSE]
        DBI::dbWriteTable(con, "plan_slots", ps_out, append = TRUE)
      }
    }

    pa <- checkpoint$plant_assignments
    if (is.data.frame(pa) && nrow(pa) > 0) {
      as_cols <- c("plan_id", "seq_no", "experiment_name", "material_name", "material_subrow_no", "field_row_no", "field_col_no", "created_at")
      as_cols <- intersect(as_cols, names(pa))
      if (length(as_cols) > 0) {
        pa_out <- pa[, as_cols, drop = FALSE]
        DBI::dbWriteTable(con, "plant_assignments", pa_out, append = TRUE)
      }
    }

    DBI::dbExecute(con,
      "DELETE FROM experiment_plant_runs WHERE experiment_id = ? AND plant_table_name = ?",
      params = list(experiment_id, plant_table_name))

    epr <- checkpoint$experiment_plant_run
    if (is.data.frame(epr) && nrow(epr) > 0) {
      DBI::dbExecute(con,
        "INSERT INTO experiment_plant_runs(experiment_id, plant_table_name, sow_table_name, plan_id, created_at, updated_at) VALUES (?, ?, ?, ?, ?, ?)",
        params = list(
          as.character(epr$experiment_id[1]),
          as.character(epr$plant_table_name[1]),
          if ("sow_table_name" %in% names(epr)) as.character(epr$sow_table_name[1]) else NA_character_,
          if ("plan_id" %in% names(epr)) as.character(epr$plan_id[1]) else NA_character_,
          as.character(epr$created_at[1]),
          as.character(epr$updated_at[1])
        ))
    }
  })

  invisible(TRUE)
}
