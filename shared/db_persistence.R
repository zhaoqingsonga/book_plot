# =============================================================================
# 数据库持久化模块
# 田间记录本生成器的数据库操作
# =============================================================================

library(DBI)
library(RSQLite)

# 默认数据库路径
defaultDbPath <- function() {
  data_dir <- file.path("data")
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  file.path(data_dir, "field_book.sqlite")
}

# 连接数据库
connectDb <- function(db_path = defaultDbPath()) {
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

# 田试记录表统一列名（三个表结构相同）
FIELD_RECORD_COLS <- c(
  "experiment_id", "experiment_name",
  "fieldid", "id", "user", "stageid", "name", "ma", "pa", "mapa", "memo", "stage", "next_stage", "f", "sele", "process", "path", "source", "former_fieldid", "former_stageid", "code", "rp", "treatment", "place", "rows", "line_number",
  "XiaoQuShiShouMianJi", "XiaoQuChanLiang", "HanShuiLiang", "MuChan",
  "BoZhongQi", "ChuMiaoQi", "ChuMiaoLiangFou", "MiaoQiTianJianPingJia",
  "KaiHuaQi", "HuaSe", "HuaQiTianJianPingJia", "YeXing", "RongMaoSe",
  "ShengZhangXiXing", "JieJiaXiXing", "DaoFuXing", "ZaoShuaiXing", "ZhuXing",
  "LuoYeXing", "LieJiaXing", "ChengShuQi", "HuoGanChengShu", "ChengShuQiTianJianPingJia",
  "ShouHuoQi", "XiaoQuShouHuoZhuShu", "ShengYuQi", "TianJianBeiZhu",
  "HuaYeBingDuBing", "NiJingDianZhongFuBing", "ShuangMeiBing", "HuiBanBing",
  "XiJunXingBanDianBing", "XiuBing", "GenFuBing", "BaoNangXianChongBing",
  "QiTaBingHai", "DouGanHeiQianYing", "DouJiaMing", "YaChong", "ShiYeXingHaiChong",
  "KaoZhongZhuShu", "ZhuGao", "DiJiaGao", "FenZhiShu", "ZhuJingJieShu", "JiaXing",
  "JiaShuSe", "YouXiaoJia", "WuXiaoJia", "DanZhuJiaShu", "DanZhuLiShu", "DanZhuLiZhong",
  "MeiJiaLiShu", "LiXing", "ZhongPiSe", "QiSe", "ZiYeSe", "ZhongPiGuangZe",
  "BaiLiZhong", "WanHaoLiLv", "PoSuiLiLv", "BingLiLv", "ZiBanLiLv", "HeBanLiLv",
  "ShuangMeiLiLv", "HuiBanLiLv", "ChongShiLiLv", "ZiLiPingJia",
  "DanBai", "ZhiFang", "DanZhiHe", "CaoGanLinKangXing", "ShiZhiJianCe", "HanJiYin",
  "BoZhongPenShu", "BoZhongLiShu", "ChuMiaoShu", "ChuMiaoLiShu",
  "NaiYanXing", "NaiHanXing", "ShiHuaQi", "ZaJiaoHuaShu", "ChengHuoJiaShu", "ZhaJiaoliShu",
  "ChuShuQi", "WanShuQi", "HuiFuLv", "SSRBuHeGeWeiDian",
  "is_ck",
  "created_at"
)

# 初始化数据库表
initDb <- function(con) {
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")

  # 群体记录表
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS population_records (
      record_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL UNIQUE,
      experiment_name TEXT NOT NULL,
      total_rows REAL DEFAULT 0,
      has_generated INTEGER DEFAULT 0,
      raw_data TEXT,
      generated_at TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )
  ")

  # 株行记录表
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS line_selection_records (
      record_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL UNIQUE,
      source_id TEXT,
      experiment_name TEXT NOT NULL,
      total_rows REAL DEFAULT 0,
      has_generated INTEGER DEFAULT 0,
      raw_data TEXT,
      generated_at TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )
  ")

  # 产比记录表
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS yield_test_records (
      record_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL UNIQUE,
      experiment_name TEXT NOT NULL,
      total_rows REAL DEFAULT 0,
      has_generated INTEGER DEFAULT 0,
      raw_data TEXT,
      generated_at TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )
  ")

  # 迁移：为已有表添加 raw_data 列（如果不存在）
  tryCatch({
    DBI::dbExecute(con, "ALTER TABLE population_records ADD COLUMN raw_data TEXT")
  }, error = function(e) { message("迁移警告 [population_records.raw_data]: ", e$message) })
  tryCatch({
    DBI::dbExecute(con, "ALTER TABLE line_selection_records ADD COLUMN raw_data TEXT")
  }, error = function(e) { message("迁移警告 [line_selection_records.raw_data]: ", e$message) })
  tryCatch({
    DBI::dbExecute(con, "ALTER TABLE yield_test_records ADD COLUMN raw_data TEXT")
  }, error = function(e) { message("迁移警告 [yield_test_records.raw_data]: ", e$message) })

  # 群体材料明细表
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS population_materials (
      material_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL,
      fieldid TEXT,
      code TEXT,
      ma TEXT,
      pa TEXT,
      f INTEGER,
      stageid TEXT,
      name TEXT,
      rows REAL DEFAULT 0,
      line_number TEXT,
      rp INTEGER DEFAULT 1,
      created_at TEXT NOT NULL,
      FOREIGN KEY(experiment_id) REFERENCES population_records(experiment_id) ON DELETE CASCADE
    )
  ")

  # 株行材料明细表
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS line_selection_materials (
      material_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL,
      fieldid TEXT,
      code TEXT,
      ma TEXT,
      pa TEXT,
      stageid TEXT,
      name TEXT,
      rows REAL DEFAULT 0,
      line_number TEXT,
      rp INTEGER DEFAULT 1,
      sele REAL DEFAULT 0,
      created_at TEXT NOT NULL,
      FOREIGN KEY(experiment_id) REFERENCES line_selection_records(experiment_id) ON DELETE CASCADE
    )
  ")

  # 产比材料明细表
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS yield_test_materials (
      material_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL,
      fieldid TEXT,
      code TEXT,
      ma TEXT,
      pa TEXT,
      stageid TEXT,
      name TEXT,
      rows REAL DEFAULT 0,
      line_number TEXT,
      rp INTEGER DEFAULT 1,
      created_at TEXT NOT NULL,
      FOREIGN KEY(experiment_id) REFERENCES yield_test_records(experiment_id) ON DELETE CASCADE
    )
  ")

  # 创建索引
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_pop_exp ON population_records(experiment_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_line_exp ON line_selection_records(experiment_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_yield_exp ON yield_test_records(experiment_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_pop_mat_exp ON population_materials(experiment_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_line_mat_exp ON line_selection_materials(experiment_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_yield_mat_exp ON yield_test_materials(experiment_id)")

  # 广表：统一所有试验记录
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS unified_records (
      record_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL UNIQUE,
      experiment_type TEXT NOT NULL CHECK(experiment_type IN ('population', 'line_selection', 'yield_test')),
      experiment_name TEXT NOT NULL,
      source_id TEXT,
      total_rows REAL DEFAULT 0,
      has_generated INTEGER DEFAULT 0,
      generated_at TEXT,
      location TEXT,
      prefix TEXT,
      interval_n INTEGER,
      rp INTEGER DEFAULT 1,
      digits INTEGER DEFAULT 3,
      rows_n INTEGER DEFAULT 2,
      ck TEXT,
      min_f INTEGER,
      max_f INTEGER,
      rows_col TEXT,
      seeds_col TEXT,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )
  ")

  # 广表：统一所有材料明细
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS unified_materials (
      material_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL,
      experiment_type TEXT NOT NULL,
      fieldid TEXT,
      code TEXT,
      ma TEXT,
      pa TEXT,
      mapa TEXT,
      f INTEGER,
      stageid TEXT,
      name TEXT,
      rows REAL DEFAULT 0,
      line_number TEXT,
      rp INTEGER DEFAULT 1,
      sele REAL DEFAULT 0,
      seeds REAL DEFAULT 0,
      place TEXT,
      next_stage TEXT,
      process TEXT,
      path TEXT,
      created_at TEXT NOT NULL,
      FOREIGN KEY(experiment_id) REFERENCES unified_records(experiment_id) ON DELETE CASCADE
    )
  ")

  # 性状调查表
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS traits_survey (
      survey_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL,
      material_id INTEGER,
      fieldid TEXT,
      code TEXT,
      stageid TEXT,
      name TEXT,
      survey_date TEXT,
      sowing_date TEXT,
      emergence_date TEXT,
      flowering_date TEXT,
      maturity_date TEXT,
      plant_height REAL,
      bottom_pod_height REAL,
      main_stem_nodes INTEGER,
      branches INTEGER,
      leaf_shape TEXT,
      flower_color TEXT,
      pod_setting TEXT,
      lodging TEXT,
      leaf_drop TEXT,
      disease_resistance TEXT,
      pest_resistance TEXT,
      seed_color TEXT,
      seed_size TEXT,
      seed_luster TEXT,
      protein_content REAL,
      oil_content REAL,
      harvest_rows INTEGER,
      harvest_weight REAL,
      seeds_weight REAL,
      notes TEXT,
      created_at TEXT NOT NULL,
      FOREIGN KEY(experiment_id) REFERENCES unified_records(experiment_id) ON DELETE CASCADE
    )
  ")

  # 广表索引
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_unified_exp_id ON unified_records(experiment_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_unified_type ON unified_records(experiment_type)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_unified_mat_exp ON unified_materials(experiment_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_traits_exp ON traits_survey(experiment_id)")

  # 田间记录本表 - 统一Schema（三个表结构相同）
  field_record_schema <- paste0(
    "record_id INTEGER PRIMARY KEY AUTOINCREMENT,",
    "experiment_id TEXT NOT NULL,",
    "experiment_name TEXT NOT NULL,",
    "fieldid TEXT,",
    "id TEXT,",
    "user TEXT,",
    "stageid TEXT,",
    "name TEXT,",
    "ma TEXT,",
    "pa TEXT,",
    "mapa TEXT,",
    "memo TEXT,",
    "stage TEXT,",
    "next_stage TEXT,",
    "f TEXT,",
    "sele REAL,",
    "process TEXT,",
    "path TEXT,",
    "source TEXT,",
    "former_fieldid TEXT,",
    "former_stageid TEXT,",
    "code TEXT,",
    "rp INTEGER,",
    "treatment TEXT,",
    "place TEXT,",
    "rows REAL,",
    "line_number TEXT,",
    "is_ck TEXT,",
    "XiaoQuShiShouMianJi REAL,",
    "XiaoQuChanLiang REAL,",
    "HanShuiLiang REAL,",
    "MuChan REAL,",
    "BoZhongQi TEXT,",
    "ChuMiaoQi TEXT,",
    "ChuMiaoLiangFou TEXT,",
    "MiaoQiTianJianPingJia TEXT,",
    "KaiHuaQi TEXT,",
    "HuaSe TEXT,",
    "HuaQiTianJianPingJia TEXT,",
    "YeXing TEXT,",
    "RongMaoSe TEXT,",
    "ShengZhangXiXing TEXT,",
    "JieJiaXiXing TEXT,",
    "DaoFuXing TEXT,",
    "ZaoShuaiXing TEXT,",
    "ZhuXing TEXT,",
    "LuoYeXing TEXT,",
    "LieJiaXing TEXT,",
    "ChengShuQi TEXT,",
    "HuoGanChengShu TEXT,",
    "ChengShuQiTianJianPingJia TEXT,",
    "ShouHuoQi TEXT,",
    "XiaoQuShouHuoZhuShu INTEGER,",
    "ShengYuQi INTEGER,",
    "TianJianBeiZhu TEXT,",
    "HuaYeBingDuBing TEXT,",
    "NiJingDianZhongFuBing TEXT,",
    "ShuangMeiBing TEXT,",
    "HuiBanBing TEXT,",
    "XiJunXingBanDianBing TEXT,",
    "XiuBing TEXT,",
    "GenFuBing TEXT,",
    "BaoNangXianChongBing TEXT,",
    "QiTaBingHai TEXT,",
    "DouGanHeiQianYing TEXT,",
    "DouJiaMing TEXT,",
    "YaChong TEXT,",
    "ShiYeXingHaiChong TEXT,",
    "KaoZhongZhuShu INTEGER,",
    "ZhuGao REAL,",
    "DiJiaGao REAL,",
    "FenZhiShu INTEGER,",
    "ZhuJingJieShu INTEGER,",
    "JiaXing TEXT,",
    "JiaShuSe TEXT,",
    "YouXiaoJia INTEGER,",
    "WuXiaoJia INTEGER,",
    "DanZhuJiaShu INTEGER,",
    "DanZhuLiShu INTEGER,",
    "DanZhuLiZhong REAL,",
    "MeiJiaLiShu REAL,",
    "LiXing TEXT,",
    "ZhongPiSe TEXT,",
    "QiSe TEXT,",
    "ZiYeSe TEXT,",
    "ZhongPiGuangZe TEXT,",
    "BaiLiZhong REAL,",
    "WanHaoLiLv REAL,",
    "PoSuiLiLv REAL,",
    "BingLiLv REAL,",
    "ZiBanLiLv REAL,",
    "HeBanLiLv REAL,",
    "ShuangMeiLiLv REAL,",
    "HuiBanLiLv REAL,",
    "ChongShiLiLv REAL,",
    "ZiLiPingJia TEXT,",
    "DanBai REAL,",
    "ZhiFang REAL,",
    "DanZhiHe REAL,",
    "CaoGanLinKangXing TEXT,",
    "ShiZhiJianCe TEXT,",
    "HanJiYin TEXT,",
    "BoZhongPenShu INTEGER,",
    "BoZhongLiShu INTEGER,",
    "ChuMiaoShu INTEGER,",
    "ChuMiaoLiShu REAL,",
    "NaiYanXing TEXT,",
    "NaiHanXing TEXT,",
    "ShiHuaQi TEXT,",
    "ZaJiaoHuaShu INTEGER,",
    "ChengHuoJiaShu INTEGER,",
    "ZhaJiaoliShu INTEGER,",
    "ChuShuQi TEXT,",
    "WanShuQi TEXT,",
    "HuiFuLv REAL,",
    "SSRBuHeGeWeiDian TEXT,",
    "created_at TEXT NOT NULL"
  )

  for (tbl in c("population_field_records", "line_selection_field_records", "yield_test_field_records")) {
    DBI::dbExecute(con, paste0("CREATE TABLE IF NOT EXISTS ", tbl, " (", field_record_schema, ")"))
  }

  # 索引
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_pop_field_exp ON population_field_records(experiment_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_line_field_exp ON line_selection_field_records(experiment_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_yield_field_exp ON yield_test_field_records(experiment_id)")

  # 列迁移：添加planting表的完整字段（如果表已存在但缺少这些列）
  new_cols <- c("id", "user", "memo", "stage", "next_stage", "f", "process", "path", "source", "former_fieldid", "former_stageid", "treatment", "is_ck")
  for (tbl in c("population_field_records", "line_selection_field_records", "yield_test_field_records")) {
    for (col in new_cols) {
      tryCatch({
        DBI::dbExecute(con, paste0("ALTER TABLE ", tbl, " ADD COLUMN ", col, " TEXT"))
      }, error = function(e) { message("迁移警告 [", tbl, ".", col, "]: ", e$message) })
    }
  }
}

# 生成唯一ID
generateExpId <- function(prefix = "EXP") {
  ts <- format(Sys.time(), "%Y%m%d%H%M%S")
  paste0(prefix, "_", ts, "_", sample(1000:9999, 1))
}

# ========== 群体记录操作 ==========

# 保存群体记录
savePopulationRecord <- function(experiment_name, materials_df, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  exp_id <- generateExpId("POP")

  # 将完整数据转为JSON存储
  raw_json <- jsonlite::toJSON(materials_df, auto_unbox = TRUE, pretty = FALSE)
  total_rows <- if ("rows" %in% names(materials_df)) sum(materials_df$rows, na.rm = TRUE) else nrow(materials_df)

  DBI::dbExecute(con,
    "INSERT INTO population_records (experiment_id, experiment_name, total_rows, has_generated, raw_data, created_at, updated_at)
     VALUES (?, ?, ?, 0, ?, ?, ?)",
    params = list(exp_id, experiment_name, total_rows, raw_json, now, now)
  )

  list(experiment_id = exp_id, experiment_name = experiment_name, total_rows = total_rows, record_count = nrow(materials_df))
}

# 获取群体记录列表
listPopulationRecords <- function(db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbGetQuery(con, "SELECT * FROM population_records ORDER BY created_at DESC")
}

# 获取群体材料明细
getPopulationMaterials <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  # 从raw_data列读取JSON并解析
  result <- DBI::dbGetQuery(con,
    "SELECT raw_data FROM population_records WHERE experiment_id = ?",
    params = list(experiment_id)
  )

  if (nrow(result) == 0 || is.null(result$raw_data) || is.na(result$raw_data)) {
    return(data.frame())
  }

  json_data <- result$raw_data[1]
  materials <- jsonlite::fromJSON(json_data, simplifyDataFrame = TRUE)
  materials
}

# 标记群体记录已生成
markPopulationGenerated <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con,
    "UPDATE population_records SET has_generated = 1, generated_at = ?, updated_at = ? WHERE experiment_id = ?",
    params = list(now, now, experiment_id)
  )
}

# 重置群体记录生成状态
resetPopulationGenerated <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con,
    "UPDATE population_records SET has_generated = 0, generated_at = NULL, updated_at = ? WHERE experiment_id = ?",
    params = list(now, experiment_id)
  )
}

# 删除群体记录
deletePopulationRecord <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbExecute(con, "DELETE FROM population_records WHERE experiment_id = ?", params = list(experiment_id))
}

# ========== 株行记录操作 ==========

# 保存株行记录
saveLineSelectionRecord <- function(experiment_name, materials_df, source_id = NULL, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  exp_id <- generateExpId("LINE")

  # 将完整数据转为JSON存储
  raw_json <- jsonlite::toJSON(materials_df, auto_unbox = TRUE, pretty = FALSE)
  total_rows <- if ("rows" %in% names(materials_df)) sum(materials_df$rows, na.rm = TRUE) else nrow(materials_df)

  # 根据source_id是否为NULL选择不同的SQL
  if (is.null(source_id)) {
    DBI::dbExecute(con,
      "INSERT INTO line_selection_records (experiment_id, experiment_name, total_rows, has_generated, raw_data, created_at, updated_at)
       VALUES (?, ?, ?, 0, ?, ?, ?)",
      params = list(exp_id, experiment_name, total_rows, raw_json, now, now)
    )
  } else {
    DBI::dbExecute(con,
      "INSERT INTO line_selection_records (experiment_id, source_id, experiment_name, total_rows, has_generated, raw_data, created_at, updated_at)
       VALUES (?, ?, ?, ?, 0, ?, ?, ?)",
      params = list(exp_id, source_id, experiment_name, total_rows, raw_json, now, now)
    )
  }

  list(experiment_id = exp_id, experiment_name = experiment_name, total_rows = total_rows, record_count = nrow(materials_df))
}

# 获取株行记录列表
listLineSelectionRecords <- function(db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbGetQuery(con, "SELECT * FROM line_selection_records ORDER BY created_at DESC")
}

# 获取株行材料明细
getLineSelectionMaterials <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  # 从raw_data列读取JSON并解析
  result <- DBI::dbGetQuery(con,
    "SELECT raw_data FROM line_selection_records WHERE experiment_id = ?",
    params = list(experiment_id)
  )

  if (nrow(result) == 0 || is.null(result$raw_data) || is.na(result$raw_data)) {
    return(data.frame())
  }

  json_data <- result$raw_data[1]
  materials <- jsonlite::fromJSON(json_data, simplifyDataFrame = TRUE)
  materials
}

# 标记株行记录已生成
markLineSelectionGenerated <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con,
    "UPDATE line_selection_records SET has_generated = 1, generated_at = ?, updated_at = ? WHERE experiment_id = ?",
    params = list(now, now, experiment_id)
  )
}

# 重置株行记录生成状态
resetLineSelectionGenerated <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con,
    "UPDATE line_selection_records SET has_generated = 0, generated_at = NULL, updated_at = ? WHERE experiment_id = ?",
    params = list(now, experiment_id)
  )
}

# 删除株行记录
deleteLineSelectionRecord <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbExecute(con, "DELETE FROM line_selection_records WHERE experiment_id = ?", params = list(experiment_id))
}

# ========== 产比记录操作 ==========

# 保存产比记录
saveYieldTestRecord <- function(experiment_name, materials_df, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  exp_id <- generateExpId("YIELD")

  # 将完整数据转为JSON存储
  raw_json <- jsonlite::toJSON(materials_df, auto_unbox = TRUE, pretty = FALSE)
  total_rows <- if ("rows" %in% names(materials_df)) sum(materials_df$rows, na.rm = TRUE) else nrow(materials_df)

  DBI::dbExecute(con,
    "INSERT INTO yield_test_records (experiment_id, experiment_name, total_rows, has_generated, raw_data, created_at, updated_at)
     VALUES (?, ?, ?, 0, ?, ?, ?)",
    params = list(exp_id, experiment_name, total_rows, raw_json, now, now)
  )

  list(experiment_id = exp_id, experiment_name = experiment_name, total_rows = total_rows, record_count = nrow(materials_df))
}

# 获取产比记录列表
listYieldTestRecords <- function(db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbGetQuery(con, "SELECT * FROM yield_test_records ORDER BY created_at DESC")
}

# 获取产比材料明细
getYieldTestMaterials <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  # 从raw_data列读取JSON并解析
  result <- DBI::dbGetQuery(con,
    "SELECT raw_data FROM yield_test_records WHERE experiment_id = ?",
    params = list(experiment_id)
  )

  if (nrow(result) == 0 || is.null(result$raw_data) || is.na(result$raw_data)) {
    return(data.frame())
  }

  json_data <- result$raw_data[1]
  materials <- jsonlite::fromJSON(json_data, simplifyDataFrame = TRUE)
  materials
}

# 标记产比记录已生成
markYieldTestGenerated <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con,
    "UPDATE yield_test_records SET has_generated = 1, generated_at = ?, updated_at = ? WHERE experiment_id = ?",
    params = list(now, now, experiment_id)
  )
}

# 重置产比记录生成状态
resetYieldTestGenerated <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con,
    "UPDATE yield_test_records SET has_generated = 0, generated_at = NULL, updated_at = ? WHERE experiment_id = ?",
    params = list(now, experiment_id)
  )
}

# 删除产比记录
deleteYieldTestRecord <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbExecute(con, "DELETE FROM yield_test_records WHERE experiment_id = ?", params = list(experiment_id))
}

# ========== 田试记录本表操作（planting + 88个性状）==========

# 保存群体田试记录
savePopulationFieldRecord <- function(experiment_id, experiment_name, planting_df, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  if (nrow(planting_df) == 0) {
    return(list(experiment_id = experiment_id, record_count = 0))
  }

  planting_df$experiment_id <- experiment_id
  planting_df$experiment_name <- experiment_name
  planting_df$created_at <- now

  cols_to_keep <- FIELD_RECORD_COLS

  cols_exist <- cols_to_keep[cols_to_keep %in% names(planting_df)]
  df_to_save <- planting_df[, cols_exist, drop = FALSE]

  DBI::dbWriteTable(con, "population_field_records", df_to_save, append = TRUE)

  list(experiment_id = experiment_id, record_count = nrow(planting_df))
}

# 保存株行田试记录
saveLineSelectionFieldRecord <- function(experiment_id, experiment_name, planting_df, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  planting_df$experiment_id <- experiment_id
  planting_df$experiment_name <- experiment_name
  planting_df$created_at <- now

  cols_to_keep <- FIELD_RECORD_COLS

  cols_exist <- cols_to_keep[cols_to_keep %in% names(planting_df)]
  df_to_save <- planting_df[, cols_exist, drop = FALSE]

  DBI::dbWriteTable(con, "line_selection_field_records", df_to_save, append = TRUE)

  list(experiment_id = experiment_id, record_count = nrow(planting_df))
}

# 保存产比田试记录
saveYieldTestFieldRecord <- function(experiment_id, experiment_name, planting_df, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  if (nrow(planting_df) == 0) {
    return(list(experiment_id = experiment_id, record_count = 0))
  }

  planting_df$experiment_id <- experiment_id
  planting_df$experiment_name <- experiment_name
  planting_df$created_at <- now

  cols_to_keep <- FIELD_RECORD_COLS

  cols_exist <- cols_to_keep[cols_to_keep %in% names(planting_df)]
  df_to_save <- planting_df[, cols_exist, drop = FALSE]

  DBI::dbWriteTable(con, "yield_test_field_records", df_to_save, append = TRUE)

  list(experiment_id = experiment_id, record_count = nrow(planting_df))
}

# 获取群体田试记录
getPopulationFieldRecord <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbGetQuery(con, "SELECT * FROM population_field_records WHERE experiment_id = ? ORDER BY rowid", params = list(experiment_id))
}

# 获取株行田试记录
getLineSelectionFieldRecord <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbGetQuery(con, "SELECT * FROM line_selection_field_records WHERE experiment_id = ? ORDER BY rowid", params = list(experiment_id))
}

# 获取产比田试记录
getYieldTestFieldRecord <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbGetQuery(con, "SELECT * FROM yield_test_field_records WHERE experiment_id = ? ORDER BY rowid", params = list(experiment_id))
}

# 删除群体田试记录
deletePopulationFieldRecord <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbExecute(con, "DELETE FROM population_field_records WHERE experiment_id = ?", params = list(experiment_id))
}

# 获取所有已生成的群体田试记录
getAllPopulationFieldRecords <- function(db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  records <- DBI::dbGetQuery(con,
    "SELECT experiment_id, experiment_name FROM population_records WHERE has_generated = 1 ORDER BY generated_at DESC"
  )

  if (nrow(records) == 0) {
    return(list())
  }

  result <- list()
  for (i in 1:nrow(records)) {
    exp_id <- records$experiment_id[i]
    exp_name <- records$experiment_name[i]
    field_data <- DBI::dbGetQuery(con,
      "SELECT * FROM population_field_records WHERE experiment_id = ? ORDER BY rowid",
      params = list(exp_id)
    )
    if (nrow(field_data) > 0) {
      result[[exp_name]] <- field_data
    }
  }

  result
}

# 删除株行田试记录
deleteLineSelectionFieldRecord <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbExecute(con, "DELETE FROM line_selection_field_records WHERE experiment_id = ?", params = list(experiment_id))
}

# 获取所有已生成的株行田试记录
getAllLineSelectionFieldRecords <- function(db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  records <- DBI::dbGetQuery(con,
    "SELECT experiment_id, experiment_name FROM line_selection_records WHERE has_generated = 1 ORDER BY generated_at DESC"
  )

  if (nrow(records) == 0) {
    return(list())
  }

  result <- list()
  for (i in 1:nrow(records)) {
    exp_id <- records$experiment_id[i]
    exp_name <- records$experiment_name[i]
    field_data <- DBI::dbGetQuery(con,
      "SELECT * FROM line_selection_field_records WHERE experiment_id = ? ORDER BY rowid",
      params = list(exp_id)
    )
    if (nrow(field_data) > 0) {
      result[[exp_name]] <- field_data
    }
  }

  result
}

# 获取所有已生成的产比田试记录
getAllYieldTestFieldRecords <- function(db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  # 获取所有已生成的记录
  records <- DBI::dbGetQuery(con,
    "SELECT experiment_id, experiment_name FROM yield_test_records WHERE has_generated = 1 ORDER BY generated_at DESC"
  )

  if (nrow(records) == 0) {
    return(list())
  }

  result <- list()
  for (i in 1:nrow(records)) {
    exp_id <- records$experiment_id[i]
    exp_name <- records$experiment_name[i]
    field_data <- DBI::dbGetQuery(con,
      "SELECT * FROM yield_test_field_records WHERE experiment_id = ? ORDER BY rowid",
      params = list(exp_id)
    )
    if (nrow(field_data) > 0) {
      result[[exp_name]] <- field_data
    }
  }

  result
}

# 删除产比田试记录
deleteYieldTestFieldRecord <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbExecute(con, "DELETE FROM yield_test_field_records WHERE experiment_id = ?", params = list(experiment_id))
}

# ========== 广表操作（统一所有记录）==========

# 保存到广表（统一记录）
saveToUnifiedTable <- function(experiment_id, experiment_type, experiment_name, materials_df,
                               source_id = NULL, location = NULL, prefix = NULL,
                               interval_n = NULL, rp = 1, digits = 3, rows_n = 2, ck = NULL,
                               min_f = NULL, max_f = NULL, rows_col = NULL, seeds_col = NULL,
                               db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  DBI::dbWithTransaction(con, {
    total_rows <- if ("rows" %in% names(materials_df)) sum(materials_df$rows, na.rm = TRUE) else 0

    DBI::dbExecute(con,
      "INSERT INTO unified_records (experiment_id, experiment_type, experiment_name, source_id, total_rows,
       has_generated, location, prefix, interval_n, rp, digits, rows_n, ck, min_f, max_f, rows_col, seeds_col, created_at, updated_at)
       VALUES (?, ?, ?, ?, ?, 0, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(experiment_id, experiment_type, experiment_name, source_id, total_rows,
                    location, prefix, interval_n, rp, digits, rows_n, ck, min_f, max_f, rows_col, seeds_col, now, now)
    )

    # 保存材料到广表
    if (nrow(materials_df) > 0) {
      materials_df$experiment_id <- experiment_id
      materials_df$experiment_type <- experiment_type
      materials_df$created_at <- now
      cols <- c("experiment_id", "experiment_type", "fieldid", "code", "ma", "pa", "mapa", "f",
                "stageid", "name", "rows", "line_number", "rp", "sele", "seeds", "place",
                "next_stage", "process", "path", "created_at")
      cols_exist <- cols[cols %in% names(materials_df)]
      DBI::dbWriteTable(con, "unified_materials", materials_df[, cols_exist, drop = FALSE], append = TRUE)
    }
  })

  list(experiment_id = experiment_id, experiment_name = experiment_name, total_rows = total_rows)
}

# 获取广表所有记录
listUnifiedRecords <- function(experiment_type = NULL, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  if (is.null(experiment_type)) {
    DBI::dbGetQuery(con, "SELECT * FROM unified_records ORDER BY created_at DESC")
  } else {
    DBI::dbGetQuery(con,
      "SELECT * FROM unified_records WHERE experiment_type = ? ORDER BY created_at DESC",
      params = list(experiment_type)
    )
  }
}

# 获取广表材料明细
getUnifiedMaterials <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbGetQuery(con, "SELECT * FROM unified_materials WHERE experiment_id = ?", params = list(experiment_id))
}

# 标记广表记录已生成
markUnifiedGenerated <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  DBI::dbExecute(con,
    "UPDATE unified_records SET has_generated = 1, generated_at = ?, updated_at = ? WHERE experiment_id = ?",
    params = list(now, now, experiment_id)
  )
}

# 删除广表记录
deleteUnifiedRecord <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbExecute(con, "DELETE FROM unified_records WHERE experiment_id = ?", params = list(experiment_id))
  DBI::dbExecute(con, "DELETE FROM unified_materials WHERE experiment_id = ?", params = list(experiment_id))
}

# ========== 性状调查表操作 ==========

# 保存性状调查数据
saveTraitsSurvey <- function(experiment_id, traits_df, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  if (nrow(traits_df) > 0) {
    traits_df$experiment_id <- experiment_id
    traits_df$created_at <- now

    # 确保必要列存在
    if (!"material_id" %in% names(traits_df)) traits_df$material_id <- NA
    if (!"code" %in% names(traits_df)) traits_df$code <- NA
    if (!"stageid" %in% names(traits_df)) traits_df$stageid <- NA
    if (!"name" %in% names(traits_df)) traits_df$name <- NA

    cols <- c("experiment_id", "material_id", "fieldid", "code", "stageid", "name",
              "survey_date", "sowing_date", "emergence_date", "flowering_date", "maturity_date",
              "plant_height", "bottom_pod_height", "main_stem_nodes", "branches",
              "leaf_shape", "flower_color", "pod_setting", "lodging", "leaf_drop",
              "disease_resistance", "pest_resistance", "seed_color", "seed_size",
              "seed_luster", "protein_content", "oil_content", "harvest_rows",
              "harvest_weight", "seeds_weight", "notes", "created_at")
    cols_exist <- cols[cols %in% names(traits_df)]
    DBI::dbWriteTable(con, "traits_survey", traits_df[, cols_exist, drop = FALSE], append = TRUE)
  }

  list(experiment_id = experiment_id, record_count = nrow(traits_df))
}

# 获取性状调查数据
getTraitsSurvey <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbGetQuery(con, "SELECT * FROM traits_survey WHERE experiment_id = ?", params = list(experiment_id))
}

# 获取所有性状调查数据（用于导出）
getAllTraitsSurvey <- function(db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbGetQuery(con,
    "SELECT t.*, r.experiment_name, r.experiment_type, r.location, r.created_at as record_created
     FROM traits_survey t
     LEFT JOIN unified_records r ON t.experiment_id = r.experiment_id
     ORDER BY t.created_at DESC"
  )
}

# 删除性状调查数据
deleteTraitsSurvey <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  DBI::dbExecute(con, "DELETE FROM traits_survey WHERE experiment_id = ?", params = list(experiment_id))
}

# 同步旧表数据到广表（一次性迁移）
syncToUnifiedTable <- function(db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  synced <- 0

  # 同步群体记录
  pop_records <- DBI::dbGetQuery(con, "SELECT * FROM population_records")
  for (i in 1:nrow(pop_records)) {
    rec <- pop_records[i, ]
    # 检查是否已存在
    existing <- DBI::dbGetQuery(con,
      "SELECT 1 FROM unified_records WHERE experiment_id = ?",
      params = list(rec$experiment_id)
    )
    if (nrow(existing) == 0) {
      DBI::dbExecute(con,
        "INSERT INTO unified_records (experiment_id, experiment_type, experiment_name, source_id, total_rows, has_generated, generated_at, location, created_at, updated_at)
         VALUES (?, 'population', ?, NULL, ?, ?, ?, '', ?, ?)",
        params = list(rec$experiment_id, rec$experiment_name, rec$total_rows, rec$has_generated, rec$generated_at, rec$created_at, rec$updated_at)
      )
      synced <- synced + 1
    }
  }

  # 同步株行记录
  line_records <- DBI::dbGetQuery(con, "SELECT * FROM line_selection_records")
  for (i in 1:nrow(line_records)) {
    rec <- line_records[i, ]
    existing <- DBI::dbGetQuery(con,
      "SELECT 1 FROM unified_records WHERE experiment_id = ?",
      params = list(rec$experiment_id)
    )
    if (nrow(existing) == 0) {
      DBI::dbExecute(con,
        "INSERT INTO unified_records (experiment_id, experiment_type, experiment_name, source_id, total_rows, has_generated, generated_at, created_at, updated_at)
         VALUES (?, 'line_selection', ?, ?, ?, ?, ?, ?, ?)",
        params = list(rec$experiment_id, rec$experiment_name, rec$source_id, rec$total_rows, rec$has_generated, rec$generated_at, rec$created_at, rec$updated_at)
      )
      synced <- synced + 1
    }
  }

  # 同步产比记录
  yield_records <- DBI::dbGetQuery(con, "SELECT * FROM yield_test_records")
  for (i in 1:nrow(yield_records)) {
    rec <- yield_records[i, ]
    existing <- DBI::dbGetQuery(con,
      "SELECT 1 FROM unified_records WHERE experiment_id = ?",
      params = list(rec$experiment_id)
    )
    if (nrow(existing) == 0) {
      DBI::dbExecute(con,
        "INSERT INTO unified_records (experiment_id, experiment_type, experiment_name, source_id, total_rows, has_generated, generated_at, location, created_at, updated_at)
         VALUES (?, 'yield_test', ?, NULL, ?, ?, ?, '', ?, ?)",
        params = list(rec$experiment_id, rec$experiment_name, rec$total_rows, rec$has_generated, rec$generated_at, rec$created_at, rec$updated_at)
      )
      synced <- synced + 1
    }
  }

  list(synced_count = synced, message = paste("已同步", synced, "条记录到广表"))
}

# =============================================================================
# Designplot 种植试验相关表和导入功能
# 从试验管理导入：读取已有试验的 planting 数据，转换为 designplot 格式
# =============================================================================

# ---- Designplot 表初始化 ----
initDesignplotTables <- function(con) {
  # designplot 专用试验表
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS designplot_experiments (
      experiment_id TEXT PRIMARY KEY,
      experiment_name TEXT NOT NULL,
      source_type TEXT CHECK(source_type IN ('population', 'line_selection', 'yield_test')),
      source_id TEXT,
      total_rows REAL DEFAULT 0,
      created_at TEXT NOT NULL,
      updated_at TEXT NOT NULL
    )
  ")

  # designplot 专用试验记录表（与 designplot2026 兼容）
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS designplot_experiment_records (
      record_id INTEGER PRIMARY KEY AUTOINCREMENT,
      experiment_id TEXT NOT NULL,
      fieldid TEXT,
      id TEXT,
      stageid TEXT,
      name TEXT,
      former_fieldid TEXT,
      former_stageid TEXT,
      source TEXT,
      code TEXT,
      rp TEXT,
      rows REAL,
      line_number TEXT,
      place TEXT,
      created_at TEXT NOT NULL,
      FOREIGN KEY(experiment_id) REFERENCES designplot_experiments(experiment_id) ON DELETE CASCADE
    )
  ")

  # 索引
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_dp_exp_id ON designplot_experiment_records(experiment_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_dp_exp_name ON designplot_experiments(experiment_name)")

  # 迁移：为已有表添加 source_type 和 source_id 列（如果不存在）
  tryCatch({
    DBI::dbExecute(con, "ALTER TABLE designplot_experiments ADD COLUMN source_type TEXT CHECK(source_type IN ('population', 'line_selection', 'yield_test'))")
  }, error = function(e) { message("迁移警告 [source_type]: ", e$message) })
  tryCatch({
    DBI::dbExecute(con, "ALTER TABLE designplot_experiments ADD COLUMN source_id TEXT")
  }, error = function(e) { message("迁移警告 [source_id]: ", e$message) })
  # 迁移：为 designplot_experiment_records 添加 place 列（如果不存在）
  tryCatch({
    DBI::dbExecute(con, "ALTER TABLE designplot_experiment_records ADD COLUMN place TEXT")
  }, error = function(e) { message("迁移警告 [place]: ", e$message) })
  # 迁移：为 designplot_experiment_records 添加 former_fieldid 列（如果不存在）
  tryCatch({
    DBI::dbExecute(con, "ALTER TABLE designplot_experiment_records ADD COLUMN former_fieldid TEXT")
  }, error = function(e) { message("迁移警告 [former_fieldid]: ", e$message) })
}

# ---- 从试验管理导入单个试验 ----
importExperimentToDesignplot <- function(source_experiment_id, source_type = c("population", "line_selection", "yield_test"),
                                        location_filter = NULL,
                                        db_path = defaultDbPath()) {
  source_type <- match.arg(source_type)
  source_experiment_id <- trimws(as.character(source_experiment_id))
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  initDesignplotTables(con)

  field_table <- switch(source_type,
    population = "population_field_records",
    line_selection = "line_selection_field_records",
    yield_test = "yield_test_field_records"
  )

  if (!is.null(location_filter) && nzchar(trimws(location_filter))) {
    records <- DBI::dbGetQuery(con,
      sprintf("SELECT * FROM %s WHERE experiment_id = ? AND TRIM(place) = ? ORDER BY rowid", field_table),
      params = list(source_experiment_id, trimws(location_filter))
    )
  } else {
    records <- DBI::dbGetQuery(con,
      sprintf("SELECT * FROM %s WHERE experiment_id = ? ORDER BY rowid", field_table),
      params = list(source_experiment_id)
    )
  }

  if (nrow(records) == 0) {
    if (!is.null(location_filter) && nzchar(trimws(location_filter))) {
      return(list(success = FALSE, message = sprintf("该试验在地点“%s”下没有可导入的田试记录", trimws(location_filter))))
    }
    return(list(success = FALSE, message = "该试验没有田试记录，请先在对应模块生成记录本"))
  }

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  new_exp_id <- paste0("DP_", source_experiment_id)
  overwrite_existing <- tryCatch(designplotExperimentExists(new_exp_id, db_path), error = function(e) FALSE)
  reset_result <- if (overwrite_existing) {
    tryCatch(resetImportedDesignplotExperiment(new_exp_id, db_path), error = function(e) stop(paste0("重置原有种植状态失败: ", e$message)))
  } else {
    list(reset = FALSE, runs_cleared = 0L, field_count = 0L, fields = character(0), message = "")
  }

  exp_name_query <- switch(source_type,
    population = "SELECT experiment_name FROM population_records WHERE experiment_id = ?",
    line_selection = "SELECT experiment_name FROM line_selection_records WHERE experiment_id = ?",
    yield_test = "SELECT experiment_name FROM yield_test_records WHERE experiment_id = ?"
  )
  exp_name_result <- DBI::dbGetQuery(con, exp_name_query, params = list(source_experiment_id))
  exp_name <- if (nrow(exp_name_result) > 0) exp_name_result$experiment_name[1] else source_experiment_id

  total_rows <- if ("rows" %in% names(records)) sum(as.numeric(records$rows), na.rm = TRUE) else nrow(records)

  DBI::dbWithTransaction(con, {
    DBI::dbExecute(con,
      "INSERT OR REPLACE INTO designplot_experiments (experiment_id, experiment_name, source_type, source_id, total_rows, created_at, updated_at)
       VALUES (?, ?, ?, ?, ?, ?, ?)",
      params = list(new_exp_id, exp_name, source_type, source_experiment_id, total_rows, now, now)
    )

    DBI::dbExecute(con, "DELETE FROM designplot_experiment_records WHERE experiment_id = ?", params = list(new_exp_id))

    dp_records <- records[, c("fieldid", "id", "stageid", "name", "former_fieldid", "former_stageid", "source", "code", "rp", "rows", "line_number", "place"), drop = FALSE]
    dp_records$experiment_id <- new_exp_id
    dp_records$created_at <- now

    for (col in names(dp_records)) {
      if (is.character(dp_records[[col]]) || is.factor(dp_records[[col]])) {
        dp_records[[col]] <- as.character(dp_records[[col]])
        dp_records[[col]][is.na(dp_records[[col]])] <- ""
      }
    }

    DBI::dbWriteTable(con, "designplot_experiment_records", dp_records, append = TRUE)
  })

  list(
    success = TRUE,
    experiment_id = new_exp_id,
    experiment_name = exp_name,
    source_type = source_type,
    record_count = nrow(records),
    total_rows = total_rows,
    overwritten = overwrite_existing,
    planting_reset = isTRUE(reset_result$reset),
    reset_fields = reset_result$fields %||% character(0),
    message = if (!is.null(location_filter) && nzchar(trimws(location_filter))) {
      if (overwrite_existing && isTRUE(reset_result$reset)) {
        sprintf("已覆盖导入：%s（%s，地点：%s），%d 条记录；原种植状态已重置，请重新生成所种地块。", exp_name, source_type, trimws(location_filter), nrow(records))
      } else if (overwrite_existing) {
        sprintf("已覆盖导入：%s（%s，地点：%s），%d 条记录。", exp_name, source_type, trimws(location_filter), nrow(records))
      } else {
        sprintf("导入成功：%s（%s，地点：%s），%d 条记录", exp_name, source_type, trimws(location_filter), nrow(records))
      }
    } else {
      if (overwrite_existing && isTRUE(reset_result$reset)) {
        sprintf("已覆盖导入：%s（%s），%d 条记录；原种植状态已重置，请重新生成所种地块。", exp_name, source_type, nrow(records))
      } else if (overwrite_existing) {
        sprintf("已覆盖导入：%s（%s），%d 条记录。", exp_name, source_type, nrow(records))
      } else {
        sprintf("导入成功：%s（%s），%d 条记录", exp_name, source_type, nrow(records))
      }
    }
  )
}

# ---- 获取所有可用地点列表（从田试记录表去重）----
getAvailableLocations <- function(db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)

  all_places <- character(0)

  for (field_table in c("population_field_records", "line_selection_field_records", "yield_test_field_records")) {
    tryCatch({
      places <- DBI::dbGetQuery(con,
        sprintf("SELECT DISTINCT place FROM %s WHERE place IS NOT NULL AND place != '' ORDER BY place", field_table)
      )
      if (nrow(places) > 0) {
        all_places <- c(all_places, places$place)
      }
    }, error = function(e) { message("查询地点失败 [", field_table, "]: ", e$message) })
  }

  unique(all_places)
}

# ---- 从试验管理导入全部试验（按地点过滤）----
importAllExperimentsToDesignplot <- function(location_filter = NULL, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  initDesignplotTables(con)

  results <- list()

  for (source_type in c("population", "line_selection", "yield_test")) {
    records_table <- switch(source_type,
      population = "population_records",
      line_selection = "line_selection_records",
      yield_test = "yield_test_records"
    )
    field_table <- switch(source_type,
      population = "population_field_records",
      line_selection = "line_selection_field_records",
      yield_test = "yield_test_field_records"
    )

    # 基础查询：已生成的实验
    query <- sprintf("SELECT experiment_id FROM %s WHERE has_generated = 1", records_table)

    if (!is.null(location_filter) && nzchar(trimws(location_filter))) {
      # 按地点过滤：只选该地点的实验
      query <- sprintf(
        "SELECT DISTINCT r.experiment_id FROM %s r
         INNER JOIN %s f ON r.experiment_id = f.experiment_id
         WHERE r.has_generated = 1 AND f.place = ?",
        records_table, field_table
      )
      experiments <- DBI::dbGetQuery(con, query, params = list(location_filter))
    } else {
      experiments <- DBI::dbGetQuery(con, query)
    }

    for (exp_id in experiments$experiment_id) {
      result <- importExperimentToDesignplot(
        source_experiment_id = exp_id,
        source_type = source_type,
        location_filter = location_filter,
        db_path = db_path
      )
      results[[length(results) + 1]] <- result
    }
  }

  success_count <- sum(sapply(results, function(r) r$success))
  loc_msg <- if (!is.null(location_filter) && nzchar(trimws(location_filter))) {
    sprintf("（地点：%s）", location_filter)
  } else {
    ""
  }
  list(
    success = (success_count > 0),
    total_imported = success_count,
    results = results,
    message = sprintf("共成功导入 %d 个试验 %s", success_count, loc_msg)
  )
}

# ---- 获取 Designplot 试验列表 ----
listDesignplotExperiments <- function(db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  initDesignplotTables(con)

  experiments <- DBI::dbGetQuery(con,
    "SELECT experiment_id, experiment_name, source_type, source_id, total_rows, created_at
     FROM designplot_experiments ORDER BY created_at DESC"
  )

  if (!is.data.frame(experiments) || nrow(experiments) == 0) {
    return(data.frame(
      experiment_id = character(),
      experiment_name = character(),
      source_type = character(),
      source_id = character(),
      total_rows = numeric(),
      created_at = character(),
      stringsAsFactors = FALSE
    ))
  }

  experiments
}

# ---- 获取 Designplot 试验记录 ----
getDesignplotExperimentRecords <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  initDesignplotTables(con)

  records <- DBI::dbGetQuery(con,
    "SELECT * FROM designplot_experiment_records WHERE experiment_id = ? ORDER BY record_id",
    params = list(experiment_id)
  )

  records
}

# ---- 删除 Designplot 试验 ----
deleteDesignplotExperiment <- function(experiment_id, db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  initDesignplotTables(con)

  deleted <- DBI::dbExecute(con,
    "DELETE FROM designplot_experiments WHERE experiment_id = ?",
    params = list(experiment_id)
  )

  invisible(deleted)
}
