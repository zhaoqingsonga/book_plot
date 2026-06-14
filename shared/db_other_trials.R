# =============================================================================
# 其他试验独立数据库模块
# 字段名与 yield_test_field_records (FIELD_RECORD_COLS) 对齐
# =============================================================================

library(DBI)
library(RSQLite)

OTHER_TRIALS_DB <- "data/other_trials.sqlite"

connectOtherDb <- function(db_path = OTHER_TRIALS_DB) {
  data_dir <- dirname(db_path)
  if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), dbname = db_path)
  DBI::dbExecute(con, "PRAGMA journal_mode = WAL")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  con
}

initOtherDb <- function(db_path = OTHER_TRIALS_DB) {
  con <- connectOtherDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # 表结构与 FIELD_RECORD_COLS 对齐
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS other_trial_data (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      import_batch_id TEXT NOT NULL,
      trial_name TEXT,
      group_label TEXT,
      import_time TEXT,
      name TEXT NOT NULL,
      place TEXT NOT NULL,
      MuChan REAL,
      XiaoQuChanLiang REAL,
      XiaoQuShiShouMianJi REAL,
      HanShuiLiang REAL,
      stageid TEXT,
      rp TEXT,
      treatment TEXT,
      is_ck INTEGER DEFAULT 0,
      ma TEXT,
      pa TEXT,
      BoZhongQi TEXT,
      ChuMiaoQi TEXT,
      ChuMiaoLiangFou TEXT,
      MiaoQiTianJianPingJia TEXT,
      KaiHuaQi TEXT,
      HuaSe TEXT,
      HuaQiTianJianPingJia TEXT,
      YeXing TEXT,
      RongMaoSe TEXT,
      ShengZhangXiXing TEXT,
      JieJiaXiXing TEXT,
      DaoFuXing TEXT,
      ZaoShuaiXing TEXT,
      ZhuXing TEXT,
      LuoYeXing TEXT,
      LieJiaXing TEXT,
      ChengShuQi TEXT,
      HuoGanChengShu TEXT,
      ChengShuQiTianJianPingJia TEXT,
      ShouHuoQi TEXT,
      XiaoQuShouHuoZhuShu REAL,
      ShengYuQi REAL,
      TianJianBeiZhu TEXT,
      HuaYeBingDuBing TEXT,
      NiJingDianZhongFuBing TEXT,
      ShuangMeiBing TEXT,
      HuiBanBing TEXT,
      XiJunXingBanDianBing TEXT,
      XiuBing TEXT,
      GenFuBing TEXT,
      BaoNangXianChongBing TEXT,
      QiTaBingHai TEXT,
      DouGanHeiQianYing TEXT,
      DouJiaMing TEXT,
      YaChong TEXT,
      ShiYeXingHaiChong TEXT,
      KaoZhongZhuShu REAL,
      ZhuGao REAL,
      DiJiaGao REAL,
      FenZhiShu REAL,
      ZhuJingJieShu REAL,
      JiaXing TEXT,
      JiaShuSe TEXT,
      YouXiaoJia REAL,
      WuXiaoJia REAL,
      DanZhuJiaShu REAL,
      DanZhuLiShu REAL,
      DanZhuLiZhong REAL,
      MeiJiaLiShu REAL,
      LiXing TEXT,
      ZhongPiSe TEXT,
      QiSe TEXT,
      ZiYeSe TEXT,
      ZhongPiGuangZe TEXT,
      BaiLiZhong REAL,
      WanHaoLiLv REAL,
      PoSuiLiLv REAL,
      BingLiLv REAL,
      ZiBanLiLv REAL,
      HeBanLiLv REAL,
      ShuangMeiLiLv REAL,
      HuiBanLiLv REAL,
      ChongShiLiLv REAL,
      ZiLiPingJia TEXT,
      DanBai REAL,
      ZhiFang REAL,
      DanZhiHe REAL,
      CaoGanLinKangXing TEXT,
      ShiZhiJianCe TEXT,
      HanJiYin TEXT,
      BoZhongPenShu REAL,
      BoZhongLiShu REAL,
      ChuMiaoShu REAL,
      ChuMiaoLiShu REAL,
      NaiYanXing TEXT,
      NaiHanXing TEXT,
      ShiHuaQi TEXT,
      ZaJiaoHuaShu REAL,
      ChengHuoJiaShu REAL,
      ZhaJiaoliShu REAL,
      ChuShuQi TEXT,
      WanShuQi TEXT,
      HuiFuLv REAL,
      SSRBuHeGeWeiDian TEXT,
      extra_cols TEXT
    )
  ")

  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS other_trial_batches (
      import_batch_id TEXT PRIMARY KEY,
      file_name TEXT,
      sheet_name TEXT,
      trial_name TEXT,
      group_label TEXT,
      import_time TEXT,
      row_count INTEGER,
      site_count INTEGER,
      variety_count INTEGER,
      column_mapping TEXT
    )
  ")

  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_other_batch ON other_trial_data(import_batch_id)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_other_name ON other_trial_data(name)")
  DBI::dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_other_place ON other_trial_data(place)")
}

generateBatchId <- function() {
  paste0("OT_", format(Sys.time(), "%Y%m%d%H%M%S"), "_", sample(1000:9999, 1))
}

saveOtherTrialData <- function(df, batch_id, file_name, sheet_name, trial_name,
                                group_label, column_mapping, db_path = OTHER_TRIALS_DB) {
  con <- connectOtherDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initOtherDb(db_path)

  now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  df$import_batch_id <- batch_id
  df$import_time <- now

  DBI::dbExecute(con, "BEGIN TRANSACTION")
  tryCatch({
    DBI::dbWriteTable(con, "other_trial_data", df, append = TRUE)

    mapping_json <- jsonlite::toJSON(column_mapping, auto_unbox = TRUE)
    DBI::dbExecute(con,
      "INSERT INTO other_trial_batches (import_batch_id, file_name, sheet_name,
       trial_name, group_label, import_time, row_count, site_count,
       variety_count, column_mapping)
       VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
      params = list(batch_id, file_name, sheet_name, trial_name, group_label,
                    now, nrow(df),
                    length(unique(df$place)),
                    length(unique(df$name)),
                    mapping_json)
    )

    DBI::dbExecute(con, "COMMIT")
  }, error = function(e) {
    DBI::dbExecute(con, "ROLLBACK")
    stop(e)
  })

  list(
    batch_id      = batch_id,
    row_count     = nrow(df),
    site_count    = length(unique(df$place)),
    variety_count = length(unique(df$name))
  )
}

listOtherTrialBatches <- function(db_path = OTHER_TRIALS_DB) {
  con <- connectOtherDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initOtherDb(db_path)
  DBI::dbGetQuery(con, "SELECT * FROM other_trial_batches ORDER BY import_time DESC")
}

getOtherTrialData <- function(batch_id, db_path = OTHER_TRIALS_DB) {
  con <- connectOtherDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initOtherDb(db_path)
  DBI::dbGetQuery(con, "SELECT * FROM other_trial_data WHERE import_batch_id = ?",
                  params = list(batch_id))
}

deleteOtherTrialBatch <- function(batch_id, db_path = OTHER_TRIALS_DB) {
  con <- connectOtherDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  initOtherDb(db_path)
  DBI::dbExecute(con, "BEGIN TRANSACTION")
  tryCatch({
    DBI::dbExecute(con, "DELETE FROM other_trial_data WHERE import_batch_id = ?",
                   params = list(batch_id))
    DBI::dbExecute(con, "DELETE FROM other_trial_batches WHERE import_batch_id = ?",
                   params = list(batch_id))
    DBI::dbExecute(con, "COMMIT")
  }, error = function(e) {
    DBI::dbExecute(con, "ROLLBACK")
    stop(e)
  })
}
