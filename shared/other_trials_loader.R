# ==============================================================================
# 其他试验库（other_trials.sqlite）读取层
#
# 背景
# ----
# book_plot 有两个数据库：
#   · data/field_book.sqlite    —— 田间记录本（产比 / 群体 / 株行），报告生成器原先只认这个
#   · data/other_trials.sqlite  —— 「其他试验」导入库（黄淮多点鉴定 / 黄淮高油 / 安徽品比 / N-5.2 10点 等）
#
# other_trials 是**用户从 Excel 导入**的外部试验数据（国家/省级区试、多点鉴定等），
# 表结构与田间记录本完全不同：
#   other_trial_batches —— 导入批次元信息（源文件 / sheet / 试验名 / 行数 / 点数 / 品种数 / 列映射）
#   other_trial_data    —— 实际数据，99 个字段（产量 / 物候 / 形态 / 抗性 / 考种 / 品质）
#
# 本文件的职责
# ------------
# 把 other_trial_data 的一批记录，**翻译成 yield_test_field_records 的列结构**，
# 这样 book_plot 现成的分析管线 run_analysis() 与报告生成器可以直接吃，无需复制一套算数逻辑。
#
# ⚠️ 关键设计约束
# ---------------
# 1. **只读**：全程 immutable=1，绝不写 other_trials.sqlite。
# 2. **stageid 不是品种 ID**：other_trials 里 stageid 是「1,2,3…」的序号，
#    真正的品种标识是 `name`。所以翻译后把 name 同时写进 stageid 与 name 两列，
#    保证管线的去重 / 分组口径正确（详见 skill 坑 #25）。
# 3. **多点**：同一品种在多个 place 各一行，pipeline 天然支持
#    （compute_derived_yield_columns 按 place+rp 分组算对照）。
# 4. **对照**：is_ck 已经是 0/1，但名字带「（CK）」后缀，需保留原样供报告显示。
# ==============================================================================

#' other_trials 库的默认路径
#' @noRd
otherTrialsDbPath <- function() {
  file.path("data", "other_trials.sqlite")
}

#' 列出 other_trials 里可分析的导入批次
#'
#' @param db_path other_trials.sqlite 路径
#' @return data.frame（批次元信息 + 产量非空行数）
#' @export
listOtherTrialsBatches <- function(db_path = otherTrialsDbPath()) {
  if (!file.exists(db_path)) return(NULL)
  con <- connectReadOnly(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  batches <- DBI::dbGetQuery(con, "
    SELECT b.import_batch_id, b.trial_name, b.group_label, b.file_name, b.sheet_name,
           b.import_time, b.row_count, b.site_count, b.variety_count,
           (SELECT COUNT(*) FROM other_trial_data d
             WHERE d.import_batch_id = b.import_batch_id AND d.MuChan IS NOT NULL) AS yield_n,
           (SELECT COUNT(DISTINCT d.place) FROM other_trial_data d
             WHERE d.import_batch_id = b.import_batch_id AND d.MuChan IS NOT NULL) AS yield_sites
    FROM other_trial_batches b
    ORDER BY b.import_time DESC")
  as.data.frame(batches, stringsAsFactors = FALSE)
}

#' 读入一个导入批次的原始数据
#'
#' @param batch_id import_batch_id（如 OT_20260614153639_8880）
#' @param db_path other_trials.sqlite 路径
#' @param place 可选，只取某个地点
#' @return data.frame（other_trial_data 原样列）
#' @export
readOtherTrialBatch <- function(batch_id, db_path = otherTrialsDbPath(), place = NULL) {
  if (!file.exists(db_path)) stop("找不到 other_trials 库：", db_path)
  con <- connectReadOnly(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  df <- DBI::dbGetQuery(con,
    "SELECT * FROM other_trial_data WHERE import_batch_id = ? ORDER BY id",
    params = list(batch_id))
  if (!is.null(place)) {
    df <- df[!is.na(df$place) & df$place == place, , drop = FALSE]
  }
  df
}

#' 把 other_trials 的一批数据翻译成 yield_test_field_records 的列结构
#'
#' 供 run_analysis() / 报告生成器直接消费。
#'
#' @param batch_id import_batch_id
#' @param db_path other_trials.sqlite 路径
#' @param place 可选，只取某个地点（单点分析用）
#' @return data.frame，列名与 yield_test_field_records 兼容
#' @export
translateOtherTrial <- function(batch_id, db_path = otherTrialsDbPath(), place = NULL) {
  raw <- readOtherTrialBatch(batch_id, db_path = db_path, place = place)
  if (is.null(raw) || nrow(raw) == 0) stop("批次 ", batch_id, " 没有数据")

  # ---- 品种标识：other_trials 的 stageid 只是序号，name 才是品种 ----
  # ⇒ stageid 列改写为 name，避免管线按 stageid 去重时把不同品种混成一个
  nm <- trimws(as.character(raw$name))
  nm[is.na(nm) | !nzchar(nm)] <- paste0("未命名_", which(is.na(nm) | !nzchar(nm)))

  out <- data.frame(
    experiment_id   = rep(batch_id, nrow(raw)),
    experiment_name = rep(attr_get_trial_name(batch_id, db_path), nrow(raw)),
    stageid         = nm,
    name            = nm,
    place           = as.character(raw$place),
    rp              = ifelse(is.na(raw$rp) | !nzchar(as.character(raw$rp)), "1", as.character(raw$rp)),
    is_ck           = suppressWarnings(as.integer(as.character(raw$is_ck))),
    stringsAsFactors = FALSE
  )

  # ---- 产量与考种（管线主用） ----
  num_cols <- c("MuChan", "XiaoQuChanLiang", "XiaoQuShiShouMianJi", "HanShuiLiang",
                "ShengYuQi", "ZhuGao", "DiJiaGao", "BaiLiZhong",
                "FenZhiShu", "ZhuJingJieShu", "DanZhuLiZhong", "DanBai", "ZhiFang",
                "DanZhuJiaShu", "YouXiaoJia", "ZiBanLiLv", "HeBanLiLv", "ChongShiLiLv")
  for (cc in num_cols) {
    out[[cc]] <- if (cc %in% names(raw)) suppressWarnings(as.numeric(raw[[cc]])) else NA_real_
  }

  # ---- 质量性状（文本） ----
  chr_cols <- c("HuaSe", "YeXing", "RongMaoSe", "JieJiaXiXing", "ZhuXing",
                "LuoYeXing", "LieJiaXing", "DaoFuXing", "ZaoShuaiXing",
                "ZhongPiSe", "QiSe", "LiXing", "ZhongPiGuangZe", "ShengZhangXiXing",
                "BoZhongQi", "ChuMiaoQi", "KaiHuaQi", "ChengShuQi", "ShouHuoQi")
  for (cc in chr_cols) {
    out[[cc]] <- if (cc %in% names(raw)) as.character(raw[[cc]]) else NA_character_
  }

  # ---- 病害（文本，报告附录用） ----
  dis_cols <- c("HuaYeBingDuBing", "NiJingDianZhongFuBing", "ShuangMeiBing",
                "HuiBanBing", "XiJunXingBanDianBing", "XiuBing", "GenFuBing",
                "BaoNangXianChongBing", "QiTaBingHai")
  for (cc in dis_cols) {
    out[[cc]] <- if (cc %in% names(raw)) as.character(raw[[cc]]) else NA_character_
  }

  # ---- 田间评价 ----
  out$TianJianBeiZhu <- if ("TianJianBeiZhu" %in% names(raw)) as.character(raw$TianJianBeiZhu) else NA_character_
  for (cc in c("MiaoQiTianJianPingJia", "HuaQiTianJianPingJia", "ChengShuQiTianJianPingJia")) {
    out[[cc]] <- if (cc %in% names(raw)) as.character(raw[[cc]]) else NA_character_
  }

  # ---- 亲本：外部试验表没有亲本信息 ----
  out$ma <- NA_character_
  out$pa <- NA_character_
  out$mapa <- NA_character_
  out$memo <- NA_character_

  out
}

#' 从批次表取试验名
#' @noRd
attr_get_trial_name <- function(batch_id, db_path) {
  con <- connectReadOnly(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  r <- DBI::dbGetQuery(con, "SELECT trial_name FROM other_trial_batches WHERE import_batch_id = ?",
                       params = list(batch_id))
  if (nrow(r) == 0) batch_id else r$trial_name[1]
}

#' 只读方式连 SQLite（immutable=1，避免 WAL 读到旧数据 / 误写）
#' @noRd
connectReadOnly <- function(db_path) {
  if (!file.exists(db_path)) stop("数据库不存在：", db_path)
  # ⚠️ 读之前确认 -wal 是 0 字节，否则 immutable=1 会静默读到旧数据
  wal <- paste0(db_path, "-wal")
  if (file.exists(wal) && file.info(wal)$size > 0) {
    warning("检测到 ", basename(wal), " 非空（",
            file.info(wal)$size, " 字节），immutable=1 可能读到旧数据")
  }
  abs_path <- normalizePath(db_path, mustWork = TRUE)
  DBI::dbConnect(RSQLite::SQLite(),
                 paste0("file:", abs_path, "?immutable=1"),
                 flags = RSQLite::SQLITE_RO)
}
