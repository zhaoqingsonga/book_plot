# =============================================================================
# 模块: 性状数据导入
# 功能: 将 Excel 数据覆盖更新到田试记录表的性状字段
# 定位键: fieldid (来自 Excel 的"唯一编号"列)
# =============================================================================

# T-code 到数据库字段的映射表
# 来源: soyplant::baiaoyun_traits 表 (T000~T088)
# 注意: T089~T178 为重复测量字段，暂未包含在 baiaoyun_traits 中
TCODE_MAPPING <- c(
  "T000" = "XiaoQuShiShouMianJi",  # 小区实收面积(m2)
  "T001" = "XiaoQuChanLiang",       # 小区产量(kg)
  "T002" = "HanShuiLiang",          # 含水量(%)
  "T003" = "MuChan",                # 亩产(kg)
  "T004" = "BoZhongQi",             # 播种期
  "T005" = "ChuMiaoQi",             # 出苗期
  "T006" = "ChuMiaoLiangFou",      # 出苗良否
  "T007" = "MiaoQiTianJianPingJia", # 苗期田间评价
  "T008" = "KaiHuaQi",             # 开花期
  "T009" = "HuaSe",                 # 花色
  "T010" = "HuaQiTianJianPingJia",  # 花期田间评价
  "T011" = "YeXing",               # 叶形
  "T012" = "RongMaoSe",             # 茸毛色
  "T013" = "ShengZhangXiXing",      # 生长习性
  "T014" = "JieJiaXiXing",          # 结荚习性
  "T015" = "DaoFuXing",             # 倒伏性
  "T016" = "ZaoShuaiXing",          # 早衰性
  "T017" = "ZhuXing",               # 株型
  "T018" = "LuoYeXing",             # 落叶性
  "T019" = "LieJiaXing",            # 裂荚性
  "T020" = "ChengShuQi",            # 成熟期
  "T021" = "HuoGanChengShu",        # 活秆成熟
  "T022" = "ChengShuQiTianJianPingJia", # 成熟期田间评价
  "T023" = "ShouHuoQi",            # 收获期
  "T024" = "XiaoQuShouHuoZhuShu",   # 小区收获株数
  "T025" = "ShengYuQi",             # 生育期(天)
  "T026" = "TianJianBeiZhu",        # 田间备注
  "T027" = "HuaYeBingDuBing",       # 花叶病毒病
  "T028" = "NiJingDianZhongFuBing", # 拟茎点种腐病
  "T029" = "ShuangMeiBing",         # 霜霉病
  "T030" = "HuiBanBing",            # 灰斑病
  "T031" = "XiJunXingBanDianBing",   # 细菌性斑点病
  "T032" = "XiuBing",               # 锈病
  "T033" = "GenFuBing",             # 根腐病
  "T034" = "BaoNangXianChongBing",  # 孢囊线虫病
  "T035" = "QiTaBingHai",           # 其他病害
  "T036" = "DouGanHeiQianYing",     # 豆秆黑潜蝇
  "T037" = "DouJiaMing",            # 豆荚螟
  "T038" = "YaChong",               # 蚜虫
  "T039" = "ShiYeXingHaiChong",     # 食叶性害虫
  "T040" = "KaoZhongZhuShu",        # 考种株数
  "T041" = "ZhuGao",                # 株高(cm)均值
  "T042" = "DiJiaGao",             # 底荚高(cm)均值
  "T043" = "FenZhiShu",             # 分枝数(均值)
  "T044" = "ZhuJingJieShu",         # 主茎节数(均值)
  "T045" = "JiaXing",               # 荚形
  "T046" = "JiaShuSe",              # 荚熟色
  "T047" = "YouXiaoJia",            # 有效荚(均值)
  "T048" = "WuXiaoJia",             # 无效荚(均值)
  "T049" = "DanZhuJiaShu",          # 单株荚数(均值)
  "T050" = "DanZhuLiShu",           # 单株粒数(均值)
  "T051" = "DanZhuLiZhong",         # 单株粒重(g)均值
  "T052" = "MeiJiaLiShu",           # 每荚粒数
  "T053" = "LiXing",                # 粒型
  "T054" = "ZhongPiSe",             # 种皮色
  "T055" = "QiSe",                  # 脐色
  "T056" = "ZiYeSe",                # 子叶色
  "T057" = "ZhongPiGuangZe",        # 种皮光泽
  "T058" = "BaiLiZhong",            # 百粒重(g)
  "T059" = "WanHaoLiLv",            # 完好粒率(%)
  "T060" = "PoSuiLiLv",             # 破碎粒率(%)
  "T061" = "BingLiLv",              # 病粒率(%)
  "T062" = "ZiBanLiLv",             # 紫斑粒率(%)
  "T063" = "HeBanLiLv",             # 褐斑粒率(%)
  "T064" = "ShuangMeiLiLv",         # 霜霉粒率(%)
  "T065" = "HuiBanLiLv",            # 灰斑粒率(%)
  "T066" = "ChongShiLiLv",          # 虫蚀粒率(%)
  "T067" = "ZiLiPingJia",           # 籽粒评价
  "T068" = "DanBai",                # 蛋白(%)
  "T069" = "ZhiFang",               # 脂肪(%)
  "T070" = "DanZhiHe",              # 蛋脂和(%)
  "T071" = "CaoGanLinKangXing",     # 草甘膦抗性
  "T072" = "ShiZhiJianCe",          # 试纸检测
  "T073" = "HanJiYin",              # 含基因
  "T074" = "BoZhongPenShu",         # 播种盆数
  "T075" = "BoZhongLiShu",          # 播种粒数
  "T076" = "ChuMiaoShu",            # 出苗数
  "T077" = "ChuMiaoLiShu",          # 出苗率
  "T078" = "NaiYanXing",            # 耐盐性
  "T079" = "NaiHanXing",            # 耐旱性
  "T080" = "ShiHuaQi",             # 始花期
  "T081" = "ZaJiaoHuaShu",          # 杂交花数
  "T082" = "ChengHuoJiaShu",        # 成活荚数
  "T083" = "ZhaJiaoliShu",          # 杂交粒数
  "T084" = "ChuShuQi",             # 初熟期
  "T085" = "WanShuQi",             # 完熟期
  "T086" = "HuiFuLv",              # 回复率
  "T087" = "SSRBuHeGeWeiDian"       # SSR不合格位点
  # T088~T178: Excel 扩展字段，暂不映射（如有需要可后续添加）
)

# 基础信息列（不作为性状更新）
BASE_INFO_COLS <- c(
  "项目名称", "试验名称", "季节", "年份", "阶段",    # A~E 试验信息
  "试点名称", "国家", "省份", "城市", "经度", "纬度", "试点重复",  # F~L 试点信息
  "品种ID", "品种名称", "登记号", "材料编号", "对照类型", "处理", "母本", "父本", "品种阶段", "行数", "排", "列", "小区ID", "唯一编号", "废弃",  # M~AB 品种信息
  "小区实收面积(m2)", "小区产量(kg)", "含水量(%)", "亩产(kg)"  # V~AA 田间信息
)

# Excel 中唯一编号列的列名（用于定位 fieldid）
FIELDID_COL_NAME <- "唯一编号"


#' 读取两行表头的 Excel 文件
#' @param file Excel 文件路径
#' @param sheet Sheet 名称，默认为 "template"
#' @return list(tcode_row, chinese_row, data_df, col_mapping)
readTraitsExcel <- function(file, sheet = "template") {
  if (!file.exists(file)) {
    stop("文件不存在: ", file)
  }

  # 读取 Excel，保留原始列名
  raw_df <- openxlsx::read.xlsx(file, sheet = sheet, colNames = FALSE)

  if (nrow(raw_df) < 4) {
    stop("Excel 行数不足，至少需要 4 行（2行表头 + 1行字段名 + 数据行）")
  }

  # Excel 结构（实际）：
  # Row 1: 合并单元格大组标题（试验信息、试点信息等）
  # Row 2: NA（空行）
  # Row 3: T-codes (T000~T178) 作为列标题 + 中文列名
  # Row 4+: 数据

  # Row 1: 大组标题（仅供参考）
  group_row <- as.character(raw_df[1, ])

  # Row 2: 空行
  chinese_row <- as.character(raw_df[2, ])

  # Row 3: 实际列标题（T-codes + 中文列名混合）
  # T-codes 在 Row 3 中，用于建立列位置映射
  tcode_row <- as.character(raw_df[3, ])

  # Row 3 也是 field_names（T-codes 作为列名）
  field_names <- tcode_row

  # 数据从 Row 4 开始
  data_df <- raw_df[4:nrow(raw_df), ]
  colnames(data_df) <- field_names

  # 建立列位置到 T-code 的映射
  # 找到 T-code 列的位置（排除 NA）
  tcode_positions <- which(!is.na(tcode_row) & grepl("^T\\d{3}$", tcode_row))
  names(tcode_positions) <- tcode_row[tcode_positions]

  # 找到 fieldid 列的位置（唯一编号列）
  # 在 Row 2（中文行）和 Row 3（T-code 行）中搜索
  fieldid_col_idx <- which(tolower(as.character(raw_df[2, ])) == tolower(FIELDID_COL_NAME) |
                           tolower(as.character(raw_df[3, ])) == tolower(FIELDID_COL_NAME))[1]
  if (is.na(fieldid_col_idx)) {
    stop("未找到 '", FIELDID_COL_NAME, "' 列，请检查 Excel 表头第2-3行")
  }

  list(
    group_row = group_row,
    tcode_row = tcode_row,
    chinese_row = chinese_row,
    field_names = field_names,
    data_df = data_df,
    tcode_positions = tcode_positions,
    fieldid_col_idx = fieldid_col_idx
  )
}


#' 将 Excel 数据转换为性状更新格式
#' @param excel_data readTraitsExcel 返回的 list
#' @return data.frame，包含 fieldid 和性状字段
mapExcelToTraits <- function(excel_data) {
  data_df <- excel_data$data_df
  tcode_positions <- excel_data$tcode_positions
  fieldid_col_idx <- excel_data$fieldid_col_idx

  # 获取 fieldid（唯一编号）— 优先按列名，回退到按列索引
  if (fieldid_col_idx > ncol(data_df)) {
    stop("'唯一编号' 列索引 (", fieldid_col_idx, ") 超出数据列数 (", ncol(data_df), ")，请检查 Excel 表头")
  }

  fieldid_col_name <- colnames(data_df)[fieldid_col_idx]
  if (!is.na(fieldid_col_name) && nchar(fieldid_col_name) > 0) {
    fieldid_values <- tryCatch(
      data_df[[fieldid_col_name]],
      error = function(e) stop("读取 '唯一编号' 列 (", fieldid_col_name, ") 失败: ", e$message)
    )
  } else {
    # Row 3 该列无名称，直接用列索引提取
    fieldid_values <- tryCatch(
      data_df[[fieldid_col_idx]],
      error = function(e) stop("读取 '唯一编号' 列 (索引 ", fieldid_col_idx, ") 失败: ", e$message)
    )
  }
  result <- data.frame(fieldid = fieldid_values, stringsAsFactors = FALSE)

  # 遍历 T-code 列，构建映射
  skipped_cols <- character()
  for (tcode in names(tcode_positions)) {
    col_idx <- tcode_positions[[tcode]]
    db_col <- TCODE_MAPPING[tcode]  # 用 [ 而非 [[ — 缺失时返回 NA 而不是抛错

    if (is.na(db_col) || is.null(db_col) || nchar(db_col) == 0) next  # 无映射的 T-code，跳过
    if (col_idx > ncol(data_df)) {
      skipped_cols <- c(skipped_cols, sprintf("%s (列索引 %d 越界)", tcode, col_idx))
      next
    }

    col_name <- excel_data$field_names[col_idx]
    if (is.na(col_name) || nchar(col_name) == 0) {
      skipped_cols <- c(skipped_cols, sprintf("%s (列名无效)", tcode))
      next
    }

    # 安全取值
    tryCatch({
      result[[db_col]] <- data_df[[col_idx]]
    }, error = function(e) {
      skipped_cols <- c(skipped_cols, sprintf("%s → %s (读取失败: %s)", tcode, db_col, e$message))
    })
  }

  if (length(skipped_cols) > 0) {
    warning("以下列无法映射，已跳过:\n  ", paste(skipped_cols, collapse = "\n  "))
  }

  # 清理 fieldid
  result$fieldid <- trimws(as.character(result$fieldid))
  result$fieldid[result$fieldid == "" | is.na(result$fieldid)] <- NA

  # 移除没有 fieldid 的行
  result <- result[!is.na(result$fieldid), , drop = FALSE]

  result
}


#' 获取田试记录表的字段列表（来自 FIELD_RECORD_COLS）
getFieldRecordTraitCols <- function() {
  # 性状字段 = FIELD_RECORD_COLS 排除基础字段
  base_cols <- c(
    "experiment_id", "experiment_name",
    "fieldid", "id", "user", "stageid", "name", "ma", "pa", "mapa", "memo",
    "stage", "next_stage", "f", "sele", "process", "path", "source",
    "former_fieldid", "former_stageid", "code", "rp", "treatment", "place",
    "rows", "line_number", "is_ck", "created_at"
  )

  trait_cols <- FIELD_RECORD_COLS[!FIELD_RECORD_COLS %in% base_cols]
  trait_cols
}


#' 批量更新田试记录表的性状数据（UPSERT）
#' @param con 数据库连接
#' @param records_df data.frame，包含 fieldid 和性状字段
#' @param experiment_type 试验类型：population, line_selection, yield_test
#' @param experiment_id 实验ID（用于新插入记录）
#' @param batch_size 每批处理行数
#' @param verbose 是否输出进度信息
#' @return list(updated = 更新行数, inserted = 新增行数, errors = 错误信息)
upsertFieldTraitsBatch <- function(con, records_df, experiment_type, experiment_id = NULL,
                                    batch_size = 500, verbose = TRUE) {
  field_table <- switch(experiment_type,
    "population" = "population_field_records",
    "line_selection" = "line_selection_field_records",
    "yield_test" = "yield_test_field_records"
  )

  if (is.null(field_table)) {
    stop("未知的试验类型: ", experiment_type)
  }

  # 获取性状字段列表
  trait_cols <- getFieldRecordTraitCols()
  trait_cols_in_df <- trait_cols[trait_cols %in% names(records_df)]

  if (length(trait_cols_in_df) == 0) {
    stop("records_df 中没有找到有效的性状字段")
  }

  updated <- 0
  inserted <- 0
  skipped <- 0
  errors <- character()

  n <- nrow(records_df)
  n_batches <- ceiling(n / batch_size)

  for (b in seq_len(n_batches)) {
    start_idx <- (b - 1) * batch_size + 1
    end_idx <- min(b * batch_size, n)
    batch <- records_df[start_idx:end_idx, , drop = FALSE]

    tryCatch({
      DBI::dbWithTransaction(con, {
        for (i in seq_len(nrow(batch))) {
          row <- batch[i, , drop = FALSE]
          fieldid_val <- row$fieldid[1]

          if (is.na(fieldid_val) || nchar(trimws(fieldid_val)) == 0) {
            skipped <- skipped + 1
            next
          }

          # 检查记录是否存在
          existing <- DBI::dbGetQuery(con,
            sprintf("SELECT rowid FROM %s WHERE fieldid = ? AND fieldid IS NOT NULL AND fieldid != ''", field_table),
            params = list(fieldid_val))

          if (nrow(existing) > 0) {
            # UPDATE - 只更新性状字段
            set_parts <- paste0(trait_cols_in_df, " = ?")
            set_clause <- paste(set_parts, collapse = ", ")

            sql <- sprintf(
              "UPDATE %s SET %s WHERE fieldid = ?",
              field_table, set_clause
            )

            values <- lapply(trait_cols_in_df, function(col) row[[col]][1])
            params <- c(values, fieldid_val)

            DBI::dbExecute(con, sql, params = params)
            updated <- updated + 1
          } else {
            # 记录不存在于数据库中
            # 由于用户要求"只更新现有记录的性状"，所以跳过不存在的新记录
            skipped <- skipped + 1
            if (verbose && b == 1 && i <= 3) {
              message(sprintf("跳过 fieldid=%s: 数据库中不存在此记录", fieldid_val))
            }
            next
          }
        }
      })

      if (verbose) {
        message(sprintf("[%d/%d] 已处理 %d-%d 行 (更新: %d, 新增: %d, 跳过: %d)",
          b, n_batches, start_idx, end_idx, updated, inserted, skipped))
      }
    }, error = function(e) {
      errors <<- c(errors, sprintf("批次 %d 错误: %s", b, e$message))
      if (verbose) {
        message(sprintf("[%d/%d] 错误: %s", b, n_batches, e$message))
      }
    })
  }

  list(updated = updated, inserted = inserted, skipped = skipped, errors = errors)
}


#' 从 Excel 导入性状数据到田试记录表
#' @param file Excel 文件路径
#' @param experiment_type 试验类型：population, line_selection, yield_test, 或 "auto"（自动检测）
#' @param experiment_id 实验ID（用于新插入记录时的必需参数）
#' @param sheet Sheet 名称
#' @param batch_size 每批处理行数
#' @param verbose 是否输出详细信息
#' @return list(total = 总行数, updated = 更新行数, inserted = 新增行数, skipped = 跳过数, errors = 错误)
importTraitsFromExcel <- function(file, experiment_type = "auto", experiment_id = NULL,
                                   sheet = "template", batch_size = 500, verbose = TRUE) {
  if (verbose) message("开始导入性状数据...")
  if (verbose) message("读取 Excel 文件: ", file)

  # 1. 读取 Excel
  excel_data <- readTraitsExcel(file, sheet)

  if (verbose) message("Excel 共 ", nrow(excel_data$data_df), " 行数据")

  # 2. 转换为性状格式
  records_df <- mapExcelToTraits(excel_data)

  if (verbose) message("有效记录数: ", nrow(records_df))

  if (nrow(records_df) == 0) {
    stop("没有有效的记录可导入（检查 fieldid 是否为空）")
  }

  # 3. 连接到数据库
  db_path <- defaultDbPath()
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # 初始化数据库（确保表存在）
  initDb(con)

  # 4. 确定试验类型并执行导入
  if (experiment_type == "auto") {
    # 混合类型：按 fieldid 归属自动分表处理
    type_tables <- c(
      "population"    = "population_field_records",
      "line_selection" = "line_selection_field_records",
      "yield_test"    = "yield_test_field_records"
    )

    valid_fids <- records_df$fieldid[!is.na(records_df$fieldid) & nchar(trimws(records_df$fieldid)) > 0]
    if (length(valid_fids) == 0) {
      stop("没有有效的 fieldid 可导入")
    }

    # 批量查询各表中存在的 fieldid（分块防SQL参数过多）
    unique_fids <- unique(valid_fids)
    CHUNK_SIZE <- 500L
    fid_type_map <- list(population = character(), line_selection = character(), yield_test = character())
    n_chunks <- ceiling(length(unique_fids) / CHUNK_SIZE)

    for (c in seq_len(n_chunks)) {
      chunk_start <- (c - 1) * CHUNK_SIZE + 1
      chunk_end <- min(c * CHUNK_SIZE, length(unique_fids))
      chunk <- unique_fids[chunk_start:chunk_end]
      chunk_sql <- paste(sprintf("'%s'", chunk), collapse = ",")

      for (tp in names(type_tables)) {
        tbl <- type_tables[[tp]]
        chunk_fids <- tryCatch(
          DBI::dbGetQuery(con, sprintf(
            "SELECT fieldid FROM %s WHERE fieldid IN (%s) AND fieldid IS NOT NULL AND fieldid != ''",
            tbl, chunk_sql
          ))$fieldid,
          error = function(e) character()
        )
        fid_type_map[[tp]] <- c(fid_type_map[[tp]], chunk_fids)
      }
    }

    # 统计归属分布
    type_counts <- sapply(fid_type_map, length)
    if (verbose) {
      message("自动分表检测: population=", type_counts["population"],
              " line_selection=", type_counts["line_selection"],
              " yield_test=", type_counts["yield_test"])
    }

    # 分别导入各类型
    all_result <- list(updated = 0L, inserted = 0L, skipped = 0L, errors = character())

    for (tp in names(type_tables)) {
      fids_of_type <- fid_type_map[[tp]]
      if (length(fids_of_type) == 0) next

      subset_df <- records_df[records_df$fieldid %in% fids_of_type, , drop = FALSE]
      if (nrow(subset_df) == 0) next

      if (verbose) message(sprintf("  → %-15s %d 条", tp, nrow(subset_df)))

      res <- upsertFieldTraitsBatch(con, subset_df, tp, experiment_id, batch_size, verbose = FALSE)

      all_result$updated  <- all_result$updated  + res$updated
      all_result$inserted <- all_result$inserted + res$inserted
      all_result$skipped  <- all_result$skipped  + res$skipped
      all_result$errors   <- c(all_result$errors,  res$errors)
    }

    # 统计无归属记录（不在任何表中的 fieldid）
    classified_fids <- unique(unlist(fid_type_map, use.names = FALSE))
    orphan_df <- records_df[records_df$fieldid %in% valid_fids & !records_df$fieldid %in% classified_fids, , drop = FALSE]
    if (nrow(orphan_df) > 0) {
      all_result$skipped <- all_result$skipped + nrow(orphan_df)
      if (verbose) message("  → (无归属)  ", nrow(orphan_df), " 条跳过")
    }

    result <- all_result
  } else {
    # 用户指定了明确类型 → 直接导入
    result <- upsertFieldTraitsBatch(con, records_df, experiment_type, experiment_id, batch_size, verbose)
  }

  if (verbose) {
    message("\n导入完成!")
    message("  总记录数: ", nrow(records_df))
    message("  更新: ", result$updated)
    message("  新增: ", result$inserted)
    message("  跳过: ", result$skipped)
    if (length(result$errors) > 0) {
      message("  错误数: ", length(result$errors))
    }
  }

  result$total <- nrow(records_df)
  result
}


#' 预览 Excel 数据（前几行）
#' @param file Excel 文件路径
#' @param sheet Sheet 名称
#' @param n 预览行数
#' @return data.frame
previewTraitsExcel <- function(file, sheet = "template", n = 10) {
  excel_data <- readTraitsExcel(file, sheet)
  records_df <- mapExcelToTraits(excel_data)

  # 返回前 n 行
  head(records_df, n)
}


#' 获取 T-code 映射表（用于 UI 显示）
#' @return data.frame with columns: TCode, DBField, ChineseName
getTCodeMappingTable <- function() {
  # 从 baiaoyun_traits 获取中文名
  chinese_map <- NULL
  tryCatch({
    library(soyplant)
    data(baiaoyun_traits)
    codes <- baiaoyun_traits[['编码TRAIT_CODE']]
    chinese_names <- baiaoyun_traits[['性状名称TRAIT_NAME']]
    chinese_map <- setNames(as.character(chinese_names), as.character(codes))
  }, error = function(e) {
    warning("无法加载 baiaoyun_traits 表: ", e$message)
  })

  # 构建映射表
  mapping_list <- lapply(names(TCODE_MAPPING), function(tcode) {
    db_col <- TCODE_MAPPING[[tcode]]
    chinese_name <- if (!is.null(chinese_map) && !is.na(chinese_map[tcode])) {
      chinese_map[tcode]
    } else {
      db_col  # 回退到 DBField
    }
    data.frame(TCode = tcode, DBField = db_col, ChineseName = chinese_name, stringsAsFactors = FALSE)
  })

  do.call(rbind, mapping_list)
}