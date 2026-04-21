# Shared helper functions for Field Book Generator

# ============================================================================
# 共用常量
# ============================================================================

# 田试记录显示列（三个模块共用）
FIELD_VIEW_COLS <- c("fieldid", "code", "place", "stageid", "name", "rows", "line_number", "rp")

# 田试记录显示列 - 包含ma, pa（用于详细视图）
FIELD_VIEW_COLS_WITH_PARENTS <- c("fieldid", "code", "place", "stageid", "name", "ma", "pa", "rows", "line_number", "rp")

# 数据库材料表需排除的列
DB_MATERIAL_COLS <- c("material_id", "experiment_id", "created_at")

# 田试记录表需排除的列（merge时保留is_ck用）
DB_FIELD_RECORD_EXCLUDE <- c("record_id", "experiment_id", "experiment_name", "created_at")

# 默认值常量
DEFAULT_LOCATION <- "安徽宿州"
INTERVAL_DISABLED <- 999
FIELDID_DELAY_SECONDS <- 1.2
PREVIEW_PAGE_LENGTH <- 15

# ============================================================================
# 共用函数
# ============================================================================

# 渲染田试记录表格（供三个模块共用）
# @param data  reactive expression returning the data frame
# @param pageLength 每页显示行数
renderFieldRecordTable <- function(data, pageLength = 15) {
  DT::renderDataTable({
    req(data())
    data()
  }, options = list(pageLength = pageLength, scrollX = TRUE, dom = 'frtip'))
}

# 添加88个性状列（数据库表结构需要这些列，值都是NA）
addTraitColumns <- function(df) {
  trait_cols <- soyplant::soy_traits$name_lib
  for (col in trait_cols) {
    if (!col %in% names(df)) {
      df[[col]] <- NA
    }
  }
  df
}

# 构建已生成/未生成选项列表（用于updateSelectInput）
buildGeneratedChoices <- function(records) {
  generated <- records[records$has_generated == 1, ]
  not_generated <- records[records$has_generated == 0, ]
  c(
    "已生成" = if (nrow(generated) > 0) setNames(generated$experiment_id, generated$experiment_name) else character(0),
    "未生成" = if (nrow(not_generated) > 0) setNames(not_generated$experiment_id, not_generated$experiment_name) else character(0)
  )
}

# 解析对照品种输入
parseCkInput <- function(ck_text) {
  if (nzchar(ck_text)) {
    unlist(strsplit(trimws(ck_text), " +"))
  } else {
    NULL
  }
}

# Get Excel sheet names
getSheetNames <- function(file) {
  wb <- openxlsx::loadWorkbook(file)
  sheets <- names(wb)
  return(sheets)
}

# 通用 Excel 导出：兼容 designplot 下载处理器当前调用签名
# @param data 可为 matrix / data.frame
# @param file 输出文件路径
# @param ref_data 可选参考矩阵，用于按参考值高亮未种地块
# @param highlight_positive 是否高亮正数单元格
# @param color_planted 是否高亮包含“|”的已种单元格
writeFormattedXlsx <- function(data, file, ref_data = NULL, highlight_positive = FALSE, color_planted = TRUE) {
  if (is.null(data)) stop("导出数据为空")

  out_df <- if (is.matrix(data)) {
    as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  } else if (is.data.frame(data)) {
    data
  } else {
    as.data.frame(data, stringsAsFactors = FALSE, check.names = FALSE)
  }

  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::writeData(wb, "Sheet1", out_df, rowNames = FALSE)

  style_header <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#D9EAF7", halign = "center")
  style_positive <- openxlsx::createStyle(fgFill = "#FFF59D")
  style_planted <- openxlsx::createStyle(fgFill = "#C8E6C9")
  style_unplanted <- openxlsx::createStyle(fgFill = "#F3F4F6")

  openxlsx::addStyle(
    wb,
    "Sheet1",
    style = style_header,
    rows = 1,
    cols = seq_len(ncol(out_df)),
    gridExpand = TRUE,
    stack = TRUE
  )

  n_rows <- nrow(out_df)
  n_cols <- ncol(out_df)
  if (n_rows > 0 && n_cols > 0) {
    value_matrix <- as.matrix(out_df)
    value_char <- matrix(as.character(value_matrix), nrow = n_rows, ncol = n_cols)

    applyStyleMask <- function(mask, style) {
      if (is.null(dim(mask))) {
        mask <- matrix(mask, nrow = n_rows, ncol = n_cols)
      }

      positions <- which(mask, arr.ind = TRUE)
      if (nrow(positions) == 0) {
        return(invisible(NULL))
      }

      openxlsx::addStyle(
        wb,
        "Sheet1",
        style = style,
        rows = positions[, 1] + 1L,
        cols = positions[, 2],
        gridExpand = FALSE,
        stack = TRUE
      )
    }

    planted_mask <- matrix(FALSE, nrow = n_rows, ncol = n_cols)
    if (isTRUE(color_planted)) {
      planted_mask <- matrix(grepl("\\|", value_char), nrow = n_rows, ncol = n_cols)
      applyStyleMask(planted_mask, style_planted)
    }

    if (isTRUE(highlight_positive)) {
      numeric_matrix <- matrix(suppressWarnings(as.numeric(value_char)), nrow = n_rows, ncol = n_cols)
      positive_mask <- !planted_mask & !is.na(numeric_matrix) & numeric_matrix > 0
      applyStyleMask(positive_mask, style_positive)
    } else if (!is.null(ref_data)) {
      ref_matrix <- if (is.matrix(ref_data)) ref_data else as.matrix(ref_data)
      compare_rows <- min(n_rows, nrow(ref_matrix))
      compare_cols <- min(n_cols, ncol(ref_matrix))

      if (compare_rows > 0 && compare_cols > 0) {
        value_numeric <- matrix(suppressWarnings(as.numeric(value_char)), nrow = n_rows, ncol = n_cols)
        ref_numeric <- matrix(NA_real_, nrow = n_rows, ncol = n_cols)
        ref_numeric[seq_len(compare_rows), seq_len(compare_cols)] <- matrix(
          suppressWarnings(as.numeric(ref_matrix[seq_len(compare_rows), seq_len(compare_cols), drop = FALSE])),
          nrow = compare_rows,
          ncol = compare_cols
        )

        unplanted_mask <-
          !planted_mask &
          !is.na(ref_numeric) &
          ref_numeric > 0 &
          !is.na(value_numeric) &
          value_numeric > 0

        applyStyleMask(unplanted_mask, style_unplanted)
      }
    }
  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

# Convert rows to line numbers
rows_to_linenumber <- function(rows) {
  result <- character(length(rows))
  pos <- 1
  for (i in seq_along(rows)) {
    result[i] <- paste0(pos, "-", pos + rows[i] - 1)
    pos <- pos + rows[i]
  }
  return(result)
}

# Combination matrix - overrides soyplant's version
combination_matrix <- function(my_combi) {
  # 过滤掉 pa 为 NA 或空的行
  valid <- !is.na(my_combi$pa) & nchar(as.character(my_combi$pa)) > 0
  if (sum(valid) == 0) {
    return(NULL)
  }
  my_combi <- my_combi[valid, , drop = FALSE]

  ma <- my_combi$ma[!duplicated(my_combi$ma)]
  pa <- my_combi$pa[!duplicated(my_combi$pa)]

  if (length(ma) == 0 || length(pa) == 0) {
    return(NULL)
  }

  mapamatri <- matrix(rep(NA, (length(ma) * length(pa))),
                      nrow = length(ma), ncol = length(pa), byrow = TRUE)
  rownames(mapamatri) <- ma
  colnames(mapamatri) <- pa

  code_col <- if ("code" %in% names(my_combi)) "code" else "name"

  for (i in 1:nrow(my_combi)) {
    row_name <- my_combi$ma[i]
    col_name <- my_combi$pa[i]
    if (!is.na(row_name) && !is.na(col_name) && row_name %in% rownames(mapamatri) && col_name %in% colnames(mapamatri)) {
      if (is.na(mapamatri[row_name, col_name])) {
        mapamatri[row_name, col_name] <- my_combi[[code_col]][i]
      } else {
        mapamatri[row_name, col_name] <- paste(mapamatri[row_name, col_name], my_combi[[code_col]][i], sep = "/")
      }
    }
  }

  mapamatri <- as.data.frame(mapamatri)
  mapamatri <- cbind(data.frame(母本 = ma), mapamatri)
  rownames(mapamatri) <- NULL
  return(mapamatri)
}

# ============================================================================
# 田试记录表通用列定义（来自soy_traits.txt的88个性状）
# ============================================================================

# 田试记录表基础列
FIELD_RECORD_BASE_COLS <- c(
  "experiment_id", "experiment_name",
  "fieldid", "code", "place", "stageid", "name", "rows", "line_number", "rp",
  "ma", "pa", "mapa"
)

# 田试记录表性状列（来自soy_traits.txt的name_lib）
FIELD_RECORD_TRAIT_COLS <- c(
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
  "ChuShuQi", "WanShuQi", "HuiFuLv", "SSRBuHeGeWeiDian"
)

# 群体田试记录额外列
FIELD_RECORD_POPULATION_EXTRA <- c("f")

# 株行田试记录额外列
FIELD_RECORD_LINE_SELECTION_EXTRA <- c("sele")

# 产比田试记录额外列
FIELD_RECORD_YIELD_TEST_EXTRA <- c("ck")

# 获取田试记录表的所有列
getFieldRecordCols <- function(type = c("population", "line_selection", "yield_test")) {
  type <- match.arg(type)
  extra <- switch(type,
    population = FIELD_RECORD_POPULATION_EXTRA,
    line_selection = FIELD_RECORD_LINE_SELECTION_EXTRA,
    yield_test = FIELD_RECORD_YIELD_TEST_EXTRA,
    character(0)
  )
  c(FIELD_RECORD_BASE_COLS, extra, FIELD_RECORD_TRAIT_COLS, "created_at")
}

# 试验记录列表每行删除按钮（用于维护页最右侧“操作”列）
fb_record_list_delete_buttons <- function(experiment_ids, ns) {
  input_id <- htmltools::htmlEscape(ns("delete_record_row"), attribute = TRUE)
  vapply(as.character(experiment_ids), function(eid) {
    eid_esc <- htmltools::htmlEscape(eid, attribute = TRUE)
    sprintf(
      paste0(
        "<button type=\"button\" class=\"btn btn-danger btn-sm fb-row-delete-btn\" ",
        "onclick=\"event.stopPropagation(); event.preventDefault(); ",
        "Shiny.setInputValue('%s', {experiment_id:'%s', nonce:Date.now()}, {priority:'event'});\">",
        "删除</button>"
      ),
      input_id, eid_esc
    )
  }, FUN.VALUE = character(1))
}
