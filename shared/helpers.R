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

# =============================================================================
# 记录本模块：流程步骤条 + 子 Tab 联动
# =============================================================================

# 顶部流程导航（点击切换「上传 / 维护 / 生成」子 Tab；active_step 高亮当前步）
fb_workflow_steps_ui <- function(ns, active_step = "upload") {
  steps <- c("upload", "maintain", "generate")
  if (!active_step %in% steps) active_step <- "upload"
  link_cls <- function(step) {
    if (identical(active_step, step)) "fb-step-link fb-step-active" else "fb-step-link"
  }
  shiny::div(
    class = "fb-workflow-steps",
    role = "navigation",
    `aria-label` = "工作流程",
    shiny::span(class = "fb-workflow-label", shiny::icon("route"), " 流程"),
    shiny::actionLink(ns("wf_upload"), "上传数据", icon = shiny::icon("upload"), class = link_cls("upload")),
    shiny::tags$span(class = "fb-step-arrow", shiny::HTML("&#8594;")),
    shiny::actionLink(ns("wf_maintain"), "维护记录", icon = shiny::icon("list-alt"), class = link_cls("maintain")),
    shiny::tags$span(class = "fb-step-arrow", shiny::HTML("&#8594;")),
    shiny::actionLink(ns("wf_generate"), "生成记录本", icon = shiny::icon("cog"), class = link_cls("generate"))
  )
}

# 根据 tabsetPanel 当前选中项刷新步骤条（须在模块 server 内调用）
render_workflow_steps <- function(output, input, ns, tab_input_name) {
  output$workflow_steps <- shiny::renderUI({
    tab <- input[[tab_input_name]]
    if (is.null(tab) || length(tab) < 1L) {
      tab <- "upload"
    } else {
      tab <- as.character(tab)[1L]
    }
    if (!nzchar(tab) || !tab %in% c("upload", "maintain", "generate")) tab <- "upload"
    fb_workflow_steps_ui(ns, active_step = tab)
  })
}

# 将步骤链接绑定到模块内 tabsetPanel（selected 对应各 tabPanel 的 value）
bind_workflow_step_tabs <- function(input, session, ns, tabs_input_id) {
  panel_id <- ns(tabs_input_id)
  shiny::observeEvent(input$wf_upload, {
    shiny::updateTabsetPanel(session, inputId = panel_id, selected = "upload")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$wf_maintain, {
    shiny::updateTabsetPanel(session, inputId = panel_id, selected = "maintain")
  }, ignoreInit = TRUE)
  shiny::observeEvent(input$wf_generate, {
    shiny::updateTabsetPanel(session, inputId = panel_id, selected = "generate")
  }, ignoreInit = TRUE)
}
