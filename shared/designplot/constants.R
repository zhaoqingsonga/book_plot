# ============ 地块类型编码常量 ============
CODE_WATER       <- -77   # 水沟
CODE_LANE        <- -111  # 纵向观察道
CODE_ROAD        <- -11   # 横向观察道
CODE_INTERVAL    <- -1    # 材料间隔
CODE_PROTECT_COL <- -9    # 纵向保护行
CODE_PROTECT_ROW <- -99   # 横向保护行
CODE_BLOCKED     <- -8    # 不能种植地块
CODE_GROUP_PAD   <- -6    # 分组后补充保护行

# 所有特殊编码（用于判断特殊列）
SPECIAL_CODES <- c(CODE_WATER, CODE_LANE, CODE_PROTECT_COL, CODE_BLOCKED, CODE_GROUP_PAD)

# DT表格颜色映射：编码断点和对应颜色
COLOR_BREAKS <- c(CODE_LANE, CODE_PROTECT_ROW, CODE_WATER, CODE_ROAD,
                  CODE_PROTECT_COL, CODE_BLOCKED, CODE_GROUP_PAD, CODE_INTERVAL, 0)
COLOR_VALUES <- c('darkorange','seagreen','lightseagreen','gray',
                  'green','red','limegreen','silver','purple','yellow')

# DT表格通用分页选项
DT_PAGE_MENU <- c(5, 10, 20, 50, 100, 200)

# 统计列规范（最后4列）
STAT_COL_COUNT <- 4
STAT_COL_OFFSET_INTERVAL <- 3   # ncol - 3
STAT_COL_OFFSET_BLOCKNO  <- 2   # ncol - 2
STAT_COL_OFFSET_ROWNO    <- 1   # ncol - 1
STAT_COL_OFFSET_SEQ      <- 0   # ncol
STAT_COL_NAMES <- c("间隔", "总长", "排数", "序号")

# 不能种植地块参数：每组4个数（起始m、终止m、起始行、终止行）
BLOCK_RANGE_PARAM_GROUP_SIZE <- 4

# Excel 颜色映射基础表（用于生成 conditional/cell 两套样式）
EXCEL_COLOR_MAP <- list(
  list(code=CODE_LANE,        color="#FF8C00"),  # 纵向观察道 darkorange
  list(code=CODE_PROTECT_ROW, color="#2E8B57"),  # 横向保护行 seagreen
  list(code=CODE_WATER,       color="#20B2AA"),  # 水沟 lightseagreen
  list(code=CODE_ROAD,        color="#808080"),  # 横向观察道 gray
  list(code=CODE_PROTECT_COL, color="#008000"),  # 纵向保护行 green
  list(code=CODE_BLOCKED,     color="#FF0000"),  # 不能种植 red
  list(code=CODE_GROUP_PAD,   color="#32CD32"),  # 分组补充保护行 limegreen
  list(code=CODE_INTERVAL,    color="#C0C0C0"),  # 材料间隔 silver
  list(code=0,                color="#FFFF00")   # 可种植/未种地块 yellow
)

# 由基础颜色映射生成 Excel 样式列表（bgFill 或 fgFill）
buildExcelStyles<-function(style_builder){
  lapply(EXCEL_COLOR_MAP, function(x) {
    list(code = x$code, style = style_builder(x$color))
  })
}

# Excel条件格式样式（bgFill用于conditionalFormatting）
EXCEL_COND_STYLES <- buildExcelStyles(function(color_hex) createStyle(bgFill = color_hex))

# 统一创建单元格填充样式：同时设置 fgFill/bgFill，提升不同Excel客户端兼容性
createCellFillStyle<-function(color_hex){
  createStyle(fgFill=color_hex, bgFill=color_hex)
}

# Excel直接着色样式（fgFill用于addStyle）
EXCEL_CELL_STYLES <- buildExcelStyles(createCellFillStyle)
