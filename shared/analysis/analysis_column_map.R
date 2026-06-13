# ==============================================================================
# 列名适配映射表（pinyin ↔ 中文，参考脚本 ↔ Shiny 数据）
# ==============================================================================

# 核心映射：拼音列名 → 参考脚本中文列名
# 用于将 Shiny 数据框列名适配到从参考脚本移植的分析函数
PINYIN_TO_REFERENCE <- c(
  # 产量相关
  MuChan = "亩产_kg",
  XiaoQuChanLiang = "小区产量_kg",
  HanShuiLiang = "含水量_pct",
  # 生育期
  ShengYuQi = "生育期_d",
  KaiHuaQi = "开花期",
  ChengShuQi = "成熟期",
  # 形态
  ZhuGao = "株高_cm",
  DiJiaGao = "底荚高_cm",
  BaiLiZhong = "百粒重_g",
  FenZhiShu = "分枝数",
  ZhuJingJieShu = "主茎节数",
  YouXiaoJia = "有效荚",
  DanZhuJiaShu = "单株荚数",
  # 品质
  DanBai = "蛋白质含量_pct",
  ZhiFang = "脂肪含量_pct",
  # 推导列（分析时计算）
  JiaoLinJinDuiZhaoZengChan = "较临近对照增产_pct",
  JiaoLinJinDuiZhaoWeiCi = "较临近对照位次",
  JiaoPingJunDuiZhaoZengChan = "较平均对照增产_pct",
  JiaoPingJunDuiZhaoWeiCi = "较平均对照位次",
  # 基础信息
  name = "品种名称",
  stageid = "阶段名称",
  ma = "母本",
  pa = "父本",
  place = "地点",
  fieldid = "田间ID",
  is_ck = "是否对照",
  rp = "重复",
  # 质量性状
  HuaSe = "花色",
  YeXing = "叶形",
  JieJiaXiXing = "结荚习性",
  DaoFuXing = "倒伏性",
  RongMaoSe = "茸毛色",
  QiSe = "脐色",
  ZhongPiSe = "种皮色",
  ZhongPiGuangZe = "种皮光泽",
  LiXing = "粒形",
  JiaXing = "荚形",
  # 抗病性
  HuaYeBingDuBing = "花叶病毒病",
  NiJingDianZhongFuBing = "拟茎点种腐病",
  QiTaBingHai = "其它病害",
  # 田间评价
  MiaoQiTianJianPingJia = "苗期田间评价",
  HuaQiTianJianPingJia = "花期田间评价",
  ChengShuQiTianJianPingJia = "成熟期田间评价",
  ZiLiPingJia = "籽粒评价",
  TianJianBeiZhu = "田间备注",
  # 草甘膦抗性
  CaoGanLinKangXing = "草甘膦抗性"
)

# 反向映射：参考脚本中文名 → 拼音列名
REFERENCE_TO_PINYIN <- setNames(names(PINYIN_TO_REFERENCE), PINYIN_TO_REFERENCE)

# 性状中文显示名（用于图表标签）
TRAIT_DISPLAY_NAMES <- c(
  MuChan = "亩产 (kg)",
  ShengYuQi = "生育期 (天)",
  ZhuGao = "株高 (cm)",
  BaiLiZhong = "百粒重 (g)",
  DiJiaGao = "底荚高 (cm)",
  FenZhiShu = "分枝数",
  ZhuJingJieShu = "主茎节数",
  YouXiaoJia = "有效荚",
  DanZhuJiaShu = "单株荚数",
  DanBai = "蛋白质 (%)",
  ZhiFang = "脂肪 (%)",
  HuaSe = "花色",
  YeXing = "叶形",
  JieJiaXiXing = "结荚习性",
  DaoFuXing = "倒伏性",
  RongMaoSe = "茸毛色",
  QiSe = "脐色",
  ZhongPiSe = "种皮色",
  ZhongPiGuangZe = "种皮光泽",
  LiXing = "粒形",
  JiaoLinJinDuiZhaoZengChan = "较临近对照增产 (%)",
  JiaoPingJunDuiZhaoZengChan = "较平均对照增产 (%)",
  JiaoLinJinDuiZhaoWeiCi = "较临近对照位次",
  JiaoPingJunDuiZhaoWeiCi = "较平均对照位次",
  # 基础信息
  name = "品种名称",
  stageid = "阶段",
  ma = "母本",
  pa = "父本",
  is_ck = "对照",
  place = "地点"
)

#' 将 Shiny 数据框列名适配为参考脚本的列名
#'
#' 只重命名存在于映射表中的列，保留未映射的列不变。
#' 同时添加参考脚本期望的字段（如 `阶段名称` 同时由 `name` 派生）。
#'
#' @param df 数据框（拼音列名）
#' @return 数据框（参考脚本中文列名）
#' @export
adapt_to_reference <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  current_cols <- colnames(df)

  # 重命名存在于映射表中的列
  for (pinyin_name in names(PINYIN_TO_REFERENCE)) {
    if (pinyin_name %in% current_cols) {
      ref_name <- PINYIN_TO_REFERENCE[[pinyin_name]]
      colnames(df)[colnames(df) == pinyin_name] <- ref_name
    }
  }

  # 补充参考脚本需要的别名（阶段名称 = name 或 stageid）
  if ("品种名称" %in% colnames(df) && !"阶段名称" %in% colnames(df)) {
    df[["阶段名称"]] <- df[["品种名称"]]
  }
  if ("stageid" %in% current_cols && !"阶段名称" %in% colnames(df)) {
    df[["阶段名称"]] <- df[["stageid"]]
  }

  df
}

#' 将数据框列名从参考脚本中文名转回拼音名
#'
#' @param df 数据框（参考脚本中文列名）
#' @return 数据框（拼音列名）
#' @export
adapt_to_pinyin <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  current_cols <- colnames(df)

  for (ref_name in names(REFERENCE_TO_PINYIN)) {
    if (ref_name %in% current_cols) {
      pinyin_name <- REFERENCE_TO_PINYIN[[ref_name]]
      colnames(df)[colnames(df) == ref_name] <- pinyin_name
    }
  }

  df
}

#' 获取性状显示名
#'
#' @param pinyin_names 字符向量，拼音列名
#' @return 字符向量，中文显示名（未映射的返回原名）
#' @export
get_trait_display_name <- function(pinyin_names) {
  sapply(pinyin_names, function(nm) {
    if (nm %in% names(TRAIT_DISPLAY_NAMES)) {
      TRAIT_DISPLAY_NAMES[[nm]]
    } else {
      nm
    }
  }, USE.NAMES = FALSE)
}

#' 检查核心性状数据的可用性
#'
#' @param df 数据框（拼音列名）
#' @param required_traits 需要检查的性状列名向量（拼音）
#' @return list(has_traits=logical, available=character(), missing=character())
#' @export
check_trait_availability <- function(df, required_traits = NULL) {
  if (is.null(required_traits)) {
    required_traits <- c("MuChan", "ShengYuQi", "ZhuGao", "BaiLiZhong")
  }

  available <- character()
  missing <- character()

  for (tr in required_traits) {
    if (tr %in% colnames(df) && any(!is.na(df[[tr]])) &&
        !all(sapply(df[[tr]], function(x) is.na(x) || (is.character(x) && x == "")))) {
      available <- c(available, tr)
    } else {
      missing <- c(missing, tr)
    }
  }

  list(
    has_traits = length(available) > 0,
    available = available,
    missing = missing,
    trait_count = length(available)
  )
}

#' 检查所有有数据的性状列
#'
#' @param df 数据框
#' @return 有非NA数据的性状列名
#' @export
get_available_traits <- function(df) {
  trait_cols <- intersect(
    c(
      "MuChan", "XiaoQuChanLiang", "HanShuiLiang", "ShengYuQi",
      "KaiHuaQi", "ChengShuQi", "ZhuGao", "DiJiaGao", "BaiLiZhong",
      "FenZhiShu", "ZhuJingJieShu", "YouXiaoJia", "DanZhuJiaShu",
      "DanBai", "ZhiFang", "HuaSe", "YeXing", "JieJiaXiXing",
      "DaoFuXing", "RongMaoSe", "QiSe", "ZhongPiSe", "ZhongPiGuangZe",
      "LiXing", "JiaXing", "HuaYeBingDuBing", "NiJingDianZhongFuBing",
      "QiTaBingHai", "CaoGanLinKangXing", "MiaoQiTianJianPingJia",
      "HuaQiTianJianPingJia", "ChengShuQiTianJianPingJia",
      "ZiLiPingJia", "TianJianBeiZhu"
    ),
    colnames(df)
  )

  trait_cols[sapply(trait_cols, function(col) {
    any(!is.na(df[[col]])) && !all(sapply(df[[col]], function(x) is.na(x) || (is.character(x) && x == "")))
  })]
}
