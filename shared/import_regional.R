# =============================================================================
# 其他试验数据导入 — 解析、标准化逻辑
# =============================================================================
# 核心流程：读取外来Excel → 列映射确认 → 结构清洗 → 标准化 → 写入独立数据库
# 数据保持品种×地点的多地点结构，不按地点拆分

library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# =============================================================================
# 通用工具
# =============================================================================

clean_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x[x %in% c("/", "-", "—", "NA", "na", "", " ")] <- NA
  x <- gsub("%", "", x, fixed = TRUE)
  x <- trimws(x)
  x <- gsub(",", "", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

is_summary_site <- function(x) {
  grepl("平均|均值|合计|总计|average|mean|total", x, ignore.case = TRUE)
}

is_ck_variety <- function(x) {
  grepl("CK|对照|check", x, ignore.case = TRUE)
}

# 判断字符向量中的值是否为空（包括 R 的 NA 和字符串 "NA"）
is_empty_str <- function(x) {
  is.na(x) | is.null(x) | as.character(x) == "NA" | as.character(x) == "na" | trimws(as.character(x)) == ""
}

# =============================================================================
# 列名自动识别
# =============================================================================

# 标准字段名与 FIELD_RECORD_COLS 对齐，涵盖全部 88 个性状列
# 关键词包含 qr_trait$name_C 中文名 + 常见变体/别称
COLUMN_KEYWORDS <- list(
  # ===== 核心字段 =====
  name                    = c("品种名称", "品种名", "品种"),
  place                   = c("试验点", "试验地点", "地点名", "地点"),
  rp                      = c("重复", "区组", "重复数"),
  treatment               = c("处理", "处理类型", "treatment"),
  MuChan                  = c("亩产", "折合.*产", "产量.*亩"),
  XiaoQuChanLiang         = c("小区.*产", "小区产量", "计产"),
  XiaoQuShiShouMianJi      = c("小区实收面积", "小区面积", "面积"),
  HanShuiLiang            = c("含水量", "含水"),
  stageid                  = c("序号", "编号"),
  ma                      = c("母本"),
  pa                      = c("父本"),
  is_ck                   = c("对照类型", "是否.*对照"),
  # ===== 生育期相关 =====
  BoZhongQi               = c("播种期", "播种日期"),
  ChuMiaoQi               = c("出苗期", "出苗日期"),
  ChuMiaoLiangFou         = c("出苗良否", "出苗质量"),
  MiaoQiTianJianPingJia   = c("苗期.*评价", "苗期田间"),
  KaiHuaQi                = c("开花期", "开花日期", "始花"),
  HuaQiTianJianPingJia    = c("花期.*评价", "花期田间"),
  ChengShuQi              = c("成熟期", "成熟日期"),
  ChengShuQiTianJianPingJia = c("成熟期.*评价", "成熟田间"),
  ShouHuoQi               = c("收获期", "收获日期"),
  ShengYuQi               = c("生育期", "生育.*日", "生育天数"),
  ShiHuaQi                = c("始花期"),
  ChuShuQi                = c("初熟期"),
  WanShuQi                = c("完熟期"),
  # ===== 形态性状 =====
  HuaSe                   = c("花色", "花颜色"),
  YeXing                  = c("叶形", "叶片形状"),
  RongMaoSe               = c("茸毛色", "茸毛颜色", "毛色"),
  ShengZhangXiXing         = c("生长习性", "生长.*型"),
  JieJiaXiXing             = c("结荚习性", "结荚.*型"),
  DaoFuXing                = c("倒伏性", "倒伏级别", "抗倒伏性", "抗倒性", "抗倒伏", "倒伏"),
  ZaoShuaiXing             = c("早衰性", "早衰"),
  ZhuXing                  = c("株型", "植株.*型"),
  LuoYeXing                = c("落叶性", "落叶"),
  LieJiaXing               = c("裂荚性", "裂荚"),
  HuoGanChengShu           = c("活秆成熟", "活杆成熟"),
  JiaXing                  = c("荚形", "荚形状", "豆荚.*形"),
  JiaShuSe                 = c("荚熟色", "荚成熟色"),
  LiXing                   = c("粒型", "粒形", "籽粒形状", "子粒.*形", "粒形"),
  ZhongPiSe                = c("种皮色", "种皮颜色", "种皮.*色"),
  QiSe                     = c("脐色", "脐颜色", "种脐色"),
  ZiYeSe                   = c("子叶色", "子叶颜色"),
  ZhongPiGuangZe           = c("种皮光泽", "籽粒光泽", "籽粒泽", "光泽度", "光泽"),
  ZiLiPingJia              = c("籽粒评价", "籽粒.*评定"),
  # ===== 株型/产量构成 =====
  KaoZhongZhuShu           = c("考种株数"),
  XiaoQuShouHuoZhuShu       = c("小区收获株数", "收获株数"),
  ZhuGao                   = c("株高", "株高.*cm"),
  DiJiaGao                 = c("底荚高度", "底荚高", "底荚高.*cm"),
  FenZhiShu                = c("分枝数", "分枝个数"),
  ZhuJingJieShu             = c("主茎节数", "主茎节"),
  YouXiaoJia                = c("有效荚", "有效荚数"),
  WuXiaoJia                 = c("无效荚", "无效荚数"),
  DanZhuJiaShu              = c("单株荚数", "单株.*荚"),
  DanZhuLiShu               = c("单株粒数", "单株.*粒"),
  DanZhuLiZhong             = c("单株粒重"),
  MeiJiaLiShu               = c("每荚粒数"),
  BaiLiZhong                = c("百粒重", "百粒.*重", "100粒"),
  # ===== 品质 =====
  WanHaoLiLv               = c("完好粒率"),
  PoSuiLiLv                 = c("破碎粒率", "破碎率"),
  BingLiLv                  = c("病粒率"),
  ZiBanLiLv                 = c("紫斑粒率", "紫斑率"),
  HeBanLiLv                 = c("褐斑粒率", "褐斑率"),
  ShuangMeiLiLv             = c("霜霉粒率"),
  HuiBanLiLv                = c("灰斑粒率"),
  ChongShiLiLv              = c("虫蚀粒率", "虫蚀率"),
  DanBai                    = c("蛋白", "粗蛋白"),
  ZhiFang                   = c("脂肪", "粗脂肪"),
  DanZhiHe                  = c("蛋脂和", "蛋脂总和"),
  # ===== 抗病性 =====
  HuaYeBingDuBing            = c("花叶病毒病", "花叶病"),
  NiJingDianZhongFuBing      = c("拟茎点种腐病", "拟茎点.*腐"),
  ShuangMeiBing              = c("霜霉病"),
  HuiBanBing                 = c("灰斑病"),
  XiJunXingBanDianBing       = c("细菌性斑点病", "细菌斑点"),
  XiuBing                    = c("锈病"),
  GenFuBing                  = c("根腐病"),
  BaoNangXianChongBing       = c("孢囊线虫病", "线虫病"),
  QiTaBingHai                = c("其他病害", "其它病害", "主要病虫害"),
  # ===== 抗虫性 =====
  DouGanHeiQianYing         = c("豆秆黑潜蝇", "豆杆.*蝇"),
  DouJiaMing                = c("豆荚螟"),
  YaChong                    = c("蚜虫"),
  ShiYeXingHaiChong          = c("食叶性害虫", "食叶.*虫"),
  # ===== 其他性状 =====
  CaoGanLinKangXing         = c("草甘膦抗性", "草甘膦"),
  NaiYanXing                = c("耐盐性", "耐盐"),
  NaiHanXing                = c("耐旱性", "耐旱"),
  TianJianBeiZhu             = c("田间备注", "备注"),
  ShiZhiJianCe               = c("试纸检测"),
  HanJiYin                   = c("含基因", "基因"),
  BoZhongPenShu              = c("播种盆数"),
  BoZhongLiShu               = c("播种粒数"),
  ChuMiaoShu                 = c("出苗数"),
  ChuMiaoLiShu               = c("出苗粒数"),
  ZaJiaoHuaShu               = c("杂交花数"),
  ChengHuoJiaShu              = c("成活荚数"),
  ZhaJiaoliShu                = c("杂交粒数"),
  HuiFuLv                    = c("恢复率"),
  SSRBuHeGeWeiDian           = c("SSR.*不合格", "SSR不合格")
)

match_single_column <- function(raw_col) {
  # 跳过无效列名（空、NA、readxl生成的占位符...25）
  if (is.na(raw_col) || raw_col == "" || grepl("^\\.\\.\\.[0-9]+$", raw_col))
    return(NA_character_)

  best_field <- NA_character_
  best_priority <- 0L     # 1=完全匹配中文名, 2=关键词包含, 3=模糊匹配
  best_score <- 0L        # 匹配文本长度

  for (field in names(COLUMN_KEYWORDS)) {
    kws <- COLUMN_KEYWORDS[[field]]
    for (kw in kws) {
      # 优先级1: 原始列名完全等于关键词（精确中文匹配）
      if (raw_col == kw) {
        if (1L > best_priority || (1L == best_priority && nchar(kw) > best_score)) {
          best_priority <- 1L; best_score <- nchar(kw); best_field <- field
        }
        next
      }
      # 优先级2: 列名中包含关键词
      if (grepl(kw, raw_col, fixed = TRUE)) {
        if (2L > best_priority || (2L == best_priority && nchar(kw) > best_score)) {
          best_priority <- 2L; best_score <- nchar(kw); best_field <- field
        }
      }
      # 优先级3: 忽略大小写匹配（用于英文关键词）
      if (grepl(kw, raw_col, ignore.case = TRUE)) {
        if (3L > best_priority || (3L == best_priority && nchar(kw) > best_score)) {
          best_priority <- 3L; best_score <- nchar(kw); best_field <- field
        }
      }
    }
  }
  best_field
}

autoDetectColumns <- function(raw_cols) {
  sapply(raw_cols, match_single_column, USE.NAMES = TRUE)
}

# =============================================================================
# 结构检测
# =============================================================================

detectStructure <- function(raw_df, name_col_orig, place_col_orig, stageid_col_orig = NULL) {
  name_col <- raw_df[[name_col_orig]]
  place_col <- raw_df[[place_col_orig]]

  n_total <- nrow(raw_df)
  n_na_name <- sum(is.na(name_col))
  has_merged_cells <- n_na_name > n_total * 0.3

  # 品种填充
  if (has_merged_cells) {
    name_filled <- raw_df %>%
      mutate(`_tmp_` = !!sym(name_col_orig)) %>%
      tidyr::fill(`_tmp_`, .direction = "down") %>%
      pull(`_tmp_`)
  } else {
    name_filled <- name_col
  }

  # stageid 合并单元格填充：当 stageid 列存在时，
  # 跟踪每个品种组（name 首次非 NA 时）对应的 stageid，
  # 后续 NA stageid 行继承该 stageid（而非被 readxl 展开的下一个合并值覆盖）
  stageid_filled <- NULL
  if (!is.null(stageid_col_orig) && stageid_col_orig %in% names(raw_df)) {
    sid_raw <- raw_df[[stageid_col_orig]]
    current_stageid <- sid_raw[1]
    current_variety  <- name_filled[1]
    stageid_filled <- sid_raw
    for (i in seq_along(sid_raw)) {
      var <- name_filled[i]
      sid <- sid_raw[i]
      var_empty <- is_empty_str(var)
      sid_empty <- is_empty_str(sid)
      if (!var_empty && var != current_variety) {
        # 品种非空且发生变化（新品种块开始）：更新追踪器
        # stageid 取自地点行的正确值（品种为 NA 的行）；品种行的 stageid 已被 readxl 污染
        if (!sid_empty) {
          current_stageid <- sid
        }
        current_variety <- var
        # 品种行本身也继承当前 stageid（地点行的 stageid 尚未到达时）
        stageid_filled[i] <- current_stageid
      } else if (var_empty) {
        # 品种为 NA → 这是地点行，stageid 来自 readxl 展开（正确值）
        stageid_filled[i] <- sid
        if (!sid_empty) {
          # 更新追踪器为地点行的正确 stageid
          current_stageid <- sid
        }
      } else {
        # 品种非空且品种没变（罕见）→ 继承当前 stageid
        stageid_filled[i] <- current_stageid
      }
    }
  }

  ck_mask <- is_ck_variety(name_filled)
  summary_mask <- is_summary_site(place_col)

  list(
    has_merged_cells = has_merged_cells,
    name_filled      = name_filled,
    stageid_filled   = stageid_filled,
    ck_mask          = ck_mask,
    summary_mask     = summary_mask
  )
}

# =============================================================================
# 质量性状标准化 (→ soyplant::qr_trait$level_C)
# =============================================================================

# 语义别名表：原始值 → level_C（覆盖常见非标准写法）
SEMANTIC_ALIASES <- list(
  花色 = c("白花" = "1-白", "紫花" = "2-紫", "白色" = "1-白", "紫色" = "2-紫"),
  叶形 = c("披针形" = "1-披针", "卵圆形" = "2-卵圆", "椭圆形" = "3-椭圆"),
  茸毛色 = c("灰色" = "1-灰", "棕色" = "2-棕", "灰" = "1-灰", "棕" = "2-棕"),
  结荚习性 = c("有限结荚" = "7-有限", "无限结荚" = "3-无限",
                "亚有限" = "5-亚有限", "有限性" = "7-有限", "无限性" = "3-无限"),
  倒伏性 = c("抗倒伏" = "1-不倒", "抗倒" = "1-不倒", "不倒伏" = "1-不倒",
              "轻倒" = "3-轻倒", "中倒" = "5-中倒", "重倒" = "7-重倒",
              "严重倒" = "9-严重倒"),
  生长习性 = c("直立型" = "1-直立", "半直立" = "3-半直立",
                "半蔓生" = "5-半蔓生", "蔓生" = "7-蔓生"),
  株型 = c("收敛" = "3-收敛型", "半开张" = "5-半开张", "开张" = "7-开张"),
  落叶性 = c("不落叶" = "1-不落", "半落叶" = "2-半落", "落叶" = "3-落"),
  裂荚性 = c("不裂" = "3-不裂", "轻裂" = "5-轻裂0-9", "中裂" = "7-中9-25", "易裂" = "9-易裂>25"),
  荚形 = c("直形" = "1-直形", "弯镰" = "2-弯镰形", "弓形" = "3-弓形"),
  荚熟色 = c("灰褐" = "1-灰褐", "黄褐" = "2-黄褐", "褐" = "3-褐",
              "深褐" = "4-深褐", "黑" = "5-黑"),
  粒型 = c("圆形" = "1-圆", "扁圆形" = "2-扁圆", "椭圆形" = "3-椭圆",
           "扁椭圆" = "4-扁椭圆", "长椭圆" = "5-长椭圆", "肾形" = "6-肾"),
  种皮色 = c("黄色" = "1-黄色", "绿色" = "2-绿色", "黑色" = "3-黑色",
              "褐色" = "4-褐色", "双色" = "5-双色"),
  脐色 = c("黄色" = "1-黄", "淡褐" = "2-淡褐", "褐" = "3-褐",
           "深褐" = "4-深褐", "蓝" = "5-蓝", "淡黑" = "6-淡黑", "黑" = "7-黑"),
  子叶色 = c("黄色" = "1-黄", "绿色" = "2-绿"),
  种皮光泽 = c("无光" = "0-无", "微光" = "1-微", "强光" = "2-强", "无" = "0-无"),
  出苗良否 = c("良" = "2-优", "优" = "2-优", "中" = "0-中", "差" = "-1-差"),
  苗期田间评价 = c("良" = "2-优", "优" = "2-优", "中" = "0-中", "差" = "-1-差"),
  花期田间评价 = c("良" = "2-优", "优" = "2-优", "中" = "0-中", "差" = "-1-差"),
  成熟期田间评价 = c("良" = "2-优", "优" = "2-优", "中" = "0-中", "差" = "-1-差"),
  籽粒评价 = c("良" = "2-优", "优" = "2-优", "中" = "0-中", "差" = "-1-差"),
  活秆成熟 = c("是" = "1-是", "否" = "0-否", "有" = "1-是", "无" = "0-否"),
  早衰性 = c("是" = "1-是", "否" = "0-否", "有" = "1-是", "无" = "0-否"),
  草甘膦抗性 = c("抗" = "7-不抗", "不抗" = "7-不抗")
)

get_levels_for_trait <- function(name_lib_val) {
  if (!requireNamespace("soyplant", quietly = TRUE)) return(character(0))
  qt <- soyplant::qr_trait
  qt <- qt[qt$name_lib == name_lib_val & !is.na(qt$level_C), ]
  as.character(qt$level_C)
}

extract_level_text <- function(lc) {
  sub("^\\d+-", "", lc)
}

match_to_level <- function(raw_val, levels, aliases = NULL) {
  if (is.na(raw_val) || raw_val == "" || as.character(raw_val) %in% c("/", "-", "—")) {
    return(NA_character_)
  }
  raw_str <- as.character(raw_val)

  # 0. 语义别名直接匹配（如 "抗倒伏" → "1-不倒"）
  if (!is.null(aliases) && raw_str %in% names(aliases)) {
    matched <- aliases[raw_str]
    if (matched %in% levels) return(unname(matched))
  }

  # 1. 原始值恰好等于 level_C（如 "1-白"）
  if (raw_str %in% levels) return(raw_str)

  level_texts <- sapply(levels, extract_level_text, USE.NAMES = TRUE)

  # 2. 原始值恰好等于 level_C 文本部分（如 "白"）
  exact <- levels[level_texts == raw_str]
  if (length(exact) > 0) return(exact[1])

  # 3. level_C 文本包含于原始值（如 "白" 在 "白花" 中）
  for (i in seq_along(level_texts)) {
    if (grepl(level_texts[i], raw_str, fixed = TRUE)) return(levels[i])
  }

  # 4. 原始值包含于 level_C 文本（如 "黄" 在 "黄色" 中）
  for (i in seq_along(level_texts)) {
    if (grepl(raw_str, level_texts[i], fixed = TRUE)) return(levels[i])
  }

  # 5. 如果别名表中有关键词部分匹配也接受
  if (!is.null(aliases)) {
    for (alias_name in names(aliases)) {
      if (grepl(alias_name, raw_str, fixed = TRUE) ||
          grepl(raw_str, alias_name, fixed = TRUE)) {
        matched <- aliases[alias_name]
        if (matched %in% levels) return(unname(matched))
      }
    }
  }

  raw_str
}

# 数值→文本转换表（国区试验分级标准）
NUMERIC_TO_TEXT <- list(
  DaoFuXing = c("1"="不倒", "2"="轻倒", "3"="中倒", "4"="重倒", "5"="严重倒")
)

standardizeQualityTraits <- function(df) {
  if (!requireNamespace("soyplant", quietly = TRUE)) return(list(df=df, unresolved=NULL))
  qt <- soyplant::qr_trait
  categorical_traits <- qt[qt$field_type == "C" & !is.na(qt$name_lib), ]
  trait_names <- unique(as.character(categorical_traits$name_lib))

  unresolved <- list()

  for (trait_col in intersect(names(df), trait_names)) {
    if (!any(!is.na(df[[trait_col]]))) next
    levels <- get_levels_for_trait(trait_col)
    if (length(levels) == 0) next
    level_set <- as.character(levels)

    trait_cn <- as.character(categorical_traits$name_C[categorical_traits$name_lib == trait_col][1])
    aliases <- SEMANTIC_ALIASES[[trait_cn]]
    n2t <- NUMERIC_TO_TEXT[[trait_col]]

    unmatched_vals <- character(0)

    matched <- sapply(seq_len(nrow(df)), function(i) {
      raw_val <- df[[trait_col]][i]
      if (is.na(raw_val) || as.character(raw_val) %in% c("","/","-","—","NA","na")) return(NA_character_)
      v <- as.character(raw_val)

      # 数值→文本转换
      if (!is.null(n2t) && v %in% names(n2t)) v <- unname(n2t[v])

      result <- match_to_level(v, levels, aliases)

      # 记录未匹配
      unresolved_vals <- if (is.na(result) || !result %in% level_set) {
        unique(c(unmatched_vals, v))
      }
      # Can't modify parent scope in sapply, we collect below

      if (is.na(result) || !result %in% level_set) {
        # Track via a side channel
        result
      } else {
        result
      }
    })

    # 第二遍收集未匹配值
    seen_unmatched <- character(0)
    for (i in seq_len(nrow(df))) {
      raw_val <- df[[trait_col]][i]
      if (is.na(raw_val) || as.character(raw_val) %in% c("","/","-","—","NA","na")) next
      v <- as.character(raw_val)
      if (!is.null(n2t) && v %in% names(n2t)) v <- unname(n2t[v])
      m <- matched[i]
      if (is.na(m) || !m %in% level_set) {
        if (!v %in% seen_unmatched) seen_unmatched <- c(seen_unmatched, v)
      }
    }

    if (length(seen_unmatched) > 0) {
      unresolved[[trait_col]] <- list(
        name_cn  = trait_cn,
        name_lib = trait_col,
        values   = seen_unmatched,
        levels   = level_set
      )
    }

    df[[trait_col]] <- matched
  }

  list(df = df, unresolved = unresolved)
}

# =============================================================================
# 主标准化函数
# =============================================================================

#' 将原始Excel数据标准化为其他试验数据库格式
#' @param raw_df 原始数据框
#' @param mapping 列映射：原始列名 → 标准字段名（命名向量）
#' @param structure detectStructure() 的结果
#' @param metadata list(trial_name, group_label)
#' @return 标准化后的 data.frame
standardizeOtherTrial <- function(raw_df, mapping, structure, metadata) {

  # 1. 品种填充
  if (structure$has_merged_cells) {
    name_col_orig <- names(mapping)[mapping == "name"][1]
    raw_df[[name_col_orig]] <- structure$name_filled
  }

  # 1.5 stageid 填充（合并单元格展开后，品种切换时的 stageid 需正确继承）
  if (!is.null(structure$stageid_filled)) {
    stageid_col_orig <- names(mapping)[mapping == "stageid"][1]
    if (!is.na(stageid_col_orig) && stageid_col_orig %in% names(raw_df)) {
      raw_df[[stageid_col_orig]] <- structure$stageid_filled
    }
  }

  # 2. 剔除汇总行
  place_col_orig <- names(mapping)[mapping == "place"][1]
  raw_df <- raw_df[!structure$summary_mask, , drop = FALSE]

  if (nrow(raw_df) == 0) return(NULL)

  # 3. 构建结果（行数与raw_df一致）— 包含全部 FIELD_RECORD_COLS 性状列
  n <- nrow(raw_df)

  # 基础列（非性状）
  base_cols <- list(
    import_batch_id = rep("", n),
    trial_name      = rep(if (is.null(metadata$trial_name) || nchar(metadata$trial_name) == 0) "" else metadata$trial_name, n),
    group_label     = rep(if (is.null(metadata$group_label) || nchar(metadata$group_label) == 0) "" else metadata$group_label, n)
  )

  # 核心映射列
  core_cols <- list(
    name            = rep(NA_character_, n),
    place           = rep(NA_character_, n),
    MuChan          = rep(NA_real_, n),
    XiaoQuChanLiang = rep(NA_real_, n),
    XiaoQuShiShouMianJi = rep(NA_real_, n),
    stageid         = rep(NA_character_, n),
    rp              = rep("1", n),
    treatment       = rep(NA_character_, n),
    is_ck           = rep(0L, n),
    ma              = rep(NA_character_, n),
    pa              = rep(NA_character_, n),
    extra_cols      = rep(NA_character_, n)
  )

  # 全部性状列（从 FIELD_RECORD_COLS 中提取，排除了基础列）
  TRAIT_COLS <- c(
    "HanShuiLiang",
    "BoZhongQi","ChuMiaoQi","ChuMiaoLiangFou","MiaoQiTianJianPingJia",
    "KaiHuaQi","HuaSe","HuaQiTianJianPingJia","YeXing","RongMaoSe",
    "ShengZhangXiXing","JieJiaXiXing","DaoFuXing","ZaoShuaiXing","ZhuXing",
    "LuoYeXing","LieJiaXing","ChengShuQi","HuoGanChengShu","ChengShuQiTianJianPingJia",
    "ShouHuoQi","XiaoQuShouHuoZhuShu","ShengYuQi","TianJianBeiZhu",
    "HuaYeBingDuBing","NiJingDianZhongFuBing","ShuangMeiBing","HuiBanBing",
    "XiJunXingBanDianBing","XiuBing","GenFuBing","BaoNangXianChongBing",
    "QiTaBingHai","DouGanHeiQianYing","DouJiaMing","YaChong","ShiYeXingHaiChong",
    "KaoZhongZhuShu","ZhuGao","DiJiaGao","FenZhiShu","ZhuJingJieShu","JiaXing",
    "JiaShuSe","YouXiaoJia","WuXiaoJia","DanZhuJiaShu","DanZhuLiShu","DanZhuLiZhong",
    "MeiJiaLiShu","LiXing","ZhongPiSe","QiSe","ZiYeSe","ZhongPiGuangZe",
    "BaiLiZhong","WanHaoLiLv","PoSuiLiLv","BingLiLv","ZiBanLiLv","HeBanLiLv",
    "ShuangMeiLiLv","HuiBanLiLv","ChongShiLiLv","ZiLiPingJia",
    "DanBai","ZhiFang","DanZhiHe","CaoGanLinKangXing","ShiZhiJianCe","HanJiYin",
    "BoZhongPenShu","BoZhongLiShu","ChuMiaoShu","ChuMiaoLiShu",
    "NaiYanXing","NaiHanXing","ShiHuaQi","ZaJiaoHuaShu","ChengHuoJiaShu","ZhaJiaoliShu",
    "ChuShuQi","WanShuQi","HuiFuLv","SSRBuHeGeWeiDian"
  )

  trait_cols <- setNames(lapply(TRAIT_COLS, function(cn) rep(NA_character_, n)), TRAIT_COLS)

  result <- do.call(data.frame,
    c(base_cols, core_cols, trait_cols, list(stringsAsFactors = FALSE)))

  # 4. 按映射填入数据
  # 数值性状列
  numeric_fields <- c("MuChan", "XiaoQuChanLiang", "XiaoQuShiShouMianJi",
    "ShengYuQi", "ZhuGao", "BaiLiZhong", "DanBai", "ZhiFang", "HanShuiLiang",
    "KaoZhongZhuShu", "XiaoQuShouHuoZhuShu", "DiJiaGao", "FenZhiShu",
    "ZhuJingJieShu", "YouXiaoJia", "WuXiaoJia", "DanZhuJiaShu", "DanZhuLiShu",
    "DanZhuLiZhong", "MeiJiaLiShu", "BaiLiZhong", "WanHaoLiLv", "PoSuiLiLv",
    "BingLiLv", "ZiBanLiLv", "HeBanLiLv", "ShuangMeiLiLv", "HuiBanLiLv",
    "ChongShiLiLv", "DanZhiHe", "BoZhongPenShu", "BoZhongLiShu",
    "ChuMiaoShu", "ChuMiaoLiShu", "ZaJiaoHuaShu", "ChengHuoJiaShu",
    "ZhaJiaoliShu", "HuiFuLv")

  extra_pairs <- list()

  for (orig_col in names(mapping)) {
    target <- mapping[[orig_col]]
    if (is.na(target) || target == "ignore") next
    if (!target %in% names(result)) {
      # 未定义字段 → 收集到 extra_cols
      if (orig_col %in% names(raw_df)) {
        extra_pairs[[orig_col]] <- raw_df[[orig_col]]
      }
      next
    }
    if (orig_col %in% names(raw_df)) {
      val <- raw_df[[orig_col]]
      if (target %in% numeric_fields) {
        result[[target]] <- clean_numeric(val)
      } else {
        result[[target]] <- as.character(val)
      }
    }
  }

  # 4.5 清洗所有字符列中的 "/" "-" "—" 为空值
  for (cn in names(result)) {
    if (!is.character(result[[cn]])) next
    x <- result[[cn]]
    x[x %in% c("/", "-", "—")] <- NA_character_
    result[[cn]] <- x
  }

  # 4.6 亩产为0视为无产量数据
  result$MuChan[!is.na(result$MuChan) & result$MuChan == 0] <- NA_real_

  # 日期性状：Excel序列数 → "YYYY-MM-DD"
  date_traits <- c("BoZhongQi","ChuMiaoQi","KaiHuaQi","ChengShuQi","ChengShuQiTianJianPingJia",
                   "ShouHuoQi","ShiHuaQi","ChuShuQi","WanShuQi")
  for (dn in intersect(date_traits, names(result))) {
    vals <- result[[dn]]
    if (!is.character(vals)) vals <- as.character(vals)
    nums <- suppressWarnings(as.numeric(vals))
    # Excel 日期序列数范围 30000-60000（1982-2064）
    date_mask <- !is.na(nums) & nums > 30000 & nums < 60000
    if (any(date_mask)) {
      converted <- as.Date(nums[date_mask], origin = "1899-12-30")
      result[[dn]] <- vals  # ensure character type
      result[[dn]][date_mask] <- format(converted, "%Y-%m-%d")
    }
  }

  # 5. 标记CK
  result$is_ck <- ifelse(is_ck_variety(result$name), 1L, 0L)

  # 6. 质量性状标准化
  qt_result <- standardizeQualityTraits(result)
  result <- qt_result$df
  unresolved <- qt_result$unresolved

  # 6.5 剔除必填列为空的行（place / name 为 NOT NULL，且有地点才能分析）
  before <- nrow(result)
  result <- result[!is.na(result$place) & nchar(trimws(result$place)) > 0 &
                   !is.na(result$name)  & nchar(trimws(result$name)) > 0, , drop = FALSE]
  if (nrow(result) < before) {
    attr(result, "dropped_empty_rows") <- before - nrow(result)
  }
  if (nrow(result) == 0) return(NULL)

  # 7. 额外列打包为 JSON
  if (length(extra_pairs) > 0) {
    extra_df <- as.data.frame(extra_pairs, stringsAsFactors = FALSE)
    result$extra_cols <- vapply(seq_len(nrow(extra_df)), function(i) {
      row_list <- as.list(extra_df[i, , drop = FALSE])
      jsonlite::toJSON(row_list, auto_unbox = TRUE)
    }, character(1))
  }

  attr(result, "unresolved_quality") <- unresolved
  result
}

# =============================================================================
# 顶层函数
# =============================================================================

readRegionalRaw <- function(file, sheet = 1) {
  raw_df <- readxl::read_xlsx(file, sheet = sheet, col_names = TRUE)
  list(raw_df = raw_df, raw_cols = names(raw_df))
}

processRegionalImport <- function(file, sheet = 1, mapping = NULL, metadata = list()) {
  raw <- readRegionalRaw(file, sheet)

  if (is.null(mapping)) {
    auto <- autoDetectColumns(raw$raw_cols)
    mapping <- as.list(auto)
    names(mapping) <- raw$raw_cols
  }

  # 定位关键列
  name_orig   <- names(mapping)[mapping == "name"][1]
  place_orig  <- names(mapping)[mapping == "place"][1]
  stageid_orig <- names(mapping)[mapping == "stageid"][1]
  if (is.na(name_orig)) stop("未找到品种列的映射")
  if (is.na(place_orig)) stop("未找到地点列的映射")

  structure <- detectStructure(raw$raw_df, name_orig, place_orig, stageid_orig)

  df <- standardizeOtherTrial(raw$raw_df, mapping, structure, metadata)
  if (is.null(df)) stop("数据为空（可能全部被过滤）")

  unresolved <- attr(df, "unresolved_quality", exact = TRUE)
  attr(df, "unresolved_quality") <- NULL
  dropped  <- attr(df, "dropped_empty_rows", exact = TRUE)
  if (is.null(dropped)) dropped <- 0L
  attr(df, "dropped_empty_rows") <- NULL

  n_sites <- length(unique(df$place[!is.na(df$place)]))
  n_varieties <- length(unique(df$name[!is.na(df$name)]))

  list(
    data        = df,
    unresolved  = unresolved,
    stats       = list(
      n_rows       = nrow(df),
      n_sites      = n_sites,
      n_varieties  = n_varieties,
      has_yield    = !all(is.na(df$MuChan)),
      dropped_rows = dropped
    )
  )
}
