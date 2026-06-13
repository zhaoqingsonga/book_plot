# ==============================================================================
# 品种描述性评述
# 移植自 参考脚本/00-main_function.R 的 soybean_comprehensive_evaluation_final()
# ==============================================================================

#' 生成晋级材料描述性评述
#'
#' @param df 数据框（拼音列名，晋级材料子集）
#' @return 字符串，所有品种的综合性状描述文本
#' @export
generate_description <- function(df) {
  rdf <- adapt_to_reference(df)

  if (!"品种名称" %in% colnames(rdf) || !"阶段名称" %in% colnames(rdf)) {
    return("数据缺少必需的品种名称或阶段名称字段。")
  }

  # 补全可能缺失的字段
  rdf <- preprocess_description_data(rdf)

  # 逐行生成评述
  results <- lapply(seq_len(nrow(rdf)), function(i) {
    build_full_description(rdf[i, , drop = FALSE])
  })

  paste(unlist(results), collapse = "")
}

# ==============================================================================
# 内部工具函数
# ==============================================================================

sv <- function(row, col, default = "") {
  val <- tryCatch(row[[col]], error = function(e) NULL)
  if (is.null(val) || length(val) == 0L) return(default)
  val <- as.character(val)
  if (is.na(val) || val == "") default else val
}

is_missing <- function(value) {
  is.na(value) || (is.character(value) && value == "")
}

extract_qual <- function(value) {
  if (is_missing(value)) return("")
  value_char <- as.character(value)
  if (grepl("^\\d+[-－](.+)$", value_char))
    sub("^\\d+[-－](.+)$", "\\1", value_char)
  else
    value_char
}

maxConsecutiveSlash <- function(str) {
  if (is_missing(str)) return(0)
  str <- as.character(str)
  match <- gregexpr("/+", str)[[1]]
  if (match[1] == -1) 0 else max(attr(match, "match.length"))
}

# ==============================================================================
# 数据预处理：补全缺失字段
# ==============================================================================

preprocess_description_data <- function(data) {
  required_fields <- c(
    "阶段名称", "品种名称", "母本", "父本", "地点", "田间ID", "田间备注",
    "生育期_d", "结荚习性", "株型", "株高_cm", "主茎节数", "分枝数", "底荚高_cm",
    "叶形", "花色", "茸毛色", "荚熟色", "有效荚", "粒形", "种皮色",
    "种皮光泽", "脐色", "百粒重_g", "倒伏性", "抗病性",
    "蛋白质含量_pct", "脂肪含量_pct",
    "亩产_kg", "较临近对照增产_pct", "较平均对照增产_pct",
    "较临近对照位次", "较平均对照位次"
  )

  for (field in required_fields) {
    if (!field %in% colnames(data)) data[[field]] <- ""
  }

  data[["阶段名称"]][is.na(data[["阶段名称"]]) | data[["阶段名称"]] == ""] <- "未命名阶段"
  data[["品种名称"]][is.na(data[["品种名称"]]) | data[["品种名称"]] == ""] <- "未命名品种"

  data
}

# ==============================================================================
# 自然语言描述生成器
# ==============================================================================

describe_traits_natural <- function(trait_values, trait_label_map = NULL) {
  if (length(trait_values) == 0) return("无相关描述信息。\n")

  descs <- c()
  for (tr in names(trait_values)) {
    label <- if (!is.null(trait_label_map)) trait_label_map[[tr]] else tr
    val <- trait_values[[tr]]
    if (is_missing(val)) next

    if (tr == "生育期_d") {
      descs <- c(descs, paste0("生育期约为", val, "天"))
    } else if (tr == "株高_cm") {
      descs <- c(descs, paste0("株高", val, "厘米"))
    } else if (tr == "底荚高_cm") {
      descs <- c(descs, paste0("底荚高", val, "厘米"))
    } else if (tr == "百粒重_g") {
      descs <- c(descs, paste0("百粒重约", val, "克"))
    } else if (tr %in% c("主茎节数", "分枝数", "有效荚")) {
      descs <- c(descs, paste0(label, val, "个"))
    } else if (tr == "花色") {
      descs <- c(descs, paste0(val, "花"))
    } else if (tr == "茸毛色") {
      descs <- c(descs, paste0(val, "毛"))
    } else if (tr == "种皮色") {
      descs <- c(descs, paste0(val, "种皮"))
    } else if (tr == "种皮光泽") {
      descs <- c(descs, paste0(val, "光泽"))
    } else if (tr == "脐色") {
      descs <- c(descs, paste0(val, "脐"))
    } else if (tr == "结荚习性") {
      descs <- c(descs, paste0(val, "结荚习性"))
    } else if (tr == "倒伏性") {
      descs <- c(descs, paste0(val, "伏"))
    } else {
      descs <- c(descs, paste0(label, val))
    }
  }

  paste0(paste(descs, collapse = "，"), "。\n")
}

describe_quality_natural <- function(protein, fat) {
  if (protein == "" && fat == "") return("无相关描述信息。\n")

  if (protein != "" && fat != "") {
    paste0("蛋白质含量约为", protein, "%，脂肪含量约为", fat, "%。\n")
  } else if (protein != "") {
    paste0("蛋白质含量约为", protein, "%。\n")
  } else {
    paste0("脂肪含量约为", fat, "%。\n")
  }
}

# ==============================================================================
# 核心：单品种完整评述
# ==============================================================================

build_full_description <- function(row) {
  # ---- 配置 ----
  trait_config <- c(
    "生育期_d", "结荚习性", "株型", "株高_cm", "主茎节数", "分枝数", "底荚高_cm",
    "叶形", "花色", "茸毛色", "荚熟色", "有效荚", "粒形", "种皮色",
    "种皮光泽", "脐色", "百粒重_g", "倒伏性", "抗病性"
  )

  trait_label_map <- list(
    "生育期_d"   = "生育期",   "结荚习性"   = "结荚习性", "株型"       = "株型",
    "株高_cm"    = "株高",     "主茎节数"   = "主茎节数", "分枝数"     = "分枝数",
    "底荚高_cm"  = "底荚高",   "叶形"       = "叶形",     "花色"       = "花色",
    "茸毛色"     = "茸毛色",   "荚熟色"     = "荚熟色",   "有效荚"     = "有效荚",
    "粒形"       = "粒形",     "种皮色"     = "种皮色",   "种皮光泽"   = "种皮光泽",
    "脐色"       = "脐色",     "百粒重_g"   = "百粒重",   "倒伏性"     = "倒伏性",
    "抗病性"     = "抗病性"
  )

  qual_cols <- c("倒伏性", "抗病性", "结荚习性", "株型", "叶形", "花色",
                 "茸毛色", "荚熟色", "粒形", "种皮色", "种皮光泽", "脐色")

  # ---- 基本信息 ----
  stage_name <- sv(row, "阶段名称")
  var_name   <- sv(row, "品种名称")
  female     <- sv(row, "母本")
  male       <- sv(row, "父本")
  location   <- sv(row, "地点")
  field_id   <- sv(row, "田间ID")

  res <- paste0(stage_name, "\n")
  res <- paste0(res, "基本信息：品种名称为", var_name, "，")

  if (female != "" || male != "") {
    if (female != "" && male != "") {
      m1 <- maxConsecutiveSlash(female)
      m2 <- maxConsecutiveSlash(male)
      connector <- paste0(rep("/", max(m1, m2) + 1), collapse = "")
      parents_val <- paste0(female, connector, male)
    } else if (female != "") {
      parents_val <- female
    } else {
      parents_val <- male
    }
    res <- paste0(res, "亲本为", parents_val, "，")
  }
  if (location != "") res <- paste0(res, "试验地点为", location, "，")
  if (field_id != "") res <- paste0(res, "田间编号为", field_id, "，")
  res <- sub("，$", "。\n", res)

  # ---- 特征特性 ----
  trait_values <- sapply(trait_config, function(tr) {
    val <- sv(row, tr)
    if (tr %in% qual_cols) val <- extract_qual(val)
    if (!is_missing(val)) val else NULL
  }, USE.NAMES = TRUE, simplify = FALSE)
  trait_values <- trait_values[!sapply(trait_values, function(x) is.null(x) || x == "")]

  res <- paste0(res, "特征特性：", describe_traits_natural(trait_values, trait_label_map))

  # ---- 品质 ----
  protein <- sv(row, "蛋白质含量_pct")
  fat     <- sv(row, "脂肪含量_pct")
  res <- paste0(res, "品质：", describe_quality_natural(protein, fat))

  # ---- 产量 ----
  yield  <- sv(row, "亩产_kg")
  inc1   <- sv(row, "较临近对照增产_pct")
  inc2   <- sv(row, "较平均对照增产_pct")
  comp1  <- sv(row, "较临近对照位次")
  comp2  <- sv(row, "较平均对照位次")

  res <- paste0(res, "产量：")
  yield_line <- ""
  if (yield != "") yield_line <- paste0(yield_line, "亩产", yield, "kg，")
  if (inc1  != "") yield_line <- paste0(yield_line, "较临近对照增产", inc1, "%，")
  if (inc2  != "") yield_line <- paste0(yield_line, "较平均对照增产", inc2, "%，")
  if (comp1 != "") yield_line <- paste0(yield_line, "较临近对照位次为", comp1, "，")
  if (comp2 != "") yield_line <- paste0(yield_line, "较平均对照位次为", comp2, "，")
  if (nchar(yield_line) > 0) {
    yield_line <- sub("，$", "。\n", yield_line)
    res <- paste0(res, yield_line)
  } else {
    res <- paste0(res, "无相关描述信息。\n")
  }

  # ---- 其它（未归类字段） ----
  classified <- c("阶段名称", "品种名称", "母本", "父本", "地点", "田间ID", "田间备注",
                   trait_config, "蛋白质含量_pct", "脂肪含量_pct",
                   "亩产_kg", "较临近对照增产_pct", "较平均对照增产_pct",
                   "较临近对照位次", "较平均对照位次")
  other_cols <- setdiff(colnames(row), classified)

  res <- paste0(res, "其它：")
  if (length(other_cols) > 0) {
    other_parts <- c()
    for (col in other_cols) {
      val <- sv(row, col)
      if (!is_missing(val)) other_parts <- c(other_parts, paste0(col, "是", as.character(val)))
    }
    if (length(other_parts) > 0) {
      res <- paste0(res, paste(other_parts, collapse = "，"), "。\n")
    } else {
      res <- paste0(res, "无相关描述信息。\n")
    }
  } else {
    res <- paste0(res, "无相关描述信息。\n")
  }

  # ---- 备注 ----
  remark <- sv(row, "田间备注")
  if (remark != "") {
    res <- paste0(res, "备注：", remark, "\n")
  }

  paste0(res, "\n")
}
