# ==============================================================================
# 试验类型自动检测
# ==============================================================================

#' 检测试验类型
#'
#' 根据数据中的 place（地点）和 rp（重复）列自动判定试验配置类型。
#'
#' @param df 数据框，需包含 place 和 rp 列
#' @return list(type, n_places, n_reps, places, reps, label)
#' @export
detect_trial_type <- function(df) {
  # 提取地点和重复信息
  places <- unique(as.character(df[["place"]]))
  places <- places[!is.na(places) & nchar(trimws(places)) > 0]

  # rp 列可能不存在（某些模块），默认为 1
  if ("rp" %in% colnames(df)) {
    reps <- unique(df[["rp"]])
    reps <- reps[!is.na(reps)]
  } else {
    reps <- 1L
  }

  n_places <- length(places)
  n_reps <- length(reps)

  # 判定类型
  if (n_places == 1L && n_reps == 1L) {
    type <- "1S1R"
    label <- "单点单重复"
    desc <- paste0("试验地点：", places[1], "，重复数：", n_reps)
  } else if (n_places == 1L && n_reps > 1L) {
    type <- "1SMR"
    label <- "单点多重复"
    desc <- paste0("试验地点：", places[1], "，重复数：", n_reps)
  } else if (n_places > 1L && n_reps == 1L) {
    type <- "MS1R"
    label <- "多点单重复"
    desc <- paste0("地点数：", n_places, "（", paste(places, collapse = "、"), "），每点", n_reps, "个重复")
  } else {
    type <- "MSMR"
    label <- "多点多重复"
    desc <- paste0("地点数：", n_places, "（", paste(places, collapse = "、"), "），每点", n_reps, "个重复")
  }

  list(
    type       = type,
    label      = label,
    desc       = desc,
    n_places   = n_places,
    n_reps     = n_reps,
    places     = places,
    reps       = reps,
    is_single_site = n_places == 1L,
    is_multi_site  = n_places > 1L,
    is_single_rep  = n_reps == 1L,
    is_multi_rep   = n_reps > 1L,
    can_do_gge     = n_places >= 3L  # GGE 至少需要 3 个环境
  )
}

#' 获取试验类型的分析能力描述
#'
#' @param trial_info detect_trial_type() 的返回值
#' @param has_traits 是否有性状数据
#' @return 字符向量，描述可用和不可用的分析
#' @export
get_analysis_capabilities <- function(trial_info, has_traits) {
  caps <- list(
    available   = character(),
    unavailable = character()
  )

  caps$available <- c(caps$available, "基础数据概览")

  if (trial_info$is_single_site) {
    caps$available <- c(caps$available, "产量排名")
  } else {
    caps$available <- c(caps$available, "跨地点排名筛选", "分地点产量统计")
    if (trial_info$can_do_gge) {
      caps$available <- c(caps$available, "GGE 双标图分析")
      caps$available <- c(caps$available, "品种稳定性分析")
      caps$available <- c(caps$available, "环境区分力分析")
    } else {
      caps$unavailable <- c(caps$unavailable,
        paste0("GGE 分析（需要 ≥3 个环境，当前 ", trial_info$n_places, " 个）"))
    }
  }

  if (has_traits) {
    caps$available <- c(caps$available, "产量统计分析")
    caps$available <- c(caps$available, "产量分布直方图")
    caps$available <- c(caps$available, "性状相关性分析")
    caps$available <- c(caps$available, "品种筛选（位次+倒伏）")
    caps$available <- c(caps$available, "筛选前后性状对比")
    caps$available <- c(caps$available, "雷达图")
    caps$available <- c(caps$available, "亲本与组合分析")
  } else {
    caps$unavailable <- c(caps$unavailable, "产量统计分析（缺少 MuChan 数据）")
    caps$unavailable <- c(caps$unavailable, "性状可视化（缺少田间调查数据）")
    caps$unavailable <- c(caps$unavailable, "品种筛选（缺少位次数据）")
    caps$unavailable <- c(caps$unavailable, "筛选前后性状对比（缺少性状数据）")
    caps$unavailable <- c(caps$unavailable, "雷达图（缺少产量数据）")
    caps$unavailable <- c(caps$unavailable, "亲本与组合分析（缺少产量数据）")
  }

  caps
}
