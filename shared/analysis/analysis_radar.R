# ==============================================================================
# 雷达图
# 从参考脚本 00-main_function.R 的 save_radar_chart() 移植
# 适配为返回 ggplot 对象的 fmsb 基础图形包装
# ==============================================================================

#' 绘制品种性能雷达图
#'
#' @param df 数据框（拼音列名，需含 MuChan, JiaoLinJinDuiZhaoZengChan, ShengYuQi, ZhuGao, BaiLiZhong, name）
#' @param top_n 展示前 N 个高产品种，默认 5
#' @return 雷达图（fmsb基础图，在renderPlot中直接调用）
#' @export
plot_radar_top <- function(df, top_n = 5) {
  if (!requireNamespace("fmsb", quietly = TRUE) ||
      !requireNamespace("scales", quietly = TRUE) ||
      !requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("tidyr", quietly = TRUE)) return(NULL)

  # 筛选高产品种
  if (!"MuChan" %in% colnames(df)) return(NULL)

  radar_cols <- intersect(c("stageid", "name", "MuChan", "JiaoLinJinDuiZhaoZengChan",
    "ShengYuQi", "ZhuGao", "BaiLiZhong"), colnames(df))

  radar_data <- df %>%
    dplyr::select(dplyr::any_of(radar_cols)) %>%
    dplyr::filter(!is.na(MuChan)) %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(
      dplyr::across(dplyr::where(is.numeric), ~ mean(.x, na.rm = TRUE)),
      dplyr::across(dplyr::any_of("stageid"), ~ dplyr::first(.x)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(MuChan)) %>%
    dplyr::slice_head(n = top_n)

  if (nrow(radar_data) < 1) return(NULL)

  # 构建显示标签：stageid<name（确保唯一）
  display_names <- radar_data$name
  if ("stageid" %in% colnames(radar_data)) {
    display_names <- ifelse(
      !is.na(radar_data$stageid) & radar_data$stageid != "" &
        radar_data$stageid != radar_data$name,
      paste0(radar_data$stageid, "<", radar_data$name),
      radar_data$name
    )
  }
  display_names <- make.unique(display_names, sep = "_")

  # 指标的显示名和单位
  indicator_info <- list(
    MuChan = list(label = "亩产", unit = "kg"),
    JiaoLinJinDuiZhaoZengChan = list(label = "增产率", unit = "%"),
    ShengYuQi = list(label = "生育期", unit = "d"),
    ZhuGao = list(label = "株高", unit = "cm"),
    BaiLiZhong = list(label = "百粒重", unit = "g")
  )

  # 选取存在的指标
  avail_indicators <- intersect(names(indicator_info), colnames(radar_data))
  if (length(avail_indicators) < 2) return(NULL)

  # 标准化到 [0, 1]
  norm_data <- radar_data
  for (idx in avail_indicators) {
    vals <- radar_data[[idx]]
    if (length(unique(vals)) > 1) {
      norm_data[[idx]] <- scales::rescale(as.numeric(vals), to = c(0, 1))
    } else {
      norm_data[[idx]] <- 0.5
    }
  }

  # 添加 fmsb 需要的最大/最小值行
  max_vals <- apply(norm_data[, avail_indicators, drop = FALSE], 2, max, na.rm = TRUE)
  min_vals <- apply(norm_data[, avail_indicators, drop = FALSE], 2, min, na.rm = TRUE)

  fmsb_data <- rbind(max_vals, min_vals,
    norm_data[, avail_indicators, drop = FALSE])
  rownames(fmsb_data) <- c("最大值", "最小值", display_names)

  # 构建指标标签（含范围）
  index_labels <- sapply(avail_indicators, function(idx) {
    info <- indicator_info[[idx]]
    raw_vals <- radar_data[[idx]]
    paste0(info$label, "\n(", round(min(raw_vals, na.rm = TRUE), 1),
      "-", round(max(raw_vals, na.rm = TRUE), 1), info$unit, ")")
  }, USE.NAMES = FALSE)

  # 返回可在 renderPlot 中直接使用的数据
  list(
    data   = fmsb_data,
    labels = index_labels,
    top_n  = top_n,
    names  = display_names
  )
}
