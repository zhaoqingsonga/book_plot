# ==============================================================================
# Excel 报告导出
# ==============================================================================

#' 构建并保存分析 Excel 报告
#'
#' @param result run_analysis() 的返回值
#' @param file 输出文件路径
#' @export
build_analysis_excel <- function(result, file) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("请先安装 openxlsx 包")
  }

  wb <- openxlsx::createWorkbook()

  # 信息页
  openxlsx::addWorksheet(wb, "分析信息")
  info_df <- data.frame(
    项目 = c("试验类型", "地点数", "重复数", "性状数据",
      paste0("地点_", seq_along(result$trial_info$places)),
      "可用分析"),
    内容 = c(
      result$trial_info$label,
      as.character(result$trial_info$n_places),
      as.character(result$trial_info$n_reps),
      if (result$has_traits) "有" else "无",
      result$trial_info$places,
      paste(result$capabilities$available, collapse = "、")
    ),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "分析信息", info_df)

  # 遍历所有 table 写入对应 sheet
  for (nm in names(result$tables)) {
    tbl <- result$tables[[nm]]
    if (is.data.frame(tbl) && nrow(tbl) > 0) {
      # 截断过长的 sheet 名
      sheet_name <- substr(nm, 1, 31)
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, tbl)
    }
  }

  # 保存
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
