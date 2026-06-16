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

  # 筛选摘要（如果有）
  if (!is.null(result$tables$screening_summary)) {
    s <- result$tables$screening_summary
    openxlsx::addWorksheet(wb, "筛选摘要")
    summary_df <- data.frame(
      项目 = c("总品种数", "晋级", "分离选单株", "淘汰",
        "淘汰-位次不达标", "淘汰-分离材料", "淘汰-倒伏排除"),
      数量 = c(s$total_n, s$promoted_n, s$select_plant_n, s$eliminated_n,
        s$breakdown$rank_fail, s$breakdown$separated, s$breakdown$lodging),
      stringsAsFactors = FALSE
    )
    openxlsx::writeData(wb, "筛选摘要", summary_df)
  }

  # Sheet 名映射（中文友好）
  sheet_name_map <- c(
    promoted       = "晋级材料",
    select_plant   = "分离选单株",
    eliminated     = "淘汰材料",
    description    = "品种描述",
    yield_stats    = "产量统计",
    yield_ranking  = "产量排名",
    overview       = "数据概览",
    growth_stats   = "生育期统计",
    increase_stats = "增产统计",
    parent_stats   = "优良亲本",
    cross_stats    = "优良组合",
    gge_stable     = "高产稳定基因型",
    gge_unstable   = "高产不稳基因型",
    gge_env        = "环境汇总",
    cross_site_ranking = "跨地点排名",
    cross_location_avg  = "跨地点平均",
    per_site_yield_stats    = "分地点产量",
    per_site_growth_stats   = "分地点生育期",
    per_site_increase_stats = "分地点增产",
    per_site_ck_mean        = "分地点对照均值",
    gen_dist       = "世代分布",
    gen_track      = "世代追踪",
    cross_top      = "优良组合",
    trait_overview = "性状概览",
    sele_overview  = "株行概览",
    sele_dist      = "株行分布",
    progeny_top    = "优良后代",
    morph_stats    = "形态统计",
    export_data    = "原始数据",
    gge_yield_growth = "产量生育期"
  )

  # 遍历所有 table 写入对应 sheet
  for (nm in names(result$tables)) {
    tbl <- result$tables[[nm]]
    if (is.data.frame(tbl) && nrow(tbl) > 0) {
      sheet_name <- if (nm %in% names(sheet_name_map)) {
        sheet_name_map[[nm]]
      } else {
        nm
      }
      sheet_name <- substr(sheet_name, 1, 31)
      openxlsx::addWorksheet(wb, sheet_name)
      openxlsx::writeData(wb, sheet_name, tbl)
    }
  }

  # 保存
  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}
