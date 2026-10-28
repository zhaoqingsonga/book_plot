# ==============================================================================
# 通用分析模块「分析」
# ------------------------------------------------------------------------------
# 以「一个试验」为单位：读田间记录本 → 跑 book_plot 自带分析管线（run_analysis）
# → 落盘图表 / 数据表 / Markdown / HTML / Excel / ZIP。
#
# 与 Shiny 应用共用同一套 run_analysis() 与 save_analysis_artifacts()，
# 保证「应用里看到的」与「命令行跑的」是同一份口径。
#
# 依赖：shared/db_persistence.R、shared/mod_analysis.R
# ==============================================================================

# 记录本类型 → 中文标签
ANALYSIS_TRIAL_TYPES <- c(
  yield_test     = "产比",
  population     = "群体",
  line_selection = "株行"
)

# 记录本类型 → 记录表名
ANALYSIS_TRIAL_TABLES <- c(
  yield_test     = "yield_test_field_records",
  population     = "population_field_records",
  line_selection = "line_selection_field_records"
)

#' 列出所有可分析的试验
#'
#' @param db_path 田间记录本 SQLite 路径
#' @return data.frame：experiment_id / experiment_name / module_type / type_label / n
#' @export
analysis_list_trials <- function(db_path = defaultDbPath()) {
  con <- connectDb(db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  rows <- list()
  for (tp in names(ANALYSIS_TRIAL_TABLES)) {
    tb <- ANALYSIS_TRIAL_TABLES[[tp]]
    if (!DBI::dbExistsTable(con, tb)) next
    d <- tryCatch(
      DBI::dbGetQuery(con, sprintf(
        "SELECT experiment_id, experiment_name, COUNT(*) AS n
           FROM %s
          GROUP BY experiment_id, experiment_name
          ORDER BY experiment_id", tb)),
      error = function(e) NULL)
    if (is.null(d) || nrow(d) == 0) next
    d$module_type <- tp
    d$type_label  <- unname(ANALYSIS_TRIAL_TYPES[[tp]])
    rows[[tp]] <- d
  }

  if (length(rows) == 0) {
    return(data.frame(experiment_id = character(), experiment_name = character(),
                      module_type = character(), type_label = character(), n = integer()))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' 读取一个试验的田间记录
#'
#' @param experiment_id 试验 ID
#' @param module_type 记录本类型：yield_test / population / line_selection
#' @param db_path 田间记录本 SQLite 路径
#' @return data.frame（拼音列名，与 run_analysis 期望一致）
#' @export
analysis_load_trial <- function(experiment_id, module_type = "yield_test",
                                 db_path = defaultDbPath()) {
  if (!module_type %in% names(ANALYSIS_TRIAL_TYPES)) {
    stop("未知记录本类型：", module_type,
         "（可选：", paste(names(ANALYSIS_TRIAL_TYPES), collapse = " / "), "）")
  }

  reader <- switch(module_type,
    yield_test     = getYieldTestFieldRecord,
    population     = getPopulationFieldRecord,
    line_selection = getLineSelectionFieldRecord
  )

  df <- reader(experiment_id, db_path = db_path)
  if (is.null(df) || nrow(df) == 0) {
    stop("试验 ", experiment_id, "（", ANALYSIS_TRIAL_TYPES[[module_type]],
         "记录本）没有田间记录")
  }
  df
}

#' 运行一个试验的完整分析并落盘
#'
#' 走 book_plot 自带管线：run_analysis() → save_analysis_artifacts()。
#' 可选传入筛选参数重跑品种筛选（与 Shiny 交互式滑块同一套参数）。
#'
#' @param experiment_id 试验 ID
#' @param module_type 记录本类型
#' @param db_path 田间记录本 SQLite 路径
#' @param out_dir 输出目录；NULL 时用「分析输出/<试验ID_类型>」
#' @param screening_params list()，传给 analyze_screening() 的参数
#'   （rank_threshold_select / rank_threshold_plant / exclude_lodging / ...）
#' @param zip_output 是否额外产出 ZIP 包
#' @return list(result, out_dir, files, messages)
#' @export
analysis_run <- function(experiment_id, module_type = "yield_test",
                         db_path = defaultDbPath(), out_dir = NULL,
                         screening_params = NULL, zip_output = TRUE) {

  df <- analysis_load_trial(experiment_id, module_type, db_path)

  result <- run_analysis(df, module_type)

  # --- 可选：按传入参数重跑品种筛选（与 Shiny 滑块一致，只重跑筛选不重跑整条管线） ---
  if (!is.null(screening_params) && module_type == "yield_test") {
    base_df <- result$tables$export_data
    if (!is.null(base_df) && is.data.frame(base_df)) {
      scr <- tryCatch(
        do.call(analyze_screening, c(list(df = base_df), screening_params)),
        error = function(e) {
          message("品种筛选失败：", e$message)
          NULL
        })
      if (!is.null(scr)) {
        result$tables$promoted          <- scr$promoted
        result$tables$select_plant      <- scr$select_plant
        result$tables$eliminated        <- scr$eliminated
        result$tables$screening_summary <- scr$summary
        result$plots$comparison         <- scr$comparison_plot
        result$plots$radar              <- scr$radar_plot
      }
    }
  }

  # --- 输出目录 ---
  if (is.null(out_dir)) {
    out_dir <- file.path("分析输出", paste0(experiment_id, "_", module_type))
  }

  save_analysis_artifacts(result, out_dir)

  if (zip_output) {
    zip_path <- file.path(out_dir, "分析报告.zip")
    tryCatch({
      build_analysis_zip(result, zip_path)
    }, error = function(e) {
      message("ZIP 打包失败：", e$message)
    })
  }

  files <- list.files(out_dir, recursive = TRUE, full.names = FALSE)

  list(
    result   = result,
    out_dir  = out_dir,
    files    = files,
    messages = result$messages
  )
}
