# ==============================================================================
# 完整管线验证：模拟多点试验 → run_analysis() → 检查 gge_yield_growth
# ==============================================================================

library(dplyr)
library(ggplot2)

# 1. 加载所有依赖模块
source("shared/analysis/analysis_column_map.R")
source("shared/analysis/analysis_trial_detect.R")
source("shared/analysis/analysis_yield_core.R")
source("shared/analysis/analysis_quality_traits.R")
source("shared/analysis/analysis_screening.R")
source("shared/analysis/analysis_radar.R")
source("shared/analysis/analysis_description.R")
source("shared/analysis/analysis_parent_cross.R")
source("shared/analysis/analysis_gge.R")
source("shared/analysis/analysis_population.R")
source("shared/analysis/analysis_line_selection.R")
source("shared/analysis/analysis_export.R")
source("shared/analysis/analysis_yield_growth_chart.R")
source("shared/analysis/analysis_html_report.R")
source("shared/mod_analysis.R")
cat("All modules loaded\n")

# 2. 构建模拟多点数据（GGE 条件：>=3 环境，>=2 公有基因型）
set.seed(42)
places <- c("北京", "济南", "郑州", "西安")
n_places <- length(places)
genotypes <- paste0("V", sprintf("%03d", 1:8))

# 每个地点 x 每个基因型 3 重复（足够 GGE）
df_list <- list()
for (pl in places) {
  for (g in genotypes) {
    for (rp in 1:3) {
      df_list[[length(df_list) + 1]] <- data.frame(
        place   = pl,
        stageid = g,
        name    = paste0("品种", substr(g, 2, 4)),
        rp      = rp,
        MuChan    = runif(1, 180, 350),
        ShengYuQi = runif(1, 95, 130),
        ZhuGao    = runif(1, 60, 100),
        BaiLiZhong = runif(1, 15, 25),
        is_ck     = if (g == genotypes[1] && rp == 1) 1L else 0L,  # V001 作为 CK
        stringsAsFactors = FALSE
      )
    }
  }
}
df <- dplyr::bind_rows(df_list)
cat(sprintf("Mock data: %d rows, %d places, %d genotypes\n",
  nrow(df), n_places, length(genotypes)))
cat("Columns:", paste(colnames(df), collapse=", "), "\n")

# 验证 is_ck 分布
cat("CK rows:", sum(df$is_ck == 1), "\n")
cat("MuChan range:", range(df$MuChan, na.rm=TRUE), "\n")
cat("ShengYuQi range:", range(df$ShengYuQi, na.rm=TRUE), "\n")

# 3. 调用 run_analysis
cat("\n=== Calling run_analysis ===\n")
result <- tryCatch(
  run_analysis(df, "yield_test"),
  error = function(e) {
    cat("ERROR:", e$message, "\n")
    NULL
  }
)
stopifnot(!is.null(result))
cat(sprintf("Result type: %s\n", result$type))
cat(sprintf("Messages (%d):\n", length(result$messages)))
for (m in head(result$messages, 30)) cat(sprintf("  - %s\n", substr(m, 1, 120)))

# 4. 检查核心结果
cat("\n=== Checking GGE results ===\n")
cat("gge_biplot:", !is.null(result$plots$gge_biplot), "\n")
cat("gge_stability:", !is.null(result$plots$gge_stability), "\n")
cat("gge_heatmap:", !is.null(result$plots$gge_heatmap), "\n")
cat("gge_ranking:", !is.null(result$plots$gge_ranking), "\n")
cat("gge_stable table:", !is.null(result$tables$gge_stable),
    if(!is.null(result$tables$gge_stable)) paste(nrow(result$tables$gge_stable), "rows"), "\n")
cat("gge_unstable table:", !is.null(result$tables$gge_unstable),
    if(!is.null(result$tables$gge_unstable)) paste(nrow(result$tables$gge_unstable), "rows"), "\n")

# 5. ★ 检查 gge_yield_growth —— 这是本次新增的核心
cat("\n=== Checking gge_yield_growth (NEW) ===\n")
cat("gge_yield_growth plots:", !is.null(result$plots$gge_yield_growth), "\n")
if (!is.null(result$plots$gge_yield_growth)) {
  yg <- result$plots$gge_yield_growth
  cat(sprintf("  Count: %d\n", length(yg)))
  cat(sprintf("  Names: %s\n", paste(names(yg), collapse=", ")))

  all_ok <- TRUE
  for (nm in names(yg)) {
    p <- yg[[nm]]
    is_gg <- inherits(p, "ggplot")
    has_col <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomCol")))
    has_line <- any(sapply(p$layers, function(l) inherits(l$geom, "GeomLine")))
    has_dual <- any(sapply(p$scales$scales, function(s) !is.null(s$secondary.axis)))
    ok <- is_gg && has_col && has_line && has_dual
    if (!ok) all_ok <- FALSE
    cat(sprintf("  %s: ggplot=%s bars=%s lines=%s dual_axis=%s %s\n",
      nm, is_gg, has_col, has_line, has_dual, if(ok) "OK" else "FAIL"))
  }
  stopifnot(all_ok)
  cat("  ALL PLOTS VALID\n")
} else {
  cat("  FAIL: gge_yield_growth is NULL!\n")
  # 诊断原因
  cat("\n  === Diagnostics ===\n")
  cat("  can_do_gge:", result$trial_info$can_do_gge, "\n")
  cat("  is_multi_site:", result$trial_info$is_multi_site, "\n")
  cat("  gge_stable rows:", if(!is.null(result$tables$gge_stable)) nrow(result$tables$gge_stable) else "NULL", "\n")
  cat("  gge_unstable rows:", if(!is.null(result$tables$gge_unstable)) nrow(result$tables$gge_unstable) else "NULL", "\n")
  stop("gge_yield_growth should not be NULL")
}

# 6. 验证报告 builder
cat("\n=== Checking report builder ===\n")
section <- build_report_yield_growth(result)
cat("build_report_yield_growth:", !is.null(section), "\n")
if (!is.null(section)) {
  cat(sprintf("  section class: %s\n", paste(class(section), collapse=", ")))
}

# 7. 验证可写 PNG
cat("\n=== Checking PNG rendering ===\n")
dir.create("test_output", showWarnings = FALSE)
if (!is.null(result$plots$gge_yield_growth)) {
  for (nm in names(result$plots$gge_yield_growth)[1:min(3, length(result$plots$gge_yield_growth))]) {
    p <- result$plots$gge_yield_growth[[nm]]
    fpath <- file.path("test_output", paste0("verify_", gsub("[\\\\/:*?\"<>|]", "_", nm), ".png"))
    ggsave(fpath, p, width = 10, height = 6, dpi = 120, bg = "white")
    cat(sprintf("  %s: %d bytes\n", basename(fpath), file.info(fpath)$size))
  }
}

# 8. 验证 HTML 报告生成
cat("\n=== Checking HTML report generation ===\n")
html_path <- file.path("test_output", "verify_report.html")
tryCatch({
  build_html_report(result, html_path)
  cat(sprintf("  HTML report: %d bytes\n", file.info(html_path)$size))
  # check 八 appears in HTML
  html_txt <- readLines(html_path, encoding = "UTF-8", warn = FALSE)
  has_yield_growth <- any(grepl("产量生育期", html_txt, fixed = TRUE))
  cat(sprintf("  Contains '产量生育期': %s\n", has_yield_growth))
  stopifnot(has_yield_growth)
}, error = function(e) {
  cat(sprintf("  HTML generation FAILED: %s\n", e$message))
})

# 9. 验证 Markdown 报告生成
cat("\n=== Checking Markdown report generation ===\n")
md_path <- file.path("test_output", "verify_report.md")
tryCatch({
  build_markdown_report(result, md_path)
  cat(sprintf("  MD report: %d bytes\n", file.info(md_path)$size))
  md_txt <- readLines(md_path, encoding = "UTF-8", warn = FALSE)
  has_yg <- any(grepl("产量生育期", md_txt, fixed = TRUE))
  has_img <- any(grepl("产量生育期_", md_txt, fixed = TRUE))
  cat(sprintf("  Contains heading: %s\n", has_yg))
  cat(sprintf("  Contains images: %s\n", has_img))
  stopifnot(has_yg && has_img)
}, error = function(e) {
  cat(sprintf("  MD generation FAILED: %s\n", e$message))
})

cat("\n===== ALL VERIFICATIONS PASSED =====\n")
cat("Output in:", normalizePath("test_output"), "\n")
