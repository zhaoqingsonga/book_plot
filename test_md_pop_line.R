# ==============================================================================
# 验证 markdown 报告对 population / line_selection 的输出
# ==============================================================================

library(dplyr)
library(ggplot2)

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

cat("===== 1. 模拟群体数据 =====\n")
set.seed(42)
gen_df <- expand.grid(
  f = c("F2", "F3", "F4", "F5"),
  name = paste0("品系", sprintf("%03d", 1:5)),
  stringsAsFactors = FALSE
) %>%
  dplyr::group_by(f) %>%
  dplyr::slice(rep(1:dplyr::n(), each = 3)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    row_id = dplyr::row_number(),
    MuChan = round(runif(n(), 180, 320), 1),
    ShengYuQi = round(runif(n(), 95, 130), 1)
  )
# 给个 ma/pa 列让群体分析能出组合排名
gen_df$ma <- rep(c("P1", "P2", "P3"), length.out = nrow(gen_df))
gen_df$pa <- rep(c("P4", "P5", "P6"), length.out = nrow(gen_df))
cat(sprintf("  群体数据: %d 行 x %d 列\n", nrow(gen_df), ncol(gen_df)))

cat("\n===== 2. 模拟株行数据 =====\n")
ls_df <- data.frame(
  name    = paste0("行", sprintf("%02d", 1:15)),
  sele    = sample(0:5, 15, replace = TRUE),  # 株行要求此列
  rows    = sample(1:10, 15, replace = TRUE), # 株行要求此列
  MuChan  = round(runif(15, 150, 350), 1),
  ShengYuQi = round(runif(15, 90, 130), 1),
  ZhuGao  = round(runif(15, 60, 110), 1),
  BaiLiZhong = round(runif(15, 15, 25), 1),
  ma = rep(c("M1", "M2"), length.out = 15),
  pa = rep(c("F1", "F2"), length.out = 15),
  stringsAsFactors = FALSE
)
cat(sprintf("  株行数据: %d 行 x %d 列\n", nrow(ls_df), ncol(ls_df)))

# ===== 3. 跑分析 =====
cat("\n===== 3. 跑分析 =====\n")
res_pop <- run_analysis(gen_df, "population")
res_ls  <- run_analysis(ls_df,  "line_selection")
cat(sprintf("  population result type: %s\n", res_pop$type))
cat(sprintf("  line_selection result type: %s\n", res_ls$type))
cat("  population tables:", paste(names(res_pop$tables), collapse=", "), "\n")
cat("  population plots: ", paste(names(res_pop$plots),  collapse=", "), "\n")
cat("  line_selection tables:", paste(names(res_ls$tables), collapse=", "), "\n")
cat("  line_selection plots: ", paste(names(res_ls$plots),  collapse=", "), "\n")

# ===== 4. 生成 markdown =====
out_dir <- "test_output"
dir.create(out_dir, showWarnings = FALSE)

md_pop <- file.path(out_dir, "verify_population.md")
md_ls  <- file.path(out_dir, "verify_line_selection.md")

build_markdown_report(res_pop, md_pop)
build_markdown_report(res_ls,  md_ls)

cat(sprintf("\n  Population MD: %d bytes\n", file.info(md_pop)$size))
cat(sprintf("  Line sel MD:   %d bytes\n", file.info(md_ls)$size))

# ===== 5. 检查内容 =====
cat("\n===== 4. 校验 markdown 内容 =====\n")
pop_txt <- readLines(md_pop, encoding = "UTF-8", warn = FALSE)
ls_txt  <- readLines(md_ls,  encoding = "UTF-8", warn = FALSE)

# 群体应有的节
expected_pop <- c("## 世代分布", "## 世代追踪", "## 组合排名", "## 性状概览")
for (h in expected_pop) {
  found <- any(grepl(h, pop_txt, fixed = TRUE))
  cat(sprintf("  population 含 [%s]: %s\n", h, found))
  stopifnot(found)
}

# 株行应有的节
expected_ls <- c("## 选择概况", "## 选择分布", "## 优良后代", "## 形态统计")
for (h in expected_ls) {
  found <- any(grepl(h, ls_txt, fixed = TRUE))
  cat(sprintf("  line_selection 含 [%s]: %s\n", h, found))
  stopifnot(found)
}

# 旧 bug 不应再出现
stopifnot(!any(grepl("## 二、群体分析", pop_txt, fixed = TRUE)))  # 应是 ## 群体分布
stopifnot(!any(grepl("## 二、株行分析",  ls_txt,  fixed = TRUE)))  # 应是 ## 选择概况

cat("\n===== 5. 校验产比试验未受影响 =====\n")
set.seed(99)
places <- c("北京", "济南", "郑州", "西安")
genotypes <- paste0("V", sprintf("%03d", 1:8))
df_list <- list()
for (pl in places) for (g in genotypes) for (rp in 1:3) {
  df_list[[length(df_list) + 1]] <- data.frame(
    place = pl, stageid = g, name = paste0("品种", substr(g, 2, 4)), rp = rp,
    MuChan = runif(1, 180, 350), ShengYuQi = runif(1, 95, 130),
    ZhuGao = runif(1, 60, 100), BaiLiZhong = runif(1, 15, 25),
    is_ck = if (g == genotypes[1] && rp == 1) 1L else 0L,
    stringsAsFactors = FALSE
  )
}
df_y <- dplyr::bind_rows(df_list)
res_y <- run_analysis(df_y, "yield_test")
md_y <- file.path(out_dir, "verify_yield_test.md")
build_markdown_report(res_y, md_y)
y_txt <- readLines(md_y, encoding = "UTF-8", warn = FALSE)

# 验证这次新加的 3 个表/节都出现
expected_y <- c(
  "### 分地点生育期统计",
  "### 分地点增产统计",
  "### 淘汰材料",
  "## 产量生育期",
  "![产量生育期_"
)
for (h in expected_y) {
  found <- any(grepl(h, y_txt, fixed = TRUE))
  cat(sprintf("  yield_test 含 [%s]: %s\n", h, found))
  stopifnot(found)
}

# safe_sn 应不含括号
bad_links <- grep("![^]]*\\)[^)]*\\)", y_txt, value = TRUE)
stopifnot(length(bad_links) == 0)
cat("  yield_test 图片链接无括号 bug: OK\n")

# ZIP 内文件名应与 markdown 链接一致
md_imgs <- regmatches(y_txt, regexpr("!\\[.*?\\]\\(图表/[^)]+\\)", y_txt))
cat(sprintf("  yield_test 引用图片 %d 张\n", length(md_imgs)))
stopifnot(length(md_imgs) > 0)

# ===== 6. 完整 ZIP 验证 =====
cat("\n===== 6. ZIP 打包验证 =====\n")
zip_path <- tempfile(fileext = ".zip")
build_analysis_zip(res_y, zip_path)
cat(sprintf("  ZIP: %d bytes\n", file.info(zip_path)$size))
stopifnot(file.info(zip_path)$size > 0)

# 解压验证 zip 内有 产量生育期_*.png 和分析报告.md
tmp_unzip <- file.path(tempdir(), paste0("zip_check_", format(Sys.time(), "%H%M%OS3")))
dir.create(tmp_unzip, showWarnings = FALSE)
utils::unzip(zip_path, exdir = tmp_unzip)
unzipped <- list.files(tmp_unzip, recursive = TRUE)
has_md <- any(grepl("分析报告\\.md$", unzipped))
has_yg_pngs <- sum(grepl("产量生育期_", unzipped)) > 0
cat(sprintf("  ZIP 包含 md: %s | 包含产量生育期图: %s\n", has_md, has_yg_pngs))
stopifnot(has_md && has_yg_pngs)

# 验证 zip 内图目录文件名
chart_files <- list.files(file.path(tmp_unzip, "图表"))
yg_pngs_in_zip <- chart_files[grepl("^产量生育期_", chart_files)]
cat("  ZIP 内产量生育期图:", paste(yg_pngs_in_zip, collapse = ", "), "\n")
stopifnot(length(yg_pngs_in_zip) > 0)

# 验证 markdown 内引用的链接与 zip 内文件名 1:1 匹配
md_text <- readLines(file.path(tmp_unzip, "分析报告.md"), warn = FALSE, encoding = "UTF-8")
md_yg_links <- regmatches(md_text, regexpr("!\\[产量生育期_[^]]*\\]\\(图表/产量生育期_[^)]+\\)", md_text))
md_yg_links <- regmatches(md_yg_links, regexpr("产量生育期_[^]]+\\.png", md_yg_links))
cat("  MD 引用:", paste(md_yg_links, collapse = ", "), "\n")
stopifnot(setequal(md_yg_links, yg_pngs_in_zip))
cat("  MD 引用与 ZIP 内文件名一致: OK\n")

cat("\n===== 全部 6 项验证通过 =====\n")
