# ==============================================================================
# 高产品种产量生育期图 验证脚本
# 在 RStudio 或 R GUI 中运行: source("test_yield_growth_chart.R")
# ==============================================================================

library(dplyr)
library(ggplot2)

source("shared/analysis/analysis_yield_growth_chart.R")

cat("===== 1. 准备模拟数据 =====\n")
set.seed(42)
places <- c("北京", "济南", "郑州", "西安", "南京")
df <- data.frame(
  place     = rep(places, each = 20),
  stageid   = rep(paste0("V", sprintf("%03d", 1:10)), each = 2, length.out = 100),
  name      = rep(paste0("品种", LETTERS[1:10]), each = 2, length.out = 100),
  MuChan    = runif(100, 180, 320),
  ShengYuQi = runif(100, 95, 130),
  is_ck     = sample(c(0, 1), 100, replace = TRUE, prob = c(0.85, 0.15)),
  stringsAsFactors = FALSE
)
cat(sprintf("  数据: %d 行 x %d 列\n", nrow(df), ncol(df)))

# ===== 2. 模拟 GGE 返回的高产品种 =====
high_yield <- rbind(
  data.frame(stageid = c("V001","V003","V005"),
             name    = c("品种A","品种C","品种E"),
             Mean_Yield = c(285, 278, 272),
             Stability  = c(12, 15, 18),
             Category   = "高产稳定",
             stringsAsFactors = FALSE),
  data.frame(stageid = c("V002","V007"),
             name    = c("品种B","品种G"),
             Mean_Yield = c(290, 268),
             Stability  = c(35, 30),
             Category   = "高产不稳",
             stringsAsFactors = FALSE)
)
cat(sprintf("  高产品种: %d 个（%d 稳定 + %d 不稳）\n",
  nrow(high_yield),
  sum(high_yield$Category == "高产稳定"),
  sum(high_yield$Category == "高产不稳")))

# ===== Test 1: 正常流程 (stageid + name) =====
cat("\n===== Test 1: stageid + name 列 =====")
r1 <- tryCatch(
  generate_yield_growth_chart_shiny(df, high_yield),
  error = function(e) { cat(sprintf("\n  ERROR: %s\n", e$message)); NULL }
)
stopifnot(!is.null(r1))
stopifnot(length(r1$plots) == 5)
cat(sprintf("\n  OK: %d plots (%s)\n", length(r1$plots),
  paste(names(r1$plots), collapse = ", ")))

# ===== Test 2: Genotype 列（无 stageid/name 拆分）=====
cat("\n===== Test 2: Genotype 列 =====")
stable_B <- data.frame(Genotype = c("V001","V003"),
  Mean_Yield = c(285, 278), Stability = c(12, 15),
  Category = "高产稳定", stringsAsFactors = FALSE)
r2 <- tryCatch(
  generate_yield_growth_chart_shiny(df, stable_B),
  error = function(e) { cat(sprintf("\n  ERROR: %s\n", e$message)); NULL }
)
stopifnot(!is.null(r2))
stopifnot(length(r2$plots) == 2)
cat(sprintf("\n  OK: %d plots\n", length(r2$plots)))

# ===== Test 3: 空 stable_genotypes =====
cat("\n===== Test 3: 空 stable_genotypes =====")
r3 <- generate_yield_growth_chart_shiny(df, data.frame())
stopifnot(is.null(r3))
cat("\n  OK: 返回 NULL（正确）")

# ===== Test 4: 缺 ShengYuQi 列 =====
cat("\n===== Test 4: 缺 ShengYuQi 列 =====")
r4 <- generate_yield_growth_chart_shiny(
  df[, setdiff(colnames(df), "ShengYuQi")], high_yield)
stopifnot(is.null(r4))
cat("\n  OK: 返回 NULL（正确）")

# ===== Test 5: 无 is_ck —— 仍然出图 =====
cat("\n===== Test 5: 无 is_ck 列 =====")
r5 <- tryCatch(
  generate_yield_growth_chart_shiny(
    df[, setdiff(colnames(df), "is_ck")], high_yield[1:2,]),
  error = function(e) { cat(sprintf("\n  ERROR: %s\n", e$message)); NULL }
)
stopifnot(!is.null(r5))
stopifnot(length(r5$plots) == 2)
cat(sprintf("\n  OK: %d plots（无对照柱体/折线）\n", length(r5$plots)))

# ===== Test 6: ggplot 结构完整性 =====
cat("\n===== Test 6: ggplot 内部结构 =====")
p <- r1$plots[["V001"]]
layers <- p$layers
has_col  <- any(sapply(layers, function(l) inherits(l$geom, "GeomCol")))
has_line <- any(sapply(layers, function(l) inherits(l$geom, "GeomLine")))
has_dual <- !is.null(p$scales$scales) &&
  any(sapply(p$scales$scales, function(s) !is.null(s$secondary.axis)))
cat(sprintf("\n  layers: %d | bars: %s | lines: %s | dual_axis: %s\n",
  length(layers), has_col, has_line, has_dual))
stopifnot(has_col && has_line && has_dual)

# ===== Test 7: 返回数据表结构 =====
cat("\n===== Test 7: 数据表结构 =====")
tbl <- r1$tables[["V001"]]
cat(sprintf("\n  列: %s\n", paste(colnames(tbl), collapse = ", ")))
cat(sprintf("  行数: %d\n", nrow(tbl)))
stopifnot(all(c("place","平均亩产","平均生育期","样本数量","对照亩产") %in% colnames(tbl)))

# ===== Test 8: 实际渲染并查看 =====
cat("\n===== Test 8: 渲染为 PNG =====")
out_dir <- "test_output"
dir.create(out_dir, showWarnings = FALSE)
for (nm in names(r1$plots)) {
  png_path <- file.path(out_dir, sprintf("yield_growth_%s.png", nm))
  ggsave(png_path, r1$plots[[nm]], width = 10, height = 6, dpi = 150, bg = "white")
  cat(sprintf("\n  %s: %d bytes", nm, file.info(png_path)$size))
}
cat(sprintf("\n\n  输出目录: %s\n", normalizePath(out_dir)))

# ===== Test 9: 中文字符测试 =====
cat("\n===== Test 9: 中文数据测试 =====")
df_cn <- data.frame(
  place     = rep(c("北京","济南","郑州"), each = 4),
  stageid   = rep(c("V001","V002","V003"), each = 4, length.out = 12),
  name      = rep(c("品种A","品种B","品种C"), each = 4, length.out = 12),
  MuChan    = runif(12, 200, 300),
  ShengYuQi = runif(12, 100, 130),
  is_ck     = rep(c(1,0,1,0), length.out = 12),
  stringsAsFactors = FALSE
)
stable_cn <- data.frame(
  stageid = c("V001","V003"), name = c("品种A","品种C"),
  Mean_Yield = c(285, 278), Stability = c(12, 15),
  Category = "高产稳定", stringsAsFactors = FALSE)
r9 <- generate_yield_growth_chart_shiny(df_cn, stable_cn)
stopifnot(!is.null(r9))
stopifnot(length(r9$plots) == 2)
# Verify Chinese renders
tmp_cn <- file.path(out_dir, "yield_growth_中文测试.png")
ggsave(tmp_cn, r9$plots[["V003"]], width = 10, height = 6, dpi = 150, bg = "white")
cat(sprintf("\n  OK: %d plots, 中文渲染: %d bytes\n",
  length(r9$plots), file.info(tmp_cn)$size))

cat("\n===== 全部 9 项测试通过！=====\n")
