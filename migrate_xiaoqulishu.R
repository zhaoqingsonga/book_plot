# =============================================================================
# migrate_xiaoqulishu.R
# 一次性脚本：遍历所有 *.sow 播种表，重新计算 XiaoQuLiShu
#
# 背景：
#   旧公式: round(int_v * 13)       —— R 的银行家舍入，.5 向偶数
#   新公式: floor(int_v * 13 + 0.5) —— 四舍五入，与业务预期一致
#
# 还原路径:
#   XiaoQuChangDu = int_v * 100      →  int_v = XiaoQuChangDu / 100
#   XiaoQuLiShu_new = floor(int_v * 13 + 0.5)
#
# 用法:
#   cd book_plot && Rscript migrate_xiaoqulishu.R
#   或在 R 中 source("migrate_xiaoqulishu.R")
# =============================================================================

library(RSQLite)

# ---- 配置 ----
db_path <- file.path("data", "field_book.sqlite")
dry_run <- FALSE   # TRUE = 仅打印差异，不写入

# ---- 连接数据库 ----
if (!file.exists(db_path)) {
  stop("数据库不存在: ", db_path)
}

con <- dbConnect(SQLite(), dbname = db_path)
on.exit(dbDisconnect(con), add = TRUE)

# 启用 WAL 和超时
dbExecute(con, "PRAGMA journal_mode = WAL")
dbExecute(con, "PRAGMA busy_timeout = 5000")

# ---- 列出所有 sow 表 ----
all_tables <- dbListTables(con)
sow_tables <- grep("\\.sow$", all_tables, value = TRUE, ignore.case = TRUE)

cat(sprintf("=== 找到 %d 个 sow 表 ===\n\n", length(sow_tables)))

if (length(sow_tables) == 0L) {
  cat("没有 sow 表，无需迁移。\n")
  quit(save = "no")
}

# ---- 辅助函数：从 XiaoQuChangDu 重新计算 XiaoQuLiShu ----
recalcXiaoQuLiShu <- function(xiao_qu_chang_du) {
  # XiaoQuChangDu = interval_width * 100
  int_v <- xiao_qu_chang_du / 100
  # 新公式: floor(int_v * 13 + 0.5)
  as.integer(floor(int_v * 13 + 0.5))
}

# ---- 遍历每个 sow 表 ----
total_updated <- 0L

for (tbl in sow_tables) {
  cat(sprintf("处理表: %s\n", tbl))

  df <- tryCatch(
    dbReadTable(con, tbl),
    error = function(e) {
      cat(sprintf("  ⚠️ 读取失败: %s\n", e$message))
      return(NULL)
    }
  )

  if (is.null(df) || nrow(df) == 0L) {
    cat("  跳过（空表或读取失败）\n\n")
    next
  }

  # 检查是否有必要列
  if (!"XiaoQuLiShu" %in% names(df) || !"XiaoQuChangDu" %in% names(df)) {
    cat("  跳过（缺少必要列 XiaoQuLiShu 或 XiaoQuChangDu）\n\n")
    next
  }

  old_vals <- as.integer(df$XiaoQuLiShu)
  new_vals <- recalcXiaoQuLiShu(df$XiaoQuChangDu)

  # 找出有变化的行
  changed <- which(old_vals != new_vals)
  n_changed <- length(changed)

  if (n_changed == 0L) {
    cat(sprintf("  ✓ 无变化 (%d 行)\n\n", nrow(df)))
    next
  }

  cat(sprintf("  %d / %d 行需要更新:\n", n_changed, nrow(df)))

  # 打印变更详情（前 20 条）
  max_show <- min(n_changed, 20L)
  for (i in seq_len(max_show)) {
    idx <- changed[i]
    int_v_val <- df$XiaoQuChangDu[idx] / 100
    cat(sprintf("    行 %d: %d → %d  (int_v=%.4f, XiaoQuChangDu=%.1f)\n",
                idx, old_vals[idx], new_vals[idx],
                int_v_val, df$XiaoQuChangDu[idx]))
  }
  if (n_changed > max_show) {
    cat(sprintf("    ... 还有 %d 行\n", n_changed - max_show))
  }

  if (!dry_run) {
    df$XiaoQuLiShu <- new_vals
    tryCatch(
      {
        dbWriteTable(con, tbl, df, overwrite = TRUE)
        cat(sprintf("  ✓ 已更新 %d 行\n", n_changed))
        total_updated <- total_updated + n_changed
      },
      error = function(e) {
        cat(sprintf("  ✗ 写入失败: %s\n", e$message))
      }
    )
  }

  cat("\n")
}

# ---- 汇总 ----
if (dry_run) {
  cat(sprintf("=== DRY RUN 完成（未实际写入） ===\n"))
} else {
  cat(sprintf("=== 迁移完成，共更新 %d 行 ===\n", total_updated))
}
