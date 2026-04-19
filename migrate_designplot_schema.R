# =============================================================================
# migrate_designplot_schema.R
# 从 designplot.sqlite 迁移到 field_book.sqlite（无前缀版本）
# =============================================================================

library(RSQLite)

dp_db <- "/Users/m4/Documents/GitHub/designplot2026/data/designplot.sqlite"
fb_db <- "/Users/m4/Documents/GitHub/book_plot/data/field_book.sqlite"

dp_con <- dbConnect(RSQLite::SQLite(), dp_db)
fb_con <- dbConnect(RSQLite::SQLite(), fb_db)

cat("=== 开始数据库迁移 ===\n")
cat("源 (designplot):", dp_db, "\n")
cat("目标 (field_book):", fb_db, "\n\n")

# 要迁移的表
tables <- c(
  "experiments",
  "experiment_records",
  "field_models",
  "plant_assignments",
  "plan_runs",
  "plan_slots",
  "experiment_plant_runs"
)

for (tbl in tables) {
  if (!dbExistsTable(dp_con, tbl)) {
    cat(sprintf("⚠️  源表 %s 不存在，跳过\n", tbl))
    next
  }
  df <- dbReadTable(dp_con, tbl)
  cat(sprintf("  %s: %d 行\n", tbl, nrow(df)))
  if (dbExistsTable(fb_con, tbl)) {
    dbRemoveTable(fb_con, tbl)
  }
  dbWriteTable(fb_con, tbl, df, append = FALSE)
  cat(sprintf("  ✓ 写入 %s\n", tbl))
}

# 动态表（表名含中文和点号，用引号）
dynamic_tables <- c("常规地块1.plant", "转基因地块.plant", "转基因地块.sow")
cat("\n=== 复制动态表 ===\n")
for (tbl in dynamic_tables) {
  if (!dbExistsTable(dp_con, tbl)) {
    cat(sprintf("⚠️  %s 不存在，跳过\n", tbl))
    next
  }
  df <- dbReadTable(dp_con, tbl)
  if (dbExistsTable(fb_con, tbl)) {
    dbRemoveTable(fb_con, tbl)
  }
  dbWriteTable(fb_con, tbl, df, append = FALSE)
  cat(sprintf("  ✓ %s: %d 行\n", tbl, nrow(df)))
}

# db_meta
cat("\n=== 复制 db_meta ===\n")
if (dbExistsTable(dp_con, "db_meta")) {
  df_meta <- dbReadTable(dp_con, "db_meta")
  if (dbExistsTable(fb_con, "db_meta")) dbRemoveTable(fb_con, "db_meta")
  dbWriteTable(fb_con, "db_meta", df_meta, append = FALSE)
  cat(sprintf("  ✓ db_meta: %d 行\n", nrow(df_meta)))
}

# 重建索引
cat("\n=== 重建索引 ===\n")
indexes <- c(
  "CREATE INDEX IF NOT EXISTS idx_exp_name ON experiments(experiment_name)",
  "CREATE INDEX IF NOT EXISTS idx_exp_records_expid ON experiment_records(experiment_id)",
  "CREATE INDEX IF NOT EXISTS idx_plant_runs_expid ON experiment_plant_runs(experiment_id)",
  "CREATE INDEX IF NOT EXISTS idx_field_models_name ON field_models(field_name)",
  "CREATE INDEX IF NOT EXISTS idx_plan_slots_plan_row_col ON plan_slots(plan_id, field_row_no, field_col_no)",
  "CREATE INDEX IF NOT EXISTS idx_assignments_plan_material ON plant_assignments(plan_id, material_name)"
)
for (idx_sql in indexes) {
  tryCatch({
    dbExecute(fb_con, idx_sql)
    cat(sprintf("  ✓ %s\n", idx_sql))
  }, error = function(e) {
    cat(sprintf("  ⚠️  %s: %s\n", idx_sql, e$message))
  })
}

dbDisconnect(dp_con)
dbDisconnect(fb_con)

# 验证
cat("\n=== 验证 ===\n")
verify_con <- dbConnect(RSQLite::SQLite(), fb_db)
for (tbl in c("experiments", "experiment_records", "field_models",
              "plant_assignments", "plan_runs", "plan_slots",
              "常规地块1.plant", "转基因地块.plant", "转基因地块.sow")) {
  tryCatch({
    n <- dbGetQuery(verify_con, sprintf("SELECT COUNT(*) FROM \"%s\"", tbl))[[1]]
    cat(sprintf("  %s: %d 行 ✓\n", tbl, n))
  }, error = function(e) {
    cat(sprintf("  %s: 错误 %s\n", tbl, e$message))
  })
}
dbDisconnect(verify_con)
cat("\n=== 迁移完成 ===\n")
