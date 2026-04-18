# CONTEXT.md — Phase 1

## 项目路径
~/Documents/GitHub/melt/make_field_book/

## 修改文件
- `shared/mod_experiments.R` — 核心模块（完全重写）
- `app.R` — 已添加 toggle_delete_btn JS 处理器（保留）

## 数据库
- SQLite: `~/Documents/GitHub/melt/make_field_book/field_book.sqlite`
- 表：population_field_records, line_selection_field_records, yield_test_field_records
- 列（共同）：record_id, experiment_id, experiment_name, fieldid, place, stage, XiaoQuShouHuoZhuShu, MuChan, DanBai, ...（约120列）
- 当前数据量：population(200条) line_selection(56条) yield_test(1646条)

## 关键约束
- 一个 experiment_id 对应多条 field 记录（如 POP_20260416153845_1794 有 41 条）
- 田间记录表结构相同，共用相同的列集
- 删除：DELETE FROM {source_table} WHERE experiment_id = ?
