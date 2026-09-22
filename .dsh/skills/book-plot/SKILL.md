---
name: book_plot
description: book_plot：在 6 份文档里全文检索，只读查询 data/field_book.sqlite（36 张表），看表结构与真实取值。当问题涉及该项目的文档内容、结构化数据或需要核对具体数字时使用。
whenToUse: 问题需要查该项目的**事实性内容**（具体数值、条目、出处）时。**凡是有唯一正确答案的问题，必须用本 skill 查，不许凭记忆作答。**
---

# book_plot（book_plot）

项目形态：**知识 + 数据 + 代码**。

| 组成 | 位置 | 规模（2026-09-23 实测） |
|---|---|---|
| 知识 | `test_output/` | **6** 份 md/txt/csv |
| 数据 | `data/field_book.sqlite`, `data/other_trials.sqlite`, `shared/designplot/designplot.db` | **36** 张表 |
| 代码 | `shared/`、`参考脚本/`、`www/` | **53** 个脚本 |

> ⚠️ **这些数字会过时，以 `capabilities` / `tables` 现查为准。** skill 是会腐坏的文档。

工具是通用的 projkit 能力层（不含本项目硬编码）：

```bash
PROJ=/Users/m4/GitHub/book_plot
PK=/Users/m4/Documents/0soyhub/lib/projkit/proj.py
python3 "$PK" --root "$PROJ" capabilities      # 先看有什么可用
python3 "$PK" --root "$PROJ" search "<实词查询>" --top 5
python3 "$PK" --root "$PROJ" tables
python3 "$PK" --root "$PROJ" schema <表名>
python3 "$PK" --root "$PROJ" sample <表名> --n 3
python3 "$PK" --root "$PROJ" query "SELECT ... "
python3 "$PK" --root "$PROJ" scripts
```

---

## 一、硬规矩

1. **数字必须现查，不许凭记忆。** 有唯一正确答案的东西（品种、编号、金额、行数）
   一律查库/查文档；查不到就说查不到——**编一个"看起来合理"的值，比说不知道糟糕得多**。
2. **先 `schema` + `sample`，再写 SQL。** 字段名与取值都不要猜。
3. **只读。** `query` 只接受 SELECT/WITH/PRAGMA/EXPLAIN；绝不写入项目任何文件或数据库。
4. **⚠️ 不要自动运行项目里的脚本。** 先读它、确认只读，再决定；要改数据让作者本人来。
5. **回答带出处**：文件路径、表名、或你跑的 SQL。
6. **文档可能和实际脱节**（README 写的表名/行数未必对）——**以 `tables`/`schema` 为准**。

---

## 二、数据地图（2026-09-23 实测，行数会变）

**`data/field_book.sqlite`**

| 表 | 行数 | 字段（前 8 个） |
|---|---|---|
| `db_meta` | 1 | `key`, `value` |
| `designplot_experiment_records` | 7018 | `record_id`, `experiment_id`, `fieldid`, `id`, `stageid`, `name`, `former_fieldid`, `former_stageid` |
| `designplot_experiments` | 31 | `experiment_id`, `experiment_name`, `source_type`, `source_id`, `total_rows`, `has_planted`, `created_at`, `updated_at` |
| `experiment_plant_runs` | 30 | `run_id`, `experiment_id`, `plant_table_name`, `sow_table_name`, `plan_id`, `created_at`, `updated_at` |
| `experiment_records` | 0 | `record_id`, `experiment_id`, `fieldid`, `id`, `stageid`, `name`, `former_stageid`, `source` |
| `experiments` | 31 | `experiment_id`, `experiment_name`, `total_rows`, `created_at`, `updated_at` |
| `field_models` | 5 | `field_model_id`, `field_name`, `field_len`, `no_plant`, `field_layout`, `strip_width`, `protect_strip`, `cross_path_width` |
| `line_selection_field_records` | 947 | `record_id`, `experiment_id`, `experiment_name`, `fieldid`, `id`, `user`, `stageid`, `name` |
| `line_selection_materials` | 0 | `material_id`, `experiment_id`, `fieldid`, `code`, `ma`, `pa`, `stageid`, `name` |
| `line_selection_records` | 3 | `record_id`, `experiment_id`, `source_id`, `experiment_name`, `total_rows`, `has_generated`, `raw_data`, `generated_at` |
| `plan_runs` | 359 | `plan_id`, `experiment_name`, `source_param_file`, `field_length`, `field_layout`, `bridge_layout`, `row_gap`, `group_rows` |
| `plan_slots` | 1772824 | `slot_id`, `plan_id`, `seq_no`, `field_row_index`, `field_row_no`, `field_col_no`, `row_length`, `total_length` |
| `plant_assignments` | 320639 | `assignment_id`, `plan_id`, `seq_no`, `experiment_name`, `material_name`, `material_subrow_no`, `field_row_no`, `field_col_no` |
| `planting_redo_stack` | 0 | `redo_id`, `plant_table_name`, `experiment_id`, `plan_id`, `redo_data`, `created_at` |
| `planting_snapshots` | 1 | `snapshot_id`, `snapshot_slot`, `experiment_id`, `plant_table_name`, `snapshot_label`, `snapshot_data`, `created_at` |
| `planting_stack` | 0 | `stack_id`, `plant_table_name`, `experiment_id`, `plan_id`, `stack_data`, `created_at` |
| `population_field_records` | 5119 | `record_id`, `experiment_id`, `experiment_name`, `fieldid`, `id`, `user`, `stageid`, `name` |
| `population_materials` | 0 | `material_id`, `experiment_id`, `fieldid`, `code`, `ma`, `pa`, `f`, `stageid` |
| `population_records` | 18 | `record_id`, `experiment_id`, `experiment_name`, `total_rows`, `has_generated`, `raw_data`, `generated_at`, `created_at` |
| `traits_survey` | 0 | `survey_id`, `experiment_id`, `material_id`, `fieldid`, `code`, `stageid`, `name`, `survey_date` |
| `unified_materials` | 0 | `material_id`, `experiment_id`, `experiment_type`, `fieldid`, `code`, `ma`, `pa`, `mapa` |
| `unified_records` | 0 | `record_id`, `experiment_id`, `experiment_type`, `experiment_name`, `source_id`, `total_rows`, `has_generated`, `generated_at` |
| `users` | 1 | `user_id`, `username`, `password`, `created_at` |
| `yield_test_field_records` | 9531 | `record_id`, `experiment_id`, `experiment_name`, `fieldid`, `id`, `user`, `stageid`, `name` |
| `yield_test_materials` | 0 | `material_id`, `experiment_id`, `fieldid`, `code`, `ma`, `pa`, `stageid`, `name` |
| `yield_test_records` | 18 | `record_id`, `experiment_id`, `experiment_name`, `total_rows`, `has_generated`, `raw_data`, `generated_at`, `created_at` |
| `常规地块1东侧.plant` | 52 | `row_index`, `L1`, `L2`, `L3`, `L4`, `L5`, `L6`, `L7` |
| `常规地块1东侧.sow` | 8320 | `Location`, `ID`, `Y`, `X`, `XiaoQuChangDu`, `GuoDaoKuanDu`, `HangJv`, `XiaoQuLiShu` |
| `常规地块2西侧终版_0606.plant` | 72 | `row_index`, `L1`, `L2`, `L3`, `L4`, `L5`, `L6`, `L7` |
| `常规地块2西侧终版_0606.sow` | 9072 | `Location`, `ID`, `Y`, `X`, `XiaoQuChangDu`, `GuoDaoKuanDu`, `HangJv`, `XiaoQuLiShu` |
| `常规地块东-人工播种_new.plant` | 92 | `row_index`, `L1`, `L2`, `L3`, `L4`, `L5`, `L6`, `L7` |
| `常规地块东-人工播种_new.sow` | 4968 | `Location`, `ID`, `Y`, `X`, `XiaoQuChangDu`, `GuoDaoKuanDu`, `HangJv`, `XiaoQuLiShu` |
| `转基因地块1.plant` | 62 | `row_index`, `L1`, `L2`, `L3`, `L4`, `L5`, `L6`, `L7` |
| `转基因地块1.sow` | 6448 | `Location`, `ID`, `Y`, `X`, `XiaoQuChangDu`, `GuoDaoKuanDu`, `HangJv`, `XiaoQuLiShu` |

**`data/other_trials.sqlite`**

| 表 | 行数 | 字段（前 8 个） |
|---|---|---|
| `other_trial_batches` | 12 | `import_batch_id`, `file_name`, `sheet_name`, `trial_name`, `group_label`, `import_time`, `row_count`, `site_count` |
| `other_trial_data` | 13966 | `id`, `import_batch_id`, `trial_name`, `group_label`, `import_time`, `name`, `place`, `MuChan` |

**`shared/designplot/designplot.db`**

| 表 | 行数 | 字段（前 8 个） |
|---|---|---|


---

## 三、代码（53 个，本层只列不跑）

- `.r` 51 个：`app.R`, `migrate_designplot_schema.R`, `migrate_xiaoqulishu.R`, `test_dup.R`, `test_full_pipeline.R`
- `.sh` 1 个：`run.sh`
- `.sql` 1 个：`test_import.sql`

---

## 四、这个项目自己的坑（都是实测踩到的，务必先读）

1. **⚠️ `data/` 目录绝对不要写。** 这个目录曾被同步工具搬过，留下过
   `field_book.sync-conflict-*.sqlite-shm` 这种冲突残留。活着的 SQLite 库 +
   两台机器同步 = 库损坏的经典方式。**只读、只读、只读。**

2. **`has_planted` 这一列全是 0，从来没被维护过。** 按它统计会得出
   "31 个试验一个都没种"的荒谬结论。**真实的种植事实在 `experiment_plant_runs`**
   （30 行 = 30 个试验真的种下去了，另有 1 个没种）。
   **接生产库的第一件事是抽查字段非空率，不是照列名猜语义。**

3. **主库是 WAL 模式，只读打开有讲究**（`data/field_book.sqlite`，1.0GB）：
   - `mode=ro` 只在 `-shm` 旁路文件**已存在**时能打开；
   - `immutable=1` 能打开，但它让 SQLite **假装这不是 WAL**，**完全看不见 `-wal`**
     —— 有未 checkpoint 的写入时会**静默读到旧数据**。
   projkit 的 `query` 会自己处理并在结果里声明用的是哪一种；**看那个字段**。

4. **有些表极大，别 `select *`**：`plan_slots` 百万行级、`plant_assignments` 32 万行。
   先 `count`、再分页/聚合。

5. **`users` 表有 `password` 列。** 不要读它、不要导出、不要出现在任何回答里。

6. **`ecosystem.config.js` 里的 `cwd` 指向一个不存在的路径**
   （`/Users/m4/Documents/GitHub/book_plot`，真实路径是 `/Users/m4/GitHub/book_plot`）。
   现在 pm2 里跑着的那份是对的，但照这个文件重载会指错地方。

7. **R 能力走纯函数**：排布图是 `shared/designplot/core_design.R` 的 `designPlot()`；
   硬约束是 `blocks >= 6`、`cols >= 9`（观察道固定在第 9 列）。
   跑它**不要启 Shiny、不要连库**（`book_plot_api/server.py` 是可参考的只读包法）。

8. **备份**：本机备份脚本在 `0soyhub/scripts/backup_field_book.py`
   （SQLite 在线备份 API，不停服务、零依赖，含恢复演练）；细节见
   `0soyhub/agentic-book/_备份与同步边界.md`。

---

## 五、这个 skill 是怎么来的

用通用能力层 projkit 生成的草稿（`python3 "$PK" --root "$PROJ" skill`），
再由人把「四」那一节按实际情况补上——**通用规矩挡不住项目特有的错**，
而真正会出事的就是项目特有的那些（比如上面第 1、2 条）。
