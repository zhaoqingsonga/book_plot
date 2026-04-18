# ROADMAP.md

## Phase 1 — 两级面板核心功能
**目标**：左侧试验列表 + 右侧田间记录 + 查看 + 删除

### 步骤
1. 修改 `mod_experiments.R`：两级面板 UI（fluidRow 左右分栏）
2. 实现 `filtered_experiments` 聚合查询（按 experiment_id 分组）
3. 实现右侧 `field_records_for_experiment` 查询
4. 实现行选择逻辑（左侧选试验 → 右侧刷新）
5. 实现查看明细弹窗（显示完整字段）
6. 实现删除（整个试验）
7. 语法验证 + 功能测试

## Phase 2 — 编辑与导出
- 田间记录编辑
- CSV/Excel 导出

## Phase 3 — 试验创建入口
- 从上游 data upload 自动创建试验记录
