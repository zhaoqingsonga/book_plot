# PLAN.md — Phase 1

## 步骤 1: 重写 mod_experiments.R UI — 两级面板
修改 `experiments_ui()`:
- 外层 fluidRow 分左右两栏
- 左侧（宽3，约350px）：card 包含筛选表单 + 试验列表（ul/li 渲染）
- 右侧（宽9）：card 包含选中试验的田间记录 DT + 操作按钮
- 左侧列表项用 CSS 类 `.exp-item` 标记，右侧列表用 `.field-item` 标记

## 步骤 2: 实现 filtered_experiments 聚合查询
在 `experiments_server()` 中：
```r
filtered_experiments <- reactive({
  req(refresh_version())
  con <- connectDb(db_path()); on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  # UNION ALL 三个表，按 experiment_id 分组
  # SELECT experiment_id, experiment_name, source_table, COUNT(*) as record_count, place, MAX(created_at) as created_at
  # WHERE 类型筛选 AND 关键词搜索
})
```

## 步骤 3: 渲染左侧试验列表
用 `renderUI` 输出：
```r
output$experiments_list <- renderUI({
  exps <- filtered_experiments()
  if (nrow(exps) == 0) return(div("暂无试验数据"))
  tagList(lapply(1:nrow(exps), function(i) {
    exp <- exps[i, ]
    # div(class = "exp-item", data-exp-id = exp$experiment_id, ...)
    # 显示：试验名称、类型标签、记录数、地点、创建时间
  }))
})
```

## 步骤 4: 实现 field_records_for_experiment
```r
field_records_for_experiment <- reactive({
  exp_id <- selected_experiment_id()
  req(exp_id)
  con <- connectDb(db_path()); on.exit(DBI::dbDisconnect(con), add = TRUE)
  initDb(con)
  # 根据 source_table 查对应表 SELECT * WHERE experiment_id = ?
  # 返回完整字段（约120列）
})
```

## 步骤 5: 渲染右侧田间记录列表
DT 显示关键列：fieldid(隐藏)、记录编号、阶段、地点、XiaoQuShouHuoZhuShu、MuChan、DanBai 等
点击行 → 弹窗显示完整行数据

## 步骤 6: 左侧列表选择逻辑
使用自定义 JS 事件：
- 左侧 `.exp-item` 点击 → 设置 `selected_experiment_id` → 右侧刷新
- 选中高亮：给选中项加 CSS 类 `.active`

## 步骤 7: 删除功能
- 选中左侧试验后，显示"删除"按钮
- 点删除 → 确认弹窗 → 确认后执行 DELETE FROM {source_table} WHERE experiment_id = ?
- 同时删除三个表中的记录

## 步骤 8: 语法验证 + 测试
