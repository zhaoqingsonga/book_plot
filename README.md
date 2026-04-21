# 田间记录本生成器 (Field Book Plot)

基于 `soyplant` 库的田间试验规划 Shiny 应用，一站式完成试验数据管理与记录本生成。

## 功能模块

| 模块 | 说明 |
|------|------|
| 群体记录本 | 处理 F1-F7 群体数据，生成群体种植记录本 |
| 株行记录本 | 从上代种植计划或产比数据中选择单株，生成株行记录本 |
| 产比记录本 | 处理杂交组合数据，生成初级产比及以上种植记录本 |
| 试验管理 | 集中管理所有试验数据，支持上传、维护、查看 |
| 田间种植 | 可视化编辑田间种植图，实时预览布局 |

## 目录结构

```
book_plot/
├── app.R                      # 主程序入口
├── run.sh                     # 启动脚本
├── ecosystem.config.js        # Shiny Server 配置
├── migrate_designplot_schema.R # 数据库迁移脚本
├── www/                       # 静态资源
│   └── styles.css             # 自定义样式
├── shared/                    # 共享模块
│   ├── helpers.R              # 辅助函数
│   ├── db_persistence.R      # SQLite 数据持久化
│   ├── mod_experiments.R      # 试验管理模块
│   ├── mod_line_selection.R   # 株行记录本模块
│   ├── mod_population.R       # 群体记录本模块
│   ├── mod_yield_test.R       # 产比记录本模块
│   └── designplot/            # 田间种植图模块
│       ├── constants.R
│       ├── sqlite_persistence.R
│       ├── parsers.R
│       ├── core_design.R
│       ├── app_ui.R
│       └── app_server.R
└── data/                      # SQLite 数据库存储目录
```

## 安装依赖

```r
install.packages(c("shiny", "shinyjs", "bslib", "dplyr", "openxlsx", "DT", "ggplot2", "DBI", "RSQLite"))
devtools::install_github("zhaoqingsonga/soyplant")
```

## 运行方式

### 本地运行
```r
shiny::runApp()
# 或使用启动脚本
./run.sh
```

### 服务器部署
配合 Shiny Server 或 ShinyProxy 使用，`ecosystem.config.js` 已配置好相关参数。

## 数据流程

```
上传数据 → 试验管理 → 选择记录本类型 → 生成记录本 → 导出 Excel
                              ↓
                        田间种植图可视化
```

## 数据库

所有数据存储在 `data/field_book.sqlite`，包含以下表：
- `population_records` - 群体记录
- `line_selection_records` - 株行记录
- `yield_test_records` - 产比记录
- `experiments` - 试验基本信息

## 输出文件

生成包含以下 Sheet 的 Excel 文件：
- **origin**: 原始上传数据
- **planting**: 田间种植计划
- **myview**: 可视化视图
- **combi_matrix**: 组合矩阵

## 开发

详见 [DEVELOPMENT.md](./DEVELOPMENT.md)

## 依赖

- R >= 4.0
- Shiny
- soyplant (GitHub: zhaoqingsonga/soyplant)