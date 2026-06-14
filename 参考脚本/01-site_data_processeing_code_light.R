setwd("E:/ChinaSeed/01-工作/05-育种试验/04-试验数据/2025/05-报告生成/01-报告生成脚本")
##
file_directory<-"E:/ChinaSeed/01-工作/05-育种试验/04-试验数据/2025/02-试验数据-output_update/output_update_analysis/"
report_directory<-"E:/ChinaSeed/01-工作/05-育种试验/04-试验数据/2025/05-报告生成/02-报告生成结果"
# 1. 加载必要的包
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readxl, tidyverse, corrplot, factoextra, fmsb, gridExtra, knitr, 
  kableExtra, scales, lubridate, openxlsx, grid
)
source("00-main_function.R")
source("00-elite_parents_selection.R")

# ==============================================================================
# 主要参数
# ==============================================================================
file_name <-"analysed_E-intelligence-G-4.2-转基因-初级产比-宿州-20250525.xlsx"
sheet_name = "promotion"

#不晋级材料
keep_eliminated_vector<-c("G25E030","G25E146","G25E207")
#不选单株材料
keep_not_select_vector<-NULL

#创建输出目录
output_directory <- file.path(report_directory, file_name)
create_output_directory(output_directory)
cat("=== 开始数据分析流程 ===\n")


# 1. 加载和清理数据
file_path = paste(file_directory,file_name,sep="")
promotion_data<-openxlsx::read.xlsx(file_path,sheet_name)
promotion_clean <-clean_promotion_data(promotion_data)
#promotion_clean<-promotion_clean|>filter(地点=="河南周口")
#获得地点名称，取第一个。
PLACE<-unique(promotion_clean$地点)[1]

#2-3. 试验核心画图分析部分（共四张图）
    analyze_promotion_core(
      promotion_clean,
      output_directory,
      PLACE,
      plot_width = 1500,
      plot_height = 1000,
      scatter_height = 500
    )

##
# 4. 材料晋级
## 4.1 材料晋级，选单株，淘汰，评述
screened<-screen_material_promotion(
    promotion_clean,
    PLACE,
    output_directory,
    keep_eliminated_vector,
    keep_not_select_vector,
    rank_threshold_select = 60,    # 晋级材料筛选阈值（原rank_threshold）
    rank_threshold_plant = 60,     # 高产分离选单株筛选阈值（新增独立控制）
    eliminate_lodging = c("9-严重倒", "7-重倒")
)

#4.2 材料晋级前后产量、生育期、株高、百粒重比较等性状比较图
    plot_selection_comparison_addck(
      data_before = promotion_clean,
      data_after = screened$select_variety,
      output_directory,
      plot_name = paste0(PLACE, "_选择前后性状比较"),
      legend_text_size = 15,
      mean_label_size = 5
    )
 

## 4.3  晋级材料雷达图（top5)
png_name<-paste0(PLACE,"_优良品种雷达图.png")
top5_varieties <- save_radar_chart(screened$select_variety, output_directory = output_directory,png_name = png_name,top_n = 5)

#5 优良亲本筛选

# 5.1 亲本分析

# 5.1 亲本和组合分析，并保存结果
preprocessed_data<-preprocess_parent_data(promotion_clean,screened$select_variety)
#
excellent<-analyze_and_save_parents_crosses(
    preprocessed_data,
    screened$select_variety,
    output_directory,
    PLACE,
    parent_min_crosses = 3,
    parent_top_pct = 0.65,
    cross_min_crosses = 2,
    cross_promote_rate_thresh = 0.2,
    cross_yield_threshold_adjust = 0.8
)

# 5. 分析结果图形化 
png_name<-paste0(PLACE,"_优良亲本及组合分析结果.png")
#
plot_analysis_results(
  parent_analysis_res = excellent$parent_res,         # 亲本分析结果（来自analyze_excellent_parents函数输出）
  cross_analysis_res = excellent$cross_res,           # 组合分析结果（来自analyze_excellent_crosses函数输出）
  top_crosses = 5,           # 可视化展示的Top优良组合数量（默认10）
  save_path = file.path(output_directory, png_name),
  plot_margin = c(18, 36, 30, 30),  # 优化默认边距（适配标签）
  parent_label_vjust = -1.2,       # 优化亲本标签垂直偏移
  cross_label_vjust = 2)        # 新增：柱状图标签垂直偏移     


