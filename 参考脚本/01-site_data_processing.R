setwd("E:/ChinaSeed/01-工作/05-育种试验/04-试验数据/2025/05-报告生成/01-R脚本")
##
file_directory<-"E:/ChinaSeed/01-工作/05-育种试验/04-试验数据/2025/02-试验数据-output_update/output_update_analysis/"
report_directory<-"E:/ChinaSeed/01-工作/05-育种试验/04-试验数据/2025/05-报告生成/02-报告"
# 1. 加载必要的包
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readxl, tidyverse, corrplot, factoextra, fmsb, gridExtra, knitr, 
  kableExtra, scales, lubridate, openxlsx, grid
)
source("00-primary_report_function.R")
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

#晋级标准




#创建输出目录
output_directory <- file.path(report_directory, file_name)
create_output_directory(output_directory)
cat("=== 开始数据分析流程 ===\n")

# 1. 加载和清理数据
file_path = paste(file_directory,file_name,sep="")
promotion_data<-openxlsx::read.xlsx(file_path,sheet_name)
promotion_clean <-clean_promotion_data(promotion_data)
#获得地点名称，取第一个。
PLACE<-unique(promotion_clean$地点)[1]

# 2. 统计分析
yield_stats <- calculate_yield_stats(promotion_clean)
ck_mean <- calc_control_means_wide(promotion_clean)
growth_stats <- calculate_growth_stats(promotion_clean)
increase_stats <- calculate_increase_stats(promotion_clean)
corr_matrix <- calculate_correlation_matrix(promotion_clean)

# 3. 数据可视化
setup_plot_theme()
##3.1 产量及生育期
## 3.1.1 产量分布 
p1 <- plot_yield_distribution(promotion_clean, yield_stats = yield_stats, ck_mean = ck_mean)

## 3.1.2 产量分级分布
grade_result <- plot_yield_grade_distribution(promotion_clean)
p2 <- grade_result$plot
## 3.1.3 
promotion_clean <- grade_result$data
increase_result <- plot_increase_distribution(promotion_clean)
p3 <- increase_result$plot
## 3.1.4 生育期分布
promotion_clean <- increase_result$data
p4 <- plot_growth_distribution(promotion_clean, growth_stats = growth_stats, ck_mean = ck_mean)

#产量分布-四幅图保存
png_name<-paste0(PLACE,"_产量生育期分析.png")
save_basic_analysis_plots(list(p1, p2, p3, p4), output_directory,png_name,width = 1500, height = 1000)


## 3.2 产量相关散点图
#3.2.1 生育期对产量
p3.2.1<-plot_trait_yield_scatter(promotion_clean, 
                             x_var="生育期_d", y_var = "亩产_kg", 
                             corr_value = round(cor(promotion_clean$生育期_d,promotion_clean$亩产_kg),2)
                             )

#3.2.2株高对产量
p3.2.2<-plot_trait_yield_scatter(promotion_clean, 
                             x_var="株高_cm", y_var = "亩产_kg", 
                             corr_value = round(cor(promotion_clean$株高_cm,promotion_clean$亩产_kg),2)
                             )
#3.2.3百粒重对产量
p3.2.3<-plot_trait_yield_scatter(promotion_clean, 
                             x_var="百粒重_g", y_var = "亩产_kg", 
                             corr_value = round(cor(promotion_clean$百粒重_g,promotion_clean$亩产_kg),2)
                             )

#产量相关-三幅图保存
png_name<-paste0(PLACE,"_产量与相关性状散点图.png")
save_basic_analysis_plots(list(p3.2.1, p3.2.2, p3.2.3), output_directory,png_name,width = 1500, height = 500)

#3.3 产量相关性状相关图
png_name<-paste0(PLACE,"_产量相关图.png")
save_correlation_plot(corr_matrix, output_directory,png_name)


# 3.4. 质量性状分布图

p6.1<-plot_single_trait_distribution(promotion_clean,"花色")
p6.2<-plot_single_trait_distribution(promotion_clean,"叶形")
p6.3<-plot_single_trait_distribution(promotion_clean,"结荚习性")
p6.4<-plot_single_trait_distribution(promotion_clean,"倒伏性")
p6.5<-plot_single_trait_distribution(promotion_clean,"茸毛色")
p6.6<-plot_single_trait_distribution(promotion_clean,"脐色")
#质量性状分布-6张图
png_name<-paste0(PLACE,"_质量性状分布图.png")
save_basic_analysis_plots(list(p6.1,p6.2,p6.3,p6.4,p6.5,p6.6),output_directory,png_name,
                          width = 1500, height = 1000)



# 4. 材料晋级
## 4.1 材料晋级标准
select_variety<-filter_by_keyword(promotion_clean,"分离",keep=FALSE)
#晋级标准：较临近两对照位次前60，较平均对照位次前60，倒伏性非重倒和严重倒伏。
select_variety<-select_variety|>filter(较临近对照位次<60)|>
                            filter(较平均对照位次<60)|>
                            filter(倒伏性!="9-严重倒",倒伏性!="7-重倒")|>
                            filter(!阶段名称%in%keep_eliminated_vector)

## 4.2 材料晋级前后性状比较
#筛选完之后与之前的比较
# 调用函数，放大所有文字并适配图表大小
plot_selection_comparison_addck(
  data_before = promotion_clean,
  data_after = select_variety,
  output_directory,
  plot_name = paste0(PLACE,"_选择前后性状比较"),
  legend_text_size = 15,
  mean_label_size = 5
)


## 4.3  晋级材料雷达图（top5)
png_name<-paste0(PLACE,"_优良品种雷达图.png")
top5_varieties <- save_radar_chart(select_variety, output_directory = output_directory,png_name = png_name,top_n = 5)


## 4.4 晋级材料描述性评述
file_name<-paste0(PLACE,"_材料综合性状描述.txt")
sink(file.path(output_directory,file_name), append = FALSE, split = TRUE)
soybean_comprehensive_evaluation_final(select_variety)
sink()


## 4.5 高产分离材料选单株
select_plant<-filter_by_keyword(promotion_clean,"分离",keep=TRUE)
select_plant<-select_plant|>filter(较临近对照位次<60)|>
          filter(较平均对照位次<60)|>
          filter(倒伏性!="9-严重倒",倒伏性!="7-重倒")|>
          filter(!阶段名称%in%keep_not_select_vector)



## 4.6 淘汰材料
# 基于“阶段名称”匹配（假设阶段名称是唯一标识）
eliminated <- promotion_clean |>
  filter(!阶段名称 %in% select_variety$阶段名称&!阶段名称 %in% select_plant$阶段名称)  # 


## 4.7 各类材料保存
## 4.7.1 保存字段
sel_cols<-c("阶段名称",  "品种名称",   "母本",    "父本", "生育期_d",
            "亩产_kg",  "较临近对照增产_pct", "较临近对照位次",  "较平均对照增产_pct", "较平均对照位次",    
            "倒伏性",  "株高_cm","百粒重_g",  "草甘膦抗性")
select_variety<-select_variety|>select(all_of(sel_cols))
select_plant<-select_plant|>select(all_of(sel_cols))
eliminated<-eliminated|>select(all_of(sel_cols))

##4.7.2 保存相关表
file_name<-paste0(PLACE,"_晋级_淘汰_分离选单.xlsx")
generate_excel_report(promotion_clean, 
                      select_variety, 
                      eliminated, 
                      select_plant,
                      output_directory,
                      filename = file_name)

cat("\n=== 数据分析流程完成！ ===\n")


#5 优良亲本筛选

preprocessed_data<-preprocess_parent_data(promotion_clean,select_variety)

# 5.1 亲本分析
parent_res <- analyze_excellent_parents(
  preprocessed_data = preprocessed_data,  # 预处理后的数据集（来自preprocess_parent_data函数输出）
  min_crosses = 3,        # 有效亲本的最小配制品种数（过滤偶然结果，默认3）
  top_pct = 0.65               # 优良亲本的晋级率分位数阈值（默认0.8=Top20%）,接近1筛选越严格，接近0筛选越宽松。
)

# 5.2 组合分析
cross_res <- analyze_excellent_crosses(
  preprocessed_data = preprocessed_data,    # 预处理后的数据集（来自preprocess_parent_data函数输出）
  promoted_data = select_variety,            # 晋级材料数据（需含"亩产_kg"列，用于计算产量基准）
  min_crosses = 2,          # 有效组合的最小配制品种数（过滤偶然结果，默认2）
  promote_rate_thresh = 0.2,# 优良组合的晋级率阈值（默认0.5=50%）越大越严格，越小越宽松
  yield_threshold_adjust = 0.8 #数值，产量阈值调节系数（0-∞），默认1.0；作用：最终产量筛选基准 = 晋级材料平均亩产 × 该系数
)



# 5.3 可视化结果
png_name<-paste0(PLACE,"_优良亲本及组合分析结果.png")
plot_analysis_results(
  parent_analysis_res = parent_res,         # 亲本分析结果（来自analyze_excellent_parents函数输出）
  cross_analysis_res = cross_res,           # 组合分析结果（来自analyze_excellent_crosses函数输出）
  top_crosses = 5,           # 可视化展示的Top优良组合数量（默认10）
  save_path = file.path(output_directory, png_name),
  plot_margin = c(30, 60, 50, 40),  # 优化默认边距（适配标签）
  parent_label_vjust = -2.2,       # 优化亲本标签垂直偏移
  cross_label_vjust = -0.6)        # 新增：柱状图标签垂直偏移     


# 5.4 保存结果
save_path<-paste0(output_directory,"/",PLACE,"_优良亲本及组合筛选结果.xlsx")
save_analysis_results(
  parent_analysis_res = parent_res,         # 亲本分析结果（来自analyze_excellent_parents函数输出）
  cross_analysis_res = cross_res,           # 组合分析结果（来自analyze_excellent_crosses函数输出）
  save_path          # 保存位置及文件名
)


