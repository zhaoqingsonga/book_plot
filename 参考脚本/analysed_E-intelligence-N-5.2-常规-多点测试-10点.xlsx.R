setwd("E:/ChinaSeed/01-工作/05-育种试验/04-试验数据/2025/05-报告生成/01-报告生成脚本")
##
file_directory <- "E:/ChinaSeed/01-工作/05-育种试验/04-试验数据/2025/02-试验数据-output_update/output_update_analysis/"
report_directory <- "E:/ChinaSeed/01-工作/05-育种试验/04-试验数据/2025/05-报告生成/02-报告生成结果"
# 1. 加载必要的包
if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readxl, tidyverse, corrplot, factoextra, fmsb, gridExtra, knitr, 
  kableExtra, scales, lubridate, openxlsx, grid
)
source("00-main_function.R")
source("00-elite_parents_selection.R")
source("00-gge_function.R")
# ==============================================================================
# 主要参数
# ==============================================================================
file_name <- "analysed_E-intelligence-N-5.2-常规-多点测试-10点.xlsx"
sheet_name = "promotion"

# 不晋级材料
keep_eliminated_vector <- c("G25E030", "G25E146", "G25E207")
# 不选单株材料
keep_not_select_vector <- NULL


#0. 首先处理ZengChan_wide数据

ZengChan_wide<-read.xlsx(file.path(file_directory,file_name),"ZengChan_wide")

top_weifang<-filter_rank_by_requirement(ZengChan_wide,threshold=10,mode="all_sites",sites_to_check = "山东潍坊")
#
top_suzhou<-filter_rank_by_requirement(ZengChan_wide,threshold=10,mode="all_sites",sites_to_check = c("安徽宿州"))
top_zhoukou<-filter_rank_by_requirement(ZengChan_wide,threshold=10,mode="all_sites",sites_to_check = c("河南周口"))
top_yangling<-filter_rank_by_requirement(ZengChan_wide,threshold=10,mode="all_sites",sites_to_check = c("陕西杨凌"))
top60_suzhou_zhoukou_yangling<-filter_rank_by_requirement(ZengChan_wide,threshold=60,mode="all_sites",
                    sites_to_check = c("安徽宿州","河南周口","陕西杨凌","安徽濉溪","河南商丘"))
combined_s_z_y <- bind_rows(
  top_suzhou %>% mutate(分类 = "宿州top10"),
  top_zhoukou %>% mutate(分类 = "周口top10"),
  top_yangling %>% mutate(分类 = "杨凌top10"),
  top60_suzhou_zhoukou_yangling %>% mutate(分类 = "宿州周口杨凌均top60")
)

top_huanggang<-filter_rank_by_requirement(ZengChan_wide,threshold=10,mode="all_sites",sites_to_check = c("湖北黄冈"))
top_dianjiang<-filter_rank_by_requirement(ZengChan_wide,threshold=10,mode="all_sites",sites_to_check = c("重庆垫江"))
top60_huanggang_dianjiang<-filter_rank_by_requirement(ZengChan_wide,threshold=60,mode="all_sites",
                                                              sites_to_check = c("湖北黄冈","重庆垫江"))
combined_h_d <- bind_rows(
  top_huanggang %>% mutate(分类 = "黄冈top10"),
  top_dianjiang %>% mutate(分类 = "垫江top10"),
  top60_huanggang_dianjiang %>% mutate(分类 = "黄冈垫江均top60")
)

wb<-createWorkbook()
addWorksheet(wb,"潍坊top10")
addWorksheet(wb,"宿州周口杨凌各top10_均top60")
addWorksheet(wb,"黄冈垫江各top10_均top60")

writeDataTable(wb,"潍坊top10",top_weifang)
writeDataTable(wb,"宿州周口杨凌各top10_均top60",combined_s_z_y)
writeDataTable(wb,"黄冈垫江各top10_均top60",combined_h_d)
saveWorkbook(wb,file.path(report_directory,file_name,#这两个组合是目录
                          "测试试验晋级.xlsx"),overwrite = TRUE)

# 1. 加载和清理数据
file_path = paste(file_directory, file_name, sep = "")
promotion_data <- openxlsx::read.xlsx(file_path, sheet_name)
promotion_clean <- clean_promotion_data(promotion_data)

# 获取所有地点
all_places <- unique(promotion_clean$地点)
cat(sprintf("=== 发现地点数量：%d，分别为：%s ===\n", length(all_places), paste(all_places, collapse=", ")))


#GGE:如果是多地点，则进行分析
#数据预处理及模型建立
output_dir=file.path(report_directory,file_name,"GGE_results")
gge_model<-gge_preprocess_and_return_model(
  gge_input_data=subset(promotion_clean,是否对照!=1),
  output_dir)
#计算结果保存
ganchanwenchan<-export_gge_analysis(gge_model,output_dir)
#生成图片
gge_plot_and_save(gge_model,output_dir)

#如果是多地点，可以查看阶段名称的产量和生育期图
generate_yield_growth_chart(c("N25M188", "N25M189"), 
                            promotion_clean, out_dir = output_dir)



# 针对每个地点，分别进行后续流程
results_list <- list()

for (PLACE in all_places) {
  
  
  cat(sprintf("\n=== 分析地点: %s ===\n", PLACE))
  # 按地点筛选数据
  promotion_clean_sub <- promotion_clean %>% filter(地点 == PLACE)
  # 创建该地点专属输出目录
  output_directory <- file.path(report_directory, file_name, PLACE)
  create_output_directory(output_directory)
  
  # 核心画图分析部分（共四张图）
      analyze_promotion_core(
        promotion_clean_sub,
        output_directory,
        PLACE,
        plot_width = 1500,
        plot_height = 1000,
        scatter_height = 500
      )
  ##
  # 4. 材料晋级
  # 4.1 材料晋级，选单株，淘汰，评述
  screened <- screen_material_promotion(
    promotion_clean_sub,
    PLACE,
    output_directory,
    keep_eliminated_vector,
    keep_not_select_vector,
    rank_threshold_select = 60,    # 晋级材料筛选阈值（原rank_threshold）
    rank_threshold_plant = 60,     # 高产分离选单株筛选阈值（新增独立控制）
    eliminate_lodging = c("9-严重倒", "7-重倒")
  )
  
  # 4.2 材料晋级前后产量、生育期、株高、百粒重比较等性状比较图
      plot_selection_comparison_addck(
        data_before = promotion_clean_sub,
        data_after = screened$select_variety,
        output_directory,
        plot_name = paste0(PLACE, "_选择前后性状比较"),
        #indicators = c("亩产_kg"),
        title = "对照、品种筛选前后核心农艺性状对比（含平均值）",
        subtitle = "对照 vs 选前 vs 选后",
        legend_text_size = 15,
        mean_label_size = 5
      )

  ## 4.3  晋级材料雷达图（top5)
  # 出错时打印提示，仍继续执行后续代码
    png_name <- paste0(PLACE, "_优良品种雷达图.png")
    top5_varieties <- save_radar_chart(
      screened$select_variety,
      output_directory = output_directory,
      png_name = png_name,
      top_n = 5
    )
  # 5 优良亲本筛选
  # 5.1 亲本和组合分析，并保存结果
  preprocessed_data <- preprocess_parent_data(promotion_clean_sub, screened$select_variety)
  excellent <- analyze_and_save_parents_crosses(
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
  png_name <- paste0(PLACE, "_优良亲本及组合分析结果.png")
  plot_analysis_results(
    parent_analysis_res = excellent$parent_res,         # 亲本分析结果
    cross_analysis_res = excellent$cross_res,           # 组合分析结果
    top_crosses = 5,           # 可视化展示的Top优良组合数量（默认10）
    save_path = file.path(output_directory, png_name),
    plot_margin = c(30, 60, 50, 40),
    parent_label_vjust = -2.2,
    cross_label_vjust = -0.6
  )
  
  # 结果收集
  results_list[[PLACE]] <- list(
    promotion_clean = promotion_clean_sub,
    screened = screened,
    top5_varieties = top5_varieties,
    excellent = excellent,
    output_directory = output_directory
  )

}
cat("\n=== 所有地点分析流程完成 ===\n")




