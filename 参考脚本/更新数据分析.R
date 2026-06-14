# 设置输出目录
setwd("E:/ChinaSeed/01-工作/05-育种试验/04-试验数据/2025/02-试验数据-output_update/output_update")
source("../01-R脚本/function_for_analysis.R")
# ---- 环境准备 ----
suppressPackageStartupMessages({
  library(openxlsx)
  library(soyplant)
  library(dplyr)
  library(tidyverse)
  library(RSQLite)
  library(readxl)
  library(DBI)
  library(data.table)
  library(purrr)
})

# 获取Excel文件列表
excel_files <- list.files(
  path = "E:/ChinaSeed/01-工作/05-育种试验/04-试验数据/2025/02-试验数据-output_update/output_update/",
  pattern = "\\.xlsx$"
)

file<-excel_files[44]

# 主循环进行数据处理
for (file in excel_files) {
  tryCatch({
    
    # 加载Excel文件及所需工作表
    wb       <- loadWorkbook(file)
    data_read <- readWorkbook(wb, sheet = "traits", startRow = 2)
    planting  <- readWorkbook(wb, sheet = "planting")
    updated   <- data_read
    
    # 日期字段转换
    updated <- convert_columns_to_date(updated)
    

    # 标记对照
   
    # 使用fieldid进行关联，先找到planting表中对应的记录，然后根据id字段是否为空来设置is_ck
    # 通过updated$fieldid在planting$fieldid中找到对应记录，然后看该记录的id字段
    # 如果planting$id不为NA（有内容），则is_ck=0
    # 如果planting$id为NA（无内容），则is_ck=1
    
    # 创建fieldid到id的映射
    fieldid_to_id <- setNames(planting$id, planting$fieldid)
    # 获取对应的id值
    corresponding_ids <- fieldid_to_id[as.character(updated$fieldid)]
    # 根据id是否为NA来设置is_ck
    updated$is_ck <- ifelse(is.na(corresponding_ids), 1, 0)
    
    
    # 检查必须字段
    required_fields <- c("code", "rp", "treatment", "place")
    if (!all(required_fields %in% colnames(planting))) {
      next
    }
    
    # 补全 updated 表缺失字段
    missing_cols <- setdiff(required_fields, names(updated))
    if (length(missing_cols) > 0) {
      updated <- cbind(updated, planting[missing_cols])
    }
  
    
    ##按地点+重复分组的数据增产计算
    updated <- setDT(updated)  # 转换为data.table（保留原转换，不影响分组逻辑）
    updated <- updated %>%
      group_by(place, rp) %>%  # 关键修改：按地点(place)和重复(rp)双重分组
      group_modify(~ {
        # 取消原有的rp数量判断和汇总逻辑（因为每个分组已是单个rp）
        calculate_MuChan_increase_with_multiple_methods(.x)  # 直接对每个分组计算增产
      }) %>%
      ungroup()  # 取消分   

    #updated进行平均,数字类型求平均，不是数字类型取第1重
      updated_summary <- updated %>%
      group_by(place,stageid) %>%  # 按stageid分组（如需加rp可改为group_by(stageid, rp)）
      summarise(
        across(everything(), ~ {
          if (is.numeric(.x)) {
            # 计算分组平均值
            mean_val <- mean(.x, na.rm = TRUE)
            
            # 判断原始数据是否全为整数（无小数部分）
            # 原理：原始数据减去其整数部分后，若所有值均为0（或接近0，考虑浮点误差），则视为整数
            is_all_integer <- all(abs(.x - as.integer(.x)) < 1e-6, na.rm = TRUE)
            
            # 根据判断结果格式化：整数则取整，否则保留原数据的小数位数特征
            if (is_all_integer) {
              as.integer(round(mean_val))  # 平均后取整（四舍五入）
            } else {
              # 保留与原始数据相同的小数位数（取原始数据的最大小数位数）
              max_decimals <- max(nchar(sub(".*\\.", "", as.character(.x[!is.na(.x)]))), 0)
              round(mean_val, digits = max_decimals)
            }
          } else {
            # 非数值型：提取rp=1的第一个值（同之前逻辑）
            .x[rp == 1][1]
          }
        })
      ) %>%
      ungroup()
    
      
    # 按place，对两个目标变量按大小重新编号并替换
    updated_summary<-rank_by_place(updated_summary,c("JiaoLinJinDuiZhaoWeiCi","JiaoPingJunDuiZhaoWeiCi"))
    updated_summary <- modify_if(updated_summary, is.numeric, ~round(., 2))
    
    
    #生成产量宽表
     yield_wide <- process_yield_wide(updated_summary)
    
    #生成增产宽表
     ZengChan_wide <- process_ZengChan_wide(updated_summary)
    
    
    # === 生成输出表格 ===
    # (1) 补齐 updated_summary 中缺失的性状列
    soy_traits <- soy_traits[order(soy_traits$class_standard), ]
    for (col in as.character(soy_traits$name_lib)) {
      if (!col %in% names(updated_summary)) {
        updated_summary[[col]] <- NA
      }
    }
    
    # (2) 准备表头
    # 田间表
    field_table_head <- as.character(subset(soy_traits, class_standard >= 100 & class_standard < 200)$name_lib)
    field_C          <- as.character(subset(soy_traits, class_standard >= 100 & class_standard < 200)$name_C)
    field_table_head <- c("place", "fieldid", "stageid", "name", field_table_head)
    field_C          <- c("地点", "田间ID", "阶段名称", "名称", field_C)
    
    # 植株表
    plant_table_head <- as.character(subset(soy_traits, class_standard >= 200 & class_standard < 300)$name_lib)
    plant_C          <- as.character(subset(soy_traits, class_standard >= 200 & class_standard < 300)$name_C)
    plant_table_head <- c("place", "fieldid", "stageid", "name", plant_table_head)
    plant_C          <- c("地点", "田间ID", "阶段名称", "名称", plant_C)
    
    # 亩产表
    yield_table_head <- as.character(subset(soy_traits, class_standard >= 300 & class_standard < 400)$name_lib)
    yield_C          <- as.character(subset(soy_traits, class_standard >= 300 & class_standard < 400)$name_C)
    yield_table_head <- c(
      "place", "fieldid", "stageid", "name", yield_table_head,
      "JiaoLinJinDuiZhaoZengChan", "JiaoPingJunDuiZhaoZengChan",
      "JiaoLinJinDuiZhaoWeiCi", "JiaoPingJunDuiZhaoWeiCi"
    )
    yield_C <- c("地点", "田间ID", "阶段名称", "名称", yield_C,
                 "较临近对照增产%", "较平均对照增产%", "较临近对照位次", "较平均对照位次")
    
    # (3) 构建输出数据表
    field_table <- as.data.frame(updated_summary[field_table_head])
    names(field_table) <- field_C
    
    plant_table <- as.data.frame(updated_summary[plant_table_head])
    names(plant_table) <- plant_C
    
    yield_table <- as.data.frame(updated_summary[yield_table_head])
    names(yield_table) <- yield_C
    
    promotion <- updated_summary[
      c("place", "fieldid","stageid", "name","ma","pa", "ShengYuQi", "MuChan",
        "JiaoLinJinDuiZhaoZengChan", "JiaoLinJinDuiZhaoWeiCi",
        "JiaoPingJunDuiZhaoZengChan", "JiaoPingJunDuiZhaoWeiCi",
        "HuaSe", "YeXing", "RongMaoSe","JieJiaXiXing",
        "DaoFuXing", "ZhuGao", 
        "DiJiaGao", "FenZhiShu", "YouXiaoJia", 
        "BaiLiZhong","ZhongPiSe", "QiSe", "ZhongPiGuangZe","ZiBanLiLv",  
        "DanBai", "ZhiFang", 
        "HuaYeBingDuBing", "NiJingDianZhongFuBing", "QiTaBingHai",
        "MiaoQiTianJianPingJia", "HuaQiTianJianPingJia",
        "ChengShuQiTianJianPingJia", "ZiLiPingJia","CaoGanLinKangXing", "TianJianBeiZhu","is_ck")
    ]
    names(promotion) <- c(
      "地点", "田间ID","阶段名称", "名称","母本","父本", "生育期(d)", "亩产(kg)",
      "较临近对照增产%", "较临近对照位次",
      "较平均对照增产%", "较平均对照位次",
      "花色", "叶形", "茸毛色","结荚习性",
      "倒伏性",  "株高(cm)", 
      "底荚高(cm)", "分枝数", "有效荚",
      "百粒重(g)","种皮色", "脐色", "种皮光泽","紫斑粒率(%)",
      "蛋白%", "脂肪%", 
      "花叶病毒病", "拟茎点种腐病", "其它病害",
      "苗期田间评价", "花期田间评价",
      "成熟期田间评价", "粒籽评价","草甘膦抗性", "田间备注","是否对照"
    )
    
    # === 写入Excel表格及格式化输出 ===
    # 添加工作表
    addWorksheet(wb, "field_table")
    addWorksheet(wb, "plant_table")
    addWorksheet(wb, "yield_table")
    addWorksheet(wb, "yield_wide")
    addWorksheet(wb, "ZengChan_wide")
    addWorksheet(wb, "promotion")
    
    # 写入数据
    writeDataTable(wb, "yield_wide", yield_wide, startRow = 2)
    writeDataTable(wb, "ZengChan_wide", ZengChan_wide, startRow = 2)
    writeDataTable(wb, "promotion", promotion, startRow = 2)
    writeData(wb, "field_table", field_table, startRow = 2)
    writeData(wb, "plant_table", plant_table, startRow = 2)
    writeData(wb, "yield_table", yield_table, startRow = 2)
    
    # 格式化 - 通用边框与行高设置函数
    add_table_style <- function(wb, sheet, nrow_table, ncol_table) {
      bodyStyle <- createStyle(border = c("Top", "Bottom", "Left", "Righ"), borderColour = "black")
      setColWidths(wb, sheet, cols = 1, widths = 10)
      setRowHeights(wb, sheet, rows = 1:(nrow_table + 2), heights = 17.5)
      addStyle(wb, sheet, bodyStyle, rows = 1:(nrow_table + 2), cols = 1:(1 + ncol_table), gridExpand = TRUE)
    }
    
    add_table_style(wb, "field_table", nrow(field_table), ncol(field_table))
    add_table_style(wb, "plant_table", nrow(plant_table), ncol(plant_table))
    add_table_style(wb, "yield_table", nrow(yield_table), ncol(yield_table))
    
    # field_table 日期格式列特殊处理
    dateFormat <- createStyle(
      numFmt      = "m/d",
      border      = c("Top", "Bottom", "Left", "Righ"),
      borderColour= "black"
    )
    date_cols <- which(unlist(lapply(field_table, is.Date)))
    if (length(date_cols) > 0) {
      addStyle(
        wb, sheet = "field_table", style = dateFormat,
        rows = 1:(nrow(field_table) + 2), cols = date_cols, gridExpand = TRUE
      )
    }
    
    # === 保存分析结果 ===
    output_path <- file.path(
      "E:/ChinaSeed/01-工作/05-育种试验/04-试验数据/2025/02-试验数据-output_update/output_update_analysis/",
      paste0("analysed_", file)
    )
    saveWorkbook(wb, output_path, overwrite = TRUE)
    
  }, error = function(e) {
    cat("处理文件", file, "时出现错误，跳过该文件继续下一个文件处理\n")
  })
}
