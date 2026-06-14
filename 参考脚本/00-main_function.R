# ==============================================================================
# 报告生成函数库
# 用于大豆品种产比试验数据分析
# 创建日期：2025-11-21
# 
# 使用说明：
# 1. 首先加载函数库：source("report_function.R")
# 2. 可以使用单个函数进行特定分析，或使用run_complete_analysis()执行完整流程
# 3. 主要函数包括：
#    - load_and_clean_promotion_data(): 加载和清理数据
#    - calculate_yield_stats(): 计算产量统计
#    - plot_yield_distribution(): 绘制产量分布图
#    - filter_high_yield_varieties(): 筛选高产优质品种
#    - generate_excel_report(): 生成Excel报告
#    - run_complete_analysis(): 执行完整分析流程
# ==============================================================================

# ==============================================================================
# 1. 数据加载和清理函数
# ==============================================================================

#' 列名标准化函数
#' 
#' @param data 数据框
#' @param custom_map 自定义列名映射（命名向量）
#' @return 标准化后的数据框
standardize_colnames <- function(data, custom_map = NULL) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("请先安装并加载 dplyr 包：install.packages('dplyr') + library(dplyr)")
  }
  
  original_cols <- colnames(data)
  
  # 定义默认改写规则
  default_rules <- list(
    "^(.*)\\((d|kg|cm|g|%)\\)$" = "\\1_\\2",
    "(.*)%" = "\\1_pct",
    "\\s+" = "_",
    "^名称$" = "品种名称",
    "^抗性$" = "草甘膦抗性",
    "^评价$" = "田间评价"
  )
  
  # 应用默认规则
  standardized_cols <- original_cols
  for (pattern in names(default_rules)) {
    standardized_cols <- gsub(
      pattern = pattern,
      replacement = default_rules[[pattern]],
      x = standardized_cols,
      perl = TRUE
    )
  }
  
  # 应用自定义映射
  if (!is.null(custom_map)) {
    if (!is.vector(custom_map) || is.null(names(custom_map))) {
      stop("custom_map 必须是命名向量，格式如 c('原始列名1'='目标列名1', '原始列名2'='目标列名2')")
    }
    match_idx <- match(names(custom_map), original_cols)
    match_idx <- match_idx[!is.na(match_idx)]
    if (length(match_idx) > 0) {
      standardized_cols[match_idx] <- custom_map[names(custom_map)[!is.na(match_idx)]]
    }
  }
  
  # 清理最终列名
  standardized_cols <- gsub("_+", "_", standardized_cols)
  standardized_cols <- gsub("^_|_$", "", standardized_cols)
  standardized_cols[standardized_cols == ""] <- "未命名列"
  
  colnames(data) <- standardized_cols
  
  cat("列名标准化完成！\n")
  cat("原始列名 → 标准化列名：\n")
  mapping_df <- data.frame(
    原始列名 = original_cols,
    标准化列名 = standardized_cols,
    stringsAsFactors = FALSE
  )
  print(mapping_df, row.names = FALSE)
  
  return(data)
}


#' 加载和清理promotion数据
#' 
#' @param file_path Excel文件完整路径
#' @param sheet_name 工作表名称，默认为"promotion"
#' @param skip_rows 跳过的行数，默认为1
#' @param custom_colmap 自定义列名映射
#' @return 清理后的数据框
clean_promotion_data <- function(promotion_data, custom_colmap = NULL) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("请先安装 dplyr 包：install.packages('dplyr')")
  }
  # # 读取数据
  # promotion_data <- readxl::read_excel(data_path, sheet = sheet_name, skip = skip_rows)
  # 
  # 删除完全空的行和列
  promotion_clean <- promotion_data %>%
    dplyr::select_if(~!all(is.na(.))) %>%
    dplyr::filter(if_any(everything(), ~!is.na(.)))
  
  # 标准化列名
  promotion_clean <- standardize_colnames(promotion_clean, custom_map = custom_colmap)
  
  # 清理列名
  colnames(promotion_clean) <- trimws(colnames(promotion_clean))
  invalid_names <- which(is.na(colnames(promotion_clean)) | colnames(promotion_clean) == "")
  if (length(invalid_names) > 0) {
    colnames(promotion_clean)[invalid_names] <- paste0("temp_col_", 1:length(invalid_names))
    cat("已将", length(invalid_names), "个无效列名替换为临时名称\n")
  }
  
  # 转换数值型变量
  original_numeric_cols <- c("生育期_d", "亩产_kg", "较临近对照增产_pct", "较临近对照位次",
                            "较平均对照增产_pct", "较平均对照位次", "百粒重_g", "株高_cm")
  numeric_cols <- intersect(original_numeric_cols, colnames(promotion_clean))
  
  if (length(numeric_cols) < length(original_numeric_cols)) {
    missing_cols <- setdiff(original_numeric_cols, numeric_cols)
    cat("警告：以下列不存在于数据框中，已跳过：", paste(missing_cols, collapse = ", "), "\n")
  }
  
  promotion_clean <- promotion_clean %>%
    dplyr::mutate(across(all_of(numeric_cols), ~ as.numeric(as.character(.x)))) %>%
    tidyr::drop_na(亩产_kg) #, 生育期_d)去掉生育期筛选
  
  cat("数据清理完成！\n")
  cat("数据维度：", dim(promotion_clean)[1], "行 × ", dim(promotion_clean)[2], "列\n")
  
  #增加对照熟期差
  promotion_clean <-calculate_maturity_diff(promotion_clean)
  #保留两位小粒
  promotion_clean <- promotion_clean %>%
    mutate(across(where(is.numeric), ~round(.x, 2)))
  return(promotion_clean)
}


#' 创建输出目录
#' 
#' @param output_directory 输出目录路径
#' @return 创建的目录路径
create_output_directory <- function(output_directory) {
  if (!file.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
    cat("目录创建成功：", output_directory, "\n")
  } else {
    cat("目录已存在：", output_directory, "\n")
  }
  return(output_directory)
}


# ==============================================================================
# 2. 统计分析函数
# ==============================================================================

#' 计算产量核心指标统计
#' 
#' @param data 数据框
#' @param yield_col 产量列名，默认为"亩产_kg"
#' @return 统计结果数据框
calculate_yield_stats <- function(data, yield_col = "亩产_kg") {
  if (!yield_col %in% colnames(data)) {
    stop(paste("数据框中不存在", yield_col, "列"))
  }
  
  yield_stats <- data %>%
    dplyr::summarise(
      平均亩产 = mean(.data[[yield_col]], na.rm = TRUE),
      产量中位数 = median(.data[[yield_col]], na.rm = TRUE),
      最高亩产 = max(.data[[yield_col]], na.rm = TRUE),
      最低亩产 = min(.data[[yield_col]], na.rm = TRUE),
      产量标准差 = sd(.data[[yield_col]], na.rm = TRUE),
      产量变异系数 = (产量标准差 / 平均亩产) * 100
    ) %>%
    round(2)
  
  return(yield_stats)
}


#' 计算对照组平均值（横向宽格式数据框输出）
#' 
#' @param data 数据框
#' @param control_col 对照列名，默认为"是否对照"
#' @param custom_numeric_cols 自定义数值列，默认为NULL（自动识别所有数值列）
#' @param na.rm 是否移除NA值，默认为TRUE
#' @return 对照组平均值数据框
calc_control_means_wide <- function(data, control_col = "是否对照", 
                                    custom_numeric_cols = NULL, na.rm = TRUE) {
  if (!control_col %in% colnames(data)) {
    stop(paste("数据框中不存在", control_col, "列，请检查列名"))
  }
  
  control_data <- data %>% dplyr::filter(.data[[control_col]] == 1)
  
  if (!is.null(custom_numeric_cols)) {
    valid_cols <- intersect(custom_numeric_cols, colnames(control_data))
    valid_cols <- valid_cols[sapply(control_data[valid_cols], is.numeric)]
  } else {
    valid_cols <- sapply(control_data, is.numeric) %>% 
      which() %>% names() %>% 
      setdiff(control_col)
  }
  
  if (length(valid_cols) == 0) stop("无有效数值型列可计算")
  
  control_means_wide <- control_data %>%
    dplyr::summarise(across(all_of(valid_cols), ~ round(mean(.x, na.rm = na.rm), 3)), 
                    .groups = "drop")
  
  return(control_means_wide)
}


#' 计算生育期统计
#' 
#' @param data 数据框
#' @param growth_col 生育期列名，默认为"生育期_d"
#' @return 统计结果数据框
calculate_growth_stats <- function(data, growth_col = "生育期_d") {
  if (!growth_col %in% colnames(data)) {
    stop(paste("数据框中不存在", growth_col, "列"))
  }
  
  growth_stats <- data %>%
    dplyr::summarise(
      平均生育期 = mean(.data[[growth_col]], na.rm = TRUE),
      生育期中位数 = median(.data[[growth_col]], na.rm = TRUE),
      最长生育期 = max(.data[[growth_col]], na.rm = TRUE),
      最短生育期 = min(.data[[growth_col]], na.rm = TRUE),
      生育期范围 = 最长生育期 - 最短生育期
    ) %>%
    round(1)
  
  return(growth_stats)
}


#' 计算增产情况统计
#' 
#' @param data 数据框
#' @param increase_col 增产列名，默认为"较临近对照增产_pct"
#' @return 统计结果数据框
calculate_increase_stats <- function(data, increase_col = "较临近对照增产_pct") {
  if (!increase_col %in% colnames(data)) {
    return(paste("数据框中不存在", increase_col, "列"))
  }
  
  increase_stats <- data %>%
    dplyr::summarise(
      平均增产率 = mean(.data[[increase_col]], na.rm = TRUE),
      增产率中位数 = median(.data[[increase_col]], na.rm = TRUE),
      最高增产率 = max(.data[[increase_col]], na.rm = TRUE),
      最大减产率 = min(.data[[increase_col]], na.rm = TRUE),
      增产品种数量 = sum(.data[[increase_col]] > 0, na.rm = TRUE),
      增产品种比例 = (增产品种数量 / n()) * 100
    ) %>%
    round(2)
  
  return(increase_stats)
}


#' 计算相关性矩阵
#' 
#' @param data 数据框
#' @param corr_vars 要计算相关性的变量名向量
#' @return 相关性矩阵
calculate_correlation_matrix <- function(
    data, 
    corr_vars = c("亩产_kg", "生育期_d", "百粒重_g", "株高_cm")
) {
  # 定义默认返回的4×4零矩阵（保留原始变量名）
  default_zero_matrix <- matrix(0, nrow = 4, ncol = 4,
                                dimnames = list(corr_vars, corr_vars)) %>%
    round(3)
  
  tryCatch({
    # ==========================================================================
    # 1. 基础检查：输入数据是否为数据框
    # ==========================================================================
    if (!is.data.frame(data)) {
      warning("输入不是有效的数据框，返回4×4零矩阵！")
      return(default_zero_matrix)
    }
    
    # ==========================================================================
    # 2. 变量存在性检查：筛选数据中实际存在的变量
    # ==========================================================================
    missing_vars <- setdiff(corr_vars, colnames(data))
    if (length(missing_vars) > 0) {
      warning(paste("以下变量不存在于数据中，已跳过：", paste(missing_vars, collapse = ", ")))
    }
    existing_vars <- intersect(corr_vars, colnames(data))
    
    # 无有效变量时返回零矩阵
    if (length(existing_vars) < 1) {
      warning("没有找到任何有效变量，返回4×4零矩阵！")
      return(default_zero_matrix)
    }
    
    # ==========================================================================
    # 3. 数值类型校验：筛选出数值型变量（非数值型无法计算相关性）
    # ==========================================================================
    # 检查每个存在的变量是否为数值型（含整数型）
    is_numeric_var <- sapply(existing_vars, function(var) {
      is.numeric(data[[var]]) || is.integer(data[[var]])
    })
    numeric_vars <- existing_vars[is_numeric_var]
    non_numeric_vars <- existing_vars[!is_numeric_var]
    
    if (length(non_numeric_vars) > 0) {
      warning(paste("以下变量非数值型，无法参与相关性分析，已跳过：", paste(non_numeric_vars, collapse = ", ")))
    }
    
    # ==========================================================================
    # 4. 有效变量数量检查：至少需要2个数值型变量才能计算相关性
    # ==========================================================================
    if (length(numeric_vars) < 2) {
      warning(paste("有效数值型变量仅", length(numeric_vars), "个（需≥2个），返回4×4零矩阵！"))
      return(default_zero_matrix)
    }
    
    # ==========================================================================
    # 5. 计算相关性矩阵（处理NA和异常值）
    # ==========================================================================
    # 筛选数值型变量列，仅保留完整观测（避免因NA导致的计算警告）
    numeric_data <- data %>%
      dplyr::select(dplyr::all_of(numeric_vars)) %>%
      stats::na.omit()
    
    # 检查筛选后的数据是否有足够观测（至少2行才够计算相关性）
    if (nrow(numeric_data) < 2) {
      warning("数值型变量的有效观测（非NA）不足2行，返回4×4零矩阵！")
      return(default_zero_matrix)
    }
    
    # 检查每个变量是否有足够变异性（方差为0的变量无法计算相关性）
    variances <- sapply(numeric_data, stats::var)
    zero_var_vars <- names(variances[variances == 0])
    if (length(zero_var_vars) > 0) {
      warning(paste("以下变量方差为0（无变异性），已跳过：", paste(zero_var_vars, collapse = ", ")))
      # 重新筛选：移除方差为0的变量
      numeric_vars <- setdiff(numeric_vars, zero_var_vars)
      # 重新检查有效变量数量
      if (length(numeric_vars) < 2) {
        warning("移除方差为0的变量后，有效数值型变量不足2个，返回4×4零矩阵！")
        return(default_zero_matrix)
      }
      # 重新提取数据
      numeric_data <- numeric_data %>%
        dplyr::select(dplyr::all_of(numeric_vars))
    }
    
    # 计算相关性矩阵（use = "complete.obs" 双重保障，避免NA）
    corr_matrix <- cor(numeric_data, use = "complete.obs") %>%
      round(3)
    
    # 若矩阵计算后有效（行数≥2），则补全为4×4矩阵（缺失变量填充0）
    if (!is.null(corr_matrix) && nrow(corr_matrix) >= 2) {
      # 创建完整的4×4矩阵框架
      full_matrix <- default_zero_matrix
      # 将计算出的相关性值填充到对应位置
      if (length(numeric_vars) >= 2) {
        full_matrix[numeric_vars, numeric_vars] <- corr_matrix
      }
      corr_matrix <- full_matrix
    } else {
      # 极端异常情况，返回零矩阵
      warning("相关性矩阵计算结果无效，返回4×4零矩阵！")
      corr_matrix <- default_zero_matrix
    }
    
  }, error = function(e) {
    # 捕获所有未预期的错误，返回零矩阵
    warning(paste("相关性矩阵计算过程中发生错误：", e$message, "，返回4×4零矩阵！"))
    corr_matrix <- default_zero_matrix
  })
  
  return(corr_matrix)
}


# ==============================================================================
# 3. 数据可视化函数
# ==============================================================================

#' 设置ggplot2主题
setup_plot_theme <- function() {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请先安装 ggplot2 包")
  }
  
  ggplot2::theme_set(
    ggplot2::theme_bw() + 
      ggplot2::theme(
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11),
        legend.position = "bottom"
      )
  )
}


#' 绘制产量分布直方图
#' 
#' @param data 数据框
#' @param yield_col 产量列名
#' @param yield_stats 产量统计结果
#' @param ck_mean 对照平均值
#' @return ggplot对象
plot_yield_distribution <- function(data, yield_col = "亩产_kg", 
                                    yield_stats, ck_mean,
                                    axis_title_size = 20,  # 轴标题字体大小（新增参数）
                                    axis_text_size = 18) { # 轴刻度文本字体大小（新增参数）
  # 加载ggplot2（核心依赖，缺失则终止）
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请先安装 ggplot2 包：install.packages('ggplot2')")
  }
  
  # 最基础的核心依赖校验（避免模糊下标错误，明确提示问题）
  stopifnot(
    is.data.frame(data),                  # data必须是数据框
    yield_col %in% colnames(data),        # data必须包含yield_col列
    "平均亩产" %in% names(yield_stats),   # yield_stats必须有平均亩产
    yield_col %in% names(ck_mean),        # ck_mean必须有yield_col列
    is.numeric(axis_title_size),          # 新增参数校验
    is.numeric(axis_text_size)
  )
  
  # 核心绘图逻辑（不变）
  hist_yield <- hist(data[[yield_col]], breaks = 25, plot = FALSE)  # 用[[取列更安全
  max_count <- max(hist_yield$counts)
  
  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[yield_col]])) +  # 用[[适配列名变量
    ggplot2::geom_histogram(bins = 25, fill = "#2E86AB", alpha = 0.7, color = "white") +
    ggplot2::geom_vline(xintercept = yield_stats$平均亩产, 
                        color = "#F18F01", linetype = "dashed", linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = ck_mean[[yield_col]],  # 用[[取列更安全
                        color = "#C73E1D", linetype = "dashed", linewidth = 1.2) +
    ggplot2::labs(x = "亩产 (kg)", y = "", title = "(1) 亩产分布情况") +
    ggplot2::annotate("text", x = yield_stats$平均亩产 + 10, y = max_count * 0.9,
                      label = paste("平均值:", round(yield_stats$平均亩产, 1), "kg"), 
                      color = "#F18F01", hjust = 0,size=6) +
    ggplot2::annotate("text", x = ck_mean[[yield_col]] + 10, y = max_count * 0.8,
                      label = paste("对照:", round(ck_mean[[yield_col]], 1), "kg"), 
                      color = "#C73E1D", hjust = 0,size=6) +
    # 新增：设置轴标题和轴文本字体大小
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = axis_title_size),  # X轴标题大小
      axis.title.y = ggplot2::element_text(size = axis_title_size),  # Y轴标题大小
      axis.text.x = ggplot2::element_text(size = axis_text_size),    # X轴刻度文本大小
      axis.text.y = ggplot2::element_text(size = axis_text_size),    # Y轴刻度文本大小
      plot.title = ggplot2::element_text(size = axis_title_size + 2), # 标题大小（可选优化）
    )
  
  return(p)
}


#' 绘制产量等级分布图
#' 
#' @param data 数据框（需要包含产量等级列）
#' @param yield_col 产量列名
#' @param axis_title_size 轴标题字体大小（默认20）
#' @param axis_text_size 轴刻度文本字体大小（默认18）
#' @return 包含ggplot对象和处理后数据的列表
plot_yield_grade_distribution <- function(data, yield_col = "亩产_kg", 
                                          axis_title_size = 20,  # 新增参数
                                          axis_text_size = 18) { # 新增参数
  # 加载依赖包并校验
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请先安装 ggplot2 包")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("请先安装 dplyr 包：install.packages('dplyr')")
  }
  
  # 参数校验（和第一个函数保持一致）
  stopifnot(
    is.data.frame(data),                  # data必须是数据框
    yield_col %in% colnames(data),        # data必须包含yield_col列
    is.numeric(axis_title_size),          # 字体大小参数必须为数值
    is.numeric(axis_text_size)
  )
  
  # 创建产量等级
  data <- data %>%
    dplyr::mutate(产量等级 = dplyr::case_when(
      .data[[yield_col]] >= 200 ~ "(≥200kg)",
      .data[[yield_col]] >= 160 ~ "(160-199kg)",
      .data[[yield_col]] >= 120 ~ "(120-159kg)",
      TRUE ~ "(<120kg)"
    )) %>%
    dplyr::mutate(产量等级 = factor(产量等级, 
                                levels = c("(≥200kg)", "(160-199kg)", 
                                           "(120-159kg)", "(<120kg)")))
  
  grade_counts <- data %>%
    dplyr::count(产量等级) %>%
    dplyr::mutate(比例_pct = (n / sum(n)) * 100)
  
  # 计算文本大小（按比例适配，保持视觉协调）
  text_size <- ifelse(axis_text_size == 18, 3.5, axis_text_size * 0.194)
  
  p <- grade_counts %>%
    ggplot2::ggplot(ggplot2::aes(x = 产量等级, y = n, fill = 产量等级)) +
    ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = paste(n, "\n(", round(比例_pct, 1), "%)")), 
                       vjust = -0.3, size = text_size) +  # 使用动态文本大小
    ggplot2::scale_fill_manual(values = c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D")) +
    ggplot2::labs(x = "产量等级", y = "", title = "(2) 产量等级分布") +
    ggplot2::theme(
      legend.position = "none",
      # 控制轴标题字体大小（和第一个函数一致）
      axis.title.x = ggplot2::element_text(size = axis_title_size),
      axis.title.y = ggplot2::element_text(size = axis_title_size),
      # 控制轴刻度文本字体大小（和第一个函数一致）
      axis.text.x = ggplot2::element_text(size = axis_text_size),
      axis.text.y = ggplot2::element_text(size = axis_text_size),
      # 控制标题字体大小（和第一个函数保持一致的样式）
      plot.title = ggplot2::element_text(size = axis_title_size + 2)
    ) +
    ggplot2::ylim(0, max(grade_counts$n) * 1.15)
  
  return(list(plot = p, data = data))
}


#' 绘制增产情况分布图
#' 
#' @param data 数据框（需要包含增产等级列）
#' @param increase_col 增产列名
#' @param axis_title_size 轴标题字体大小（默认20）
#' @param axis_text_size 轴刻度文本字体大小（默认18）
#' @return 包含ggplot对象和处理后数据的列表
plot_increase_distribution <- function(data, increase_col = "较临近对照增产_pct",
                                       axis_title_size = 20,  # 新增字体大小参数
                                       axis_text_size = 18) { # 新增字体大小参数
  # 加载依赖包并校验
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请先安装 ggplot2 包")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("请先安装 dplyr 包：install.packages('dplyr')")
  }
  
  # 参数合法性校验（和前两个函数保持一致）
  stopifnot(
    is.data.frame(data),                  # data必须是数据框
    increase_col %in% colnames(data),     # 必须包含增产列
    is.numeric(axis_title_size),          # 字体大小参数必须为数值
    is.numeric(axis_text_size)
  )
  
  # 创建增产等级
  data <- data %>%
    dplyr::mutate(增产等级 = dplyr::case_when(
      .data[[increase_col]] >= 10 ~ "增产(≥10%)",
      .data[[increase_col]] >= 0 ~ "增产(0-9%)",
      .data[[increase_col]] >= -10 ~ "减产(-10%~0)",
      TRUE ~ "减产(< -10%)"
    )) %>%
    dplyr::mutate(增产等级 = factor(增产等级, 
                                levels = c("增产(≥10%)", "增产(0-9%)", 
                                           "减产(-10%~0)", "减产(< -10%)")))
  
  increase_counts <- data %>%
    dplyr::count(增产等级) %>%
    dplyr::mutate(比例_pct = (n / sum(n)) * 100)
  
  # 计算文本大小（按比例适配，保持视觉协调）
  text_size <- ifelse(axis_text_size == 18, 3.5, axis_text_size * 0.194)
  
  p <- increase_counts %>%
    ggplot2::ggplot(ggplot2::aes(x = 增产等级, y = n, fill = 增产等级)) +
    ggplot2::geom_bar(stat = "identity", alpha = 0.8) +
    ggplot2::geom_text(ggplot2::aes(label = paste(n, "\n(", round(比例_pct, 1), "%)")), 
                       vjust = -0.3, size = text_size) +  # 动态文本大小
    ggplot2::scale_fill_manual(values = c("#6A994E", "#8B5A3C", "#F18F01", "#C73E1D")) +
    ggplot2::labs(x = "增产等级", y = "", 
                  title = "(3) 较临近对照增产情况分布") +
    ggplot2::theme(
      legend.position = "none",
      # 控制轴标题字体大小（和前两个函数一致）
      axis.title.x = ggplot2::element_text(size = axis_title_size),
      axis.title.y = ggplot2::element_text(size = axis_title_size),
      # 控制轴刻度文本字体大小（保留原有角度设置，新增大小控制）
      axis.text.x = ggplot2::element_text(size = axis_text_size, angle = 15, hjust = 1),
      axis.text.y = ggplot2::element_text(size = axis_text_size),
      # 控制标题字体大小（和前两个函数保持一致）
      plot.title = ggplot2::element_text(size = axis_title_size + 2)
    ) +
    ggplot2::ylim(0, max(increase_counts$n) * 1.15)
  
  return(list(plot = p, data = data))
}


#' 绘制生育期分布直方图
#' 
#' @param data 数据框
#' @param growth_col 生育期列名
#' @param growth_stats 生育期统计结果
#' @param ck_mean 对照平均值
#' @param axis_title_size 轴标题字体大小（默认20）
#' @param axis_text_size 轴刻度文本字体大小（默认18）
#' @return ggplot对象
plot_growth_distribution <- function(data, growth_col = "生育期_d", 
                                     growth_stats, ck_mean,
                                     axis_title_size = 20,  # 新增字体大小参数
                                     axis_text_size = 18) { # 新增字体大小参数
  # 加载ggplot2包检查（保留原有逻辑）
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请先安装 ggplot2 包")
  }
  
  # 引入ggplot2（避免重复写ggplot2::前缀，简化代码）
  library(ggplot2, quietly = TRUE)
  
  # 定义白板图生成函数（出错时调用）- 新增字体大小适配
  create_error_blank_plot <- function(error_msg) {
    # 计算错误文本大小（按比例适配）
    error_text_size <- ifelse(axis_text_size == 18, 4, axis_text_size * 0.222)
    
    # 创建空白基础图，隐藏所有默认元素
    ggplot() +
      # 显示错误信息（自动换行，居中显示）
      annotate("text", x = 0.5, y = 0.5, 
               label = paste("绘图失败：", error_msg),
               color = "#C73E1D", size = error_text_size, hjust = 0.5, vjust = 0.5) +
      # 设置白板主题（隐藏坐标轴、网格、背景线）
      theme_void() +
      # 确保绘图区域为白色背景
      theme(plot.background = element_rect(fill = "white", color = NA),
            # 调整边距，让文字居中显示
            plot.margin = margin(20, 20, 20, 20)) +
      # 设置坐标范围，固定白板大小比例
      xlim(0, 1) +
      ylim(0, 1)
  }
  
  # 主绘图逻辑（用tryCatch捕获所有错误）
  tryCatch({
    # ==========================================================================
    # 新增：前置参数有效性检查（包含字体大小参数校验）
    # ==========================================================================
    # 检查data是否为数据框
    if (!is.data.frame(data)) {
      stop("参数 'data' 必须是数据框")
    }
    # 检查growth_col是否存在于data中
    if (!growth_col %in% colnames(data)) {
      stop(paste("数据框中不存在列名：", growth_col))
    }
    # 检查growth_col是否为数值型
    if (!is.numeric(data[[growth_col]]) && !is.integer(data[[growth_col]])) {
      stop(paste("列", growth_col, "必须是数值型（当前为", class(data[[growth_col]]), "）"))
    }
    # 检查growth_stats是否包含"平均生育期"字段
    if (is.null(growth_stats$平均生育期)) {
      stop("参数 'growth_stats' 缺少 '平均生育期' 字段")
    }
    # 检查ck_mean是否包含growth_col字段
    if (is.null(ck_mean[[growth_col]])) {
      stop(paste("参数 'ck_mean' 缺少", growth_col, "字段"))
    }
    # 检查数据是否有有效观测（非NA、非空）
    valid_data <- data[[growth_col]] %>% stats::na.omit()
    if (length(valid_data) < 2) {
      stop(paste("列", growth_col, "的有效数值观测不足2个（无法绘制直方图）"))
    }
    # 新增：字体大小参数校验
    if (!is.numeric(axis_title_size) || axis_title_size <= 0) {
      stop("参数 'axis_title_size' 必须是大于0的数值")
    }
    if (!is.numeric(axis_text_size) || axis_text_size <= 0) {
      stop("参数 'axis_text_size' 必须是大于0的数值")
    }
    
    # ==========================================================================
    # 原有绘图逻辑（优化冗余代码 + 新增字体大小控制）
    # ==========================================================================
    # 计算直方图参数（删除重复计算）
    hist_growth <- hist(data[[growth_col]], breaks = 15, plot = FALSE)
    max_count_growth <- max(hist_growth$counts)
    
    # 计算标注文本大小（按比例适配）
    annotate_text_size <- ifelse(axis_text_size == 18, 6, axis_text_size * 0.333)
    
    # 绘制分布图
    p <- data %>%
      ggplot(aes(x = .data[[growth_col]])) +
      geom_histogram(bins = 15, fill = "#8B5A3C", alpha = 0.7, color = "white") +
      geom_vline(xintercept = growth_stats$平均生育期, 
                 color = "#F18F01", linetype = "dashed", linewidth = 1.2) +
      geom_vline(xintercept = ck_mean[[growth_col]], 
                 color = "#C73E1D", linetype = "dashed", linewidth = 1.2) +
      labs(x = "生育期 (天)", y = "", title = "(4) 生育期分布情况") +
      annotate("text", x = growth_stats$平均生育期 + 1, y = max_count_growth * 0.9,
               label = paste("平均值:", growth_stats$平均生育期, "天"), 
               color = "#F18F01", hjust = 0, size = annotate_text_size) +  # 动态文本大小
      annotate("text", x = ck_mean[[growth_col]] + 1, y = max_count_growth * 0.8,
               label = paste("对照:", ck_mean[[growth_col]], "天"), 
               color = "#C73E1D", hjust = 0, size = annotate_text_size) +  # 动态文本大小
      # 新增：字体大小控制（和其他函数统一）
      theme(
        # 轴标题字体大小
        axis.title.x = element_text(size = axis_title_size),
        axis.title.y = element_text(size = axis_title_size),
        # 轴刻度文本字体大小
        axis.text.x = element_text(size = axis_text_size),
        axis.text.y = element_text(size = axis_text_size),
        # 标题字体大小（和其他函数保持一致）
        plot.title = element_text(size = axis_title_size + 2)
      )
    
    return(p)
    
  }, error = function(e) {
    # 捕获所有错误，生成白板错误图
    error_msg <- gsub("^Error: ", "", e$message)  # 清理错误信息格式
    return(create_error_blank_plot(error_msg))
  })
}


#' 保存基础分析图表（支持任意数量图表）
#' 
#' @param plots 图表列表（可包含1个或多个ggplot对象）
#' @param output_directory 输出路径
#' @param pngname 输出文件名（含.png后缀）
#' @param ncol 列数（控制子图排列，默认自动计算最优布局）
#' @param width 图片宽度（默认1500）
#' @param height 图片高度（默认1000）
#' @param title 图表总标题（默认无标题）
#' @param title_size 标题字体大小（默认16）
#' @param title_bold 是否加粗标题（默认TRUE）
save_basic_analysis_plots <- function(plots, output_directory, pngname,
                                      ncol = NULL,
                                      width = 1500,
                                      height = 1000,
                                      title = "",
                                      title_size = 16,
                                      title_bold = TRUE) {
  # 检查必要包
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("请先安装 gridExtra 包：install.packages('gridExtra')")
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("请先安装 grid 包：install.packages('grid')")
  }
  
  # 输入验证
  if (!is.list(plots)) {
    stop("参数 'plots' 必须是图表对象组成的列表（例如：list(p1, p2, p3)）")
  }
  if (length(plots) == 0) {
    stop("参数 'plots' 不能为空列表，请至少传入1个图表对象")
  }
  
  # 处理输出路径（确保路径存在）
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
    message(paste("已创建输出目录：", output_directory))
  }
  
  # 构建输出文件路径
  pngpath <- file.path(output_directory, pngname)  # 更安全的路径拼接方式
  
  # 自动计算最优列数（如果未指定ncol）
  if (is.null(ncol)) {
    n_plots <- length(plots)
    if (n_plots == 1) {
      ncol <- 1
    } else if (n_plots %in% c(2, 4)) {
      ncol <- 2  # 2图/4图用2列布局
    } else if (n_plots == 3) {
      ncol <- 3  # 3图用3列布局
    } else {
      ncol <- ceiling(sqrt(n_plots))  # 更多图时用平方根向上取整作为列数
    }
  }
  
  # 计算行数
  nrow <- ceiling(length(plots) / ncol)
  
  # 调整图片高度（根据子图数量自动适配，避免拥挤）
  if (nrow > 2) {
    height <- height * (nrow / 2)  # 超过2行时按比例增加高度
  }
  
  # 保存图片
  png(pngpath, width = width, height = height, res = 150)
  
  # 排列图表
  gridExtra::grid.arrange(
    grobs = plots,  # 图表列表
    ncol = ncol,
    nrow = nrow,
    top = grid::textGrob(
      label = title,
      gp = grid::gpar(
        fontsize = title_size,
        fontface = ifelse(title_bold, "bold", "plain")
      )
    )
  )
  
  dev.off()
  
  # 输出成功信息
  cat(sprintf("基础分析图表已保存至：%s\n", normalizePath(pngpath)))
  cat(sprintf("布局：%d行 × %d列\n", nrow, ncol))
}



#' 绘制相关性分析图
#' 
#' @param corr_matrix 相关性矩阵
#' @param output_directory 输出目录
#' @param png_name 图片名称
save_correlation_plot <- function(corr_matrix, output_directory,png_name) {
  if (!requireNamespace("corrplot", quietly = TRUE)) {
    stop("请先安装 corrplot 包")
  }
  
  pngpath <- paste(output_directory, "/", png_name, sep = "")
  png(pngpath, width = 800, height = 700, res = 150)
  
  corrplot::corrplot(corr_matrix, 
                    method = "circle", 
                    type = "upper", 
                    order = "hclust",
                    tl.col = "black", 
                    tl.srt = 45,
                    addCoef.col = "black",
                    number.cex = 0.8,
                    title = "农艺性状与产量相关性分析",
                    mar = c(0, 0, 2, 0))
  dev.off()
  
  cat("相关性分析图已保存\n")
}


#' 绘制性状与产量关系散点图
#' 
#' @param data 数据框
#' @param x_var x轴变量名
#' @param y_var y轴变量名（默认亩产）
#' @param corr_value 相关系数值
#' @return ggplot对象
plot_trait_yield_scatter <- function(data, x_var, y_var = "亩产_kg", 
                                    corr_value = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请先安装 ggplot2 包")
  }
  
  title_label <- paste0(x_var, "与产量关系")
  if (!is.null(corr_value)) {
    title_label <- paste0(title_label, "\n相关系数: ", corr_value)
  }
  
  p <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[x_var]], y = .data[[y_var]], 
                                color = .data[[y_var]])) +
    ggplot2::geom_point(alpha = 0.6, size = 1.5) +
    ggplot2::geom_smooth(method = "lm", color = "#F18F01", se = FALSE, linewidth = 1) +
    ggplot2::scale_color_viridis_c(option = "viridis") +
    ggplot2::labs(x = x_var, y = "亩产 (kg)", title = title_label, color = "亩产(kg)") +
    ggplot2::theme(
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 7),
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      plot.title = element_text(size = 10, hjust = 0.5)
    )
  
  return(p)
}


#' 绘制百粒重分布图
#' 
#' @param data 数据框
#' @param weight_col 百粒重列名，默认为"百粒重_g"
#' @return ggplot对象
plot_weight_distribution <- function(data, weight_col = "百粒重_g") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请先安装 ggplot2 包")
  }
  
  mean_weight <- mean(data[[weight_col]], na.rm = TRUE)
  hist_weight <- hist(na.omit(data[[weight_col]]), breaks = 20, plot = FALSE)
  max_count_weight <- max(hist_weight$counts)
  
  p <- data %>%
    tidyr::drop_na(weight_col) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[weight_col]])) +
    ggplot2::geom_histogram(bins = 20, fill = "#C73E1D", alpha = 0.7, color = "white") +
    ggplot2::geom_vline(xintercept = mean_weight, color = "#F18F01", 
                       linetype = "dashed", linewidth = 1) +
    ggplot2::labs(x = "百粒重 (g)", y = "品种数量", 
                 title = paste("(2) 百粒重分布\n平均值: ", round(mean_weight, 2), "g", sep = "")) +
    ggplot2::annotate("text", x = mean_weight + 0.5, y = max_count_weight * 0.9,
                     label = paste("平均值:", round(mean_weight, 2), "g"), 
                     color = "#F18F01", hjust = 0, size = 3) +
    ggplot2::theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      plot.title = element_text(size = 10, hjust = 0.5)
    )
  
  return(p)
}


#' 绘制株高分布图
#' 
#' @param data 数据框
#' @param height_col 株高列名，默认为"株高_cm"
#' @return ggplot对象
plot_height_distribution <- function(data, height_col = "株高_cm") {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请先安装 ggplot2 包")
  }
  
  height_stats <- data %>%
    tidyr::drop_na(height_col) %>%
    dplyr::summarise(
      mean_height = mean(.data[[height_col]], na.rm = TRUE),
      max_count_height = hist(.data[[height_col]], breaks = 20, plot = FALSE)$counts %>% max()
    )
  
  mean_height <- height_stats$mean_height
  max_count_height <- height_stats$max_count_height
  
  p <- data %>%
    tidyr::drop_na(height_col) %>%
    ggplot2::ggplot(ggplot2::aes(x = .data[[height_col]])) +
    ggplot2::geom_histogram(bins = 20, fill = "#8B5A3C", alpha = 0.7, color = "white") +
    ggplot2::geom_vline(xintercept = mean_height, color = "#F18F01", 
                       linetype = "dashed", linewidth = 1) +
    ggplot2::labs(x = "株高 (cm)", y = "品种数量", 
                 title = paste("(3) 株高分布\n平均值: ", round(mean_height, 1), "cm", sep = "")) +
    ggplot2::annotate("text", x = mean_height + 3, y = max_count_height * 0.9,
                     label = paste("平均值:", round(mean_height, 1), "cm"), 
                     color = "#F18F01", hjust = 0, size = 3) +
    ggplot2::theme(
      axis.text = element_text(size = 8),
      axis.title = element_text(size = 9),
      plot.title = element_text(size = 10, hjust = 0.5)
    )
  
  return(p)
}



#' 绘制农艺性状（单一性状）分布图
#'
#' @param trait_data 性状数据框，必须包含"类型"和"n"列
#' @param trait_name  性状名称（字符型），用于图例和标题
#' @return ggplot对象
plot_single_trait_distribution <- function(data, column) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("请先安装 ggplot2 包。")
  }
  
  # 将选择的列转为因子（保持出现顺序）
  data[[column]] <- factor(data[[column]], levels = unique(data[[column]]))
  
  # 统计频数
  plot_data <- as.data.frame(table(data[[column]]))
  colnames(plot_data) <- c("类型", "数量")
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = 类型, y = 数量, fill = 类型)) +
    # 柱状图主体
    ggplot2::geom_col(width = 0.7, alpha = 0.85) +
    # 数值标签（同步放大）
    ggplot2::geom_text(
      ggplot2::aes(label = 数量),
      vjust = -0.3, size = 4.5, fontface = "bold"  # 标签字体从3.5放大到4.5
    ) +
    # 标题和轴标签
    ggplot2::labs(
      x = paste0(column, "类型"),
      y = "",
      title = ""
    ) +
    # 基础主题
    ggplot2::theme_bw() +
    # 自定义主题（核心：放大字体和坐标）
    ggplot2::theme(
      # X轴刻度文字放大
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, size = 18),
      # Y轴刻度文字放大
      axis.text.y = ggplot2::element_text(size = 24),
      # X轴标题文字放大
      axis.title.x = ggplot2::element_text(size = 24, face = "bold"),
      # Y轴标题文字放大
      axis.title.y = ggplot2::element_text(size = 24, face = "bold"),
      # 标题文字放大
      plot.title = ggplot2::element_text(size = 24, face = "bold", hjust = 0.5),
      # 隐藏图例
      legend.position = "none"
    ) +
    # Y轴范围调整（避免标签超出）
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.15)))
}


#' 保存农艺性状分析图表
#' 
#' @param p1 图1（产量分布或散点图）
#' @param p2 图2（产量等级分布或散点图）
#' @param p3 图3（增产情况分布或散点图）
#' @param p8 图8（性状分布图）
#' @param output_directory 输出路径
#' @param title 图表标题
save_traits_analysis_plots <- function(p1, p2, p3, p8, output_directory,
                                      title = "大豆品种农艺性状与产量相关性分析") {
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("请先安装 gridExtra 包")
  }
  
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("请先安装 grid 包")
  }
  
  pngpath <- file.path(output_directory, png_name)
  png(pngpath, width = 1400, height = 1000, res = 150)
  
  gridExtra::grid.arrange(
    p1, p2, p3, p8,
    ncol = 2, nrow = 2,
    widths = c(1, 1.2),
    heights = c(1, 1.2),
    top = grid::textGrob(title, gp = grid::gpar(fontsize = 16, fontface = "bold")),
    bottom = grid::textGrob("注：数据来源为大豆品种产比试验", 
                           gp = grid::gpar(fontsize = 8, col = "gray50"))
  )
  dev.off()
  
  cat("农艺性状分析图表已保存\n")
}


#' 绘制雷达图
#' 
#' @param data 数据框
#' @param variety_col 品种/阶段列名，默认为"阶段名称"
#' @param top_n 选择前N个高产品种（当未指定specified_varieties时生效），默认为5
#' @param specified_varieties 手动指定要绘制的品种/阶段名称向量，若提供则忽略top_n，默认为NULL
#' @param output_directory 输出路径（必填）
#' @param title 图表标题，默认为"核心推荐品种综合性能雷达图"
#' @return 实际绘制的品种/阶段名称向量
save_radar_chart <- function(data, variety_col = "阶段名称", top_n = 5, 
                             specified_varieties = NULL, output_directory, png_name,
                             title = "品种综合性能雷达图") {
  # ========================= 1. 包依赖检查（警告替代终止）=========================
  required_packages <- c("fmsb", "scales", "tibble", "dplyr", "tidyr")
  missing_pkgs <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_pkgs) > 0) {
    warning(paste("缺少必需包，无法执行绘图：", paste(missing_pkgs, collapse = ", "), 
                  "\n请运行：install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))"))
    return(invisible(NULL))
  }
  
  # 加载包（静默加载）
  lapply(required_packages, library, character.only = TRUE, quietly = TRUE)
  
  # ========================= 2. 数据筛选（容错处理）=========================
  tryCatch({
    # 筛选要绘制的品种/阶段
    if (!is.null(specified_varieties)) {
      valid_varieties <- intersect(specified_varieties, unique(data[[variety_col]]))
      if (length(valid_varieties) == 0) {
        warning("指定的品种/阶段在数据中均不存在，请检查 specified_varieties 参数")
        return(invisible(NULL))
      }
      if (length(valid_varieties) < length(specified_varieties)) {
        warning(paste("以下品种/阶段未找到，已过滤：", 
                      paste(setdiff(specified_varieties, valid_varieties), collapse = ", ")))
      }
      selected_varieties <- valid_varieties
    } else {
      # 按亩产筛选前N个品种（处理无亩产数据的情况）
      high_yield_varieties <- data %>%
        drop_na(亩产_kg) %>%
        arrange(desc(亩产_kg))
      
      if (nrow(high_yield_varieties) == 0) {
        warning("数据中无有效（非NA）亩产数据，无法筛选品种")
        return(invisible(NULL))
      }
      
      selected_varieties <- head(high_yield_varieties, top_n)[[variety_col]]
      if (length(selected_varieties) < top_n) {
        warning(paste("有效高产品种不足", top_n, "个，仅使用", length(selected_varieties), "个有效品种"))
      }
    }
    
    # ========================= 3. 雷达图数据准备（双重NA过滤）=========================
    radar_data <- data %>%
      filter(.data[[variety_col]] %in% selected_varieties) %>%
      select(
        品种 = all_of(variety_col),
        亩产 = 亩产_kg,
        增产率 = 较临近对照增产_pct,
        生育期 = 生育期_d,
        株高 = 株高_cm,
        百粒重 = 百粒重_g
      ) %>%
      drop_na()  # 第一次过滤：原始指标NA
    
    if (nrow(radar_data) == 0) {
      warning("筛选后的品种无完整指标数据（存在NA），无法绘图")
      return(invisible(NULL))
    }
    
    # ========== 新增：提取原始指标的最大/最小值（用于显示范围） ==========
    # 定义各指标的单位（匹配列名）
    index_units <- list(
      亩产 = "kg",
      增产率 = "%",
      生育期 = "d",
      株高 = "cm",
      百粒重 = "g"
    )
    # 提取原始指标的最大/最小值（保留1位小数）
    raw_stats <- radar_data %>%
      select(-品种) %>%
      summarise(across(everything(), list(
        min = ~ round(min(.x, na.rm = TRUE), 1),
        max = ~ round(max(.x, na.rm = TRUE), 1)
      )))
    # 构造带范围+单位的指标标签（核心修改）
    index_labels <- sapply(colnames(radar_data)[-1], function(idx) {
      min_val <- raw_stats[[paste0(idx, "_min")]]
      max_val <- raw_stats[[paste0(idx, "_max")]]
      unit <- index_units[[idx]]
      paste0(idx, "\n(", min_val, "-", max_val, unit, ")")
    })
    
    # 标准化（处理极值/重复值导致的NA）
    radar_data_norm <- radar_data %>%
      mutate(across(-品种, ~ scales::rescale(.x, to = c(0, 1), na.rm = TRUE))) %>%
      drop_na() %>%  # 第二次过滤：标准化后产生的NA
      column_to_rownames("品种")
    
    if (nrow(radar_data_norm) == 0) {
      warning("标准化后无有效数据，无法绘图")
      return(invisible(NULL))
    }
    
    # 添加fmsb必需的最大/最小值行（标准化后）
    max_vals <- apply(radar_data_norm, 2, max, na.rm = TRUE)
    min_vals <- apply(radar_data_norm, 2, min, na.rm = TRUE)
    radar_fmsb <- rbind(
      "最大值" = max_vals,
      "最小值" = min_vals,
      radar_data_norm
    ) %>%
      rownames_to_column("品种")
    
    # ========================= 4. 输出目录创建（容错）=========================
    if (!dir.exists(output_directory)) {
      dir_create_result <- tryCatch({
        dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
        message(paste("已创建输出目录：", output_directory))
        TRUE
      }, error = function(e) {
        warning(paste("创建输出目录失败：", e$message))
        FALSE
      })
      
      if (!dir_create_result) return(invisible(NULL))
    }
    
    # ========================= 5. 绘图核心（错误捕获+资源清理）=========================
    pngpath <- file.path(output_directory, png_name)
    plot_success <- FALSE
    
    tryCatch({
      # 打开PNG设备（指定背景色避免透明问题）
      png(pngpath, width = 1200, height = 900, res = 150, bg = "white")  # 加宽画布适配长标签
      
      # 绘制雷达图（核心修改：vlabels使用带范围的标签）
      if (ncol(radar_fmsb) < 3) stop("雷达图指标不足（至少需要2个数值指标）")
      if (nrow(radar_fmsb) < 3) stop("无有效品种数据用于绘图")
      
      radarchart(
        df = radar_fmsb[, -1],
        axistype = 1,
        title = title,
        title.col = "black",
        title.cex = 1.2,
        lwd = 2,
        lty = 1,
        col = rainbow(nrow(radar_fmsb) - 2),
        cglcol = "gray80",
        cglty = 1,
        cglwd = 0.8,
        vlabels = index_labels,  # 替换为带范围的标签
        vlcex = 0.9,  # 调整标签字体大小适配换行
        pcol = rainbow(nrow(radar_fmsb) - 2),
        plwd = 2,
        plty = 1
      )
      
      # 添加图例（容错处理图例数量）
      legend_varieties <- radar_fmsb$品种[-c(1:2)]
      if (length(legend_varieties) > 0) {
        legend(
          x = "bottomright",
          legend = legend_varieties,
          col = rainbow(length(legend_varieties)),
          lwd = 2,
          cex = 0.9,
          bty = "n"
        )
      }
      
      plot_success <- TRUE
    }, error = function(e) {
      # 捕获绘图过程中的所有错误
      warning(paste("绘图失败：", e$message))
    }, finally = {
      # 确保无论成功与否，都关闭图形设备（关键！避免设备占用）
      if (dev.cur() > 1) {  # 检查是否有打开的设备
        dev.off()
      }
    })
    
    # ========================= 6. 结果处理（成功/失败分支）=========================
    if (plot_success) {
      message(paste("✅ 雷达图已保存至：", pngpath))
      plotted_varieties <- radar_fmsb$品种[-c(1:2)]
      message(paste("📊 实际绘制品种：", paste(plotted_varieties, collapse = ", ")))
      # 额外输出指标范围信息
      message("📏 各指标原始范围：")
      for (idx in names(index_labels)) {
        message(paste("  -", index_labels[idx]))
      }
      return(invisible(plotted_varieties))
    } else {
      # 清理绘图失败产生的空文件
      if (file.exists(pngpath)) {
        file.remove(pngpath)
        warning(paste("❌ 已删除绘图失败的空文件：", pngpath))
      }
      return(invisible(NULL))
    }
    
  }, error = function(e) {
    # 捕获函数执行过程中的未预期错误
    warning(paste("函数执行异常：", e$message))
    return(invisible(NULL))
  })
}

# ==============================================================================
# 4. 品种筛选函数
# ==============================================================================

#' 筛选高产优质品种
#' 
#' @param data 数据框
#' @param yield_col 产量列名
#' @param increase_col 增产列名
#' @param top_pct 选择前百分之多少，默认为0.2（前20%）
#' @param min_increase 最小增产百分比，默认为0
#' @return 筛选结果数据框
filter_high_yield_varieties <- function(data, yield_col = "亩产_kg", 
                                       increase_col = "较临近对照增产_pct",
                                       top_pct = 0.2, min_increase = 0) {
  high_yield_threshold <- quantile(data[[yield_col]], 1 - top_pct, na.rm = TRUE)
  
  high_yield_varieties <- data %>%
    dplyr::filter(.data[[yield_col]] >= high_yield_threshold, 
                 .data[[increase_col]] > min_increase) %>%
    dplyr::arrange(desc(.data[[yield_col]])) %>%
    dplyr::select(阶段名称, all_of(yield_col), all_of(increase_col), 
                生育期_d, 株高_cm, 百粒重_g)
  
  return(list(varieties = high_yield_varieties, threshold = high_yield_threshold))
}


#' 筛选早熟高产品种
#' 
#' @param data 数据框
#' @param growth_col 生育期列名
#' @param yield_col 产量列名
#' @param increase_col 增产列名
#' @param max_growth_days 最大生育期天数，默认为95
#' @param min_yield_ratio 最小产量相对于平均值的比例，默认为1（即≥平均值）
#' @param min_increase 最小增产百分比，默认为0
#' @return 筛选结果数据框
filter_early_mature_varieties <- function(data, growth_col = "生育期_d", 
                                         yield_col = "亩产_kg",
                                         increase_col = "较临近对照增产_pct",
                                         max_growth_days = 95, 
                                         min_yield_ratio = 1,
                                         min_increase = 0) {
  avg_yield <- mean(data[[yield_col]], na.rm = TRUE)
  min_yield <- avg_yield * min_yield_ratio
  
  early_mature_varieties <- data %>%
    dplyr::filter(.data[[growth_col]] <= max_growth_days, 
                 .data[[yield_col]] >= min_yield, 
                 .data[[increase_col]] > min_increase) %>%
    dplyr::arrange(desc(.data[[yield_col]])) %>%
    dplyr::select(阶段名称, all_of(yield_col), all_of(increase_col), 
                 all_of(growth_col))
  
  return(list(varieties = early_mature_varieties, avg_yield = avg_yield))
}


#' 筛选显著增产品种
#' 
#' @param data 数据框
#' @param increase_col 增产列名
#' @param min_increase_pct 最小增产百分比，默认为20
#' @return 筛选结果数据框
filter_high_increase_varieties <- function(data, increase_col = "较临近对照增产_pct",
                                          min_increase_pct = 20) {
  high_increase_varieties <- data %>%
    dplyr::filter(.data[[increase_col]] >= min_increase_pct) %>%
    dplyr::arrange(desc(.data[[increase_col]])) %>%
    dplyr::select(阶段名称, 亩产_kg, all_of(increase_col), 生育期_d)
  
  return(high_increase_varieties)
}



#' 筛选晋级品种（按阶段匹配田间ID）
#' @param data 核心数据框（含田间ID列：fieldid）
#' @param planting 晋级信息表（必须含 fieldid 列和 next_stage 列）
#' @param stagename 需要筛选的阶段名称（默认为"产比"）
#' @return 返回data中与planting表next_stage为stagename的fieldid匹配的记录
filter_promotion_varieties <- function(data, planting, stagename = "初级产比") {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("请先安装 dplyr 包（运行：install.packages('dplyr')）")
  }
  
  # 1. 在planting中筛选next_stage等于stagename的行，获得fieldid
  ids <- planting %>%
    dplyr::filter(next_stage == stagename) %>%
    dplyr::pull(fieldid)
  
  if (length(ids) == 0) {
    warning(sprintf("planting表中无next_stage为'%s'的记录，返回空数据框", stagename))
    return(data[0, ])
  }
  
  # 2. 在data中筛选fieldid列与上述ids匹配的记录
  matched <- data %>%
    dplyr::filter(田间ID %in% ids)
  
  return(matched)
}



# ==============================================================================
# 5. 报告生成函数
# ==============================================================================

#' 生成Excel分析报告
#' 
#' @param promotion_clean 清理后的数据
#' @param high_yield_varieties 高产优质品种数据框
#' @param early_mature_varieties 早熟高产品种数据框
#' @param high_increase_varieties 显著增产品种数据框

#' @param output_directory 输出路径
#' @param filename 文件名，默认为"promotion_analysis_results_R.xlsx"
generate_excel_report <- function(promotion_clean, 
                                  promoted_varieties, 
                                  eliminated_varieties, 
                                  selected_plant_varieties,
                                  output_directory, 
                                  filename = "promotion_analysis_results_R.xlsx") {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("请先安装 openxlsx 包")
  }
  
  wb <- openxlsx::createWorkbook()
  
  # 添加数据工作表
  openxlsx::addWorksheet(wb, "原始清理数据")
  openxlsx::writeData(wb, "原始清理数据", promotion_clean)
  
  if (nrow(promoted_varieties) > 0) {
    openxlsx::addWorksheet(wb, "晋级材料")
    promoted <- promoted_varieties %>%
      dplyr::mutate(across(c(亩产_kg, 较临近对照增产_pct, 百粒重_g), round, 2)) %>%
      dplyr::mutate(across(c(生育期_d, 株高_cm), round, 1))
    openxlsx::writeData(wb, "晋级材料", promoted)
  }
  
  if (nrow(eliminated_varieties) > 0) {
    openxlsx::addWorksheet(wb, "淘汰材料")
    eliminated <- eliminated_varieties %>%
      dplyr::mutate(across(c(亩产_kg, 较临近对照增产_pct), round, 2)) %>%
      dplyr::mutate(生育期_d = round(生育期_d, 0))
    openxlsx::writeData(wb, "淘汰材料", eliminated)
  }
  
  if (nrow(selected_plant_varieties) > 0) {
    openxlsx::addWorksheet(wb, "选单株材料")
    selected <- selected_plant_varieties %>%
      dplyr::mutate(across(c(亩产_kg, 较临近对照增产_pct), round, 2)) %>%
      dplyr::mutate(生育期_d = round(生育期_d, 0))
    openxlsx::writeData(wb, "选单株材料", selected)
  }
  
  
  
  # 保存Excel文件
  filepath <- file.path(output_directory, filename)
  openxlsx::saveWorkbook(wb, filepath, overwrite = TRUE)
  
  cat("分析结果已保存到Excel文件：", filename, "\n")
  
  return(filepath)
}


#' 生成Rmarkdown报告
#' 
#' @param promotion_clean 清理后的数据
#' @param yield_stats 产量统计
#' @param growth_stats 生育期统计
#' @param increase_stats 增产统计
#' @param high_yield_varieties 高产优质品种
#' @param early_mature_varieties 早熟高产品种
#' @param high_increase_varieties 显著增产品种
#' @param corr_matrix 相关性矩阵
#' @param top5_varieties 前5个品种名
#' @param output_directory 输出路径
#' @param filename 文件名，默认为"promotion_analysis_report.Rmd"
generate_rmarkdown_report <- function(promotion_clean, yield_stats, growth_stats, 
                                      increase_stats, high_yield_varieties,
                                      early_mature_varieties, high_increase_varieties,
                                      corr_matrix, top5_varieties, output_directory,
                                      filename = "promotion_analysis_report.Rmd") {
  # --------------------------
  # 1. 前期检查：避免关键数据缺失
  # --------------------------
  if (!requireNamespace("dplyr", quietly = TRUE) || !requireNamespace("knitr", quietly = TRUE)) {
    stop("请安装必要包：install.packages(c('dplyr', 'knitr'))")
  }
  
  # 检查 promotion_clean 关键列
  required_cols <- c("生育期_d", "亩产_kg", "较临近对照增产_pct", "地点", "株高_cm", "百粒重_g")
  missing_cols <- setdiff(required_cols, colnames(promotion_clean))
  if (length(missing_cols) > 0) {
    stop(paste("promotion_clean 缺少必要列：", paste(missing_cols, collapse = "、")))
  }
  
  # 检查空数据框
  if (nrow(promotion_clean) == 0) stop("promotion_clean 为空数据框，无法生成报告")
  
  # --------------------------
  # 2. 计算统计量（增加容错）
  # --------------------------
  numeric_cols <- intersect(required_cols, colnames(promotion_clean))
  
  avg_yield <- mean(promotion_clean$亩产_kg, na.rm = TRUE)
  high_yield_threshold <- if (nrow(high_yield_varieties) > 0) {
    quantile(promotion_clean$亩产_kg, 0.8, na.rm = TRUE)
  } else {
    0
  }
  
  # 早熟品种相关（处理列表/数据框两种格式）
  avg_yield_early <- avg_yield  # 默认值
  early_mature_df <- early_mature_varieties
  if (is.list(early_mature_varieties)) {
    if ("avg_yield" %in% names(early_mature_varieties)) {
      avg_yield_early <- early_mature_varieties$avg_yield
    }
    if ("varieties" %in% names(early_mature_varieties)) {
      early_mature_df <- early_mature_varieties$varieties
    }
  }
  # 确保 early_mature_df 是数据框
  if (!is.data.frame(early_mature_df)) early_mature_df <- data.frame()
  
  # 相关性系数（容错）
  corr_growth_yield <- if ("亩产_kg" %in% rownames(corr_matrix) && "生育期_d" %in% colnames(corr_matrix)) {
    round(corr_matrix["亩产_kg", "生育期_d"], 2)
  } else {
    "未计算"
  }
  
  # 顶部品种信息（容错）
  top1_variety <- if (length(top5_varieties) > 0) top5_varieties[1] else "无"
  top1_yield <- if (nrow(high_yield_varieties) > 0) {
    round(high_yield_varieties$亩产_kg[1], 2)
  } else {
    "0"
  }
  top1_increase <- if (nrow(high_yield_varieties) > 0) {
    round(high_yield_varieties$较临近对照增产_pct[1], 2)
  } else {
    "0"
  }
  
  # --------------------------
  # 3. 生成 Rmd 内容（容错格式）
  # --------------------------
  rmarkdown_code <- paste0('
---
title: "大豆品种产比试验数据分析报告"
author: "R语言自动生成"
date: "`r Sys.Date()`"
output: 
  html_document:
    toc: TRUE
    toc_float: TRUE
    theme: readable
    code_folding: hide
---

## 1. 分析概述

本报告基于`promotion`表数据，对大豆品种产比试验进行全面分析，包含', nrow(promotion_clean), '个大豆品种的产量、农艺性状和对照增产表现。

## 2. 数据基本信息

### 2.1 数据维度
- 有效品种数量：', nrow(promotion_clean), '个
- 分析指标数量：', length(numeric_cols), '个数值型指标
- 试验地点：', unique(promotion_clean$地点)[1], '

### 2.2 核心统计指标

#### 产量统计
`r if(nrow(yield_stats) > 0) kable(yield_stats, caption = "产量核心指标") %>% kable_styling() else "无产量统计数据"`

#### 生育期统计
`r if(nrow(growth_stats) > 0) kable(growth_stats, caption = "生育期统计指标") %>% kable_styling() else "无生育期统计数据"`

#### 增产统计
`r if(nrow(increase_stats) > 0) kable(increase_stats, caption = "较对照增产统计指标") %>% kable_styling() else "无增产统计数据"`

## 3. 数据可视化分析

### 3.1 基础分析图表
![基础分析图表](promotion_analysis_overview_R.png)

### 3.2 农艺性状与产量相关性分析
![性状分析图表](promotion_traits_analysis_R.png)

### 3.3 相关性矩阵
`r if(!is.null(corr_matrix) && nrow(corr_matrix) > 0) kable(corr_matrix, caption = "主要性状相关系数矩阵") %>% kable_styling() else "无相关性矩阵数据"`

### 3.4 核心品种雷达图
![核心品种雷达图](promotion_radar_chart_R.png)

## 4. 优良品种筛选结果

### 4.1 高产优质品种（TOP10）
筛选标准：亩产≥', round(high_yield_threshold, 1), 'kg 且 较对照增产>0%
`r if(nrow(high_yield_varieties) > 0) kable(head(high_yield_varieties, 10) %>% 
  dplyr::mutate(dplyr::across(c(亩产_kg, 较临近对照增产_pct, 百粒重_g), round, 2),
                dplyr::across(c(生育期_d, 株高_cm), round, 1)), 
  caption = "高产优质品种TOP10") %>% kable_styling() else "无符合条件的品种"`

### 4.2 早熟高产品种
筛选标准：生育期≤95天 且 亩产≥', round(avg_yield_early, 1), 'kg 且 较对照增产>0%
`r if(nrow(early_mature_df) > 0) kable(early_mature_df %>% 
  dplyr::mutate(dplyr::across(c(亩产_kg, 较临近对照增产_pct), round, 2),
                生育期_d = round(生育期_d, 0)), 
  caption = "早熟高产品种") %>% kable_styling() else "无符合条件的品种"`

### 4.3 显著增产品种（TOP10）
筛选标准：较对照增产≥20%
`r if(nrow(high_increase_varieties) > 0) kable(head(high_increase_varieties, 10) %>% 
  dplyr::mutate(dplyr::across(c(亩产_kg, 较临近对照增产_pct), round, 2),
                生育期_d = round(生育期_d, 0)), 
  caption = "显著增产品种TOP10") %>% kable_styling() else "无符合条件的品种"`

## 5. 主要结论与建议

### 5.1 主要结论
1. **产量表现**：参试品种平均亩产', round(avg_yield, 2), 'kg，产量差异显著（', 
                           round(min(promotion_clean$亩产_kg, na.rm = TRUE), 2), '-', 
                           round(max(promotion_clean$亩产_kg, na.rm = TRUE), 2), 'kg）
2. **增产情况**：仅', if(nrow(increase_stats) > 0) round(increase_stats$增产品种比例, 1) else "0", 
                           '%的品种较对照增产，整体表现有待提升
3. **性状影响**：生育期与产量呈正相关（r=', corr_growth_yield, '），100-105天品种产量最优
4. **品种储备**：筛选出', nrow(high_yield_varieties), '个高产优质品种，为宿州地区大豆生产提供良好基础

### 5.2 推广建议
1. **核心推广品种**：', top1_variety, '（亩产', top1_yield, 'kg，增产', top1_increase, '%）
2. **区域适配**：正常种植区选择100-105天品种，晚播区选择95天以内早熟品种
3. **风险防控**：优先选择株高70-90cm的抗倒伏品种

### 5.3 育种方向
1. 重点培育生育期100-105天、百粒重16-18g的品种
2. 加强矮秆抗倒伏性状选育
3. 提高品种的产量稳定性和抗逆性

## 6. 分析文件说明
- 原始数据文件：analysed_E-intelligence-N-4.2-常规-产比-宿州-20250626.xlsx
- R分析脚本：promotion_analysis_script.R
- 分析结果文件：promotion_analysis_results_R.xlsx
- 可视化图表：3个PNG格式图表文件
')
  
  # 确保输出路径存在
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE)
  }
  filepath <- file.path(output_directory, filename)  # 更安全的路径拼接
  writeLines(rmarkdown_code, filepath)
  cat("Rmarkdown报告已生成：", filepath, "\n")
  
  return(filepath)
}


#' 向量元素对比分析函数（修复版）
#'
#' 全面对比两个向量的元素（支持重复元素、不同长度），判断是否完全一致，
#' 找出各自独有元素、共同元素，统计数量差异，输出直观的对比结果。
#'
#' @param vec1 第一个向量（任意类型：数值、字符、因子等）
#' @param vec2 第二个向量（与vec1类型一致，否则会自动尝试转换）
#' @param ignore_order 逻辑值，是否忽略元素顺序（默认FALSE：严格按顺序对比；TRUE：仅对比元素集合和频次）
#' @param show_details 逻辑值，是否显示详细差异信息（默认TRUE）
#'
#' @return 列表（list），包含以下元素：
#'   \item{is_identical}{逻辑值，两个向量是否完全一致（考虑顺序/频次，取决于ignore_order）}
#'   \item{length_diff}{数值，vec1长度 - vec2长度（正数：vec1更长；负数：vec2更长；0：长度一致）}
#'   \item{common_elements}{数据框，共同元素及在两个向量中的频次}
#'   \item{vec1_unique}{数据框，vec1独有的元素及频次}
#'   \item{vec2_unique}{数据框，vec2独有的元素及频次}
#'   \item{total_unique_vec1}{数值，vec1独有元素的总个数（含重复）}
#'   \item{total_unique_vec2}{数值，vec2独有元素的总个数（含重复）}
#'   \item{vec1_summary}{数据框，vec1所有元素的频次统计}
#'   \item{vec2_summary}{数据框，vec2所有元素的频次统计}
#'
#' @details
#' 1. 当`ignore_order = FALSE`（默认）：严格对比元素的「值+顺序+频次」，完全一致才返回TRUE；
#' 2. 当`ignore_order = TRUE`：仅对比元素的「值+频次」，忽略顺序，元素集合和频次完全一致则返回TRUE；
#' 3. 支持重复元素：统计时会区分「元素类型独有」和「元素总个数独有」（例如vec1=c(1,1,2)，vec2=c(1,2)，则vec1独有1个1）；
#' 4. 自动处理向量类型转换：若vec1和vec2类型不同（如数值vs字符），会尝试将数值向量转换为字符向量后对比。
#'
#' @examples
#' # 示例1：用户场景对比
#' myvec <- c("G25ECA005", "G25ECA017", "G25ECA018", "G25ECA019", "G25ECA023", "G25ECA024", "G25ECA025", "G25ECA026", "G25ECA027",
#'            "G25ECA037", "G25ECA050", "G25ECA051", "G25ECA054", "G25ECA071", "G25ECA072", "G25ECA077", "G25ECA085", "G25ECA092",
#'            "G25ECA095", "G25ECA105", "G25ECA107", "G25ECA155", "G25ECA203", "G25ECA204", "G25ECA206", "G25ECA207", "G25ECA213",
#'            "G25ECA214", "G25ECA215", "G25ECA216", "G25ECA217", "G25ECA222", "G25ECA231", "G25ECA234", "G25ECA236", "G25ECA239",
#'            "G25ECA243", "G25ECA244", "G25ECA245", "G25ECA246", "G25ECA248", "G25ECA249", "G25ECA254", "G25ECA255", "G25ECA301",
#'            "G25ECA327", "G25ECA346", "G25ECA361", "G25ECA373", "G25ECA399")
#' # 假设select_variety$阶段名称已存在
#' # compare_vectors(myvec, select_variety$阶段名称)
#'
#' @export
compare_vectors <- function(vec1, vec2, ignore_order = FALSE, show_details = TRUE) {
  # -------------------------- 预处理：类型统一 + 空值处理 --------------------------
  # 处理空向量
  if (length(vec1) == 0 && length(vec2) == 0) {
    if (show_details) cat("✅ 两个向量均为空向量，完全一致！\n")
    return(list(
      is_identical = TRUE,
      length_diff = 0,
      common_elements = data.frame(元素 = character(0), vec1频次 = integer(0), vec2频次 = integer(0)),
      vec1_unique = data.frame(元素 = character(0), 频次 = integer(0)),
      vec2_unique = data.frame(元素 = character(0), 频次 = integer(0)),
      total_unique_vec1 = 0,
      total_unique_vec2 = 0,
      vec1_summary = data.frame(元素 = character(0), 频次 = integer(0)),
      vec2_summary = data.frame(元素 = character(0), 频次 = integer(0))
    ))
  }
  
  # 类型统一（避免数值vs字符对比错误）
  if (typeof(vec1) != typeof(vec2)) {
    warning("⚠️  两个向量类型不同，自动将数值型转换为字符型后对比！")
    vec1 <- as.character(vec1)
    vec2 <- as.character(vec2)
  }
  
  # -------------------------- 核心逻辑：统计元素频次 --------------------------
  # 统计每个向量的元素频次（含所有元素）
  count_vec1 <- as.data.frame(table(vec1, useNA = "ifany"), stringsAsFactors = FALSE)
  colnames(count_vec1) <- c("元素", "频次")
  count_vec2 <- as.data.frame(table(vec2, useNA = "ifany"), stringsAsFactors = FALSE)
  colnames(count_vec2) <- c("元素", "频次")
  
  # 合并频次表，计算共同元素、独有元素
  all_elements <- unique(c(count_vec1$元素, count_vec2$元素))
  freq_merge <- data.frame(
    元素 = all_elements,
    vec1频次 = sapply(all_elements, function(x) ifelse(x %in% count_vec1$元素, count_vec1$频次[count_vec1$元素 == x], 0)),
    vec2频次 = sapply(all_elements, function(x) ifelse(x %in% count_vec2$元素, count_vec2$频次[count_vec2$元素 == x], 0)),
    stringsAsFactors = FALSE
  )
  
  # 分类：共同元素、vec1独有、vec2独有
  common_df <- freq_merge[freq_merge$vec1频次 > 0 & freq_merge$vec2频次 > 0, ]
  vec1_unique_df <- freq_merge[freq_merge$vec1频次 > 0 & freq_merge$vec2频次 == 0, c("元素", "vec1频次")]
  vec2_unique_df <- freq_merge[freq_merge$vec2频次 > 0 & freq_merge$vec1频次 == 0, c("元素", "vec2频次")]
  colnames(vec1_unique_df)[2] <- "频次"
  colnames(vec2_unique_df)[2] <- "频次"
  
  # 计算总独有个数（含重复元素）
  total_unique_vec1 <- sum(vec1_unique_df$频次)
  total_unique_vec2 <- sum(vec2_unique_df$频次)
  
  # 判断是否完全一致
  if (ignore_order) {
    # 忽略顺序：对比元素频次是否完全一致
    is_identical <- identical(
      freq_merge[, c("元素", "vec1频次")],
      freq_merge[, c("元素", "vec2频次")]
    )
  } else {
    # 严格顺序：直接对比向量是否完全相同
    is_identical <- identical(vec1, vec2)
  }
  
  # 长度差异（vec1长度 - vec2长度）
  length_diff <- length(vec1) - length(vec2)
  
  # -------------------------- 输出：直观的文字说明（修复字符串重复语法） --------------------------
  if (show_details) {
    # 修复：用strrep()实现字符串重复（R 3.3.0+支持），兼容旧版用paste(rep())
    if (utils::packageVersion("base") >= "3.3.0") {
      cat(strrep("=", 60), "\n")
    } else {
      cat(paste(rep("=", 60), collapse = ""), "\n")
    }
    cat("📊 向量对比结果\n")
    if (utils::packageVersion("base") >= "3.3.0") {
      cat(strrep("=", 60), "\n")
    } else {
      cat(paste(rep("=", 60), collapse = ""), "\n")
    }
    
    # 一致性判断
    if (is_identical) {
      cat("✅ 两个向量", ifelse(ignore_order, "元素集合和频次完全一致（忽略顺序）", "完全一致（含顺序）"), "！\n")
    } else {
      cat("❌ 两个向量不一致！\n")
    }
    
    # 长度统计
    cat(sprintf("📏 长度对比：vec1（%d个元素） vs vec2（%d个元素）\n", length(vec1), length(vec2)))
    if (length_diff > 0) {
      cat(sprintf("   → vec1比vec2多%d个元素\n", length_diff))
    } else if (length_diff < 0) {
      cat(sprintf("   → vec2比vec1多%d个元素\n", abs(length_diff)))
    } else {
      cat("   → 两个向量长度一致\n")
    }
    
    # 共同元素
    cat(sprintf("\n🤝 共同元素（共%d种）：\n", nrow(common_df)))
    if (nrow(common_df) > 0) {
      print(common_df, row.names = FALSE)
    } else {
      cat("   → 无共同元素\n")
    }
    
    # 独有元素
    cat(sprintf("\n🔴 vec1独有的元素（共%d种，总个数：%d）：\n", nrow(vec1_unique_df), total_unique_vec1))
    if (nrow(vec1_unique_df) > 0) {
      print(vec1_unique_df, row.names = FALSE)
    } else {
      cat("   → 无独有元素\n")
    }
    
    cat(sprintf("\n🔵 vec2独有的元素（共%d种，总个数：%d）：\n", nrow(vec2_unique_df), total_unique_vec2))
    if (nrow(vec2_unique_df) > 0) {
      print(vec2_unique_df, row.names = FALSE)
    } else {
      cat("   → 无独有元素\n")
    }
    
    if (utils::packageVersion("base") >= "3.3.0") {
      cat(strrep("=", 60), "\n")
    } else {
      cat(paste(rep("=", 60), collapse = ""), "\n")
    }
  }
  
  # -------------------------- 返回结构化结果 --------------------------
  return(list(
    is_identical = is_identical,
    length_diff = length_diff,
    common_elements = common_df,
    vec1_unique = vec1_unique_df,
    vec2_unique = vec2_unique_df,
    total_unique_vec1 = total_unique_vec1,
    total_unique_vec2 = total_unique_vec2,
    vec1_summary = count_vec1,
    vec2_summary = count_vec2
  ))
}


#################################################################################
#核心评价函数：大豆品种综合描述性评价辅助函数加主函数
# ============================================================================
# 辅助函数1：关键词筛选函数（原代码依赖）
# ============================================================================
filter_by_keyword <- function(data, keyword, keep = TRUE) {
  stopifnot(is.data.frame(data), is.character(keyword), length(keyword) == 1, is.logical(keep))
  if (keep) {
    data |> dplyr::filter(grepl(keyword, 阶段名称, ignore.case = TRUE))
  } else {
    data |> dplyr::filter(!grepl(keyword, 阶段名称, ignore.case = TRUE))
  }
}

# ============================================================================
# 辅助函数2：Excel导出函数（原代码依赖）
# ============================================================================
generate_excel_report <- function(
    raw_data, select_variety, eliminated, select_plant,
    output_dir, filename
) {
  if (!require(writexl)) install.packages("writexl")
  library(writexl)
  
  # 构建Excel工作表列表
  excel_sheets <- list(
    "原始数据" = raw_data,
    "晋级材料" = select_variety,
    "淘汰材料" = eliminated,
    "高产分离选单株" = select_plant
  )
  
  # 导出Excel
  filepath <- file.path(output_dir, filename)
  write_xlsx(excel_sheets, path = filepath)
  return(invisible(filepath))
}

# ============================================================================
# 数据预处理函数：补全评价函数所需的缺失字段
# ============================================================================
preprocess_soybean_data <- function(data) {
  # 定义soybean_comprehensive_evaluation_final依赖的所有字段
  required_fields <- c(
    # 基本信息
    "阶段名称", "品种名称", "母本", "父本", "地点", "田间ID", "田间备注",
    # 特征特性
    "生育期_d", "结荚习性", "株型", "株高_cm", "主茎节数", "分枝数", "底荚高_cm", 
    "叶形", "花色", "茸毛色", "荚熟色", "有效荚", "粒形", "种皮色", 
    "种皮光泽", "脐色", "百粒重_g", "倒伏性", "抗病性",
    # 品质
    "蛋白质含量_pct", "脂肪含量_pct",
    # 产量
    "亩产_kg", "较临近对照增产_pct", "较平均对照增产_pct", 
    "较临近对照位次", "较平均对照位次"
  )
  
  # 检查并补全缺失字段（空字符串填充）
  missing_fields <- setdiff(required_fields, colnames(data))
  if (length(missing_fields) > 0) {
    message(sprintf("⚠️  检测到缺失字段：%s，自动补全为空值", paste(missing_fields, collapse = "、")))
    for (field in missing_fields) {
      data[[field]] <- ""
    }
  }
  
  # 核心字段非空防护（避免原函数报错）
  if (any(is.na(data[["阶段名称"]]) | data[["阶段名称"]] == "")) {
    warning("⚠️  核心字段「阶段名称」存在空值，填充默认值")
    data[["阶段名称"]][is.na(data[["阶段名称"]]) | data[["阶段名称"]] == ""] <- "未命名阶段"
  }
  if (any(is.na(data[["品种名称"]]) | data[["品种名称"]] == "")) {
    warning("⚠️  核心字段「品种名称」存在空值，填充默认值")
    data[["品种名称"]][is.na(data[["品种名称"]]) | data[["品种名称"]] == ""] <- "未命名品种"
  }
  
  return(data)
}

# ============================================================================
# 核心评价函数：大豆品种综合描述性评价
# ============================================================================
soybean_comprehensive_evaluation_final <- function(data) {
  # ==== 配置与分组 ====
  config <- list(
    info = c("阶段名称", "品种名称", "母本", "父本", "地点", "田间ID"),
    traits = c(
      "生育期_d", "结荚习性", "株型", "株高_cm", "主茎节数", "分枝数", "底荚高_cm", 
      "叶形", "花色", "茸毛色", "荚熟色", "有效荚", "粒形", "种皮色", 
      "种皮光泽", "脐色", "百粒重_g", "倒伏性", "抗病性"
    ),
    quality = c("蛋白质含量_pct", "脂肪含量_pct"),
    yield = c("亩产_kg", "较临近对照增产_pct", "较平均对照增产_pct", "较临近对照位次", "较平均对照位次"),
    base_cols = c(
      "阶段名称", "品种名称", "母本", "父本", "地点", "田间ID",
      "田间备注"
    )
  )
  
  # 工具函数
  is_missing <- function(value) {
    is.na(value) || (is.character(value) && value == "")
  }
  
  safe_get_value <- function(var, col, default = "") {
    if (col %in% colnames(var) && !is_missing(var[[col]])) as.character(var[[col]]) else default
  }
  
  extract_field_name <- function(field_name) sub("^(.+?)_.*$", "\\1", field_name)
  
  extract_qual <- function(value) {
    if (is_missing(value)) "" else {
      value_char <- as.character(value)
      if (grepl("^\\d+[-－](.+)$", value_char))
        sub("^\\d+[-－](.+)$", "\\1", value_char)
      else
        value_char
    }
  }
  
  maxConsecutiveSlash <- function(str) {
    if (is_missing(str)) return(0)
    str <- as.character(str)
    match <- gregexpr("/+", str)[[1]]
    if (match[1] == -1) 0 else max(attr(match, "match.length"))
  }
  
  # ---- 性状和品质自然语言描述函数 ----
  describe_traits_natural <- function(traits_list, trait_label_map = NULL) {
    if (length(traits_list) == 0) return("无相关描述信息。\n")
    descs <- c()
    for (tr in names(traits_list)) {
      label <- if (!is.null(trait_label_map)) trait_label_map[[tr]] else tr
      val <- traits_list[[tr]]
      if (tr %in% c("生育期_d")) {
        descs <- c(descs, paste0("生育期约为", val, "天"))
      } else if (tr %in% c("株高_cm")) {
        descs <- c(descs, paste0("株高", val, "厘米"))
      } else if (tr %in% c("底荚高_cm")) {
        descs <- c(descs, paste0("底荚高", val, "厘米"))
      } else if (tr %in% c("百粒重_g")) {
        descs <- c(descs, paste0("百粒重约", val, "克"))
      } else if (tr %in% c("主茎节数", "分枝数", "有效荚")) {
        descs <- c(descs, paste0(label, val, "个"))
      } else if (tr %in% c("花色")) {
        descs <- c(descs, paste0(val, "花"))
      }else if (tr %in% c("茸毛色")) {
        descs <- c(descs, paste0(val, "毛"))
      }else if (tr %in% c("种皮色")) {
        descs <- c(descs, paste0(val, "种皮"))
      }else if (tr %in% c("种皮光泽")) {
        descs <- c(descs, paste0(val, "光泽"))
      }else if (tr %in% c("脐色")) {
        descs <- c(descs, paste0(val, "脐"))
      }else if (tr %in% c("结荚习性")) {
        descs <- c(descs, paste0(val, "结荚习性"))
      }else if (tr %in% c("倒伏性")) {
        descs <- c(descs, paste0(val, "伏"))
      } else {
        descs <- c(descs, paste0(label, val))
      }
    }
    out <- paste0(paste(descs, collapse = "，"), "。\n")
    out
  }
  
  describe_quality_natural <- function(protein, fat) {
    if (protein == "" & fat == "") return("无相关描述信息。\n")
    line <- ""
    if (protein != "" & fat != "") {
      line <- paste0("蛋白质含量约为", protein, "%，脂肪含量约为", fat, "%。\n")
    } else if (protein != "") {
      line <- paste0("蛋白质含量约为", protein, "%。\n")
    } else if (fat != "") {
      line <- paste0("脂肪含量约为", fat, "%。\n")
    }
    line
  }
  
  # ==== 字段准备 ====
  all_defined_cols <- unique(unlist(config))
  all_defined_group_cols <- unique(c(config$info, config$traits, config$quality, config$yield, config$base_cols))
  all_cols <- colnames(data)
  existing_cols <- intersect(all_defined_group_cols, all_cols)
  classified_cols <- unique(c(config$info, config$traits, config$quality, config$yield, "田间备注"))
  other_cols <- setdiff(all_cols, classified_cols)
  
  required_core <- c("阶段名称", "品种名称")
  if (!all(required_core %in% all_cols)) {
    stop(
      paste(
        "数据必须包含以下核心基础字段：",
        paste(setdiff(required_core, all_cols), collapse = "、")
      )
    )
  }
  
  trait_cols <- setdiff(existing_cols, config$base_cols)
  if (length(trait_cols) == 0) {
    stop("数据中未识别到任何性状字段！至少需包含1个以上性状字段。")
  }
  
  # ==== 逐行迭代处理 ====
  missing_log_list <- list()
  
  process_one_variety <- function(var, idx) {
    # 一、基本信息
    stage_name <- safe_get_value(var, "阶段名称", paste0("未指定阶段_", idx))
    var_name   <- safe_get_value(var, "品种名称", paste0("无名品种_", idx))
    female_parent <- safe_get_value(var, "母本", "")
    male_parent   <- safe_get_value(var, "父本", "")
    location   <- safe_get_value(var, "地点", "")
    field_id   <- safe_get_value(var, "田间ID", "")
    
    # 缺失记录
    missing_fields <- all_cols[sapply(all_cols, function(col) is_missing(var[[col]]))]
    if (length(missing_fields) > 0) {
      missing_log_list[[idx]] <<- data.frame(
        阶段名称 = stage_name,
        品种名称 = var_name,
        缺失字段 = paste(missing_fields, collapse = "、"), stringsAsFactors = FALSE
      )
    }
    
    # === 分组抽取 ===
    traits_label_map <- list(
      "生育期_d"="生育期", "结荚习性"="结荚习性", "株型"="株型", "株高_cm"="株高", 
      "主茎节数"="主茎节数", "分枝数"="分枝数", "底荚高_cm"="底荚高", 
      "叶形"="叶形", "花色"="花色", "茸毛色"="茸毛色", "荚熟色"="荚熟色",
      "有效荚"="有效荚", "粒形"="粒形", "种皮色"="种皮色", 
      "种皮光泽"="种皮光泽", "脐色"="脐色", "百粒重_g"="百粒重", 
      "倒伏性"="倒伏性", "抗病性"="抗病性"
    )
    
    trait_values <- sapply(config$traits, function(tr) {
      val <- safe_get_value(var, tr)
      if(tr %in% c("倒伏性", "抗病性", "结荚习性", "株型", "叶形", "花色", "茸毛色", "荚熟色", 
                   "粒形", "种皮色", "种皮光泽", "脐色")) {
        val <- extract_qual(val)
      }
      if(!is_missing(val)) val else NULL
    }, USE.NAMES = TRUE, simplify = FALSE)
    
    trait_values <- trait_values[!sapply(trait_values, function(x) is.null(x) || x == "")]
    
    # 三、品质
    protein <- safe_get_value(var, "蛋白质含量_pct")
    fat <- safe_get_value(var, "脂肪含量_pct")
    
    # 四、产量
    yield <- safe_get_value(var, "亩产_kg")
    inc1  <- safe_get_value(var, "较临近对照增产_pct")
    inc2  <- safe_get_value(var, "较平均对照增产_pct")
    comp1 <- safe_get_value(var, "较临近对照位次")
    comp2 <- safe_get_value(var, "较平均对照位次")
    
    # 综合备注
    remark <- safe_get_value(var, "田间备注")
    
    # 五、其它（未归类字段）
    other_field_values <- list()
    if(length(other_cols) > 0){
      for(col in other_cols) {
        val <- safe_get_value(var, col)
        if(!is_missing(val)) {
          other_field_values[[col]] <- as.character(val)
        }
      }
    }
    
    # ==== 模板组装 ====
    res <- ""
    res <- paste0(res, stage_name,"\n")
    
    # 一、基本信息
    res <- paste0(res, "基本信息：")
    res <- paste0(res, "品种名称为", var_name, "，")
    if (female_parent!="" || male_parent!="") {
      if (female_parent!="" && male_parent!="") {
        m1 <- maxConsecutiveSlash(female_parent)
        m2 <- maxConsecutiveSlash(male_parent)
        connector <- paste0(rep("/", max(m1, m2) + 1), collapse = "")
        parents_val <- paste0(female_parent, connector, male_parent)
      } else if (female_parent!="") {
        parents_val <- female_parent
      } else {
        parents_val <- male_parent
      }
      res <- paste0(res, "亲本为", parents_val, "，")
    }
    if (location != "") {
      res <- paste0(res, "试验地点为", location, "，")
    }
    if (field_id != "") {
      res <- paste0(res, "田间编号为", field_id, "，")
    }
    res <- sub("，$", "。\n", res)
    
    # 二、特征特性
    res <- paste0(res, "特征特性：")
    res <- paste0(res, describe_traits_natural(trait_values, traits_label_map))
    
    # 三、品质
    res <- paste0(res, "品质：")
    res <- paste0(res, describe_quality_natural(protein, fat))
    
    # 四、产量
    res <- paste0(res, "产量：")
    yield_line <- ""
    if (yield != "") yield_line <- paste0(yield_line, "亩产", yield, "kg，")
    if (inc1 != "") yield_line <- paste0(yield_line, "较临近对照增产", inc1, "%，")
    if (inc2 != "") yield_line <- paste0(yield_line, "较平均对照增产", inc2, "%，")
    if (comp1 != "") yield_line <- paste0(yield_line, "较临近对照位次为", comp1,"，")
    if (comp2 != "") yield_line <- paste0(yield_line, "较平均对照位次为", comp2,"，")
    if (nchar(yield_line) > 0) {
      yield_line <- sub("，$", "。", yield_line)
      res <- paste0(res, yield_line, "\n")
    } else {
      res <- paste0(res, "无相关描述信息。\n")
    }
    
    # 五、其它
    res <- paste0(res, "其它：")
    if(length(other_field_values) > 0) {
      other_line <- ""
      for(col in names(other_field_values)){
        other_line <- paste0(other_line, col, "是", other_field_values[[col]], "，")
      }
      if(nchar(other_line) > 0){
        other_line <- sub("，$", "。", other_line)
        res <- paste0(res, other_line, "\n")
      } else {
        res <- paste0(res, "无相关描述信息。\n")
      }
    } else {
      res <- paste0(res, "无相关描述信息。\n")
    }
    
    # 综合备注
    if (remark != "") {
      res <- paste0(res, "备注：", remark, "\n")
    }
    res
  }
  
  # ==== 主流程 ====
  evaluation_result <- lapply(seq_len(nrow(data)), function(i) {
    process_one_variety(data[i, ], i)
  })
  
  # 缺失日志
  missing_log <- if (length(missing_log_list) > 0) {
    do.call(rbind, missing_log_list)
  } else {
    data.frame(阶段名称 = character(), 品种名称 = character(), 缺失字段 = character(), stringsAsFactors = FALSE)
  }
  
  # 字段名映射表
  field_mapping <- if (length(all_cols) > 0) {
    data.frame(
      原始字段名 = all_cols,
      标准字段名 = sapply(all_cols, extract_field_name),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(原始字段名 = character(), 标准字段名 = character(), stringsAsFactors = FALSE)
  }
  
  # ==== 报告输出 ====
  cat("=== 大豆品种综合描述性评价报告 ===\n")
  cat("注：本报告分为五类：基本信息，特征特性，品质，产量，其它（未归类字段）。\n")
  cat("----------------------------------------------------------------\n")
  cat("识别的有效字段（原始字段名）：", paste(all_cols, collapse = "、"), "\n")
  cat("----------------------------------------------------------------\n")
  cat(paste(evaluation_result, collapse = ""), "\n")
  # cat("=== 缺失字段日志 ===\n")
  # if (nrow(missing_log) == 0) {
  #   cat("所有品种的有效字段数据均完整，无缺失记录。\n")
  # } else {
  #   print(missing_log, row.names = FALSE)
  # }
  
  # 返回结构化结果（可选取消注释）
  # list(
  #   综合评价报告 = paste(evaluation_result, collapse = ""),
  #   缺失字段日志 = missing_log,
  #   识别的原始字段 = all_cols,
  #   字段名映射表 = field_mapping
  # )
}

# ============================================================================
# 主筛选函数：材料晋级筛选（整合字段补全逻辑）
# ============================================================================
#' 材料晋级筛选与高产分离选单株筛选函数
#'
#' 该函数用于根据指定的位次阈值筛选晋级材料、高产分离选单株材料，并识别淘汰材料，
#' 支持灵活适配位次字段存在情况（较临近对照位次/较平均对照位次），可自定义排除倒伏性等级，
#' 最终输出筛选结果Excel和晋级材料综合性状描述文本文件。
#'
#' @param promotion_clean data.frame 清洗后的材料试验数据，必须包含核心字段：阶段名称、倒伏性，
#'        至少包含一个位次字段（较临近对照位次/较平均对照位次）
#' @param PLACE character 试验地点名称（用于输出文件命名），非空字符串
#' @param output_directory character 输出文件保存目录路径，非空字符串，目录不存在时自动创建
#' @param keep_eliminated_vector vector 可选，指定不参与晋级筛选的阶段名称向量，默认NULL（无排除）
#' @param keep_not_select_vector vector 可选，指定不参与高产分离选单株筛选的阶段名称向量，默认NULL（无排除）
#' @param rank_threshold_select numeric 晋级材料筛选阈值，位次需<=该值，默认60，必须>0
#' @param rank_threshold_plant numeric 高产分离选单株筛选阈值，位次需<该值，默认60，必须>0
#' @param eliminate_lodging vector 可选，指定需要排除的倒伏性等级（如c("9-严重倒", "7-重倒")），
#'        设置为NULL时不排除任何倒伏性等级，默认c("9-严重倒", "7-重倒")
#'
#' @return list 返回包含以下元素的列表：
#' \item{select_variety}{data.frame 晋级材料数据（包含全部列）}
#' \item{select_plant}{data.frame 高产分离选单株材料数据（包含全部列）}
#' \item{eliminated}{data.frame 淘汰材料数据（包含全部列）}
#' \item{used_thresholds}{list 实际使用的筛选阈值，包含rank_threshold_select和rank_threshold_plant}
#' \item{used_eliminate_lodging}{vector 实际使用的倒伏性排除等级（NULL表示未排除）}
#' \item{used_rank_columns}{vector 实际参与筛选的位次字段名称（用于追溯）}
#'
#' @details
#' 1. 位次字段适配逻辑：
#'    - 若"较临近对照位次"和"较平均对照位次"都存在：使用两个字段共同筛选
#'    - 若仅存在其中一个：仅使用该字段筛选，并输出提示信息
#'    - 若两个都不存在：终止运行并提示缺少必需字段
#' 2. 倒伏性筛选逻辑：设置eliminate_lodging=NULL时，不筛选任何倒伏性等级
#' 3. 输出文件说明：
#'    - Excel文件：包含原始数据、晋级材料、淘汰材料、高产分离选单株材料四个sheet
#'    - 文本文件：晋级材料的综合性状描述（无晋级材料时跳过生成）
#'
#' @examples
#' \dontrun{
#' # 基础使用示例（默认阈值，默认排除严重倒伏）
#' result <- screen_material_promotion(
#'   promotion_clean = test_data,
#'   PLACE = "山东济南",
#'   output_directory = "./筛选结果"
#' )
#'
#' # 自定义阈值+不排除倒伏性+指定排除阶段
#' result <- screen_material_promotion(
#'   promotion_clean = test_data,
#'   PLACE = "河南郑州",
#'   output_directory = "./筛选结果",
#'   rank_threshold_select = 50,
#'   rank_threshold_plant = 40,
#'   eliminate_lodging = NULL,
#'   keep_eliminated_vector = c("预试-重复1", "预试-重复2")
#' )
#' }
#'
#' @importFrom dplyr filter
#' @importFrom utils file.path
#' @export
screen_material_promotion <- function(
    promotion_clean,
    PLACE,
    output_directory,
    keep_eliminated_vector = NULL,
    keep_not_select_vector = NULL,
    # 新增：拆分两个独立阈值，分别命名，保留默认值60（兼容原有使用习惯）
    rank_threshold_select = 60,    # 晋级材料筛选阈值（原rank_threshold）
    rank_threshold_plant = 60,     # 高产分离选单株筛选阈值（新增独立控制）
    eliminate_lodging = c("9-严重倒", "7-重倒")  # 支持设为NULL
) {
  # ============================================================================
  # 1. 输入检查与环境准备
  # ============================================================================
  cat("📌 开始材料晋级筛选流程...\n")
  
  # 核心参数合法性校验（更新：移除sel_cols校验，允许eliminate_lodging为NULL）
  stopifnot(
    is.data.frame(promotion_clean),
    is.character(PLACE) && nchar(PLACE) > 0,
    is.character(output_directory) && nchar(output_directory) > 0,
    is.vector(keep_eliminated_vector) | is.null(keep_eliminated_vector),
    is.vector(keep_not_select_vector) | is.null(keep_not_select_vector),
    # 检查两个阈值均为正数
    is.numeric(rank_threshold_select) && rank_threshold_select > 0,
    is.numeric(rank_threshold_plant) && rank_threshold_plant > 0,
    # 允许eliminate_lodging为NULL或字符向量
    (is.character(eliminate_lodging) | is.null(eliminate_lodging))
  )
  
  # 自动创建输出目录（若不存在）
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
    cat(sprintf("📂 已自动创建输出目录：%s\n", output_directory))
  }
  
  # 位次字段存在性检查与处理
  rank_cols <- c("较临近对照位次", "较平均对照位次")
  existing_rank_cols <- intersect(rank_cols, colnames(promotion_clean))
  
  # 检查位次字段存在情况并给出提示/报错
  if (length(existing_rank_cols) == 0) {
    stop("❌ 数据中缺少必需的位次筛选字段：'较临近对照位次' 和 '较平均对照位次' 均不存在！")
  } else if (length(existing_rank_cols) == 1) {
    cat(sprintf("⚠️  提示：数据中仅存在一个位次筛选字段：%s，将仅使用该字段进行筛选\n", existing_rank_cols))
  } else {
    cat("✅ 数据中存在完整的位次筛选字段：较临近对照位次、较平均对照位次，将使用两个字段进行筛选\n")
  }
  
  # 必要字段存在性检查（确保核心筛选字段不缺失）
  required_filter_cols <- c("阶段名称", "倒伏性")
  missing_filter_cols <- setdiff(required_filter_cols, colnames(promotion_clean))
  if (length(missing_filter_cols) > 0) {
    stop(sprintf("❌ 数据缺少筛选必需字段：%s", paste(missing_filter_cols, collapse = ", ")))
  }
  
  # 复制原始数据，避免修改输入对象（无需处理sel_cols，直接复制全部列）
  data_raw <- promotion_clean
  
  # ============================================================================
  # 2. 材料筛选（支持eliminate_lodging=NULL，不排除倒伏性，适配位次字段存在情况）
  # ============================================================================
  cat("\n🔍 执行材料筛选...\n")
  # 打印当前使用的阈值，方便用户确认
  cat(sprintf("   - 晋级材料筛选阈值：%s（位次<=该值）\n", rank_threshold_select))
  cat(sprintf("   - 高产分离选单株筛选阈值：%s（位次<该值）\n", rank_threshold_plant))
  
  # 倒伏性筛选提示
  if (is.null(eliminate_lodging)) {
    cat("   - 倒伏性筛选：不排除任何倒伏性等级\n")
  } else {
    cat(sprintf("   - 倒伏性筛选：排除等级 = %s\n", paste(eliminate_lodging, collapse = ", ")))
  }
  
  ## 2.1 晋级材料筛选（适配位次字段存在情况 + 支持倒伏性不筛选）
  cat("   - 筛选晋级材料...\n")
  select_variety <- filter_by_keyword(data_raw, "分离", keep = FALSE)
  
  # 根据存在的位次字段进行筛选
  if ("较临近对照位次" %in% existing_rank_cols) {
    select_variety <- dplyr::filter(select_variety, 较临近对照位次 <= rank_threshold_select)
  }
  if ("较平均对照位次" %in% existing_rank_cols) {
    select_variety <- dplyr::filter(select_variety, 较平均对照位次 <= rank_threshold_select)
  }
  
  # 后续筛选逻辑
  select_variety <- select_variety |>
    # eliminate_lodging为NULL时，不筛选倒伏性（条件恒为TRUE）
    dplyr::filter(if (is.null(eliminate_lodging)) TRUE else !倒伏性 %in% eliminate_lodging) |>
    dplyr::filter(is.null(keep_eliminated_vector) | !阶段名称 %in% keep_eliminated_vector)
  
  ## 2.2 高产分离材料选单株筛选（适配位次字段存在情况 + 支持倒伏性不筛选）
  cat("   - 筛选高产分离选单株材料...\n")
  select_plant <- filter_by_keyword(data_raw, "分离", keep = TRUE)
  
  # 根据存在的位次字段进行筛选
  if ("较临近对照位次" %in% existing_rank_cols) {
    select_plant <- dplyr::filter(select_plant, 较临近对照位次 < rank_threshold_plant)
  }
  if ("较平均对照位次" %in% existing_rank_cols) {
    select_plant <- dplyr::filter(select_plant, 较平均对照位次 < rank_threshold_plant)
  }
  
  # 后续筛选逻辑
  select_plant <- select_plant |>
    # eliminate_lodging为NULL时，不筛选倒伏性（条件恒为TRUE）
    dplyr::filter(if (is.null(eliminate_lodging)) TRUE else !倒伏性 %in% eliminate_lodging) |>
    dplyr::filter(is.null(keep_not_select_vector) | !阶段名称 %in% keep_not_select_vector)
  
  ## 2.3 淘汰材料识别（阶段名称不在晋级和分离材料中）
  cat("   - 识别淘汰材料...\n")
  eliminated <- data_raw |>
    dplyr::filter(!阶段名称 %in% select_variety$阶段名称 & !阶段名称 %in% select_plant$阶段名称)
  
  # 筛选结果汇总提示
  cat(sprintf(
    "✅ 筛选完成：\n   - 晋级材料：%s份（输出全部列）\n   - 高产分离选单株：%s份（输出全部列）\n   - 淘汰材料：%s份（输出全部列）\n",
    nrow(select_variety), nrow(select_plant), nrow(eliminated)
  ))
  
  # ============================================================================
  # 3. 晋级材料描述性评述（完全复刻原始逻辑）
  # ============================================================================
  cat("\n📝 生成晋级材料综合性状描述...\n")
  if (nrow(select_variety) > 0) {
    txt_filename <- paste0(PLACE, "_材料综合性状描述.txt")
    txt_filepath <- file.path(output_directory, txt_filename)
    
    # 捕获可能的报错，确保sink正常关闭
    tryCatch({
      sink(txt_filepath, append = FALSE, split = TRUE)
      soybean_comprehensive_evaluation_final(select_variety)
      sink()
      cat(sprintf("✅ 已保存描述性评述：%s\n", txt_filepath))
    }, error = function(e) {
      sink()  # 强制关闭sink，避免资源泄露
      stop(sprintf("❌ 生成描述性评述失败：%s", e$message))
    })
  } else {
    warning("⚠️  未筛选出晋级材料，跳过描述性评述生成")
  }
  
  # ============================================================================
  # 4. 结果保存（输出全部列，无需筛选字段）
  # ============================================================================
  cat("\n💾 导出筛选结果（全部列）...\n")
  
  ## 4.2 导出Excel报告（位置传参，仅filename指定参数名）
  excel_filename <- paste0(PLACE, "_晋级_淘汰_分离选单.xlsx")
  generate_excel_report(
    data_raw,  # 第1个参数：原始数据（全部列）
    select_variety,  # 第2个参数：晋级材料（全部列）
    eliminated,  # 第3个参数：淘汰材料（全部列）
    select_plant,  # 第4个参数：分离选单株材料（全部列）
    output_directory,  # 第5个参数：输出目录
    filename = excel_filename  # 第6个参数：文件名
  )
  cat(sprintf("✅ 已保存Excel报告（含全部列）：%s\n", file.path(output_directory, excel_filename)))
  
  # ============================================================================
  # 5. 流程结束与返回结果
  # ============================================================================
  cat("\n=== 数据分析流程完成！ ===\n")
  return(list(
    select_variety = select_variety,  # 返回全部列
    select_plant = select_plant,      # 返回全部列
    eliminated = eliminated,          # 返回全部列
    # 返回使用的阈值参数，方便后续追溯
    used_thresholds = list(
      rank_threshold_select = rank_threshold_select,
      rank_threshold_plant = rank_threshold_plant
    ),
    # 返回倒伏性筛选参数，方便追溯
    used_eliminate_lodging = eliminate_lodging,
    # 返回实际使用的位次字段，方便追溯
    used_rank_columns = existing_rank_cols
  ))
}



#' @title 按关键词筛选数据框中的行
#' @description
#' 在数据框的所有字符型列中，检索是否含有指定关键词，根据匹配情况保留或剔除相关行。
#' 支持输入可转换为数据框的向量和矩阵。
#'
#' @param data 数据框或可自动转换为数据框的对象（如向量、矩阵等）。
#' @param keyword 字符串型，需搜索的关键词，默认“分离”。
#' @param keep 逻辑值，是否保留含有关键词的行。为TRUE时保留，FALSE时剔除。默认为TRUE。
#'
#' @return 一个数据框，包含（或剔除）含有指定关键词的行。
#'
#' @examples
#' df <- data.frame(a = c("abc分离", "xyz", "无关"), b = c("无", "分离物", "测试"))
#' filter_by_keyword(df, keyword="分离", keep=TRUE)
#' filter_by_keyword(df, keyword="分离", keep=FALSE)
#'
#' @export
filter_by_keyword <- function(data, keyword = "分离", keep = TRUE) {
  # 输入校验
  if (!is.data.frame(data)) {
    data <- try(as.data.frame(data), silent = TRUE)
    if (inherits(data, "try-error")) {
      stop("输入的data必须是数据框，或可转换为数据框的对象（如向量、矩阵）")
    }
  }
  if (ncol(data) == 0) {
    warning("输入数据框没有列，返回空数据框")
    return(data)
  }
  n_rows <- nrow(data)
  if (n_rows == 0) {
    warning("输入数据框没有行，返回空数据框")
    return(data)
  }
  # 每列关键词匹配
  col_logic_matrix <- vapply(seq_along(data), function(i) {
    col <- data[[i]]
    if (!is.character(col)) {
      return(rep(FALSE, n_rows))
    }
    if (length(col) != n_rows) {
      warning("某列长度与数据框行数不匹配，已强制调整")
      col <- if (length(col) > n_rows) col[1:n_rows] else c(col, rep(NA, n_rows - length(col)))
    }
    grepl(keyword, col, fixed = TRUE, useBytes = TRUE) & !is.na(col)
  }, logical(n_rows))
  
  if (!is.matrix(col_logic_matrix)) {
    # 仅一个有效列时vapply返回向量而不是矩阵，需转为矩阵
    col_logic_matrix <- matrix(col_logic_matrix, ncol=1)
  }
  
  # 若所有列无字符匹配则为0列
  if (ncol(col_logic_matrix) == 0) {
    if (keep) {
      warning("没有有效列可匹配关键词'(", keyword, ")'，返回空数据框")
      return(data[FALSE, , drop = FALSE])
    } else {
      warning("没有有效列可匹配关键词'(", keyword, ")'，返回原始数据框")
      return(data)
    }
  }
  # 检查所有逻辑列都没有任何TRUE（全为FALSE）
  if (all(!as.logical(col_logic_matrix))) {
    if (keep) {
      warning("没有任何列包含关键词'(", keyword, ")'，返回空数据框")
      return(data[FALSE, , drop = FALSE])
    } else {
      warning("没有任何列包含关键词'(", keyword, ")'，返回原始数据框")
      return(data)
    }
  }
  # 正常筛选
  if (keep) {
    row_filter <- apply(col_logic_matrix, 1, any)
  } else {
    row_filter <- apply(col_logic_matrix, 1, function(x) all(!x))
  }
  data[row_filter, , drop = FALSE]
}


#' 可清晰展示指标对比的小提琴箱线图（高质量美观、适合发表、报告、常规查看）
#'
#' @param data_before 筛选前数据框
#' @param data_after  筛选后数据框
#' @param output_directory 输出目录
#' @param plot_name   图文件名（无需后缀）
#' @param indicators  对比的列名
#' @param indicator_labels 显示名，需与 indicators 顺序一致
#' @param title      主标题
#' @param subtitle   副标题
#' @param title_size 主标题字号（默认22）
#' @param subtitle_size 副标题字号（默认18）
#' @param facet_title_size 子图标题字号（默认16）
#' @param axis_x_size X轴字体大小（默认15）
#' @param axis_y_size Y轴字体大小（默认15）
#' @param legend_text_size 图例字体（默认15）
#' @param mean_label_size 均值标签字号（默认7）
#' @param mean_point_size 均值点大小（默认6）
#' @param plot_width  图宽（默认10英寸，适合常规显示/报告）
#' @param plot_height 图高（默认7）
#' @return ggplot对象并保存PNG图
plot_selection_comparison <- function(
    data_before,
    data_after,
    output_directory,
    plot_name,
    indicators = c("亩产_kg", "生育期_d", "株高_cm", "百粒重_g"),
    indicator_labels = c("亩产 (kg)", "生育期 (天)", "株高 (cm)", "百粒重 (g)"),
    title = "品种筛选前后核心农艺性状对比（含平均值）",
    subtitle = "选前 vs 选后",
    title_size = 24,         # 字再高一些
    subtitle_size = 20,      # 字再高一些
    facet_title_size = 18,   # 子图标题字号更大点
    axis_x_size = 17,        # 坐标字再高一些
    axis_y_size = 17,        # 坐标字再高一些
    legend_text_size = 17,   # 图例字再高一些
    mean_label_size = 10,    # 均值标签字号再高一些
    mean_point_size = 3,     # 均值点小一些
    plot_width = 10,         # 图宽
    plot_height = 7          # 图高
) {
  # 加载依赖
  pkgs <- c("dplyr", "ggplot2", "tidyr", "ggthemes")
  need <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
  if (length(need) > 0) {
    stop(sprintf("请先安装必需包: %s\ninstall.packages(c('%s'))",
                 paste(need, collapse = ", "), paste(need, collapse = "','")))
  }
  lapply(pkgs, library, character.only = TRUE)
  
  # 校验参数
  stopifnot(is.data.frame(data_before), is.data.frame(data_after))
  miss_b <- setdiff(indicators, colnames(data_before))
  miss_a <- setdiff(indicators, colnames(data_after))
  if (length(miss_b) > 0)
    stop(paste("筛选前数据缺少列:", paste(miss_b, collapse = ",")))
  if (length(miss_a) > 0)
    stop(paste("筛选后数据缺少列:", paste(miss_a, collapse = ",")))
  if (length(indicator_labels) != length(indicators))
    stop("指标标签与指标列长度不一致")
  size_params <- c(title_size, subtitle_size, facet_title_size, axis_x_size,
                   axis_y_size, legend_text_size, mean_label_size, mean_point_size, plot_width)
  if (any(size_params <= 0)) stop("所有大小参数必须为正数")
  if (is.null(plot_height)) plot_height <- max(6, ceiling(length(indicators)/2)*3.8)
  
  # 合并&拉长数据
  dplyr::bind_rows(
    dplyr::select(data_before, dplyr::all_of(indicators)) %>% dplyr::mutate(筛选状态 = "选前"),
    dplyr::select(data_after,  dplyr::all_of(indicators)) %>% dplyr::mutate(筛选状态 = "选后")
  ) %>%
    tidyr::drop_na() -> comparison_data
  
  if (nrow(comparison_data) == 0) stop("合并后无有效数据（可能全缺失）")
  
  comparison_data %>%
    tidyr::pivot_longer(
      cols = dplyr::all_of(indicators),
      names_to = "指标列名",
      values_to = "指标数值"
    ) %>%
    dplyr::mutate(指标名称 = factor(指标列名, levels = indicators, labels = indicator_labels)
    ) -> long_data
  
  # 只计算平均值
  summary_data <- long_data %>%
    dplyr::group_by(筛选状态, 指标名称) %>%
    dplyr::summarise(
      平均数值 = mean(指标数值, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 配色
  fill_colors <- c("选前" = "#1b9e77", "选后" = "#d95f02")
  violin_alpha <- 0.25
  
  # 深红色用于均值标签字体
  mean_label_color <- "#B22222"
  
  # 绘图
  p <- ggplot2::ggplot(
    long_data, ggplot2::aes(x = 筛选状态, y = 指标数值, fill = 筛选状态)
  ) +
    ggplot2::geom_violin(
      alpha = violin_alpha, trim = FALSE, color = NA
    ) +
    ggplot2::geom_boxplot(
      alpha = 0.45, width = 0.18,
      outlier.shape = 21, outlier.size = 2, color = "#444", position = position_dodge(1),
      show.legend = FALSE, fatten = 1.2
    ) +
    ggplot2::geom_point(
      data = summary_data, aes(y = 平均数值),
      shape = 21, fill = "#FFD166", color = "#B22222",
      size = mean_point_size, stroke = 0.8, inherit.aes = TRUE
    ) +
    # 只绘制均值标签，字号再高一些，深红色字
    ggplot2::geom_text(
      data = summary_data,
      aes(y = 平均数值, label = paste0("均值: ", round(平均数值, 1))),
      color = mean_label_color, size = mean_label_size, vjust = -1, fontface = "bold",
      check_overlap = TRUE, show.legend = FALSE
    ) +
    ggplot2::facet_wrap(~指标名称, ncol = 2, scales = "free_y") +
    ggplot2::scale_fill_manual(values = fill_colors) +
    # 使用classic主题+自定义，以便更美观发表
    ggplot2::theme_classic(base_family = ifelse(Sys.info()["sysname"] == "Windows", "SimHei", "Arial")) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = title_size, face = "bold"), # 恢复为默认黑色
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = subtitle_size), # 恢复为默认黑色
      strip.text = ggplot2::element_text(size = facet_title_size, face = "bold"), # 恢复为默认黑色
      axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = axis_x_size), # 恢复为默认黑色
      axis.text.y = ggplot2::element_text(size = axis_y_size), # 恢复为默认黑色
      axis.title.y = ggplot2::element_text(size = axis_y_size + 5, face = "bold"), # 恢复为默认黑色
      legend.position = "top",
      legend.text = ggplot2::element_text(size = legend_text_size), # 恢复为默认黑色
      legend.title = ggplot2::element_text(size = legend_text_size + 2), # 恢复为默认黑色
      panel.spacing = ggplot2::unit(1.1, "lines"),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA),
      strip.background = ggplot2::element_rect(fill = "white", color = "grey85", linewidth = 0.5),
      legend.key = ggplot2::element_rect(fill = "white", color = NA)
    ) +
    ggplot2::labs(
      title = title, subtitle = subtitle,
      x = "", y = "指标数值",
      fill = "筛选状态"
    ) +
    ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
  
  # 输出PNG
  if (!dir.exists(output_directory)) {
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
    message("已创建输出目录：", output_directory)
  }
  plot_path <- file.path(output_directory, paste0(plot_name, ".png"))
  ggplot2::ggsave(
    filename = plot_path, plot = p,
    width = plot_width, height = plot_height, dpi = 300, device = "png", bg = "white"
  )
  message("图表已保存至：", plot_path)
  invisible(p)
}


#增加对照材料的比较
plot_selection_comparison_addck <- function(
    data_before,
    data_after,
    output_directory,
    plot_name,
    indicators = c("亩产_kg", "生育期_d", "株高_cm", "百粒重_g"),
    indicator_labels = c("亩产 (kg)", "生育期 (天)", "株高 (cm)", "百粒重 (g)"),
    title = "对照、品种筛选前后核心农艺性状对比（含平均值）",
    subtitle = "对照 vs 选前 vs 选后",
    title_size = 24,
    subtitle_size = 20,
    facet_title_size = 18,
    axis_x_size = 17,
    axis_y_size = 17,
    legend_text_size = 17,
    mean_label_size = 10,
    mean_point_size = 3,
    plot_width = 12,
    plot_height = 8
) {
  # 加载依赖（增加错误捕获）
  tryCatch({
    pkgs <- c("dplyr", "ggplot2", "tidyr", "ggthemes")
    need <- pkgs[!sapply(pkgs, requireNamespace, quietly = TRUE)]
    if (length(need) > 0) {
      stop(sprintf("请先安装必需包: %s\ninstall.packages(c('%s'))",
                   paste(need, collapse = ", "), paste(need, collapse = "','")))
    }
    lapply(pkgs, library, character.only = TRUE)
    message("✅ 依赖包加载成功")
  }, error = function(e) {
    warning("⚠️ 依赖包加载失败：", e$message)
    return(invisible(NULL))
  })
  
  # 全局错误捕获：包裹核心执行逻辑
  result <- tryCatch({
    if (!is.data.frame(data_before) || nrow(data_before) == 0) {
      stop("data_before 必须是有效数据框（非空）")
    }
    if (!is.data.frame(data_after) || nrow(data_after) == 0) {
      stop("data_after 必须是有效数据框（非空）")
    }
    if (!is.character(output_directory) || output_directory == "") {
      stop("output_directory 必须是有效的目录路径字符串")
    }
    if (!is.character(plot_name) || plot_name == "") {
      stop("plot_name 必须是有效的图表名称字符串")
    }
    
    # ========== 核心修改：自动补充缺失列 ==========
    # 1. 检查并补充 data_before 的「是否对照」列
    if (!"是否对照" %in% colnames(data_before)) {
      message("⚠️ data_before 缺少「是否对照」列，自动补充（默认全部为0，需确认数据逻辑）")
      data_before <- data_before %>% dplyr::mutate(是否对照 = 0L)
    }
    
    # 2. 检查「是否对照」列的值范围，补充后仍校验
    if (!all(unique(data_before$是否对照) %in% c(0, 1))) {
      stop("「是否对照」字段只能包含0（选前）和1（对照）两个值，当前值：", 
           paste(unique(data_before$是否对照), collapse = ","))
    }
    if (sum(data_before$是否对照 == 1, na.rm = TRUE) == 0) {
      message("⚠️ data_before 中「是否对照=1」（对照组）无数据，若需补充请检查原始数据")
      # 可选：自动补充一行对照数据（根据选前数据均值）
      # 这里仅提示，不自动生成对照数据（避免数据失真）
    }
    if (sum(data_before$是否对照 == 0, na.rm = TRUE) == 0) {
      stop("data_before 中「是否对照=0」（选前组）无数据")
    }
    
    # 3. 检查并补充指标列（缺失列填充NA，并提示）
    # 处理 data_before 缺失的指标列
    miss_b <- setdiff(indicators, colnames(data_before))
    if (length(miss_b) > 0) {
      message("⚠️ 筛选前数据缺少指标列：", paste(miss_b, collapse = ","), "，自动补充（值为NA）")
      for (col in miss_b) {
        data_before[[col]] <- NA_real_
      }
    }
    
    # 处理 data_after 缺失的指标列
    miss_a <- setdiff(indicators, colnames(data_after))
    if (length(miss_a) > 0) {
      message("⚠️ 筛选后数据缺少指标列：", paste(miss_a, collapse = ","), "，自动补充（值为NA）")
      for (col in miss_a) {
        data_after[[col]] <- NA_real_
      }
    }
    
    if (length(indicator_labels) != length(indicators)) {
      stop("指标标签与指标列长度不一致（标签数：", length(indicator_labels), 
           "，指标数：", length(indicators), "）")
    }
    
    size_params <- list(
      title_size = title_size, subtitle_size = subtitle_size,
      facet_title_size = facet_title_size, axis_x_size = axis_x_size,
      axis_y_size = axis_y_size, legend_text_size = legend_text_size,
      mean_label_size = mean_label_size, mean_point_size = mean_point_size,
      plot_width = plot_width, plot_height = plot_height
    )
    for (param_name in names(size_params)) {
      param_val <- size_params[[param_name]]
      if (!is.numeric(param_val) || param_val <= 0 || param_val > 100) {
        stop(param_name, " 必须是0-100之间的正数（当前值：", param_val, "）")
      }
    }
    if (is.null(plot_height)) {
      plot_height <- max(7, ceiling(length(indicators)/2)*4)
    }
    
    message("✅ 参数校验通过，开始数据处理")
    
    comparison_data <- tryCatch({
      dplyr::bind_rows(
        dplyr::filter(data_before, 是否对照 == 1) %>% 
          dplyr::select(dplyr::all_of(indicators)) %>% 
          dplyr::mutate(筛选状态 = "对照"),
        dplyr::filter(data_before, 是否对照 == 0) %>% 
          dplyr::select(dplyr::all_of(indicators)) %>% 
          dplyr::mutate(筛选状态 = "选前"),
        dplyr::select(data_after, dplyr::all_of(indicators)) %>% 
          dplyr::mutate(筛选状态 = "选后")
      ) %>%
        tidyr::drop_na()
    }, error = function(e) {
      stop("数据合并失败：", e$message)
    })
    
    if (nrow(comparison_data) == 0) {
      stop("合并后无有效数据（可能所有数据都包含缺失值，或三组数据无重叠指标）")
    }
    group_counts <- table(comparison_data$筛选状态)
    missing_groups <- setdiff(c("对照", "选前", "选后"), names(group_counts))
    if (length(missing_groups) > 0) {
      stop("以下分组无有效数据：", paste(missing_groups, collapse = ","))
    }
    message("✅ 数据合并完成，有效数据量：", nrow(comparison_data), 
            "行（对照：", group_counts["对照"], "，选前：", group_counts["选前"], 
            "，选后：", group_counts["选后"], "）")
    
    long_data <- tryCatch({
      comparison_data %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(indicators),
          names_to = "指标列名",
          values_to = "指标数值"
        ) %>%
        dplyr::mutate(
          指标名称 = factor(指标列名, levels = indicators, labels = indicator_labels)
        )
    }, error = function(e) {
      stop("数据格式转换失败（长格式转换）：", e$message)
    })
    
    summary_data <- tryCatch({
      long_data %>%
        dplyr::group_by(筛选状态, 指标名称) %>%
        dplyr::summarise(
          平均数值 = mean(指标数值, na.rm = TRUE),
          .groups = "drop"
        )
    }, error = function(e) {
      stop("均值计算失败：", e$message)
    })
    
    message("✅ 数据预处理完成，开始绘图")
    
    fill_colors <- c("对照" = "#2c7fb8", "选前" = "#1b9e77", "选后" = "#d95f02")
    violin_alpha <- 0.25
    mean_label_color <- "#B22222"
    
    p <- tryCatch({
      ggplot2::ggplot(
        long_data, ggplot2::aes(x = 筛选状态, y = 指标数值, fill = 筛选状态)
      ) +
        ggplot2::geom_violin(alpha = violin_alpha, trim = FALSE, color = NA) +
        ggplot2::geom_boxplot(
          alpha = 0.45, width = 0.18,
          outlier.shape = 21, outlier.size = 2, color = "#444", 
          position = position_dodge(1), show.legend = FALSE, fatten = 1.2
        ) +
        ggplot2::geom_point(
          data = summary_data, aes(y = 平均数值),
          shape = 21, fill = "#FFD166", color = "#B22222",
          size = mean_point_size, stroke = 0.8, inherit.aes = TRUE
        ) +
        ggplot2::geom_text(
          data = summary_data,
          aes(y = 平均数值, label = paste0("均值: ", round(平均数值, 1))),
          color = mean_label_color, size = mean_label_size, 
          vjust = -1, fontface = "bold", check_overlap = TRUE, show.legend = FALSE
        ) +
        ggplot2::facet_wrap(~指标名称, ncol = 2, scales = "free_y") +
        ggplot2::scale_fill_manual(values = fill_colors) +
        ggplot2::theme_classic(
          base_family = ifelse(Sys.info()["sysname"] == "Windows", "SimHei", "Arial")
        ) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, size = title_size, face = "bold"),
          plot.subtitle = ggplot2::element_text(hjust = 0.5, size = subtitle_size),
          strip.text = ggplot2::element_text(size = facet_title_size, face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = axis_x_size),
          axis.text.y = ggplot2::element_text(size = axis_y_size),
          axis.title.y = ggplot2::element_text(size = axis_y_size + 5, face = "bold"),
          legend.position = "top",
          legend.text = ggplot2::element_text(size = legend_text_size),
          legend.title = ggplot2::element_text(size = legend_text_size + 2),
          panel.spacing = ggplot2::unit(1.1, "lines"),
          panel.background = ggplot2::element_rect(fill = "white", color = NA),
          plot.background = ggplot2::element_rect(fill = "white", color = NA),
          strip.background = ggplot2::element_rect(fill = "white", color = "grey85", linewidth = 0.5),
          legend.key = ggplot2::element_rect(fill = "white", color = NA)
        ) +
        ggplot2::labs(
          title = title, subtitle = subtitle,
          x = "", y = "指标数值",
          fill = "分组"
        ) +
        ggplot2::guides(fill = ggplot2::guide_legend(title = NULL))
    }, error = function(e) {
      stop("绘图失败：", e$message)
    })
    
    # 保存图表（增强目录处理）
    tryCatch({
      if (!dir.exists(output_directory)) {
        dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
        message("📂 已创建输出目录：", output_directory)
      }
      # 安全文件名处理：仅替换非法字符，保留中文
      safe_plot_name <- plot_name
      safe_plot_name <- gsub("[\\\\/:*?\"<>|]", "_", safe_plot_name)
      safe_plot_name <- trimws(safe_plot_name)
      if (nchar(safe_plot_name) == 0) safe_plot_name <- "plot"
      plot_path <- file.path(output_directory, paste0(safe_plot_name, ".png"))
      
      ggplot2::ggsave(
        filename = plot_path, plot = p,
        width = plot_width, height = plot_height, dpi = 300, device = "png", bg = "white"
      )
      message("🎉 图表已成功保存至：", plot_path)
    }, error = function(e) {
      stop("图表保存失败：", e$message)
    })
    
    invisible(p)
    
  }, error = function(e) {
    warning("\n❌ 函数执行失败：", e$message, "\n")
    tryCatch({
      if (exists("plot_path") && file.exists(plot_path)) {
        file.remove(plot_path)
        message("🗑️ 已清理无效图表文件")
      }
    }, silent = TRUE)
    return(invisible(NULL))
  }, warning = function(w) {
    message("\n⚠️ 函数执行警告：", w$message, "\n")
  }, finally = {
    message("🔚 图表生成流程结束\n")
  })
  
  return(result)
}


#' 作物推广数据统计分析与可视化一体化函数
#'
#' 对推广数据进行产量、生育期、性状相关性等统计分析，并生成标准化可视化图表（保存至指定目录）
#'
#' @param promotion_clean 数据框，预处理后的推广数据（需包含：亩产_kg、生育期_d、株高_cm、百粒重_g、
#'                        花色、叶形、结荚习性、倒伏性、茸毛色、脐色等字段）
#' @param output_directory 字符型，图表输出目录路径（若目录不存在会自动创建）
#' @param PLACE 字符型，区域/地点标识（用于图表文件名前缀）
#' @param plot_width 数值型，图表宽度（默认：1500）
#' @param plot_height 数值型，图表高度（默认：1000）
#' @param scatter_height 数值型，散点图专用高度（默认：500，适配3幅散点图布局）
#'
#' @return 列表，包含所有统计分析结果和处理后的数据：
#'         \item{yield_stats}{产量统计结果（来自calculate_yield_stats）}
#'         \item{ck_mean}{对照均值（来自calc_control_means_wide）}
#'         \item{growth_stats}{生育期统计结果（来自calculate_growth_stats）}
#'         \item{increase_stats}{增产统计结果（来自calculate_increase_stats）}
#'         \item{corr_matrix}{性状相关性矩阵（来自calculate_correlation_matrix）}
#'         \item{processed_data}{经分级/增产标记处理后的数据框}
#'
#' @details 依赖函数说明（需提前加载/定义）：
#'          1. 统计函数：calculate_yield_stats、calc_control_means_wide、calculate_growth_stats、
#'             calculate_increase_stats、calculate_correlation_matrix
#'          2. 可视化函数：setup_plot_theme、plot_yield_distribution、plot_yield_grade_distribution、
#'             plot_increase_distribution、plot_growth_distribution、plot_trait_yield_scatter、
#'             plot_single_trait_distribution、save_basic_analysis_plots、save_correlation_plot
#'             - 注意：save_basic_analysis_plots参数顺序为：plot_list, output_dir, filename, width, height
#'             - 注意：save_correlation_plot参数顺序为：corr_matrix, output_dir, filename（可能含width/height）
#'
#' @examples
#' \dontrun{
#' # 调用示例
#' result <- analyze_promotion_core(
#'   promotion_clean = my_processed_data,
#'   output_directory = "./output/analysis_plots",
#'   PLACE = "山东济南",
#'   plot_width = 1600,
#'   plot_height = 1100
#' )
#'
#' # 查看统计结果
#' print(result$yield_stats)
#' print(result$corr_matrix)
#' }
#'
#' @export
analyze_promotion_core <- function(
    promotion_clean,
    output_directory,
    PLACE,
    plot_width = 1500,
    plot_height = 1000,
    scatter_height = 500,
    increase_col = "较临近对照增产_pct"
) {
  # 复制原始数据
  processed_data <- promotion_clean
  
  # 创建输出目录
  tryCatch({
    dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
  }, error = function(e) {
    message("Error in dir.create: ", e$message)
  })
  
  # 核心统计分析
  yield_stats <- tryCatch({
    calculate_yield_stats(processed_data)
  }, error = function(e) {
    message("Error in calculate_yield_stats: ", e$message)
    NULL
  })
  ck_mean <- tryCatch({
    calc_control_means_wide(processed_data)
  }, error = function(e) {
    message("Error in calc_control_means_wide: ", e$message)
    NULL
  })
  growth_stats <- tryCatch({
    calculate_growth_stats(processed_data)
  }, error = function(e) {
    message("Error in calculate_growth_stats: ", e$message)
    NULL
  })
  increase_stats <- tryCatch({
    calculate_increase_stats(processed_data)
  }, error = function(e) {
    message("Error in calculate_increase_stats: ", e$message)
    NULL
  })
  corr_matrix <- tryCatch({
    calculate_correlation_matrix(processed_data)
  }, error = function(e) {
    message("Error in calculate_correlation_matrix: ", e$message)
    NULL
  })
  
  # 设置图表主题
  tryCatch({
    setup_plot_theme()
  }, error = function(e) {
    message("Error in setup_plot_theme: ", e$message)
  })
  
  # 1. 产量、生育期直方图
  p1 <- tryCatch({
    plot_yield_distribution(
      data = processed_data, 
      yield_col = "亩产_kg",
      yield_stats = yield_stats, 
      ck_mean = ck_mean
    )
  }, error = function(e) {
    message("Error in plot_yield_distribution: ", e$message)
    NULL
  })
  grade_result <- tryCatch({
    plot_yield_grade_distribution(processed_data)
  }, error = function(e) {
    message("Error in plot_yield_grade_distribution: ", e$message)
    list(plot = NULL, data = processed_data)
  })
  p2 <- grade_result$plot
  processed_data <- grade_result$data
  increase_result <- tryCatch({
    plot_increase_distribution(processed_data,increase_col)
  }, error = function(e) {
    message("Error in plot_increase_distribution: ", e$message)
    list(plot = NULL, data = processed_data)
  })
  p3 <- increase_result$plot
  processed_data <- increase_result$data
  p4 <- tryCatch({
    plot_growth_distribution(
      processed_data, 
      growth_col = "生育期_d",
      growth_stats,
      ck_mean
    )
  }, error = function(e) {
    message("Error in plot_growth_distribution: ", e$message)
    NULL
  })
  
  # 保存产量生育期分布图
  tryCatch({
    save_basic_analysis_plots(
      list(p1, p2, p3, p4), output_directory, paste0(PLACE, "_产量生育期分析.png"),
      width = plot_width, height = plot_height
    )
  }, error = function(e) {
    message("Error in save_basic_analysis_plots (yield/growth plots): ", e$message)
  })
  
  # 2. 产量与相关性状散点图
  trait_yield_mapping <- list(
    list(x_var = "生育期_d", y_var = "亩产_kg"),
    list(x_var = "株高_cm", y_var = "亩产_kg"),
    list(x_var = "百粒重_g", y_var = "亩产_kg")
  )
  
  scatter_plots <- vector("list", length(trait_yield_mapping))
  for (i in seq_along(trait_yield_mapping)) {
    map <- trait_yield_mapping[[i]]
    scatter_plots[[i]] <- tryCatch({
      # 检查变量在数据中是否都存在
      if (!all(c(map$x_var, map$y_var) %in% names(processed_data))) {
        stop(sprintf("One or both of %s, %s not found in data.", map$x_var, map$y_var))
      }
      paired_data <- processed_data[, c(map$x_var, map$y_var)]
      paired_data <- na.omit(paired_data)
      # 检查是否有足够的数据进行相关性分析
      if (nrow(paired_data) == 0) {
        corr_val <- NA
      } else {
        corr_val <- tryCatch({
          unname(round(cor(paired_data[[1]], paired_data[[2]], use = "complete.obs"), 2))
        }, error = function(e) NA)
      }
      plot_trait_yield_scatter(processed_data, map$x_var, map$y_var, corr_val)
    }, error = function(e) {
      message(sprintf("Error in plot_trait_yield_scatter for %s~%s: %s", map$y_var, map$x_var, e$message))
      NULL
    })
  }
  
  tryCatch({
    save_basic_analysis_plots(
      scatter_plots, output_directory, paste0(PLACE, "_产量与相关性状散点图.png"),
      width = plot_width, height = scatter_height
    )
  }, error = function(e) {
    message("Error in save_basic_analysis_plots (scatter plots): ", e$message)
  })
  
  # 3. 产量相关性状相关性图
  tryCatch({
    save_correlation_plot(corr_matrix, output_directory, paste0(PLACE, "_产量相关图.png"))
  }, error = function(e) {
    message("Error in save_correlation_plot: ", e$message)
  })
  
  # 4. 质量性状分布图
  quality_traits <- c("花色", "叶形", "结荚习性", "倒伏性", "茸毛色", "脐色")
  quality_plots <- vector("list", length(quality_traits))
  for (i in seq_along(quality_traits)) {
    trait <- quality_traits[[i]]
    quality_plots[[i]] <- tryCatch({
      plot_single_trait_distribution(processed_data, trait)
    }, error = function(e) {
      message(sprintf("Error in plot_single_trait_distribution for trait %s: %s", trait, e$message))
      NULL
    })
  }
  
  tryCatch({
    save_basic_analysis_plots(
      quality_plots, output_directory, paste0(PLACE, "_质量性状分布图.png"),
      width = plot_width, height = plot_height
    )
  }, error = function(e) {
    message("Error in save_basic_analysis_plots (quality plots): ", e$message)
  })
  
  # 返回核心结果
  return(list(
    yield_stats = yield_stats,
    ck_mean = ck_mean,
    growth_stats = growth_stats,
    increase_stats = increase_stats,
    corr_matrix = corr_matrix,
    processed_data = processed_data
  ))
}


#' 作物材料晋级筛选、评述与结果保存函数（修复Excel导出参数匹配问题+支持倒伏性不筛选）
#'
#' 实现材料晋级筛选、高产分离选单株筛选、淘汰材料识别，生成晋级材料描述性评述文本，
#' 筛选指定保存字段并导出Excel报告（完全复刻原始代码逻辑，修复参数传递错误，支持倒伏性不筛选）
#'
#' @param promotion_clean 数据框，预处理后的推广材料数据（需包含：阶段名称、品种名称、母本、父本、生育期_d、
#'                        亩产_kg、较临近对照增产_pct、较临近对照位次、较平均对照增产_pct、较平均对照位次、
#'                        倒伏性、株高_cm、百粒重_g、草甘膦抗性等核心字段）
#' @param PLACE 字符型，区域/地点标识（用于文件命名前缀）
#' @param output_directory 字符型，输出目录路径（自动创建不存在的目录）
#' @param keep_eliminated_vector 字符向量，晋级材料需排除的阶段名称（对应原始逻辑中的同名变量）
#' @param keep_not_select_vector 字符向量，高产分离选单株需排除的阶段名称（对应原始逻辑中的同名变量）
#' @param rank_threshold 数值型，晋级位次阈值（默认：60，即位次<阈值）
#' @param eliminate_lodging 字符向量/NULL，需排除的倒伏性等级（默认：c("9-严重倒", "7-重倒")；设为NULL则不排除任何倒伏性）
#' @param sel_cols 字符向量，需保存到Excel的字段集合（默认：原始逻辑指定的14个核心字段）
#'
#' @return 列表，包含三类筛选后的数据框：
#'         \item{select_variety}{晋级材料数据框}
#'         \item{select_plant}{高产分离选单株材料数据框}
#'         \item{eliminated}{淘汰材料数据框}
#'         \item{used_thresholds}{使用的阈值参数（含晋级/分离选单株阈值）}
#'         \item{added_missing_cols}{自动添加的缺失字段（NULL表示无）}
#'
#' @details 依赖函数（需提前定义）：
#'          1. filter_by_keyword(data, keyword, keep)：按关键词筛选数据的函数
#'          2. soybean_comprehensive_evaluation_final(data)：生成材料综合性状描述的函数
#'          3. generate_excel_report(原始数据, 晋级材料, 淘汰材料, 分离选单株, 输出目录, 文件名)：Excel报告导出函数（按位置传参）
#'
#' @export
#' 
#' 
 

# screen_material_promotion <- function(
#     promotion_clean,
#     PLACE,
#     output_directory,
#     keep_eliminated_vector = NULL,
#     keep_not_select_vector = NULL,
#     # 新增：拆分两个独立阈值，分别命名，保留默认值60（兼容原有使用习惯）
#     rank_threshold_select = 60,    # 晋级材料筛选阈值（原rank_threshold）
#     rank_threshold_plant = 60,     # 高产分离选单株筛选阈值（新增独立控制）
#     eliminate_lodging = c("9-严重倒", "7-重倒")  # 支持设为NULL
# ) {
#   # ============================================================================
#   # 1. 输入检查与环境准备
#   # ============================================================================
#   cat("📌 开始材料晋级筛选流程...\n")
#   
#   # 核心参数合法性校验（更新：移除sel_cols校验，允许eliminate_lodging为NULL）
#   stopifnot(
#     is.data.frame(promotion_clean),
#     is.character(PLACE) && nchar(PLACE) > 0,
#     is.character(output_directory) && nchar(output_directory) > 0,
#     is.vector(keep_eliminated_vector) | is.null(keep_eliminated_vector),
#     is.vector(keep_not_select_vector) | is.null(keep_not_select_vector),
#     # 检查两个阈值均为正数
#     is.numeric(rank_threshold_select) && rank_threshold_select > 0,
#     is.numeric(rank_threshold_plant) && rank_threshold_plant > 0,
#     # 允许eliminate_lodging为NULL或字符向量
#     (is.character(eliminate_lodging) | is.null(eliminate_lodging))
#   )
#   
#   # 自动创建输出目录（若不存在）
#   if (!dir.exists(output_directory)) {
#     dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
#     cat(sprintf("📂 已自动创建输出目录：%s\n", output_directory))
#   }
#   
#   # 必要字段存在性检查（确保核心筛选字段不缺失）
#   required_filter_cols <- c("阶段名称", "较临近对照位次", "较平均对照位次", "倒伏性")
#   missing_filter_cols <- setdiff(required_filter_cols, colnames(promotion_clean))
#   if (length(missing_filter_cols) > 0) {
#     stop(sprintf("❌ 数据缺少筛选必需字段：%s", paste(missing_filter_cols, collapse = ", ")))
#   }
#   
#   # 复制原始数据，避免修改输入对象（无需处理sel_cols，直接复制全部列）
#   data_raw <- promotion_clean
#   
#   # ============================================================================
#   # 2. 材料筛选（支持eliminate_lodging=NULL，不排除倒伏性）
#   # ============================================================================
#   cat("\n🔍 执行材料筛选...\n")
#   # 打印当前使用的阈值，方便用户确认
#   cat(sprintf("   - 晋级材料筛选阈值：%s（位次<=该值）\n", rank_threshold_select))
#   cat(sprintf("   - 高产分离选单株筛选阈值：%s（位次<该值）\n", rank_threshold_plant))
#   
#   # 倒伏性筛选提示
#   if (is.null(eliminate_lodging)) {
#     cat("   - 倒伏性筛选：不排除任何倒伏性等级\n")
#   } else {
#     cat(sprintf("   - 倒伏性筛选：排除等级 = %s\n", paste(eliminate_lodging, collapse = ", ")))
#   }
#   
#   ## 2.1 晋级材料筛选（使用 rank_threshold_select + 支持倒伏性不筛选）
#   cat("   - 筛选晋级材料...\n")
#   select_variety <- filter_by_keyword(data_raw, "分离", keep = FALSE) |>
#     dplyr::filter(较临近对照位次 <= rank_threshold_select) |>
#     dplyr::filter(较平均对照位次 <= rank_threshold_select) |>
#     # eliminate_lodging为NULL时，不筛选倒伏性（条件恒为TRUE）
#     dplyr::filter(if (is.null(eliminate_lodging)) TRUE else !倒伏性 %in% eliminate_lodging) |>
#     dplyr::filter(is.null(keep_eliminated_vector) | !阶段名称 %in% keep_eliminated_vector)
#   
#   ## 2.2 高产分离材料选单株筛选（使用 rank_threshold_plant + 支持倒伏性不筛选）
#   cat("   - 筛选高产分离选单株材料...\n")
#   select_plant <- filter_by_keyword(data_raw, "分离", keep = TRUE) |>
#     dplyr::filter(较临近对照位次 < rank_threshold_plant) |>
#     dplyr::filter(较平均对照位次 < rank_threshold_plant) |>
#     # eliminate_lodging为NULL时，不筛选倒伏性（条件恒为TRUE）
#     dplyr::filter(if (is.null(eliminate_lodging)) TRUE else !倒伏性 %in% eliminate_lodging) |>
#     dplyr::filter(is.null(keep_not_select_vector) | !阶段名称 %in% keep_not_select_vector)
#   
#   ## 2.3 淘汰材料识别（阶段名称不在晋级和分离材料中）
#   cat("   - 识别淘汰材料...\n")
#   eliminated <- data_raw |>
#     dplyr::filter(!阶段名称 %in% select_variety$阶段名称 & !阶段名称 %in% select_plant$阶段名称)
#   
#   # 筛选结果汇总提示
#   cat(sprintf(
#     "✅ 筛选完成：\n   - 晋级材料：%s份（输出全部列）\n   - 高产分离选单株：%s份（输出全部列）\n   - 淘汰材料：%s份（输出全部列）\n",
#     nrow(select_variety), nrow(select_plant), nrow(eliminated)
#   ))
#   
#   # ============================================================================
#   # 3. 晋级材料描述性评述（完全复刻原始逻辑）
#   # ============================================================================
#   cat("\n📝 生成晋级材料综合性状描述...\n")
#   if (nrow(select_variety) > 0) {
#     txt_filename <- paste0(PLACE, "_材料综合性状描述.txt")
#     txt_filepath <- file.path(output_directory, txt_filename)
#     
#     # 捕获可能的报错，确保sink正常关闭
#     tryCatch({
#       sink(txt_filepath, append = FALSE, split = TRUE)
#       soybean_comprehensive_evaluation_final(select_variety)
#       sink()
#       cat(sprintf("✅ 已保存描述性评述：%s\n", txt_filepath))
#     }, error = function(e) {
#       sink()  # 强制关闭sink，避免资源泄露
#       stop(sprintf("❌ 生成描述性评述失败：%s", e$message))
#     })
#   } else {
#     warning("⚠️  未筛选出晋级材料，跳过描述性评述生成")
#   }
#   
#   # ============================================================================
#   # 4. 结果保存（输出全部列，无需筛选字段）
#   # ============================================================================
#   cat("\n💾 导出筛选结果（全部列）...\n")
#   
#   ## 4.2 导出Excel报告（位置传参，仅filename指定参数名）
#   excel_filename <- paste0(PLACE, "_晋级_淘汰_分离选单.xlsx")
#   generate_excel_report(
#     data_raw,  # 第1个参数：原始数据（全部列）
#     select_variety,  # 第2个参数：晋级材料（全部列）
#     eliminated,  # 第3个参数：淘汰材料（全部列）
#     select_plant,  # 第4个参数：分离选单株材料（全部列）
#     output_directory,  # 第5个参数：输出目录
#     filename = excel_filename  # 第6个参数：文件名
#   )
#   cat(sprintf("✅ 已保存Excel报告（含全部列）：%s\n", file.path(output_directory, excel_filename)))
#   
#   # ============================================================================
#   # 5. 流程结束与返回结果
#   # ============================================================================
#   cat("\n=== 数据分析流程完成！ ===\n")
#   return(list(
#     select_variety = select_variety,  # 返回全部列
#     select_plant = select_plant,      # 返回全部列
#     eliminated = eliminated,          # 返回全部列
#     # 返回使用的阈值参数，方便后续追溯
#     used_thresholds = list(
#       rank_threshold_select = rank_threshold_select,
#       rank_threshold_plant = rank_threshold_plant
#     ),
#     # 返回倒伏性筛选参数，方便追溯
#     used_eliminate_lodging = eliminate_lodging
#   ))
# }


#' Analyze and save excellent parents and crosses
#'
#' 对预处理后的育种数据进行优良亲本与优良组合分析，并将结果导出为 Excel 文件。
#'
#' @param preprocessed_data 数据框。由 `preprocess_parent_data()` 输出的已清洗数据。
#' @param select_variety 数据框。晋级品种数据，应包含产量相关性状（如 `"亩产_kg"`）。
#' @param output_directory 字符串。输出文件存储路径。
#' @param PLACE 字符串。文件命名使用的地点标识。
#' @param parent_min_crosses 整数。优良亲本最小配制品种数（默认 3）。
#' @param parent_top_pct 数值。优良亲本晋级率分位阈值（默认 0.65）。
#' @param cross_min_crosses 整数。优良组合最小配制品种数（默认 2）。
#' @param cross_promote_rate_thresh 数值。组合晋级率阈值（默认 0.2）。
#' @param cross_yield_threshold_adjust 数值。组合产量阈值调节系数（默认 0.8）。
#'
#' @return 一个 list，包括：
#' * `parent_res`：优良亲本分析结果  
#' * `cross_res`：优良组合分析结果  
#'
#' 并自动将结果保存为 Excel 文件。
#'
#' @export
analyze_and_save_parents_crosses <- function(
    preprocessed_data,
    select_variety,
    output_directory,
    PLACE,
    parent_min_crosses = 3,
    parent_top_pct = 0.65,
    cross_min_crosses = 2,
    cross_promote_rate_thresh = 0.2,
    cross_yield_threshold_adjust = 0.8
) {
  
  # ==== 输入有效性检查（避免用户传入错误对象）====
  if (!is.data.frame(preprocessed_data)) {
    stop("`preprocessed_data` must be a data frame.")
  }
  
  if (!is.data.frame(select_variety)) {
    stop("`select_variety` must be a data frame.")
  }
  
  if (!dir.exists(output_directory)) {
    stop("`output_directory` does not exist. Please provide a valid directory.")
  }
  
  # ==== 1. 优良亲本分析 ====
  # 亲本分析依赖于预处理后的配制统计，因此无需晋级品种数据。
  parent_res <- analyze_excellent_parents(
    preprocessed_data = preprocessed_data,
    min_crosses = parent_min_crosses,
    top_pct = parent_top_pct
  )
  
  # ==== 2. 优良组合分析 ====
  # 组合分析需要晋级品种数据以估计晋级率。
  cross_res <- analyze_excellent_crosses(
    preprocessed_data = preprocessed_data,
    promoted_data = select_variety,
    min_crosses = cross_min_crosses,
    promote_rate_thresh = cross_promote_rate_thresh,
    yield_threshold_adjust = cross_yield_threshold_adjust
  )
  
  # ==== 3. 保存分析结果 ====
  save_path <- file.path(output_directory, paste0(PLACE, "_优良亲本及组合筛选结果.xlsx"))
  
  save_analysis_results(
    parent_analysis_res = parent_res,
    cross_analysis_res = cross_res,
    save_path = save_path
  )
  
  # 返回结果，方便后续在代码中进一步使用
  list(
    parent_res = parent_res,
    cross_res = cross_res
  )
}




#' 按“同一地点双位次阈值”筛选记录，并另外添加一列注明在哪个地点满足了怎么样的选择条件
#' 
#' @param df 输入数据框（如ZengChan_wide）
#' @param threshold 位次阈值，默认60（筛选两个位次都小于该值的记录）
#' @param mode 筛选模式：
#'   "any_site" = 任意一个地点双位次<阈值就入选（默认）；
#'   "all_sites" = 所有指定地点双位次都<阈值才能入选
#' @param sites_to_check 需要进行筛选的地点名向量（如c("安徽宿州", "山东潍坊")，默认全部地点）
#' @return 筛选后的数据框，并附加一列“满足筛选的地点说明”
#' @examples
#' # 示例1：任意地点双位次<60就入选
#' result1 <- filter_rank_by_requirement(ZengChan_wide)
#' 
#' # 示例2：所有地点双位次<60才入选
#' result2 <- filter_rank_by_requirement(ZengChan_wide, mode = "all_sites")
#' 
#' # 示例3：自定义阈值 - 任意地点双位次<50就入选
#' result3 <- filter_rank_by_requirement(ZengChan_wide, threshold = 50)
#' 
#' # 示例4：只筛选指定地点
#' result4 <- filter_rank_by_requirement(ZengChan_wide, sites_to_check = c("安徽宿州","山东潍坊"))
filter_rank_by_requirement <- function(df, threshold = 60, mode = c("any_site", "all_sites"), sites_to_check = NULL) {
  
  mode <- match.arg(mode)
  
  # ========== 1. 严格参数校验 ==========
  # 校验数据框
  if (!is.data.frame(df)) {
    stop("参数df必须是数据框格式！")
  }
  # 校验阈值
  if (!is.numeric(threshold) || threshold <= 0) {
    stop("参数threshold必须是大于0的数值（如60）！")
  }
  # 校验mode模式
  if (!mode %in% c("any_site", "all_sites")) {
    stop("参数mode只能是'any_site'（任一地点满足）或 'all_sites'（所有地点都满足）！")
  }
  
  # ========== 2. 提取所有有效地点名（关键：保证“同一地点”） ==========
  # 步骤1：提取所有“较平均对照位次_地点”列（排除“平均”列）
  avg_rank_cols <- grep("^较平均对照位次_", colnames(df), value = TRUE)
  avg_rank_cols <- setdiff(avg_rank_cols, "较平均对照位次_平均")
  
  # 容错：无有效地点列时返回原数据并警告
  if (length(avg_rank_cols) == 0) {
    warning("数据框中未找到“较平均对照位次_地点”列（如安徽宿州/山东潍坊），返回原数据")
    message("\n===== 筛选结果总结 =====\n",
            "1. 筛选参数：\n",
            "   - 位次阈值：", threshold, "\n",
            "   - 筛选模式：", if (mode=="any_site") "any_site（任意地点满足）" else "all_sites（所有地点满足）", "\n",
            "   - 筛选地点：无有效地点列\n",
            "2. 数据规模：\n",
            "   - 原始数据总行数：", nrow(df), "\n",
            "   - 筛选后数据行数：", nrow(df), "\n",
            "   - 筛选出比例：100.0%\n",
            "3. 备注：未找到有效地点列，直接返回原数据\n",
            "========================\n")
    return(df)
  }
  
  # 步骤2：从列名拆分纯地点名（如“较平均对照位次_安徽宿州”→“安徽宿州”）
  all_sites <- sub("^较平均对照位次_", "", avg_rank_cols)
  
  # 筛选指定的地点
  if (is.null(sites_to_check)) {
    sites <- all_sites
  } else {
    if (!is.character(sites_to_check)) {
      stop("参数sites_to_check应为地点名的字符向量")
    }
    # 检查给定地点是否在all_sites
    invalid_sites <- setdiff(sites_to_check, all_sites)
    if (length(invalid_sites) > 0) {
      stop(paste0("如下地点名未出现在数据中：", paste(invalid_sites, collapse = "；")))
    }
    sites <- sites_to_check
  }
  
  # ========== 3. 按模式执行筛选，并构建地点说明列 ==========
  # 构建每个地点的逻辑矩阵（每行每个地点是否都满足条件）
  cond_mat <- sapply(sites, function(site) {
    avg_col <- paste0("较平均对照位次_", site)
    near_col <- paste0("较临近对照位次_", site)
    if (!avg_col %in% colnames(df)) stop(paste("缺失列：", avg_col))
    if (!near_col %in% colnames(df)) stop(paste("缺失列：", near_col))
    !is.na(df[[avg_col]]) & df[[avg_col]] < threshold &
      !is.na(df[[near_col]]) & df[[near_col]] < threshold
  })
  # 保证cond_mat为matrix
  if (is.vector(cond_mat)) cond_mat <- matrix(cond_mat, ncol = 1)
  colnames(cond_mat) <- sites
  
  # mode控制：any_site 任意地点满足，all_sites 所有地点都满足
  if (mode == "any_site") {
    qualified <- apply(cond_mat, 1, any)
  } else {
    qualified <- apply(cond_mat, 1, all)
  }
  
  # 为筛选出的行生成说明列
  qualified_cond <- cond_mat[qualified, , drop = FALSE]
  explain_vec <- apply(qualified_cond, 1, function(row_stat) {
    pos_sites <- names(row_stat)[which(row_stat)]
    if (length(pos_sites) == 0) {
      return("")
    }
    if (mode == "any_site") {
      # 多个地点都满足的，列出来
      paste0(
        "满足筛选地点：",
        paste(pos_sites, collapse = "；"),
        "，在这些地点“较平均对照位次”与“较临近对照位次”均 < ", threshold
      )
    } else {
      # 必须所有指定地点都满足才入选
      paste0(
        "所有指定筛选地点（", paste(pos_sites, collapse = "；"), 
        "）“较平均对照位次”与“较临近对照位次”均 < ", threshold
      )
    }
  })
  
  filtered_df <- df[qualified, , drop = FALSE]
  # 添加新列，注明满足条件的地点与理由
  filtered_df$满足筛选的地点说明 <- explain_vec
  
  # ========== 4. 生成并输出总结性信息 ==========
  # 基础统计
  total_rows <- nrow(df)
  filtered_rows <- nrow(filtered_df)
  filtered_ratio <- if (total_rows > 0) round((filtered_rows / total_rows) * 100, 1) else 0
  
  # 各地点满足条件的行数（所有行中满足该地点双位次<阈值的数量）
  site_qualified_counts <- colSums(cond_mat, na.rm = TRUE)
  site_counts_text <- paste0(
    "  - ", names(site_qualified_counts), ": ", site_qualified_counts, "行",
    collapse = "\n"
  )
  
  # 模式说明文本
  mode_explain <- if (mode == "any_site") {
    "any_site（任意地点满足双位次<阈值即入选）"
  } else {
    "all_sites（所有指定地点均满足双位次<阈值才入选）"
  }
  
  # 筛选地点文本
  sites_text <- paste(sites, collapse = "；")
  
  # 补充边界提示（无满足条件记录时）
  boundary_note <- if (filtered_rows == 0) {
    "\n4. 备注：无满足筛选条件的记录\n"
  } else {
    ""
  }
  
  # 构建总结信息
  summary_msg <- paste0(
    "\n===== 筛选结果总结 =====\n",
    "1. 筛选参数：\n",
    "   - 位次阈值：", threshold, "\n",
    "   - 筛选模式：", mode_explain, "\n",
    "   - 筛选地点：", sites_text, "\n",
    "2. 数据规模：\n",
    "   - 原始数据总行数：", total_rows, "\n",
    "   - 筛选后数据行数：", filtered_rows, "\n",
    "   - 筛选出比例：", filtered_ratio, "%\n",
    "3. 各筛选地点满足“双位次<", threshold, "”的行数（所有行中）：\n",
    site_counts_text, "\n",
    boundary_note,
    "========================\n"
  )
  
  # 输出总结信息（用message，不干扰返回值，且在控制台可见）
  message(summary_msg)
  
  # ========== 5. 返回筛选结果 ==========
  return(filtered_df)
}






#' 计算各材料生育期与对照平均生育期的差值（对照熟期差）
#'
#' 该函数首先筛选数据中「是否对照=1」的材料，计算其平均生育期；
#' 然后将每个材料的生育期与对照平均生育期做差值计算，规则为：
#' 材料生育期 - 对照平均生育期，结果负值表示比对照早熟，正值表示晚熟；
#' 最终在原数据框末尾新增一列「对照熟期差_d」，存放保留0位小数的计算结果。
#'
#' @param promotion_clean 数据框，必须包含以下列：
#'        \itemize{
#'          \item 「是否对照」：数值型，1表示对照材料，0表示非对照材料
#'          \item 「生育期_d」：数值型，材料的生育期（单位：天）
#'        }
#' @return 返回扩展后的数据框，在原数据框基础上新增一列「对照熟期差_d」，
#'         该列值为整数（0位小数），负值=早熟，正值=晚熟，0=与对照持平
#' @details
#' 1. 函数会先校验输入数据的合法性（是否为数据框、必需列是否存在、列值是否合法）；
#' 2. 对照平均生育期计算时会自动忽略「生育期_d」的NA值；
#' 3. 若「生育期_d」存在NA值，对应「对照熟期差_d」也为NA，并给出警告提示；
#' 4. 对照材料自身也会参与差值计算（结果可能非0），符合统计逻辑。
#' @examples
#' # 模拟测试数据
#' promotion_clean <- data.frame(
#'   材料编号 = paste0("M", 1:8),
#'   是否对照 = c(1, 1, 0, 0, 0, 0, 0, 0),  # 前2个为对照材料
#'   生育期_d = c(120, 122, 118, 125, 115, 120, 119, 123),  # 生育期数据
#'   亩产_kg = c(500, 510, 490, 520, 480, 505, 495, 515)    # 其他无关列
#' )
#'
#' # 调用函数计算对照熟期差
#' result_df <- calculate_maturity_diff(promotion_clean)
#'
#' # 查看结果
#' print(result_df)
#' @importFrom dplyr filter summarise pull mutate
#' @export
#' 
calculate_maturity_diff <- function(promotion_clean) {
  # 加载必要包
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("需要安装dplyr包：install.packages('dplyr')")
  }
  library(dplyr, quietly = TRUE)
  
  # ========== 第一步：参数校验 ==========
  # 检查输入是否为数据框
  if (!is.data.frame(promotion_clean)) {
    stop("输入参数promotion_clean必须是数据框格式！")
  }
  
  # 检查必需列是否存在
  required_cols <- c("是否对照", "生育期_d")
  missing_cols <- setdiff(required_cols, colnames(promotion_clean))
  if (length(missing_cols) > 0) {
    stop("数据框缺少必需列：", paste(missing_cols, collapse = ", "))
  }
  
  # 检查"是否对照"列值是否合法（仅允许0/1）
  if (!all(promotion_clean$是否对照 %in% c(0, 1), na.rm = TRUE)) {
    stop("「是否对照」列仅允许值为0（非对照）或1（对照），请检查数据！")
  }
  
  # 检查"生育期_d"列是否为数值型
  if (!is.numeric(promotion_clean$生育期_d)) {
    stop("「生育期_d」列必须是数值型，请检查数据！")
  }
  
  # 检查对照组是否有数据
  control_data <- promotion_clean %>% filter(是否对照 == 1)
  if (nrow(control_data) == 0) {
    stop("数据中无对照材料（是否对照=1），无法计算对照平均生育期！")
  }
  
  # 检查生育期是否有缺失值
  if (any(is.na(promotion_clean$生育期_d))) {
    warning("⚠️ 「生育期_d」列存在缺失值，缺失值对应的「对照熟期差_d」将为NA")
  }
  
  # ========== 第二步：计算对照平均生育期 ==========
  control_mean_maturity <- control_data %>% 
    summarise(mean = mean(生育期_d, na.rm = TRUE)) %>% 
    pull(mean)
  
  message(paste0("✅ 对照材料平均生育期：", round(control_mean_maturity, 0), " 天"))
  
  # ========== 第三步：计算各材料与对照的熟期差 ==========
  result_df <- promotion_clean %>%
    mutate(
      # 计算差值：材料生育期 - 对照平均生育期（早于对照为负，晚于为正）
      对照熟期差_d = 生育期_d - control_mean_maturity,
      # 保留0位小数（四舍五入）
      对照熟期差_d = round(对照熟期差_d, 0)
    )
  
  # ========== 第四步：结果校验与提示 ==========
  # 统计熟期差分布
  diff_stats <- result_df %>%
    filter(!is.na(对照熟期差_d)) %>%
    summarise(
      早熟材料数 = sum(对照熟期差_d < 0),
      晚熟材料数 = sum(对照熟期差_d > 0),
      持平材料数 = sum(对照熟期差_d == 0)
    )
  
  message(paste0("✅ 熟期差计算完成："))
  message(paste0("   - 早熟材料（负值）：", diff_stats$早熟材料数, " 个"))
  message(paste0("   - 晚熟材料（正值）：", diff_stats$晚熟材料数, " 个"))
  message(paste0("   - 与对照持平：", diff_stats$持平材料数, " 个"))
  
  # 返回结果数据框
  return(result_df)
}


#' 批量生成不同地点产量与生育期对比图（含对照数据）
#'
#' 该函数支持传入单个或多个阶段名称，批量生成各阶段下不同测试地点的产量柱状图+生育期折线图组合图表。
#' 产量部分同时展示「平均亩产」和「对照亩产」分组柱状图，并在柱体底部标注数值；生育期部分展示「平均生育期」实线折线（标注数值）
#' 和「对照生育期」虚线折线（标注整数数值），采用双Y轴协调展示产量（kg）和生育期（天）维度。
#' 图表自动适配Windows/Mac/Linux系统中文字体，生成后保存为PNG文件，同时返回各阶段的统计结果数据。
#'
#' @param stage_name 字符型/字符向量，必填。指定一个或多个需要分析的阶段名称（如 'N25M010', c('N25M010', 'N25M020')）。
#'   若传入非向量形式的单个字符串，函数会自动转为长度为1的向量。
#' @param promotion 数据框，必填。多点测试的原始数据集，必须包含以下列：
#'   \itemize{
#'     \item 地点：测试地点名称（字符型）
#'     \item 阶段名称：试验阶段标识（字符型，需与 stage_name 匹配）
#'     \item 亩产_kg：亩产数据（数值型，单位：kg）
#'     \item 生育期_d：生育期数据（数值型，单位：天）
#'     \item 是否对照：对照标识（支持逻辑型/数值型/字符型，如 TRUE/1/"是"/"对照" 均判定为对照）
#'   }
#'
#' @return 列表/数据框。若仅传入单个阶段名称，返回该阶段的统计数据框；若传入多个阶段名称，返回以阶段名称为命名的列表，
#'   列表每个元素为对应阶段的统计数据框。数据框包含列：
#'   \itemize{
#'     \item 地点：测试地点名称
#'     \item 平均亩产：该地点该阶段的平均亩产（kg）
#'     \item 平均生育期：该地点该阶段的平均生育期（天）
#'     \item 样本数量：该地点该阶段的有效样本数
#'     \item 对照亩产：该地点的对照平均亩产（kg，无对照则为NA）
#'   }
#'
#' @details
#' 1. 数据验证：函数会先检查输入数据是否包含必要列，若缺少则抛出错误；若指定阶段名称不存在/无有效数据，会抛出警告并跳过该阶段。
#' 2. 颜色映射：
#'    \itemize{
#'      \item 对照亩产（柱形/标签）：橙色 (#E67E22)
#'      \item 平均亩产（柱形/标签）：蓝色 (#2E86AB)
#'      \item 平均生育期（折线/点/标签）：绿色 (#27AE60)
#'      \item 对照生育期（折线/标签）：橙色 (#E67E22)
#'    }
#' 3. 图表保存：生成的图表以 `yield_growth_chart_阶段名称.png` 命名，保存到当前工作目录，分辨率300dpi，尺寸14×10英寸。
#' 4. 字体适配：自动识别系统类型（Windows/Mac/Linux）并加载对应中文字体，避免乱码。
#'
#' @examples
#' \dontrun{
#' # 示例1：单阶段分析
#' # 构造测试数据
#' test_data <- data.frame(
#'   地点 = rep(c("地点A", "地点B", "地点C"), each = 2),
#'   阶段名称 = rep("N25M010", 6),
#'   亩产_kg = c(500, 480, 520, 510, 490, 470),
#'   生育期_d = c(120, 118, 125, 123, 119, 117),
#'   是否对照 = c(1, 0, 1, 0, 1, 0)
#' )
#' # 生成单阶段图表并返回数据
#' single_result <- generate_yield_growth_chart(stage_name = "N25M010", promotion = test_data)
#' print(single_result)
#' # 保存单阶段结果到CSV
#' write.csv(single_result, "阶段_N25M010_分析结果.csv", row.names = FALSE, fileEncoding = "UTF-8")
#'
#' # 示例2：多阶段批量分析
#' # 扩展测试数据（增加第二个阶段）
#' test_data_multi <- rbind(
#'   test_data,
#'   data.frame(
#'     地点 = rep(c("地点A", "地点B", "地点C"), each = 2),
#'     阶段名称 = rep("N25M020", 6),
#'     亩产_kg = c(510, 490, 530, 520, 500, 480),
#'     生育期_d = c(122, 120, 127, 125, 121, 119),
#'     是否对照 = c(1, 0, 1, 0, 1, 0)
#'   )
#' )
#' # 批量生成两个阶段的图表
#' multi_results <- generate_yield_growth_chart(stage_name = c("N25M010", "N25M020"), promotion = test_data_multi)
#' # 查看第二个阶段的结果
#' print(multi_results[["N25M020"]])
#' # 保存第二个阶段结果到CSV
#' write.csv(multi_results[["N25M020"]], "阶段_N25M020_分析结果.csv", row.names = FALSE, fileEncoding = "UTF-8")
#' }
#'
#' @note
#' 1. 若对照数据缺失，对照亩产/生育期相关元素会自动隐藏，不影响平均亩产/生育期的展示。
#' 2. Y轴范围会自动适配数据最大值，预留15%的顶部空间，保证标签不超出图表范围。
#' 3. 生育期数值标注：平均生育期标注在折线上方（保留1位小数），对照生育期标注在折线下方（取整）。
#'
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @export
generate_yield_growth_chart <- function(stage_name, promotion, out_dir = ".") {
  # 支持stage_name为单个字符或字符向量
  # 新增参数 out_dir，指定输出目录，默认为当前目录
  # 返回值为一个list，内容为每个阶段对应结果数据
  if (missing(stage_name) || length(stage_name) == 0) {
    stop("请提供至少一个阶段名称（stage_name）")
  }
  # 若只传入单个字符串也转为向量
  stage_names <- as.character(stage_name)
  results_list <- list()
  
  # 检查输出目录
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
    cat(paste("输出目录不存在，已创建：", out_dir, "\n"))
  }
  
  for (name in stage_names) {
    # 1. 读取数据
    cat("正在读取数据文件...\n")
    data <- promotion
    
    # 2. 数据验证和预处理
    required_cols <- c("地点", "阶段名称", "亩产_kg", "生育期_d", "是否对照")
    missing_cols <- setdiff(required_cols, colnames(data))
    if (length(missing_cols) > 0) {
      stop(paste("数据文件缺少必要的列:", paste(missing_cols, collapse = ", ")))
    }
    
    # 检查阶段名称是否存在
    if (!name %in% data$阶段名称) {
      available_stages <- unique(data$阶段名称)
      warning(
        paste0(
          "指定的阶段名称 '", name, "' 不存在于数据中。\n",
          "可用的阶段名称示例: ", paste(head(available_stages, 5), collapse = ", "), "..."
        )
      )
      next
    }
    
    # 3. 数据处理：计算当前阶段平均亩产、生育期、样本数
    cat(paste("正在处理阶段名称:", name, "\n"))
    
    stage_data <- data %>%
      dplyr::filter(阶段名称 == name) %>%
      dplyr::group_by(地点) %>%
      dplyr::summarise(
        平均亩产 = mean(`亩产_kg`, na.rm = TRUE),
        平均生育期 = mean(`生育期_d`, na.rm = TRUE),
        样本数量 = n(),
        .groups = "drop"
      ) %>%
      dplyr::arrange(dplyr::desc(平均亩产))
    
    # 计算每个地点的"对照"平均产量与平均生育期
    control_flag <- function(v) {
      if (is.logical(v)) return(v)
      if (is.numeric(v)) return(v == 1)
      v <- as.character(v)
      v_trim <- trimws(v)
      v_trim %in% c("1", "是", "对照", "Y", "y", "TRUE", "True", "true")
    }
    
    control_data <- data %>%
      dplyr::filter(control_flag(是否对照)) %>%
      dplyr::group_by(地点) %>%
      dplyr::summarise(
        对照生育期 = mean(`生育期_d`, na.rm = TRUE),
        对照亩产 = mean(`亩产_kg`, na.rm = TRUE),
        .groups = "drop"
      )
    
    # 合并地点保持顺序
    control_data_merge <- stage_data %>%
      dplyr::select(地点) %>%
      dplyr::left_join(control_data, by = "地点")
    
    # 检查筛选后的数据是否为空
    if (nrow(stage_data) == 0) {
      warning(paste("阶段名称 '", name, "' 没有对应的有效测试数据"))
      next
    }
    
    # 计算Y轴比例系数（用于双Y轴协调）
    yield_max <- max(stage_data$平均亩产, control_data_merge$对照亩产, na.rm = TRUE)
    growth_max <- max(stage_data$平均生育期, na.rm = TRUE)
    # 对照生育期也进最大值判定
    if (nrow(control_data_merge) > 0 && any(!is.na(control_data_merge$对照生育期))) {
      growth_max <- max(growth_max, control_data_merge$对照生育期, na.rm = TRUE)
    }
    scale_factor <- ifelse(growth_max == 0, 1, yield_max / growth_max)
    
    cat(paste("成功筛选到", nrow(stage_data), "个地点的数据\n"))
    
    # 4. 创建分组柱状数据（长表）
    plot_yield_ctrl <- control_data_merge %>%
      dplyr::mutate(类型 = "对照亩产", 值 = 对照亩产) %>%
      dplyr::select(地点, 类型, 值)
    plot_yield <- stage_data %>%
      dplyr::mutate(类型 = "平均亩产", 值 = 平均亩产) %>%
      dplyr::select(地点, 类型, 值)
    plot_yield_all <- dplyr::bind_rows(plot_yield_ctrl, plot_yield)
    plot_yield_all <- plot_yield_all[!is.na(plot_yield_all$值), ]
    
    # 保证类型列是有序因子，对照亩产在前，平均亩产在后
    plot_yield_all$类型 <- factor(plot_yield_all$类型, levels = c("对照亩产", "平均亩产"))
    # 映射颜色，对照亩产=橙色、平均亩产=蓝色，bar和label都一致
    type_color_map <- c("对照亩产" = "#E67E22", "平均亩产" = "#2E86AB")
    plot_yield_all$label_color <- type_color_map[as.character(plot_yield_all$类型)]
    
    # 5. 创建组合图表
    cat("正在生成图表...\n")
    if (Sys.info()["sysname"] == "Windows") {
      windowsFonts(
        微软雅黑 = windowsFont("Microsoft YaHei"),
        宋体 = windowsFont("SimSun")
      )
      font_family <- "微软雅黑"
    } else if (Sys.info()["sysname"] == "Darwin") {  # Mac OS
      font_family <- "PingFang SC"
    } else {  # Linux
      font_family <- "WenQuanYi Zen Hei"
    }
    
    # -- 构建主图
    p <- ggplot() +
      # 1. 产量分组柱状图
      geom_col(
        data = plot_yield_all,
        aes(x = 地点, y = 值, fill = 类型),
        alpha = 0.8, width = 0.6, position = position_dodge(width = 0.65)
      ) +
      # 1.1 在柱体底部标注数值
      geom_text(
        data = plot_yield_all,
        aes(x = 地点, y = 0, label = sprintf("%.1f", 值), group = 类型, color = 类型),
        fill = NA,
        vjust = 1.1,
        family = font_family,
        size = 4,
        fontface = "bold",
        position = position_dodge(width = 0.65),
        show.legend = FALSE
      ) +
      scale_color_manual(
        values = c(
          "对照亩产" = "#E67E22",
          "平均亩产" = "#2E86AB",
          # 折线色
          "平均生育期" = "#27AE60",
          "对照生育期" = "#E67E22"
        ),
        breaks = c("对照亩产", "平均亩产", "平均生育期", "对照生育期"),
        guide = "none"
      ) +
      # 2. 生育期折线图&点
      geom_line(
        data = stage_data,
        aes(x = 地点, y = 平均生育期 * scale_factor, color = "平均生育期", group = 1),
        linewidth = 1.2
      ) +
      geom_point(
        data = stage_data,
        aes(x = 地点, y = 平均生育期 * scale_factor, color = "平均生育期"),
        size = 3, shape = 16, alpha = 0.9
      ) +
      # 2.1 在折线上每个点标注“平均生育期”数值
      geom_text(
        data = stage_data,
        aes(
          x = 地点,
          y = 平均生育期 * scale_factor,
          label = sprintf("%.1f", 平均生育期)
        ),
        vjust = -1.2,
        family = font_family,
        size = 3.8,
        color = "#27AE60",
        fontface = "bold"
      )
    
    # 3. 对照生育期折线（虚线）及在其下方标注其整数值
    if (nrow(control_data_merge) > 0 && any(!is.na(control_data_merge$对照生育期))) {
      p <- p + 
        geom_line(
          data = control_data_merge,
          mapping = aes(x = 地点, y = 对照生育期 * scale_factor, group = 1, color = "对照生育期"),
          linewidth = 1.2, linetype = "dashed", na.rm = TRUE, inherit.aes = FALSE
        ) +
        geom_text(
          data = control_data_merge,
          aes(
            x = 地点,
            y = 对照生育期 * scale_factor,
            label = sprintf("%.0f", 对照生育期)   # 0位小数
          ),
          vjust = 2.2,  # 显示在折线下方
          family = font_family,
          size = 3.8,
          color = "#E67E22",
          fontface = "bold",
          na.rm = TRUE
        )
    }
    
    # 4. 设置双Y轴和色映射
    p <- p +
      scale_y_continuous(
        name = "平均亩产 (kg)",
        sec.axis = sec_axis(
          transform = ~ . / scale_factor,
          name = "平均生育期 (天)"
        ),
        limits = c(0, yield_max * 1.15 + max(20, yield_max * 0.1, na.rm = TRUE))
      ) +
      scale_fill_manual(
        values = c(
          "对照亩产" = "#E67E22",
          "平均亩产" = "#2E86AB"
        ),
        name = "产量",
        labels = c("对照亩产", "平均亩产")
      ) +
      scale_color_manual(
        values = c(
          "平均生育期" = "#27AE60",
          "对照生育期" = "#E67E22"
        ),
        name = "生育期",
        breaks = c("平均生育期", "对照生育期"),
        labels = c("平均生育期", "对照生育期")
      ) +
      theme_minimal(base_family = font_family) +
      theme(
        plot.title = element_text(family = font_family, hjust = 0.5, size = 32, face = "bold"),
        plot.subtitle = element_text(family = font_family, hjust = 0.5, size = 32),  # 修改为x轴一样大
        axis.title.x = element_text(family = font_family, size = 32, face = "bold"),
        axis.title.y = element_text(family = font_family, size = 32, face = "bold"),
        axis.text.x = element_text(family = font_family, size = 32, angle = 45, hjust = 1, vjust = 1),
        axis.text.y = element_text(family = font_family, size = 32),
        legend.title = element_text(family = font_family, size = 32, face = "bold"),
        legend.text = element_text(family = font_family, size = 32),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.margin = margin(t = 10, b = 10),
        panel.grid.major.y = element_line(linetype = "dashed"),
        panel.grid.minor.y = element_blank(),
        plot.margin = margin(20, 20, 20, 20, "pt")
      ) +
      labs(
        title = "不同地点产量与生育期对比图",
        subtitle = paste("阶段名称:", name),
        x = "测试地点",
        y = "平均亩产 (kg)",
        caption = paste0(
          "数据来源: 多点测试数据 | 样本量: ", paste(unique(stage_data$样本数量), collapse = ","),
          " | 分析时间:", Sys.time()
        )
      ) +
      guides(
        fill = guide_legend(order = 1, override.aes = list(linetype = "blank")),
        color = guide_legend(order = 2, override.aes = list(fill = NA))
      )+
      ggplot2::theme(
        # 底部说明
        plot.caption = ggplot2::element_text(size = 22, hjust = 1)
      )
    
    # 5. 显示图表
    print(p)
    
    # 6. 保存图表到指定输出目录
    output_file <- file.path(out_dir, paste0("yield_growth_chart_", gsub("\\W", "_", name), ".png"))
    ggsave(output_file, plot = p, width = 20, height = 10, dpi = 300,
           bg = "white", device = "png")
    cat(paste("图表已保存为:", output_file, "\n"))
    
    # 7. 返回数据（保存到结果列表）
    result <- stage_data %>%
      dplyr::left_join(control_data_merge %>% dplyr::select(地点, 对照亩产), by = "地点")
    results_list[[name]] <- result
  }
  
  if (length(results_list) == 1) {
    return(invisible(results_list[[1]]))
  } else {
    return(invisible(results_list))
  }
}



# 函数使用说明（终端友好版）
cat(strrep("=", 70), "\n")
cat("函数 generate_yield_growth_chart 使用说明（可批量，stage_name支持向量，含对照生育期和对照亩产，不同产量分组条形图，含产量和生育期标记）\n")
cat(strrep("=", 70), "\n")
cat("功能: 根据指定的一个或多个阶段名称，批量生成各地点的产量柱状图和生育期折线图，产量部分含对照平均产量柱体，图上显示产量、生育期数值。对照生育期用虚线折线，**并在其下方标注整数生育期值**。\n")
cat("\n参数说明:\n")
cat("  stage_name: 字符型/字符向量，指定的一个或多个阶段名称（如 'N25M010', 'N25M020' 等）\n")
cat("  promotion: 数据框，多点测试相关数据，需含“是否对照”字段\n")
cat("  out_dir: 字符型，指定输出目录，默认当前目录\n")
cat("\n使用示例:\n")
cat("  # 1. 单个阶段\n")
cat("  result_data <- generate_yield_growth_chart(stage_name = 'N25M010', promotion = data)\n")
cat("\n")
cat("  # 2. 多个阶段，可批量画图\n")
cat("  results_list <- generate_yield_growth_chart(stage_name = c('N25M010', 'N25M020'), promotion = data)\n")
cat("  print(results_list[['N25M010']])\n")
cat("\n")
cat("  # 3. 指定输出目录\n")
cat("  results_list <- generate_yield_growth_chart(stage_name = c('N25M010', 'N25M020'), promotion = data, out_dir = 'output_dir')\n")
cat("\n")
cat("  # 4. 保存结果到CSV文件\n")
cat("  write.csv(results_list[['N25M010']], paste0('阶段_', 'N25M010', '_分析结果.csv'), row.names = FALSE, fileEncoding = 'UTF-8')\n")
cat(strrep("=", 70), "\n")










