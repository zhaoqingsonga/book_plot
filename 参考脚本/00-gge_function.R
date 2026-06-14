#' @title GGE数据预处理与分析一体化函数
#' @description
#' 针对多环境试验中的GGE分析设计的综合数据预处理与GGE分析R函数，自动完成数据清洗、缺失值与异常值处理、标准化、基础统计与可视化，并保存各类分析、预处理结果。
#'
#' @param gge_input_data data.frame 初始输入的数据框，需包含“地点”、“阶段名称”、“亩产_kg”三列（或类似）。
#' @param output_dir character 输出目录（默认为" GGE输出结果 "），分析和图件将自动保存在该文件夹下。
#'
#' @return 无直接返回值。分析结果将自动输出为Excel、图片与txt等多格式文件。
#'
#' @details
#' 本函数完成数据预处理流程如下：
#' 1. 检查并创建输出目录。
#' 2. 加载或自动安装所需R包，并自动注册中文字体（推荐使用SimHei或黑体）。
#' 3. 变量重命名——将输入数据的“地点”列名映射为env，“阶段名称”->gen，“亩产_kg”->yield。
#' 4. 缺失值（NA）统计与公有基因型筛选，所有环境均出现的基因型才会保留。缺失值自动以“基因型-环境均值”填充。
#' 5. 异常值判别（基于环境均值±3×标准差、农学阈值）与自动修正（以同环境非异常值均值替换），并输出统计。
#' 6. 异常值箱线图可视化，红点标记异常；自动保存至PNG图片（支持中文）。
#' 7. 检查并去除重复的“基因型-环境”组合，重复则均值合并。
#' 8. 构造完整“基因型-环境”产量矩阵，检查缺失，输出统计信息。
#' 9. yield按环境进行标准化（Z-score）。
#' 10. 所有中间表/统计、最终数据和异常值统计汇总输出为Excel。
#' 11. 自动执行GGE分析（metan::gge），主结果保存到txt文本。
#'
#' @seealso \code{\link[metan]{gge}}
#' @export
gge_preprocess_and_return_model <- function(
    gge_input_data,
    output_dir = "GGE输出结果"
) {
  # -- 0. 检查并创建输出目录 --
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # 设定所有输出文件的路径
  save_preproc_path <- file.path(output_dir, "GGE数据预处理结果.xlsx")
  save_result_path  <- file.path(output_dir, "GGE分析完整结果.xlsx")
  p_outlier_plot_path <- file.path(output_dir, "p_outlier_plot.png")
  gge_model_txt_path <- file.path(output_dir, "gge_model.txt")
  
  # -- 1. 环境准备与依赖 --
  pkgs <- c("metan", "tidyr", "dplyr", "writexl", "ggplot2", "ggrepel", "grDevices", "scales")
  for (pkg in pkgs) {
    if (!require(pkg, character.only = TRUE)) install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
  if (!require("extrafont")) install.packages("extrafont")
  library(extrafont)
  
  
  #安装字体 
  if (!requireNamespace("Cairo", quietly = TRUE)) {
    install.packages("Cairo")
  }
  library(Cairo)
  # 检查是否安装了中文字体SimHei，否则使用系统默认
  font_family <- "SimHei"
  if (!(font_family %in% extrafont::fonts())) {
    cat("⚠️ 未检测到系统已安装字体SimHei，将尝试使用'黑体'或系统默认字体。\n")
    if ("黑体" %in% extrafont::fonts()) {
      font_family <- "黑体"
    } else {
      font_family <- "sans"
    }
  }
  
  # -- 2. 数据引入与清洗 --
  data_raw <- gge_input_data
  data_clean <- data_raw %>%
    dplyr::rename(
      env = 地点,
      gen = 阶段名称,
      yield = 亩产_kg
    )
  cat("=== 原始数据基本信息 ===\n")
  cat("数据维度（行×列）：", nrow(data_clean), "×", ncol(data_clean), "\n")
  cat("环境列表：", paste(unique(data_clean$env), collapse = "、"), "\n")
  cat("基因型数量：", length(unique(data_clean$gen)), "\n")
  print(head(data_clean, 3))
  
  # -- 3. 缺失值处理 --
  data_clean <- data_clean %>%
    dplyr::mutate(
      yield = as.numeric(yield),
      is_missing = is.na(yield)
    )
  missing_stats <- data_clean %>%
    dplyr::group_by(env) %>%
    dplyr::summarise(
      总记录数 = n(),
      缺失记录数 = sum(is_missing),
      缺失率 = round(缺失记录数 / 总记录数 * 100, 2),
      基因型数量 = length(unique(gen))
    ) %>%
    dplyr::ungroup()
  cat("\n=== 缺失值与基因型分布统计 ===\n")
  print(missing_stats)
  gen_env_count <- data_clean %>%
    dplyr::group_by(gen) %>%
    dplyr::summarise(存在的环境数 = length(unique(env))) %>%
    dplyr::ungroup()
  total_env <- length(unique(data_clean$env))
  common_gens <- gen_env_count %>%
    dplyr::filter(存在的环境数 == total_env) %>%
    dplyr::pull(gen)
  data_clean <- data_clean %>%
    dplyr::filter(gen %in% common_gens) %>%
    dplyr::select(-is_missing)
  final_missing <- sum(is.na(data_clean$yield))
  cat("\n=== 基因型匹配后的缺失值检查 ===\n")
  cat("删除非共有基因型后，剩余缺失值数量：", final_missing, "\n")
  if (final_missing > 0) {
    data_clean <- data_clean %>%
      group_by(gen, env) %>%
      mutate(yield = ifelse(is.na(yield), mean(yield, na.rm = TRUE), yield)) %>%
      ungroup()
    cat("已用「基因型-环境均值」填充", final_missing, "个缺失值\n")
  }
  
  # -- 4. 异常值检测与处理 --
  outlier_threshold <- data_clean %>%
    group_by(env) %>%
    summarise(
      环境均值 = mean(yield, na.rm = TRUE),
      环境标准差 = sd(yield, na.rm = TRUE),
      下阈值 = 环境均值 - 3 * 环境标准差,
      上阈值 = 环境均值 + 3 * 环境标准差,
      农学下阈值 = 30,
      农学上阈值 = 400
    ) %>%
    ungroup()
  data_clean <- data_clean %>%
    left_join(outlier_threshold %>% select(env, 下阈值, 上阈值, 农学下阈值, 农学上阈值), by = "env") %>%
    mutate(
      is_outlier = ifelse(
        (yield < 下阈值 | yield > 上阈值) & (yield < 农学下阈值 | yield > 农学上阈值),
        TRUE, FALSE
      ),
      yield_clean = ifelse(is_outlier, mean(yield[!is_outlier], na.rm = TRUE), yield)
    )
  outlier_stats <- data_clean %>%
    group_by(env) %>%
    summarise(
      异常值数量 = sum(is_outlier),
      异常值比例 = round(异常值数量 / n() * 100, 2),
      异常值列表 = paste(gen[is_outlier], collapse = ", ")
    ) %>%
    ungroup()
  cat("\n=== 异常值统计 ===\n")
  print(outlier_stats)
  
  # ----- 在此添加每个环境的均值数据以便标注 -----
  mean_stats <- data_clean %>%
    dplyr::group_by(env) %>%
    dplyr::summarise(
      yield_mean = mean(yield, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  
  # -- 5. 异常值可视化箱线图 --
  p_outlier <- ggplot(data_clean, aes(x = env, y = yield)) +
    geom_boxplot(
      aes(fill = factor(env)),
      alpha = 0.8,
      color = "black",
      outlier.shape = NA,
      width = 0.7
    ) +
    geom_point(
      data = subset(data_clean, is_outlier == TRUE),
      aes(x = env, y = yield),
      color = "#e74c3c",
      size = 2,
      alpha = 0.8,
      shape = 19,
      stroke = 0.5
    ) +
    # 在箱线图上标出平均值数据点
    geom_point(
      data = mean_stats,
      aes(x = env, y = yield_mean),
      color = "#2c3e50",
      size = 3,
      shape = 18, # 菱形
      inherit.aes = FALSE
    ) +
    # 添加均值的数值（标签），显示在均值点上方
    geom_text(
      data = mean_stats,
      aes(x = env, y = yield_mean, label = sprintf("%.1f", yield_mean)),
      vjust = -0.7,
      fontface = "bold",
      color = "#1b2631",
      size = 8,
      family = font_family,
      inherit.aes = FALSE
    ) +
    scale_fill_brewer(palette = "Set2", guide = "none") +
    scale_y_continuous(
      expand = expansion(mult = c(0.05, 0.1)),
      labels = comma
    ) +
    labs(
      title = "各环境亩产异常值检测",
      subtitle = "红色实心点为检测出的异常值，菱形为平均值",
      x = "环境（地点）",
      y = "亩产(kg)",
      caption = "注：异常值基于箱线图四分位距(IQR)方法判定"
    ) +
    theme_bw(base_family = font_family) +     # 设置中文字体
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, vjust = 1, size = 19, color = "#333333", family = font_family),
      axis.text.y = element_text(size = 19, color = "#333333", family = font_family),
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5, family = font_family),
      plot.subtitle = element_text(size = 12, hjust = 0.5, color = "#666666", family = font_family),
      axis.title.x = element_text(size = 21, face = "bold", margin = margin(t = 10), family = font_family),
      axis.title.y = element_text(size = 21, face = "bold", margin = margin(r = 10), family = font_family),
      legend.position = "none",
      panel.grid.major = element_line(color = "#f0f0f0"),
      panel.grid.minor = element_blank(),
      plot.margin = margin(15, 15, 15, 15)
    )
  # print(p_outlier)
  
  tryCatch({
    CairoPNG(filename = p_outlier_plot_path, 
             width = 11, 
             height = 8, 
             units = "in", 
             dpi = 300)
    print(p_outlier)
    dev.off()
  }, error = function(e) {
    warning("保存p_outlier_plot图时字体异常，中文可能无法正确显示，或者中文显示为乱码。")
  })
  
  # -- 6. 基因型-环境矩阵完整性验证 --
  duplicate_stats <- data_clean %>%
    group_by(env, gen) %>%
    summarise(重复次数 = n()) %>%
    filter(重复次数 > 1) %>%
    ungroup()
  if (nrow(duplicate_stats) > 0) {
    cat("\n=== 重复记录处理 ===\n")
    print(duplicate_stats)
    data_clean <- data_clean %>%
      group_by(env, gen) %>%
      summarise(
        yield_clean = mean(yield_clean, na.rm = TRUE),
        .groups = "drop"
      )
    cat("已对重复记录取均值去重\n")
  } else {
    cat("\n=== 重复记录检查 ===\n")
    cat("无重复的基因型-环境组合，无需去重\n")
  }
  gxe_matrix <- data_clean %>%
    select(env, gen, yield_clean) %>%
    pivot_wider(
      id_cols = gen,
      names_from = env,
      values_from = yield_clean
    )
  cat("\n=== GxE矩阵完整性验证 ===\n")
  cat("GxE矩阵维度（基因型数×环境数）：", nrow(gxe_matrix), "×", ncol(gxe_matrix) - 1, "\n")
  cat("是否存在缺失值：", ifelse(sum(is.na(gxe_matrix)) > 0, "是", "否"), "\n")
  
  # -- 7. 数据标准化 --
  data_final <- data_clean %>%
    group_by(env) %>%
    mutate(
      yield_standardized = scale(yield_clean)[, 1]
    ) %>%
    ungroup() %>%
    select(env, gen, yield_raw = yield, yield_clean, yield_standardized)
  cat("\n=== 预处理后最终数据预览 ===\n")
  print(head(data_final, 5))
  cat("最终数据维度：", nrow(data_final), "×", ncol(data_final), "\n")
  
  # -- 8. 保存预处理结果 --
  write_xlsx(
    list(
      预处理后数据 = data_final,
      GxE矩阵 = gxe_matrix,
      异常值统计 = outlier_stats
    ),
    path = save_preproc_path
  )
  cat("\n=== 数据预处理完成 ===\n")
  cat("预处理结果已保存至：", save_preproc_path, "\n")
  cat("后续GGE分析可直接使用 data_final 中的 yield_clean 或 yield_standardized\n")
  
  # -- 9. GGE分析与核心结果 --
  cat("=== 预处理后数据核心信息 ===\n")
  print(head(data_final[, c("env", "gen", "yield_clean")]))
  cat("环境数量：", length(unique(data_final$env)), "\n")
  cat("基因型数量：", length(unique(data_final$gen)), "\n")
  cat("是否存在缺失值：", sum(is.na(data_final$yield_clean)), "\n")
  #
  gge_model <- gge(
    .data = data_final,
    env = env,
    gen = gen,
    resp = yield_clean,
    centering = "global",
    scaling = "sd",
    biplot = TRUE
  )
  return(gge_model)
}



#' 从GGE模型结果中提取关键信息并导出到Excel
#'
#' @param gge_model GGE模型对象
#' @param output_dir 输出Excel文件保存的目录
#' @return 返回包含多个工作表的Excel文件路径
export_gge_analysis <- function(gge_model, 
                                output_dir = ".",
                                yield_weight_ratio = 0.7
                                ) {
  # 加载必要的包
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    install.packages("openxlsx")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    install.packages("dplyr")
  }
  library(openxlsx)
  library(dplyr)
  
  # 保证输出目录存在
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  # 构造文件名
  output_file <- file.path(output_dir, "gge_analysis_results.xlsx")
  
  # 创建新的工作簿
  wb <- createWorkbook()
  
  # ==================== 1. 基因型综合评分表 ====================
  cat("正在计算基因型综合评分...\n")
  
  # 计算稳定性得分（标准差法）
  if (!is.null(gge_model$yield_clean$ge_mat)) {
    # 计算基因型在各环境中的标准差
    stability_scores <- apply(gge_model$yield_clean$ge_mat, 1, sd, na.rm = TRUE)
    
    # 创建基因型综合数据框
    genotype_summary <- data.frame(
      基因型 = gge_model$yield_clean$labelgen[1:nrow(gge_model$yield_clean$ge_mat)],  # 取前n个基因型
      平均产量 = gge_model$yield_clean$mean_gen[1:nrow(gge_model$yield_clean$ge_mat)],
      产量排名 = rank(-gge_model$yield_clean$mean_gen[1:nrow(gge_model$yield_clean$ge_mat)]),  # 负号表示降序
      稳定性得分 = stability_scores,
      稳定性排名 = rank(stability_scores),  # 值越小越稳定
      高产性等级 = ifelse(gge_model$yield_clean$mean_gen[1:nrow(gge_model$yield_clean$ge_mat)] > gge_model$yield_clean$grand_mean, 
                     "高产", "低产"),
      稳定性等级 = ifelse(stability_scores < median(stability_scores, na.rm = TRUE), 
                     "稳定", "不稳定"),
      综合得分 = (rank(-gge_model$yield_clean$mean_gen[1:nrow(gge_model$yield_clean$ge_mat)]) * yield_weight_ratio + 
                rank(stability_scores) * (1- yield_weight_ratio))  # 70%权重给产量，30%给稳定性
    )
    
    # 添加具体环境的表现（如果环境数不多）
    if (!is.null(gge_model$yield_clean$ge_mat) && ncol(gge_model$yield_clean$ge_mat) <= 10) {
      env_names <- colnames(gge_model$yield_clean$ge_mat)
      if (is.null(env_names)) {
        env_names <- gge_model$yield_clean$labelenv[1:ncol(gge_model$yield_clean$ge_mat)]
      }
      
      # 添加每个环境的标准化值
      for (i in 1:length(env_names)) {
        col_name <- paste0(env_names[i], "_表现")
        genotype_summary[[col_name]] <- gge_model$yield_clean$ge_mat[, i]
      }
    }
    
    # 按综合得分排序
    genotype_summary <- genotype_summary[order(genotype_summary$综合得分), ]
    
    # 添加工作簿
    addWorksheet(wb, "基因型综合评分")
    writeData(wb, "基因型综合评分", genotype_summary, startRow = 1, startCol = 1)
    
    # 添加标题和说明
    writeData(wb, "基因型综合评分", 
              data.frame(说明 = c(
                "基因型综合评分表",
                paste0("总均值: ", round(gge_model$yield_clean$grand_mean, 2)),
                "稳定性得分: 值越小越稳定",
                "综合得分: 产量排名×0.7 + 稳定性排名×0.3 (值越小越好)",
                "高产性等级: 高于总均值为'高产'，否则为'低产'",
                "稳定性等级: 低于稳定性得分中位数为'稳定'，否则为'不稳定'"
              )), startRow = 1, startCol = ncol(genotype_summary) + 2)
  }
  
  # ==================== 2. 环境特征分析表 ====================
  cat("正在计算环境特征...\n")
  
  if (!is.null(gge_model$yield_clean$coordenv) && !is.null(gge_model$yield_clean$mean_env)) {
    # 计算环境区分力（前两个主成分的向量长度）
    if (ncol(gge_model$yield_clean$coordenv) >= 2) {
      env_distinguishing_power <- sqrt(gge_model$yield_clean$coordenv[, 1]^2 + gge_model$yield_clean$coordenv[, 2]^2)
      
      # 计算环境代表性角度（与平均环境的夹角）
      if (!is.null(gge_model$yield_clean$mean_env)) {
        # 假设平均环境坐标为各环境坐标的均值
        mean_env_coord <- colMeans(gge_model$yield_clean$coordenv[, 1:2])
        
        # 计算角度
        env_angles <- sapply(1:nrow(gge_model$yield_clean$coordenv), function(i) {
          vec1 <- gge_model$yield_clean$coordenv[i, 1:2]
          vec2 <- mean_env_coord
          angle <- acos(sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2))))
          angle * 180 / pi  # 转换为角度
        })
      } else {
        env_angles <- rep(NA, nrow(gge_model$yield_clean$coordenv))
      }
      
      environment_summary <- data.frame(
        环境 = gge_model$yield_clean$labelenv,
        平均产量 = gge_model$yield_clean$mean_env,
        产量排名 = rank(-gge_model$yield_clean$mean_env),
        标准差 = gge_model$yield_clean$scale_val,
        PC1坐标 = gge_model$yield_clean$coordenv[, 1],
        PC2坐标 = gge_model$yield_clean$coordenv[, 2],
        区分力 = env_distinguishing_power,
        区分力排名 = rank(-env_distinguishing_power),
        代表性角度 = env_angles,
        环境类型 = ifelse(gge_model$yield_clean$mean_env > mean(gge_model$yield_clean$mean_env), 
                      "高产环境", "低产环境")
      )
      
      # 添加建议用途
      environment_summary$建议用途 <- sapply(1:nrow(environment_summary), function(i) {
        env <- environment_summary[i, ]
        if (env$区分力 > median(env_distinguishing_power) && env$平均产量 > mean(gge_model$yield_clean$mean_env)) {
          return("高产筛选环境")
        } else if (env$区分力 > median(env_distinguishing_power) && env$平均产量 <= mean(gge_model$yield_clean$mean_env)) {
          return("抗逆筛选环境")
        } else {
          return("稳定性测试环境")
        }
      })
      
      # 添加工作簿
      addWorksheet(wb, "环境特征分析")
      writeData(wb, "环境特征分析", environment_summary, startRow = 1, startCol = 1)
    }
  }
  
  # ==================== 3. 基因型-环境互作矩阵 ====================
  cat("正在整理基因型-环境互作矩阵...\n")
  
  if (!is.null(gge_model$yield_clean$ge_mat)) {
    # 转换互作矩阵为数据框
    ge_interaction <- as.data.frame(gge_model$yield_clean$ge_mat)
    colnames(ge_interaction) <- gge_model$yield_clean$labelenv[1:ncol(gge_model$yield_clean$ge_mat)]
    ge_interaction$基因型 <- gge_model$yield_clean$labelgen[1:nrow(gge_model$yield_clean$ge_mat)]
    
    # 重新排列列，让基因型列在第一列
    ge_interaction <- ge_interaction[, c("基因型", gge_model$yield_clean$labelenv[1:ncol(gge_model$yield_clean$ge_mat)])]
    
    addWorksheet(wb, "基因型-环境互作")
    writeData(wb, "基因型-环境互作", ge_interaction, startRow = 1, startCol = 1)
  }
  
  # ==================== 4. 推荐与淘汰名单 ====================
  cat("正在生成推荐与淘汰名单...\n")
  
  if (exists("genotype_summary")) {
    # 高产稳定型（高产且稳定）
    high_yield_stable <- genotype_summary %>%
      filter(高产性等级 == "高产" & 稳定性等级 == "稳定") %>%
      arrange(综合得分) %>%
      select(基因型, 平均产量, 稳定性排名, 综合得分)
    
    # 高产不稳定型（高产但不稳定）
    high_yield_unstable <- genotype_summary %>%
      filter(高产性等级 == "高产" & 稳定性等级 == "不稳定") %>%
      arrange(稳定性排名) %>%
      select(基因型, 平均产量, 稳定性排名, 综合得分)
    
    # 低产稳定型（低产但稳定）
    low_yield_stable <- genotype_summary %>%
      filter(高产性等级 == "低产" & 稳定性等级 == "稳定") %>%
      arrange(desc(平均产量)) %>%
      select(基因型, 平均产量, 稳定性得分)
    
    # 建议淘汰型（低产且不稳定）
    淘汰候选 <- genotype_summary %>%
      filter(高产性等级 == "低产" & 稳定性等级 == "不稳定") %>%
      arrange(desc(平均产量)) %>%
      select(基因型, 平均产量, 稳定性排名, 综合得分)
    
    # 创建推荐名单工作表
    addWorksheet(wb, "推荐与淘汰名单")
    
    # 写入高产稳定型
    writeData(wb, "推荐与淘汰名单", 
              data.frame(类别 = "推荐: 高产稳定型 (优先推广)"), 
              startRow = 1)
    writeData(wb, "推荐与淘汰名单", high_yield_stable, startRow = 3)
    
    # 写入高产不稳定型
    start_row <- nrow(high_yield_stable) + 5
    writeData(wb, "推荐与淘汰名单", 
              data.frame(类别 = "观察: 高产不稳定型 (需谨慎推广)"), 
              startRow = start_row)
    writeData(wb, "推荐与淘汰名单", high_yield_unstable, startRow = start_row + 2)
    
    # 写入低产稳定型
    start_row <- start_row + nrow(high_yield_unstable) + 5
    writeData(wb, "推荐与淘汰名单", 
              data.frame(类别 = "备用: 低产稳定型 (稳定性好但产量低)"), 
              startRow = start_row)
    writeData(wb, "推荐与淘汰名单", low_yield_stable, startRow = start_row + 2)
    
    # 写入淘汰候选
    start_row <- start_row + nrow(low_yield_stable) + 5
    writeData(wb, "推荐与淘汰名单", 
              data.frame(类别 = "淘汰候选: 低产不稳定型"), 
              startRow = start_row)
    writeData(wb, "推荐与淘汰名单", 淘汰候选, startRow = start_row + 2)
  }
  
  # ==================== 5. 模型摘要信息 ====================
  cat("正在整理模型摘要信息...\n")
  
  model_summary <- data.frame(
    项目 = c("总变异量", "总均值", "基因型数", "环境数", 
           "居中方法", "标准化方法", "奇异值分割方法"),
    值 = c(
      as.character(round(gge_model$yield_clean$totalvar, 2)),
      as.character(round(gge_model$yield_clean$grand_mean, 2)),
      as.character(length(gge_model$yield_clean$labelgen)),
      as.character(length(gge_model$yield_clean$labelenv)),
      gge_model$yield_clean$centering,
      gge_model$yield_clean$scaling,
      gge_model$yield_clean$svp
    )
  )
  
  # 主成分解释方差
  if (!is.null(gge_model$yield_clean$varexpl)) {
    pc_summary <- data.frame(
      主成分 = paste0("PC", 1:length(gge_model$yield_clean$varexpl)),
      特征值 = if (!is.null(gge_model$yield_clean$eigenvalues)) 
        round(gge_model$yield_clean$eigenvalues[1:length(gge_model$yield_clean$varexpl)], 2) else NA,
      解释方差百分比 = round(gge_model$yield_clean$varexpl, 2),
      累计解释方差 = round(cumsum(gge_model$yield_clean$varexpl), 2)
    )
  }
  
  addWorksheet(wb, "模型摘要")
  writeData(wb, "模型摘要", data.frame(标题 = "GGE模型分析摘要"), startRow = 1)
  writeData(wb, "模型摘要", model_summary, startRow = 3)
  
  if (exists("pc_summary")) {
    writeData(wb, "模型摘要", data.frame(标题 = "主成分分析结果"), startRow = nrow(model_summary) + 5)
    writeData(wb, "模型摘要", pc_summary, startRow = nrow(model_summary) + 7)
  }
  
  # ==================== 6. 环境-基因型匹配推荐 ====================
  cat("正在计算环境-基因型匹配推荐...\n")
  
  if (!is.null(gge_model$yield_clean$ge_mat) && exists("environment_summary")) {
    # 为每个环境推荐最佳基因型
    env_recommendations <- list()
    
    for (i in 1:ncol(gge_model$yield_clean$ge_mat)) {
      env_name <- colnames(gge_model$yield_clean$ge_mat)[i]
      if (is.null(env_name)) {
        env_name <- gge_model$yield_clean$labelenv[i]
      }
      
      # 在该环境中表现最好的基因型
      best_in_env <- order(gge_model$yield_clean$ge_mat[, i], decreasing = TRUE)[1:5]
      
      env_recommendations[[env_name]] <- data.frame(
        排名 = 1:5,
        基因型 = genotype_summary$基因型[best_in_env],
        在该环境表现值 = gge_model$yield_clean$ge_mat[best_in_env, i],
        平均产量 = genotype_summary$平均产量[best_in_env],
        稳定性等级 = genotype_summary$稳定性等级[best_in_env]
      )
    }
    
    # 创建环境推荐工作表
    addWorksheet(wb, "环境专用推荐")
    
    start_row <- 1
    for (env_name in names(env_recommendations)) {
      writeData(wb, "环境专用推荐", 
                data.frame(环境 = paste0(env_name, " - 最佳适配基因型")), 
                startRow = start_row)
      writeData(wb, "环境专用推荐", env_recommendations[[env_name]], 
                startRow = start_row + 2)
      start_row <- start_row + nrow(env_recommendations[[env_name]]) + 5
    }
  }
  
  # ==================== 保存Excel文件 ====================
  cat("正在保存Excel文件:", output_file, "...\n")
  saveWorkbook(wb, output_file, overwrite = TRUE)
  
  cat("分析完成! 文件已保存至:", output_file, "\n")
  cat("生成的工作表:\n")
  cat("1. 基因型综合评分\n")
  cat("2. 环境特征分析\n")
  cat("3. 基因型-环境互作\n")
  cat("4. 推荐与淘汰名单\n")
  cat("5. 模型摘要\n")
  cat("6. 环境专用推荐\n")
  #返回高产稳产清单
  return(high_yield_stable)
}

#' 从GGE模型文本文件中读取数据并创建GGE对象
#' 
#' @param file_path GGE模型文本文件路径
#' @return GGE模型对象
read_gge_from_txt <- function(file_path) {
  # 读取文件内容
  lines <- readLines(file_path)
  
  # 初始化GGE对象
  gge_model <- list()
  
  # 提取各个部分
  current_section <- ""
  section_content <- c()
  
  for (line in lines) {
    # 检查是否是新的部分
    if (grepl("^\\$[a-zA-Z_]+", line)) {
      # 保存前一个部分的内容
      if (current_section != "" && length(section_content) > 0) {
        gge_model[[current_section]] <- parse_section(current_section, section_content)
      }
      
      # 开始新的部分
      current_section <- gsub("^\\$", "", line)
      section_content <- c()
    } else if (current_section != "" && line != "") {
      section_content <- c(section_content, line)
    }
  }
  
  # 保存最后一个部分
  if (current_section != "" && length(section_content) > 0) {
    gge_model[[current_section]] <- parse_section(current_section, section_content)
  }
  
  # 设置类属性
  class(gge_model) <- "gge"
  
  return(gge_model)
}

#' 解析各个部分的内容
#' 
#' @param section_name 部分名称
#' @param content 内容向量
#' @return 解析后的对象
parse_section <- function(section_name, content) {
  if (section_name %in% c("coordgen", "coordenv", "ge_mat")) {
    # 解析矩阵
    # 移除空行和注释行
    content <- content[!grepl("^\\s*$", content)]
    content <- content[!grepl("^\\[.*\\]$", content)]
    
    # 转换为数值矩阵
    data_lines <- content
    # 使用正则表达式提取所有数字
    all_numbers <- as.numeric(unlist(strsplit(gsub("\\[|\\]", "", data_lines), "\\s+")))
    all_numbers <- all_numbers[!is.na(all_numbers)]
    
    # 确定行数和列数
    if (section_name == "coordgen") {
      n_rows <- 125  # 从文件得知
      n_cols <- 8
    } else if (section_name == "coordenv") {
      n_rows <- 8
      n_cols <- 8
    } else if (section_name == "ge_mat") {
      n_rows <- 200  # 从文件得知
      n_cols <- 8
    }
    
    # 创建矩阵
    mat <- matrix(all_numbers[1:(n_rows * n_cols)], nrow = n_rows, ncol = n_cols, byrow = TRUE)
    
    return(mat)
    
  } else if (section_name %in% c("eigenvalues", "varexpl")) {
    # 解析数值向量
    nums <- as.numeric(unlist(strsplit(content, "\\s+")))
    return(nums[!is.na(nums)])
    
  } else if (section_name %in% c("labelgen", "labelenv", "labelaxes")) {
    # 解析标签向量
    # 移除方括号和数字
    clean_content <- gsub("\\[\\d+\\]", "", content)
    items <- unlist(strsplit(clean_content, "\\s+"))
    items <- items[items != ""]
    return(items)
    
  } else if (section_name %in% c("totalvar", "grand_mean", "d")) {
    # 解析单个数值
    return(as.numeric(content))
    
  } else if (section_name %in% c("centering", "scaling", "svp")) {
    # 解析字符串
    return(as.character(content))
    
  } else if (section_name %in% c("mean_gen", "mean_env", "scale_val")) {
    # 解析命名向量
    lines <- content
    names <- c()
    values <- c()
    
    for (line in lines) {
      if (grepl("[a-zA-Z]", line)) {
        parts <- unlist(strsplit(line, "\\s+"))
        parts <- parts[parts != ""]
        
        if (length(parts) >= 2) {
          names <- c(names, parts[1])
          values <- c(values, as.numeric(parts[2]))
        }
      }
    }
    
    result <- setNames(values, names)
    return(result)
    
  } else {
    # 其他部分原样返回
    return(content)
  }
}

# 使用示例
if (FALSE) {
  # 示例1: 直接使用现有gge模型对象
  # 假设gge_model是你的gge模型对象
  # export_gge_analysis(gge_model, output_dir = ".")
  
  # 示例2: 从文本文件读取
  # gge_model <- read_gge_from_txt("gge_model.txt")
  # export_gge_analysis(gge_model, output_dir = "your_output_directory")
}

# 快捷函数：直接处理文件
analyze_gge_file <- function(input_file, output_dir = NULL) {
  if (is.null(output_dir)) {
    output_dir <- dirname(input_file)
  }
  cat("正在读取GGE模型文件:", input_file, "...\n")
  gge_model <- read_gge_from_txt(input_file)
  cat("GGE模型读取完成，开始分析...\n")
  
  result_file <- export_gge_analysis(gge_model, output_dir = output_dir)
  return(cat("分析完成! 结果保存至:", result_file, "\n"))
}

# 如果你有名为"gge_model.txt"的文件，可以直接运行：
# analyze_gge_file("gge_model.txt", output_dir = ".")

# 安装必要的包（如果尚未安装）
required_packages <- c("ggplot2", "dplyr", "tidyr", "RColorBrewer", 
                       "ggrepel", "reshape2", "gridExtra", "scales", "viridis")
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# 加载包
library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggrepel)
library(reshape2)
library(gridExtra)
library(scales)
library(viridis)

#' @title GGE结果分析与绘图批量保存
#' @description
#' 封装GGE模型的分析结果，自动生成常用互作可视化大图并批量保存。
#'
#' @param gge_model 已经拟合的GGE模型对象，应至少包含`yield_clean`属性等。
#' @param result_dir 图片保存目录，默认"results"
#' @param biplot_top 双标图高产标注基因型数，默认15
#' @param scatter_top 稳定性-均值散点标注高产基因型数，默认15
#' @param heatmap_top 互作热图显示高产基因型数，默认30
#' @param ranking_top 排名条形+稳定线图展示高产基因型数，默认20
#' 
#' @return 返回一个包含GGE数据和所有绘图对象的不可见列表，并自动保存PNG图片到指定目录。
#' @examples
#' gge_plot_and_save(gge_model, result_dir = "your_output_dir")
#' 
#' @export
gge_plot_and_save <- function(gge_model, result_dir = "results", 
                              biplot_top = 15, 
                              scatter_top = 15,
                              heatmap_top = 30,
                              ranking_top = 20) {
  if (!dir.exists(result_dir)) {
    dir.create(result_dir, recursive = TRUE)
  }
  
  # 数据准备
  prepare_gge_data <- function(gge_model) {
    n_genotypes <- nrow(gge_model$yield_clean$coordgen)  # 基因型数量
    n_environments <- nrow(gge_model$yield_clean$coordenv)  # 环境数量
    
    gen_pc_data <- data.frame(
      Genotype = gge_model$yield_clean$labelgen[1:n_genotypes],
      PC1 = gge_model$yield_clean$coordgen[, 1],
      PC2 = gge_model$yield_clean$coordgen[, 2],
      #PC3 = gge_model$yield_clean$coordgen[, 3],
      Mean_Yield = gge_model$yield_clean$mean_gen[1:n_genotypes]
    )
    env_pc_data <- data.frame(
      Environment = gge_model$yield_clean$labelenv,
      PC1 = gge_model$yield_clean$coordenv[, 1],
      PC2 = gge_model$yield_clean$coordenv[, 2],
      #PC3 = gge_model$yield_clean$coordenv[, 3],
      Mean_Yield = gge_model$yield_clean$mean_env,
      Scale = gge_model$yield_clean$scale_val
    )
    env_pc_data$Vector_Length <- sqrt(env_pc_data$PC1^2 + env_pc_data$PC2^2)
    env_pc_data$Angle <- atan2(env_pc_data$PC2, env_pc_data$PC1) * 180 / pi
    ge_matrix <- as.data.frame(gge_model$yield_clean$ge_mat[1:n_genotypes, ])
    colnames(ge_matrix) <- gge_model$yield_clean$labelenv
    ge_matrix$Genotype <- gge_model$yield_clean$labelgen[1:n_genotypes]
    stability_data <- data.frame(
      Genotype = gge_model$yield_clean$labelgen[1:n_genotypes],
      Mean_Yield = gge_model$yield_clean$mean_gen[1:n_genotypes],
      Stability = apply(gge_model$yield_clean$ge_mat[1:n_genotypes, ], 1, sd, na.rm = TRUE)
    )
    stability_data$Yield_Rank <- rank(-stability_data$Mean_Yield)  # 降序排名
    stability_data$Stability_Rank <- rank(stability_data$Stability)  # 升序排名
    pc_explained <- data.frame(
      PC = paste0("PC", 1:length(gge_model$yield_clean$varexpl)),
      Eigenvalue = gge_model$yield_clean$eigenvalues[1:length(gge_model$yield_clean$varexpl)],
      Variance_Explained = gge_model$yield_clean$varexpl,
      Cumulative_Variance = cumsum(gge_model$yield_clean$varexpl)
    )
    return(list(
      gen_pc_data = gen_pc_data,
      env_pc_data = env_pc_data,
      ge_matrix = ge_matrix,
      stability_data = stability_data,
      pc_explained = pc_explained,
      total_var = gge_model$yield_clean$totalvar,
      grand_mean = gge_model$yield_clean$grand_mean
    ))
  }
  
  gge_data <- prepare_gge_data(gge_model)
  
  plot_gge_biplot <- function(gge_data, highlight_top = 10) {
    gen_data <- gge_data$gen_pc_data
    env_data <- gge_data$env_pc_data
    gen_data$Top_Yield <- ifelse(
      rank(-gen_data$Mean_Yield) <= highlight_top, 
      "Top", 
      "Other")
    p <- ggplot() +
      geom_point(data = gen_data, 
                 aes(x = PC1, y = PC2, color = Mean_Yield, size = Top_Yield)) +
      geom_segment(data = env_data,
                   aes(x = 0, y = 0, xend = PC1 * 0.8, yend = PC2 * 0.8),
                   arrow = arrow(length = unit(0.2, "cm")),
                   color = "red", alpha = 0.7) +
      geom_text_repel(data = env_data,
                      aes(x = PC1 * 0.9, y = PC2 * 0.9, label = Environment),
                      color = "red", size = 4, fontface = "bold") +
      geom_text_repel(data = subset(gen_data, Top_Yield == "Top"),
                      aes(x = PC1, y = PC2, label = Genotype),
                      color = "blue", size = 3, max.overlaps = 20) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
      scale_color_gradientn(
        colors = rev(brewer.pal(11, "RdYlBu")),
        name = "平均产量",
        limits = c(min(gen_data$Mean_Yield), max(gen_data$Mean_Yield))
      ) +
      scale_size_manual(values = c(Top = 3, Other = 2)) +
      labs(
        title = "GGE双标图: 基因型与环境互作分析",
        subtitle = paste("PC1解释方差:", round(gge_data$pc_explained$Variance_Explained[1], 1), "%",
                         "| PC2解释方差:", round(gge_data$pc_explained$Variance_Explained[2], 1), "%"),
        x = paste0("PC1 (", round(gge_data$pc_explained$Variance_Explained[1], 1), "%)"),
        y = paste0("PC2 (", round(gge_data$pc_explained$Variance_Explained[2], 1), "%)")
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "right",
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12, face = "bold")
      )
    return(p)
  }
  
  plot_stability_yield_scatter <- function(gge_data, highlight_top = 15) {
    data <- gge_data$stability_data
    median_yield <- median(data$Mean_Yield, na.rm = TRUE)
    median_stability <- median(data$Stability, na.rm = TRUE)
    data$Category <- case_when(
      data$Mean_Yield >= median_yield & data$Stability <= median_stability ~ "高产稳定",
      data$Mean_Yield >= median_yield & data$Stability > median_stability ~ "高产不稳",
      data$Mean_Yield < median_yield & data$Stability <= median_stability ~ "低产稳定",
      data$Mean_Yield < median_yield & data$Stability > median_stability ~ "低产不稳"
    )
    data$Highlight <- ifelse(
      rank(-data$Mean_Yield) <= highlight_top,
      "高产基因型",
      "其他"
    )
    p <- ggplot(data, aes(x = Mean_Yield, y = Stability)) +
      geom_hline(yintercept = median_stability, 
                 linetype = "dashed", 
                 color = "gray50", 
                 alpha = 0.7) +
      geom_vline(xintercept = median_yield, 
                 linetype = "dashed", 
                 color = "gray50", 
                 alpha = 0.7) +
      geom_point(aes(color = Category, size = Highlight)) +
      geom_text_repel(
        data = subset(data, Highlight == "高产基因型"),
        aes(label = Genotype),
        size = 5,
        max.overlaps = 20,
        box.padding = 0.5,
        segment.color = "gray50",
        segment.alpha = 0.5
      ) +
      scale_color_manual(
        values = c(
          "高产稳定" = "#2E8B57",    # 绿色
          "高产不稳" = "#FF8C00",    # 橙色
          "低产稳定" = "#1E90FF",    # 蓝色
          "低产不稳" = "#DC143C"     # 红色
        )
      ) +
      scale_size_manual(values = c("高产基因型" = 4, "其他" = 2)) +
      labs(
        title = "基因型稳定性与产量关系图",
        subtitle = paste("总基因型数:", nrow(data), 
                         "| 总平均产量:", round(gge_data$grand_mean, 2)),
        x = "平均产量",
        y = "稳定性得分（标准差，越小越稳定）",
        color = "基因型类别",
        caption = paste("中位数线：产量 =", round(median_yield, 2), 
                        "，稳定性 =", round(median_stability, 3))
      ) +
      annotate("text", 
               x = max(data$Mean_Yield) * 0.85, 
               y = min(data$Stability) * 0.9, 
               label = "高产稳定", 
               color = "#2E8B57", size = 5, fontface = "bold") +
      annotate("text", 
               x = max(data$Mean_Yield) * 0.85, 
               y = max(data$Stability) * 0.9, 
               label = "高产不稳", 
               color = "#FF8C00", size = 5, fontface = "bold") +
      annotate("text", 
               x = min(data$Mean_Yield) * 0.85, 
               y = min(data$Stability) * 0.9, 
               label = "低产稳定", 
               color = "#1E90FF", size = 5, fontface = "bold") +
      annotate("text", 
               x = min(data$Mean_Yield) * 0.85, 
               y = max(data$Stability) * 0.9, 
               label = "低产不稳", 
               color = "#DC143C", size = 5, fontface = "bold") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 18),
        legend.position = "right",
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        axis.text = element_text(size = 18),
        axis.title = element_text(size = 20, face = "bold")
      )
    return(p)
  }
  
  plot_environment_analysis <- function(gge_data) {
    env_data <- gge_data$env_pc_data
    radar_data <- env_data %>%
      dplyr::select(Environment, Mean_Yield, Vector_Length, Scale) %>%
      dplyr::mutate(
        Mean_Yield_scaled = rescale(Mean_Yield, to = c(0, 100)),
        Vector_Length_scaled = rescale(Vector_Length, to = c(0, 100)),
        Scale_scaled = rescale(Scale, to = c(0, 100))
      )
    p1 <- ggplot(env_data, aes(x = reorder(Environment, Vector_Length), y = Vector_Length)) +
      geom_bar(stat = "identity", aes(fill = Vector_Length), width = 0.7) +
      geom_text(aes(label = round(Vector_Length, 2)), 
                vjust = -0.5, size = 3, fontface = "bold") +
      scale_fill_gradient(low = "#D6EAF8", high = "#2E86C1", name = "区分力") +
      labs(title = "环境区分力分析",
           subtitle = "向量长度表示区分基因型的能力",
           x = "环境",
           y = "区分力（向量长度）") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(hjust = 0.5, face = "bold"))
    p2 <- ggplot(env_data, aes(x = Mean_Yield, y = Vector_Length, label = Environment)) +
      geom_point(aes(size = Scale, color = Mean_Yield), alpha = 0.8) +
      geom_text_repel(size = 4, fontface = "bold") +
      scale_color_gradient(low = "#F1948A", high = "#2E86C1", name = "平均产量") +
      scale_size_continuous(name = "标准差", range = c(3, 10)) +
      labs(title = "环境产量与区分力关系",
           subtitle = "点大小表示环境内变异程度",
           x = "环境平均产量",
           y = "区分力（向量长度）") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    p3 <- ggplot(env_data, aes(x = PC1, y = PC2, label = Environment)) +
      geom_point(aes(size = Mean_Yield, color = Vector_Length), alpha = 0.8) +
      geom_text_repel(size = 4, fontface = "bold", box.padding = 0.5) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
      geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
      scale_color_gradient(low = "#FAD7A0", high = "#E74C3C", name = "区分力") +
      scale_size_continuous(name = "平均产量", range = c(5, 15)) +
      labs(title = "环境在主成分空间中的分布",
           subtitle = paste("PC1解释:", round(gge_data$pc_explained$Variance_Explained[1], 1), "%",
                            "| PC2解释:", round(gge_data$pc_explained$Variance_Explained[2], 1), "%"),
           x = paste0("PC1 (", round(gge_data$pc_explained$Variance_Explained[1], 1), "%)"),
           y = paste0("PC2 (", round(gge_data$pc_explained$Variance_Explained[2], 1), "%)")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    combined_plot <- grid.arrange(p1, p2, p3, ncol = 2,
                                  layout_matrix = rbind(c(1, 2), c(3, 3)))
    return(combined_plot)
  }
  
  plot_ge_heatmap <- function(gge_data, top_n = 30) {
    ge_data <- gge_data$ge_matrix
    yield_ranks <- gge_data$stability_data %>%
      arrange(desc(Mean_Yield)) %>%
      head(top_n)
    heatmap_data <- ge_data %>%
      filter(Genotype %in% yield_ranks$Genotype) %>%
      melt(id.vars = "Genotype", variable.name = "Environment", value.name = "Value")
    heatmap_data$Genotype <- factor(
      heatmap_data$Genotype,
      levels = yield_ranks$Genotype
    )
    p <- ggplot(heatmap_data, aes(x = Environment, y = Genotype, fill = Value)) +
      geom_tile(color = "white", size = 0.5) +
      geom_text(aes(label = round(Value, 1)), 
                color = "black", size = 2.5) +
      scale_fill_gradient2(
        low = "#D73027",    # 红色（表现差）
        mid = "#FFFFBF",    # 黄色（中等）
        high = "#1A9850",   # 绿色（表现好）
        midpoint = 0,
        name = "标准化值",
        breaks = seq(-5, 5, by = 1),
        labels = seq(-5, 5, by = 1)
      ) +
      labs(
        title = paste("基因型-环境互作热图（前", top_n, "个高产基因型）"),
        subtitle = "正值（绿色）表示表现优于环境平均水平，负值（红色）表示表现低于平均水平",
        x = "环境",
        y = "基因型",
        caption = paste("数据已标准化，总基因型数:", nrow(gge_data$ge_matrix))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 8),
        legend.position = "right",
        panel.grid = element_blank(),
        panel.border = element_rect(color = "gray80", fill = NA, size = 0.5)
      ) +
      guides(fill = guide_colorbar(
        barwidth = 1,
        barheight = 15,
        title.position = "top",
        title.hjust = 0.5
      ))
    return(p)
  }
  
  plot_genotype_ranking <- function(gge_data, top_n = 20) {
    data <- gge_data$stability_data
    top_data <- data %>%
      arrange(desc(Mean_Yield)) %>%
      head(top_n) %>%
      mutate(
        Genotype = factor(Genotype, levels = Genotype[order(Mean_Yield)]),
        Category = case_when(
          Yield_Rank <= 5 ~ "顶级高产",
          Yield_Rank <= 10 ~ "高产",
          TRUE ~ "中高产"
        )
      )
    p <- ggplot(top_data, aes(x = reorder(Genotype, Mean_Yield), y = Mean_Yield)) +
      geom_bar(aes(fill = Category), stat = "identity", width = 0.7) +
      geom_text(aes(label = round(Mean_Yield, 1)), 
                hjust = -0.2, size = 3, fontface = "bold") +
      geom_line(aes(x = as.numeric(reorder(Genotype, Mean_Yield)), 
                    y = Stability * 10, group = 1),
                color = "#E74C3C", size = 1.5, alpha = 0.8) +
      geom_point(aes(x = as.numeric(reorder(Genotype, Mean_Yield)), 
                     y = Stability * 10),
                 color = "#E74C3C", size = 3) +
      geom_text(aes(x = as.numeric(reorder(Genotype, Mean_Yield)), 
                    y = Stability * 10 + 2,
                    label = round(Stability, 2)),
                color = "#E74C3C", size = 2.5,nudge_y = 2) +
      scale_fill_manual(
        values = c(
          "顶级高产" = "#2E86C1",
          "高产" = "#3498DB",
          "中高产" = "#85C1E9"
        )
      ) +
      scale_y_continuous(
        name = "平均产量",
        sec.axis = sec_axis(~./10, name = "稳定性得分（越小越稳定）")
      ) +
      labs(
        title = paste("前", top_n, "个高产基因型排名"),
        subtitle = "条形：平均产量 | 红线：稳定性得分（右侧Y轴）",
        x = "基因型",
        caption = paste("总基因型数:", nrow(data), 
                        "| 总平均产量:", round(gge_data$grand_mean, 2))
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
        axis.text.y = element_text(size = 10),
        axis.title.y.left = element_text(color = "#2E86C1", size = 16, face = "bold"),
        axis.title.y.right = element_text(color = "#E74C3C", size = 16, face = "bold"),
        axis.text.y.right = element_text(color = "#E74C3C"),
        legend.position = "bottom",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(color = "gray80", fill = NA, size = 0.5)
      ) +
      coord_flip(ylim = c(0, max(top_data$Mean_Yield) * 1.15))
    return(p)
  }
  
  # 生成并保存所有图片
  biplot_static <- plot_gge_biplot(gge_data, highlight_top = biplot_top)
  print(biplot_static)
  ggsave(file.path(result_dir, "GGE_Biplot.png"), biplot_static, width = 12, height = 8, dpi = 300)
  
  stability_scatter_static <- plot_stability_yield_scatter(gge_data, highlight_top = scatter_top)
  print(stability_scatter_static)
  ggsave(file.path(result_dir, "Stability_Yield_Scatter.png"), stability_scatter_static, width = 12, height = 8, dpi = 300)
  
  env_plot <- plot_environment_analysis(gge_data)
  print(env_plot)
  ggsave(file.path(result_dir, "Environment_Analysis.png"), env_plot, width = 14, height = 12, dpi = 300)
  
  heatmap_static <- plot_ge_heatmap(gge_data, top_n = heatmap_top)
  print(heatmap_static)
  ggsave(file.path(result_dir, "GE_Heatmap.png"), heatmap_static, width = 14, height = 10, dpi = 300)
  
  ranking_static <- plot_genotype_ranking(gge_data, top_n = ranking_top)
  print(ranking_static)
  ggsave(file.path(result_dir, "Genotype_Ranking.png"), ranking_static, width = 12, height = 10, dpi = 300)
  
  invisible(list(
    gge_data = gge_data,
    plots = list(biplot = biplot_static,
                 stability_scatter = stability_scatter_static,
                 env_plot = env_plot,
                 heatmap = heatmap_static,
                 ranking = ranking_static)
  ))
}


# 使用案例
# gge_plot_and_save(gge_model, result_dir = "your_output_dir")





