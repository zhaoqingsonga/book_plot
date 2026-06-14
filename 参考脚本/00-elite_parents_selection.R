#' 作物育种亲本与组合筛选分析工具包
#'
#' 本工具包封装了作物育种中"优良亲本"和"优良亲本组合"的筛选流程，
#' 通过"晋级率"和"产量"双维度筛选核心材料，支持可视化展示和结果导出，
#' 适用于育种试验数据的快速分析与决策支持。
#'
#' @details 核心流程：数据预处理 → 亲本表现分析 → 组合表现分析 → 可视化 → 结果保存
#' @author 自动生成（基于原代码封装）
#' @import dplyr ggplot2 tidyr gridExtra utils
#' @export
NULL

# ================= 加载依赖包函数 =================
#' 加载分析所需依赖包
#'
#' 自动检查并安装缺失的依赖包，然后加载所有必需包（dplyr、ggplot2、tidyr、gridExtra）
#'
#' @return 无返回值（隐形加载包）
#' @examples
#' \dontrun{
#' # 加载依赖包（首次使用需安装，后续直接加载）
#' load_required_packages()
#' }
#' @export
load_required_packages <- function() {
  required_pkgs <- c("dplyr", "ggplot2", "tidyr", "gridExtra","ggrepel")
  new_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[,"Package"])]
  if (length(new_pkgs) > 0) {
    install.packages(new_pkgs, dependencies = TRUE)
  }
  invisible(lapply(required_pkgs, library, character.only = TRUE))
  cat("✅ 所有依赖包已加载完成！\n")
}

# ================= 数据预处理函数 =================
#' 育种数据预处理（提取亲本信息+标记晋级状态）
#'
#' 从原始育种数据中筛选有效亲本记录，标记品种"是否晋级"，构建统一格式的"亲本组合"
#'
#' @param raw_data 数据框，原始育种试验数据（必需列：阶段名称、母本、父本、亩产_kg）
#' @param promoted_data 数据框，晋级材料数据（必需列：阶段名称，用于匹配晋级状态）
#'
#' @return 数据框，预处理后的数据集，包含列：阶段名称、母本、父本、亩产_kg、晋级状态、亲本组合
#'
#' @section 数据过滤规则：
#' 自动过滤母本/父本缺失（NA或空字符串）的无效记录，确保后续分析准确性
#'
#' @examples
#' \dontrun{
#' # 假设已有原始数据 promotion_clean 和晋级数据 G25E
#' preprocessed_df <- preprocess_parent_data(
#'   raw_data = promotion_clean,
#'   promoted_data = G25E
#' )
#' head(preprocessed_df)
#' }
#' @export
preprocess_parent_data <- function(raw_data, promoted_data) {
  # 输入列名检查
  if (!all(c("阶段名称", "母本", "父本", "亩产_kg") %in% colnames(raw_data))) {
    stop("❌ 原始数据缺少必需列！需包含：阶段名称、母本、父本、亩产_kg")
  }
  if (!"阶段名称" %in% colnames(promoted_data)) {
    stop("❌ 晋级数据缺少必需列！需包含：阶段名称")
  }
  
  parent_analysis <- raw_data |>
    dplyr::select(阶段名称, 母本, 父本, 亩产_kg) |>
    dplyr::mutate(
      晋级状态 = ifelse(阶段名称 %in% promoted_data$阶段名称, "晋级", "未晋级"),
      亲本组合 = paste(母本, 父本, sep = "×")  # 统一亲本组合格式（母本×父本）
    ) |>
    dplyr::filter(!is.na(母本) & !is.na(父本) & 母本 != "" & 父本 != "")
  
  cat(sprintf("✅ 数据预处理完成！有效记录数：%d\n", nrow(parent_analysis)))
  return(parent_analysis)
}

# ================= 优良亲本分析函数 =================
#' 优良亲本筛选与表现统计
#'
#' 分别统计母本和父本的表现（总配制品种数、晋级率、平均亩产），
#' 按"高晋级率+高产量"双标准筛选优良亲本
#'
#' @param preprocessed_data 数据框，预处理后的数据集（preprocess_parent_data 输出）
#' @param min_crosses 整数，有效亲本的最小配制品种数（避免偶然结果），默认3
#' @param top_pct 数值，优良亲本的晋级率分位数阈值（0-1），默认0.8（即Top20%）
#'
#' @return 列表，包含4个元素：
#' \itemize{
#'   \item all_parent_stats 数据框：所有有效亲本的完整统计结果
#'   \item excellent_parents 数据框：筛选出的优良亲本（含优良等级标记）
#'   \item parent_avg_yield 数值：所有有效亲本的平均亩产（筛选基准）
#'   \item top_percentile 数值：晋级率分位数阈值（筛选基准）
#' }
#'
#' @section 优良亲本筛选规则：
#' 1. 晋级率 ≥ 所有有效亲本的 top_pct 分位数（默认Top20%）
#' 2. 平均亩产 ≥ 所有有效亲本的平均亩产
#'
#' @examples
#' \dontrun{
#' # 基于预处理数据进行亲本分析（调整参数：最小配制4个品种，Top15%晋级率）
#' parent_res <- analyze_excellent_parents(
#'   preprocessed_data = preprocessed_df,
#'   min_crosses = 4,
#'   top_pct = 0.85
#' )
#' # 查看优良亲本
#' print(parent_res$excellent_parents)
#' }
#' @export
analyze_excellent_parents <- function(preprocessed_data, min_crosses = 3, top_pct = 0.8) {
  # 合并母本和父本的统计结果
  parent_stats <- dplyr::bind_rows(
    # 母本视角统计
    preprocessed_data |>
      dplyr::group_by(亲本类型 = "母本", 亲本名称 = 母本) |>
      dplyr::summarise(
        总配制品种数 = dplyr::n(),
        晋级品种数 = sum(晋级状态 == "晋级"),
        晋级率 = 晋级品种数 / 总配制品种数,
        平均亩产 = mean(亩产_kg, na.rm = TRUE),
        .groups = "drop"
      ),
    # 父本视角统计
    preprocessed_data |>
      dplyr::group_by(亲本类型 = "父本", 亲本名称 = 父本) |>
      dplyr::summarise(
        总配制品种数 = dplyr::n(),
        晋级品种数 = sum(晋级状态 == "晋级"),
        晋级率 = 晋级品种数 / 总配制品种数,
        平均亩产 = mean(亩产_kg, na.rm = TRUE),
        .groups = "drop"
      )
  ) |>
    dplyr::filter(总配制品种数 >= min_crosses) |>  # 筛选有效亲本
    dplyr::arrange(dplyr::desc(晋级率), dplyr::desc(平均亩产)) |>  # 排序
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))  # 数值列保留3位小数
  
  # 计算筛选基准
  parent_avg_yield <- mean(parent_stats$平均亩产, na.rm = TRUE)
  top_percentile <- quantile(parent_stats$晋级率, top_pct, na.rm = TRUE)
  
  # 筛选优良亲本
  excellent_parents <- parent_stats |>
    dplyr::filter(
      晋级率 >= top_percentile,
      平均亩产 >= parent_avg_yield
    ) |>
    dplyr::mutate(优良等级 = "优良亲本")
  
  cat(sprintf("✅ 亲本分析完成！有效亲本数：%d，优良亲本数：%d\n", nrow(parent_stats), nrow(excellent_parents)))
  return(list(
    all_parent_stats = parent_stats,
    excellent_parents = excellent_parents,
    parent_avg_yield = parent_avg_yield,
    top_percentile = top_percentile
  ))
}


# ================= 优良组合分析函数 =================
#' 优良亲本组合筛选与表现统计
#'
#' 统计"母本×父本"组合的表现（配制品种数、晋级率、平均亩产），
#' 按"高晋级率+高产量"双标准筛选优良组合，支持灵活调整产量筛选阈值
#'
#' @param preprocessed_data 数据框，预处理后的数据集（preprocess_parent_data 输出）
#' @param promoted_data 数据框，晋级材料数据（必需列：亩产_kg，用于计算产量基准）
#' @param min_crosses 整数，有效组合的最小配制品种数（避免偶然结果），默认2
#' @param promote_rate_thresh 数值，优良组合的晋级率阈值（0-1），默认0.5（即50%）
#' @param yield_threshold_adjust 数值，产量阈值调节系数（0-∞），默认1.0；
#'   作用：最终产量筛选基准 = 晋级材料平均亩产 × 该系数
#'
#' @return 列表，包含3个元素：
#' \itemize{
#'   \item all_cross_stats 数据框：所有有效组合的完整统计结果
#'   \item excellent_crosses 数据框：筛选出的优良组合（含优良等级标记）
#'   \item promoted_avg_yield 数值：晋级材料的原始平均亩产（未乘调节系数）
#'   \item adjusted_yield_threshold 数值：调整后的最终产量筛选基准（用于结果解读）
#' }
#'
#' @section 优良组合筛选规则：
#' 1. 晋级率 ≥ promote_rate_thresh（默认≥50%，即至少一半后代晋级）
#' 2. 平均亩产 ≥ 晋级材料平均亩产 × yield_threshold_adjust（调整后的产量基准）
#'
#' @section 产量调节系数使用说明：
#' - 当晋级材料产量太高，选不出优良组合时：设为 0.8-0.9（降低产量门槛）
#' - 需严格筛选高产组合时：设为 1.1-1.2（提高产量门槛）
#' - 保持原逻辑不变：维持默认值 1.0
#'
#' @examples
#' \dontrun{
#' # 示例1：晋级材料产量太高，降低产量门槛（仅需达到晋级平均亩产的85%）
#' cross_res <- analyze_excellent_crosses(
#'   preprocessed_data = preprocessed_df,
#'   promoted_data = G25E,
#'   promote_rate_thresh = 0.5,
#'   yield_threshold_adjust = 0.85  # 降低产量筛选标准
#' )
#'
#' # 示例2：严格筛选高产组合（需达到晋级平均亩产的110%）
#' cross_res <- analyze_excellent_crosses(
#'   preprocessed_data = preprocessed_df,
#'   promoted_data = G25E,
#'   promote_rate_thresh = 0.5,
#'   yield_threshold_adjust = 1.1  # 提高产量筛选标准
#' )
#'
#' # 查看优良组合及调整后的产量基准
#' print(cross_res$excellent_crosses)
#' cat(sprintf("调整后的产量筛选基准：%.2f kg\n", cross_res$adjusted_yield_threshold))
#' }
#' @export
analyze_excellent_crosses <- function(preprocessed_data,
                                      promoted_data, 
                                      min_crosses = 2,
                                      promote_rate_thresh = 0.5,
                                      yield_threshold_adjust = 1.0) {
  # 输入列名检查
  if (!"亩产_kg" %in% colnames(promoted_data)) {
    stop("❌ 晋级数据缺少必需列！需包含：亩产_kg")
  }
  
  # 输入参数合理性检查（产量调节系数不能为负数）
  if (yield_threshold_adjust <= 0) {
    stop("❌ 产量调节系数 yield_threshold_adjust 必须大于0！")
  }
  
  # 统计亲本组合表现
  cross_stats <- preprocessed_data |>
    dplyr::group_by(亲本组合, 母本, 父本) |>
    dplyr::summarise(
      配制品种数 = dplyr::n(),
      晋级品种数 = sum(晋级状态 == "晋级"),
      晋级率 = 晋级品种数 / 配制品种数,
      平均亩产 = mean(亩产_kg, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(配制品种数 >= min_crosses) |>  # 筛选有效组合
    dplyr::arrange(dplyr::desc(晋级率), dplyr::desc(平均亩产)) |>  # 排序
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))  # 数值列保留3位小数
  
  # 计算原始产量基准 + 调整后的最终产量基准
  promoted_avg_yield <- mean(promoted_data$亩产_kg, na.rm = TRUE)
  adjusted_yield_threshold <- promoted_avg_yield * yield_threshold_adjust  # 核心修改：应用调节系数
  
  # 筛选优良组合（使用调整后的产量基准）
  excellent_crosses <- cross_stats |>
    dplyr::filter(
      晋级率 >= promote_rate_thresh,
      平均亩产 >= adjusted_yield_threshold  # 核心修改：替换为调整后的阈值
    ) |>
    dplyr::mutate(优良等级 = "优良组合")
  
  # 打印结果提示（含调整后的产量基准，方便用户解读）
  cat(sprintf("✅ 组合分析完成！\n"))
  cat(sprintf(" - 有效组合数：%d，优良组合数：%d\n", nrow(cross_stats), nrow(excellent_crosses)))
  cat(sprintf(" - 晋级材料原始平均亩产：%.2f kg\n", promoted_avg_yield))
  cat(sprintf(" - 产量调节系数：%.2f\n", yield_threshold_adjust))
  cat(sprintf(" - 最终产量筛选基准：%.2f kg\n", adjusted_yield_threshold))
  
  # 返回结果（新增 adjusted_yield_threshold 便于后续查看）
  return(list(
    all_cross_stats = cross_stats,
    excellent_crosses = excellent_crosses,
    promoted_avg_yield = promoted_avg_yield,
    adjusted_yield_threshold = adjusted_yield_threshold
  ))
}



# ================= 可视化结果函数 =================
#' 分析结果可视化（亲本+组合图表）
#'
#' 生成两张核心图表：亲本表现散点图（突出优良亲本）、TopN优良组合柱状图，
#' 组合后保存为高清图片（300dpi），支持调整边距和文字偏移避免标签溢出
#'
#' @param parent_analysis_res 列表，亲本分析结果（analyze_excellent_parents 输出）
#' @param cross_analysis_res 列表，组合分析结果（analyze_excellent_crosses 输出）
#' @param top_crosses 整数，可视化展示的Top优良组合数量，默认10
#' @param save_path 字符串，图表保存路径（含文件名），默认"./亲本组合分析结果.png"
#' @param plot_margin 数值向量，图表内边距（上、右、下、左），单位：pt，默认c(30, 60, 50, 40)；
#'   标签右侧溢出→增大第2个值（右 margin）；底部溢出→增大第3个值（下 margin）；左侧溢出→增大第4个值
#' @param parent_label_vjust 数值，亲本散点图标签垂直偏移量，默认-2.2；
#'   标签超出顶部→减小该值（如-2.5）；标签与点重叠→增大该值（如-1.8）
#' @param cross_label_vjust 数值，组合柱状图标签垂直偏移量，默认-0.6；
#'   标签超出顶部→减小该值（如-0.8）；标签与柱子重叠→增大该值（如-0.4）
#'
#' @return 组合图表对象（gridExtra::grid.arrange 输出，可直接打印展示）
#'
#' @section 图表说明：
#' 1. 亲本散点图：X轴=晋级率，Y轴=平均亩产，红色菱形=优良亲本，虚线=筛选基准线
#' 2. 组合柱状图：按晋级率降序排列，颜色深浅对应晋级率，标注晋级率和亩产
#'
#' @section 标签溢出调整指南：
#' | 溢出场景                | 调整参数                | 调整方向                  |
#' |-------------------------|-------------------------|---------------------------|
#' | 亲本名称/基准线标签右侧溢出 | plot_margin[2]（右 margin） | 从60→80/90                |
#' | 组合名称底部溢出         | plot_margin[3]（下 margin） | 从50→70/90                |
#' | 亲本标签超出图表顶部     | parent_label_vjust       | 从-2.2→-2.5/-3.0          |
#' | 柱状图标签超出顶部       | cross_label_vjust        | 从-0.6→-0.8/-1.0          |
#' | 标签与点/柱子重叠        | 对应 vjust 参数          | 增大（如-2.2→-1.8）       |
#'
#' @examples
#' \dontrun{
#' # 示例1：默认参数（优化后布局，无重叠溢出）
#' plot_obj <- plot_analysis_results(
#'   parent_analysis_res = parent_res,
#'   cross_analysis_res = cross_res,
#'   top_crosses = 15,
#'   save_path = "./自定义_亲本组合分析图.png"
#' )
#'
#' # 示例2：标签右侧溢出+底部溢出，调整边距
#' plot_obj <- plot_analysis_results(
#'   parent_analysis_res = parent_res,
#'   cross_analysis_res = cross_res,
#'   top_crosses = 15,
#'   save_path = "./自定义_亲本组合分析图.png",
#'   plot_margin = c(30, 80, 70, 40),  # 增大右、下 margin
#'   parent_label_vjust = -2.5          # 亲本标签上移，避免顶部溢出
#' )
#'
#' # 直接展示图表
#' print(plot_obj)
#' }
#' @export
plot_analysis_results <- function(parent_analysis_res, cross_analysis_res, 
                                  top_crosses = 10, save_path = "./亲本组合分析结果.png",
                                  plot_margin = c(30, 60, 50, 40),  # 优化默认边距（适配标签）
                                  parent_label_vjust = -2.2,       # 优化亲本标签垂直偏移
                                  cross_label_vjust = -0.6,
                                  width=12,
                                  height=12) {      # 优化柱状图标签垂直偏移
  
  # 1. 亲本表现散点图（优化标签防重叠+布局）
  parent_plot <- ggplot2::ggplot(parent_analysis_res$all_parent_stats, 
                                 ggplot2::aes(x = 晋级率, y = 平均亩产, color = 亲本类型)) +
    # 普通亲本点（稍放大，增强视觉）
    ggplot2::geom_point(alpha = 0.6, size = 4.5) +
    # 优良亲本点（红色菱形，大幅放大，突出重点）
    #ggplot2::geom_point(data = parent_analysis_res$excellent_parents, 
    # color = "#E74C3C", size = 4, shape = 18, stroke = 1.2) +
    # 优良亲本名称标签（防重叠+高对比度）
    ggplot2::geom_text(
      data = parent_analysis_res$excellent_parents, 
      ggplot2::aes(label = 亲本名称), 
      vjust = parent_label_vjust, 
      hjust = 0.5,
      size = 7, 
      fontface = "bold",
      color = "#2C3E50",  # 深灰色标签，高对比度不刺眼
      check_overlap = TRUE  # 自动隐藏重叠标签（核心防重叠）
    ) +
    # 产量基准线（加粗，增强视觉层次）
    ggplot2::geom_hline(
      yintercept = parent_analysis_res$parent_avg_yield, 
      linetype = "dashed", 
      color = "#34495E", 
      alpha = 0.9, 
      linewidth = 1.2
    ) +
    # 晋级率基准线（加粗，增强视觉层次）
    ggplot2::geom_vline(
      xintercept = parent_analysis_res$top_percentile, 
      linetype = "dashed", 
      color = "#34495E", 
      alpha = 0.9, 
      linewidth = 1.2
    ) +
    # 基准线标签（移至空白处，避免重叠）
    ggplot2::annotate(
      "text", 
      x = parent_analysis_res$top_percentile, 
      y = max(parent_analysis_res$all_parent_stats$平均亩产, na.rm = TRUE) * 1.06,  # 顶部空白区
      label = paste0("晋级率Top", round((1 - parent_analysis_res$top_percentile)*100), "%"),
      vjust = 0, 
      hjust = 0.5, 
      size = 5.5, 
      fontface = "italic",
      color = "#2C3E50"
    ) +
    ggplot2::annotate(
      "text", 
      x = min(parent_analysis_res$all_parent_stats$晋级率, na.rm = TRUE) * 0.92,  # 左侧空白区
      y = parent_analysis_res$parent_avg_yield, 
      label = paste0("平均亩产：", round(parent_analysis_res$parent_avg_yield, 1), "kg"),
      vjust = 0.5, 
      hjust = 1, 
      size = 5.5, 
      fontface = "italic",
      color = "#2C3E50"
    ) +
    # 图表标签（清晰醒目）
    ggplot2::labs(
      title = "亲本晋级表现分析",
      x = "晋级率",
      y = "平均亩产（kg）",
      color = "亲本类型"
    ) +
    # 主题优化（统一风格+宽松布局）
    ggplot2::theme_bw() +
    ggplot2::theme(
      # 标题（居中+加粗+宽松间距）
      plot.title = ggplot2::element_text(
        hjust = 0.5, 
        size = 22, 
        face = "bold", 
        margin = ggplot2::margin(b = 25),
        color = "#2C3E50"
      ),
      # 坐标轴标题（加粗+放大）
      axis.title = ggplot2::element_text(
        size = 19, 
        face = "bold",
        color = "#2C3E50"
      ),
      # 坐标轴刻度文字（加粗+放大，易读）
      axis.text = ggplot2::element_text(
        size = 17, 
        face = "bold",
        color = "#34495E"
      ),
      # 图例（放大+清晰）
      legend.title = ggplot2::element_text(
        size = 18, 
        face = "bold",
        color = "#2C3E50"
      ),
      legend.text = ggplot2::element_text(
        size = 17, 
        face = "bold",
        color = "#34495E"
      ),
      legend.key.size = ggplot2::unit(1.5, "cm"),  # 图例放大，方便查看
      legend.position = "top",  # 图例移至顶部，避免遮挡散点
      legend.margin = ggplot2::margin(b = 15),  # 图例底部间距
      # 图表边距（宽松，避免标签溢出）
      plot.margin = ggplot2::margin(
        plot_margin[1], 
        plot_margin[2], 
        plot_margin[3], 
        plot_margin[4], 
        unit = "pt"
      ),
      # 背景网格（淡化，不干扰数据）- 修复：移除alpha，用浅灰色实现淡化
      panel.grid = ggplot2::element_line(color = "#E0E0E0")
    )
  
  # 2. TopN优良组合柱状图（优化标签对齐+可读性）
  top_excellent_crosses <- cross_analysis_res$excellent_crosses |>
    dplyr::arrange(dplyr::desc(晋级率)) |>
    dplyr::slice_head(n = top_crosses)  # 取TopN组合
  
  cross_plot <- ggplot2::ggplot(top_excellent_crosses,
                                ggplot2::aes(x = reorder(亲本组合, 晋级率), y = 晋级率, fill = 晋级率)) +
    # 柱状图（加粗边框+高透明度）
    ggplot2::geom_col(
      alpha = 0.85, 
      linewidth = 1.2, 
      color = "#2C3E50"  # 深灰色边框，区分柱子
    ) +
    # 柱状图标签（统一位置+高对比度+易读）
    ggplot2::geom_text(
      ggplot2::aes(label = paste0("晋级率:", round(晋级率*100, 1), "%\n亩产:", round(平均亩产, 1), "kg")),
      hjust = 0.5, 
      vjust = cross_label_vjust, 
      size = 6.8, 
      fontface = "bold",
      color = "white",  # 白色标签，与彩色柱子形成强反差
      lineheight = 0.9  # 减小行间距，避免标签过长
    ) +
    # 颜色渐变（优化配色，更美观）
    ggplot2::scale_fill_gradient(
      low = "#27AE60", 
      high = "#E74C3C",
      name = "晋级率"
    ) +
    # 图表标签（清晰醒目）
    ggplot2::labs(
      title = sprintf("Top%d优良亲本组合晋级率", top_crosses),
      x = "亲本组合（母本×父本）",
      y = "晋级率",
      fill = "晋级率"
    ) +
    # 主题优化（统一风格+宽松布局）
    ggplot2::theme_bw() +
    ggplot2::theme(
      # 标题（居中+加粗+宽松间距）
      plot.title = ggplot2::element_text(
        hjust = 0.5, 
        size = 22, 
        face = "bold", 
        margin = ggplot2::margin(b = 30),
        color = "#2C3E50"
      ),
      # 坐标轴标题（加粗+放大）
      axis.title = ggplot2::element_text(
        size = 19, 
        face = "bold",
        color = "#2C3E50"
      ),
      # X轴标签（增大旋转角度+宽松对齐，避免截断）
      axis.text.x = ggplot2::element_text(
        angle = 60, 
        hjust = 1.1, 
        vjust = 1.1,
        size = 15, 
        face = "bold",
        color = "#34495E"
      ),
      # Y轴刻度文字（加粗+放大）
      axis.text.y = ggplot2::element_text(
        size = 17, 
        face = "bold",
        color = "#34495E"
      ),
      # 图例（放大+清晰）
      legend.title = ggplot2::element_text(
        size = 18, 
        face = "bold",
        color = "#2C3E50"
      ),
      legend.text = ggplot2::element_text(
        size = 17, 
        face = "bold",
        color = "#34495E"
      ),
      legend.key.size = ggplot2::unit(1.5, "cm"),
      legend.position = "top",  # 图例移至顶部，避免遮挡柱子
      legend.margin = ggplot2::margin(b = 15),
      # 图表边距（宽松，避免标签溢出）
      plot.margin = ggplot2::margin(
        plot_margin[1], 
        plot_margin[2], 
        plot_margin[3], 
        plot_margin[4], 
        unit = "pt"
      ),
      # 背景网格（淡化，不干扰数据）- 修复：移除alpha，用浅灰色实现淡化
      panel.grid = ggplot2::element_line(color = "#E0E0E0")
    )
  
  # 组合图表（优化高度比，适配信息密度）
  combined_plot <- gridExtra::grid.arrange(
    parent_plot, 
    cross_plot, 
    ncol = 1, 
    heights = c(1, 1.4)  # 柱状图占比更高（1.4倍），适配多柱子+标签
  )
  
  # 保存图表（高清+兼容中文）
  ggplot2::ggsave(
    filename = save_path, 
    plot = combined_plot, 
    width = width,  # 加宽，容纳X轴长标签
    height = height,  # 加高，避免上下图表拥挤
    dpi = 300,
    device = "png",  # 明确指定图片设备，避免兼容问题
    type = "cairo-png"  # 优化中文显示，避免乱码
  )
  
  cat(sprintf("✅ 可视化图表已保存至：%s\n", save_path))
  return(combined_plot)
}



# ================= 保存结果到XLSX函数（openxlsx终极兼容版） =================
#' 保存分析结果为XLSX文件（Excel格式，基于openxlsx）
#'
#' 将筛选出的优良亲本和优良组合分别保存为XLSX文件，支持中文，兼容所有版本openxlsx，
#' 可直接用Excel/WPS打开，无需转码
#'
#' @param parent_analysis_res 列表，亲本分析结果（analyze_excellent_parents 输出）
#' @param cross_analysis_res 列表，组合分析结果（analyze_excellent_crosses 输出）
#' @param parent_save_path 字符串，优良亲本XLSX保存路径，默认"./优良亲本分析结果.xlsx"
#'
#' @return 无返回值（直接保存文件）
#'
#' @section 保存说明：
#' - 格式：XLSX（Excel 2007+ 兼容）
#' - 基础优化：表头加粗，列宽自适应内容
#' - 支持中文：原生兼容中文，无乱码问题
#' - 不含行号，保留所有统计列和"优良等级"标记
#' - 依赖包：openxlsx（无需Java环境，兼容所有版本）
#'
#' @examples
#' \dontrun{
#' # 保存分析结果（自定义XLSX路径）
#' save_analysis_results(
#'   parent_analysis_res = parent_res,
#'   cross_analysis_res = cross_res,
#'   parent_save_path = "./自定义_优良亲本.xlsx",
#'   cross_save_path = "./自定义_优良组合.xlsx"
#' )
#' }
#' @export
save_analysis_results <- function(parent_analysis_res, cross_analysis_res, 
                                  save_path) {
  # 检查并安装依赖包 openxlsx
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    cat("ℹ️  缺少依赖包 'openxlsx'，正在自动安装...\n")
    utils::install.packages("openxlsx", dependencies = TRUE)
    if (!requireNamespace("openxlsx", quietly = TRUE)) {
      stop("❌ 包 'openxlsx' 安装失败，请手动执行：install.packages('openxlsx')")
    }
  }
  
  # ---------------------- 简化样式：仅保留表头加粗（兼容所有版本） ----------------------
  # 低版本openxlsx仅支持 textDecoration = "bold"（表头加粗）
  header_style <- openxlsx::createStyle(
    textDecoration = "bold"  # 仅保留“字体加粗”，去掉所有可能不兼容的参数
  )
  
  # ---------------------- 保存优良亲本及优良组合----------------------
  # 创建工作簿
  parent_wb <- openxlsx::createWorkbook()
  # 添加工作表（默认名称"优良亲本"）
  openxlsx::addWorksheet(parent_wb, sheetName = "优良亲本")
  openxlsx::addWorksheet(parent_wb, sheetName = "优良组合")
  # 写入数据（rowNames = FALSE 不保留行号）
  openxlsx::writeData(parent_wb, sheet = "优良亲本", x = parent_analysis_res$excellent_parents, rowNames = FALSE)
  # 写入数据
  openxlsx::writeData(parent_wb, sheet = "优良组合", x = cross_analysis_res$excellent_crosses, rowNames = FALSE)
  # 应用表头样式（第1行加粗）
  openxlsx::addStyle(
    wb = parent_wb,
    sheet = "优良亲本",
    style = header_style,
    rows = 1,  # 表头行
    cols = 1:ncol(parent_analysis_res$excellent_parents),  # 所有列
    gridExpand = TRUE  # 自动适配列数
  )
  
  # 列宽自适应（避免内容截断，核心功能）
  openxlsx::setColWidths(
    wb = parent_wb,
    sheet = "优良亲本",
    cols = 1:ncol(parent_analysis_res$excellent_parents),
    widths = "auto"
  )

  # 应用相同表头样式（加粗）
  openxlsx::addStyle(
    wb = parent_wb,
    sheet = "优良组合",
    style = header_style,
    rows = 1,
    cols = 1:ncol(cross_analysis_res$excellent_crosses),
    gridExpand = TRUE
  )
  
  # 列宽自适应
  openxlsx::setColWidths(
    wb = parent_wb,
    sheet = "优良组合",
    cols = 1:ncol(cross_analysis_res$excellent_crosses),
    widths = "auto"
  )
  # 保存工作簿（overwrite = TRUE 允许覆盖已有文件）
  openxlsx::saveWorkbook(parent_wb, file = save_path, overwrite = TRUE)
  
  # 打印保存成功信息
  cat(sprintf("✅ 分析结果已保存为XLSX文件"))}













#############
#待研究
##############
#' 亲本表现分析函数（支持优良/劣等亲本筛选）
#'
#' 从母本和父本两个视角，统计亲本的杂交配制次数、晋级率和平均亩产，筛选出优良亲本（推荐后续杂交使用）或劣等亲本（建议避免使用）。
#' 优良亲本基于晋级率分位数和平均亩产均值筛选，劣等亲本采用对称的反向筛选规则，确保分析逻辑的一致性。
#'
#' @param preprocessed_data 预处理后的完整数据集（data.frame/tibble）
#'   必需列：`母本`（母本名称）、`父本`（父本名称）、`晋级状态`（是否晋级，取值为"晋级"或其他）、`亩产_kg`（产量数据，数值型）
#'
#' @param min_crosses 有效亲本的最低杂交次数阈值（数值型，默认=3）
#'   仅筛选总配制品种数≥该阈值的亲本，排除杂交次数过少、统计结果不可靠的亲本
#'
#' @param top_pct 优良亲本的晋级率分位数比例（数值型，默认=0.8）
#'   取值范围为(0,1)，表示取有效亲本晋级率的第 top_pct 分位数作为优良亲本的晋级率基准；
#'   劣等亲本的晋级率分位数自动计算为 `1 - top_pct`（与优良亲本对称）
#'
#' @param select_type 筛选类型（字符型，默认="excellent"）
#'   可选值：
#'   - "excellent"：筛选优良亲本（推荐后续杂交使用）
#'   - "inferior"：筛选劣等亲本（建议后续杂交避免）
#'
#' @return 列表（list），包含以下元素：
#'   \item{all_parent_stats}{tibble，所有有效亲本（杂交次数≥min_crosses）的完整统计数据，包含列：
#'     `亲本类型`（"母本"或"父本"）、`亲本名称`、`总配制品种数`、`晋级品种数`、`晋级率`、`平均亩产`（数值列保留3位小数）
#'   }
#'   \item{target_parents}{tibble，筛选出的目标亲本（优良/劣等），在all_parent_stats基础上新增`亲本等级`列（标记"优良亲本"或"劣等亲本"）}
#'   \item{parent_avg_yield}{数值型，所有有效亲本的平均亩产均值（产量筛选基准）}
#'   \item{top_percentile}{数值型，优良亲本的晋级率分位数（有效亲本晋级率的第 top_pct 分位数）}
#'   \item{bottom_percentile}{数值型，劣等亲本的晋级率分位数（有效亲本晋级率的第 (1-top_pct) 分位数）}
#'   \item{select_type}{字符型，本次筛选类型（"excellent"或"inferior"）}
#'   \item{target_label}{字符型，目标亲本标签（"优良亲本"或"劣等亲本"）}
#'
#' @details
#' 筛选逻辑说明：
#' 1. 数据合并：分别统计母本和父本的表现，合并为统一的亲本统计数据集
#' 2. 有效亲本筛选：筛选总配制品种数≥min_crosses的亲本，确保统计可靠性
#' 3. 筛选基准计算：
#'    - 产量基准：所有有效亲本的平均亩产均值（parent_avg_yield）
#'    - 晋级率基准：优良亲本取晋级率第 top_pct 分位数（top_percentile），劣等亲本取第 (1-top_pct) 分位数（bottom_percentile）
#' 4. 目标亲本筛选：
#'    - 优良亲本：晋级率≥top_percentile 且 平均亩产≥parent_avg_yield
#'    - 劣等亲本：晋级率≤bottom_percentile 且 平均亩产≤parent_avg_yield
#'
#' @examples
#' \dontrun{
#' # 示例1：筛选优良亲本（默认参数）
#' # 假设已准备好预处理数据集 preprocessed_data
#' excellent_parents <- analyze_parents(
#'   preprocessed_data = preprocessed_data,
#'   min_crosses = 3,
#'   top_pct = 0.8,
#'   select_type = "excellent"
#' )
#' # 查看优良亲本列表
#' print(excellent_parents$target_parents)
#'
#' # 示例2：筛选劣等亲本（用于后续杂交规避）
#' inferior_parents <- analyze_parents(
#'   preprocessed_data = preprocessed_data,
#'   min_crosses = 3,  # 仅保留杂交次数≥3的有效亲本
#'   top_pct = 0.7,    # 优良亲本取前30%分位数，劣等亲本取后30%分位数
#'   select_type = "inferior"
#' )
#' # 查看劣等亲本列表（建议避免使用）
#' print(inferior_parents$target_parents)
#' }
#'
#' @export
analyze_parents <- function(preprocessed_data, min_crosses = 3, top_pct = 0.8, select_type = "excellent") {
  # 验证select_type参数有效性
  if (!select_type %in% c("excellent", "inferior")) {
    stop("select_type参数只能是 'excellent'（优良亲本） 或 'inferior'（劣等亲本）")
  }
  
  # 合并母本和父本的统计结果
  parent_stats <- dplyr::bind_rows(
    # 母本视角统计
    preprocessed_data |>
      dplyr::group_by(亲本类型 = "母本", 亲本名称 = 母本) |>
      dplyr::summarise(
        总配制品种数 = dplyr::n(),
        晋级品种数 = sum(晋级状态 == "晋级"),
        晋级率 = 晋级品种数 / 总配制品种数,
        平均亩产 = mean(亩产_kg, na.rm = TRUE),
        .groups = "drop"
      ),
    # 父本视角统计
    preprocessed_data |>
      dplyr::group_by(亲本类型 = "父本", 亲本名称 = 父本) |>
      dplyr::summarise(
        总配制品种数 = dplyr::n(),
        晋级品种数 = sum(晋级状态 == "晋级"),
        晋级率 = 晋级品种数 / 总配制品种数,
        平均亩产 = mean(亩产_kg, na.rm = TRUE),
        .groups = "drop"
      )
  ) |>
    dplyr::filter(总配制品种数 >= min_crosses) |>  # 筛选有效亲本（杂交次数达标）
    dplyr::arrange(dplyr::desc(晋级率), dplyr::desc(平均亩产)) |>  # 排序
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))  # 数值列保留3位小数
  
  # 计算筛选基准（与原逻辑一致）
  parent_avg_yield <- mean(parent_stats$平均亩产, na.rm = TRUE)
  top_percentile <- quantile(parent_stats$晋级率, top_pct, na.rm = TRUE)
  bottom_percentile <- quantile(parent_stats$晋级率, 1 - top_pct, na.rm = TRUE)  # 劣等亲本晋级率分位数（与优良对应）
  
  # 根据select_type筛选目标亲本
  if (select_type == "excellent") {
    # 优良亲本：晋级率≥top_pct分位数 且 平均亩产≥整体均值
    target_parents <- parent_stats |>
      dplyr::filter(
        晋级率 >= top_percentile,
        平均亩产 >= parent_avg_yield
      ) |>
      dplyr::mutate(亲本等级 = "优良亲本")
    target_label <- "优良亲本"
  } else {
    # 劣等亲本：晋级率≤(1-top_pct)分位数 且 平均亩产≤整体均值（与优良条件相反）
    target_parents <- parent_stats |>
      dplyr::filter(
        晋级率 <= bottom_percentile,
        平均亩产 <= parent_avg_yield
      ) |>
      dplyr::mutate(亲本等级 = "劣等亲本")
    target_label <- "劣等亲本"
  }
  
  # 输出统计信息
  cat(sprintf("✅ 亲本分析完成！\n"))
  cat(sprintf("📊 有效亲本数（杂交次数≥%d）：%d\n", min_crosses, nrow(parent_stats)))
  cat(sprintf("🏆 %s数：%d\n", target_label, nrow(target_parents)))
  cat(sprintf("📈 筛选基准 - 平均亩产均值：%.3f kg，%s晋级率分位数：%.3f\n",
              parent_avg_yield,
              ifelse(select_type == "excellent", "优良", "劣等"),
              ifelse(select_type == "excellent", top_percentile, bottom_percentile)))
  
  # 返回结果（包含所有统计信息，方便后续使用）
  return(list(
    all_parent_stats = parent_stats,          # 所有有效亲本的统计数据
    target_parents = target_parents,          # 筛选出的目标亲本（优良/劣等）
    parent_avg_yield = parent_avg_yield,      # 所有有效亲本的平均亩产均值
    top_percentile = top_percentile,          # 优良亲本晋级率分位数
    bottom_percentile = bottom_percentile,    # 劣等亲本晋级率分位数
    select_type = select_type,                # 本次筛选类型
    target_label = target_label               # 目标亲本标签
  ))
}


#' 亲本组合分析函数（支持优良/劣等组合筛选）
#'
#' 基于杂交组合的配制次数、晋级率和平均亩产，筛选出优良组合（推荐后续杂交使用）或劣等组合（建议避免使用）。
#' 保留原有的产量调节系数功能，劣等组合采用与优良组合对称的筛选规则，确保筛选逻辑的一致性和合理性。
#'
#' @param preprocessed_data 预处理后的完整数据集（data.frame/tibble）
#'   必需列：`亲本组合`（组合名称）、`母本`（母本名称）、`父本`（父本名称）、`晋级状态`（是否晋级，取值为"晋级"或其他）、`亩产_kg`（产量数据，数值型）
#'
#' @param promoted_data 仅包含晋级材料的数据集（data.frame/tibble）
#'   必需列：`亩产_kg`（用于计算晋级材料的平均亩产，作为产量筛选基准）
#'
#' @param min_crosses 有效组合的最低配制次数阈值（数值型，默认=2）
#'   仅筛选配制品种数≥该阈值的组合，确保统计结果的可靠性
#'
#' @param promote_rate_thresh 优良组合的晋级率最低阈值（数值型，默认=0.5）
#'   取值范围为(0,1)，劣等组合的晋级率阈值会自动计算为 `1 - promote_rate_thresh`（与优良组合对称）
#'
#' @param yield_threshold_adjust 产量调节系数（数值型，默认=1.0）
#'   用于调整产量筛选基准：
#'   - 优良组合：产量基准 = 晋级材料平均亩产 × 该系数
#'   - 劣等组合：产量基准 = 晋级材料平均亩产 ÷ 该系数
#'   系数必须大于0，系数>1时会提高优良组合的产量要求，同时降低劣等组合的产量要求（反之亦然）
#'
#' @param select_type 筛选类型（字符型，默认="excellent"）
#'   可选值：
#'   - "excellent"：筛选优良组合（推荐使用）
#'   - "inferior"：筛选劣等组合（建议避免）
#'
#' @return 列表（list），包含以下元素：
#'   \item{all_cross_stats}{tibble，所有有效组合（配制次数≥min_crosses）的完整统计数据，包含列：
#'     `亲本组合`、`母本`、`父本`、`配制品种数`、`晋级品种数`、`晋级率`、`平均亩产`（数值列保留3位小数）
#'   }
#'   \item{target_crosses}{tibble，筛选出的目标组合（优良/劣等），在all_cross_stats基础上新增`组合等级`列（标记"优良组合"或"劣等组合"）}
#'   \item{promoted_avg_yield}{数值型，晋级材料的平均亩产（产量基准计算基础）}
#'   \item{adjusted_yield_threshold}{数值型，优良组合的产量筛选基准（晋级材料平均亩产 × 调节系数）}
#'   \item{inferior_yield_threshold}{数值型，劣等组合的产量筛选基准（晋级材料平均亩产 ÷ 调节系数）}
#'   \item{promote_rate_thresh}{数值型，优良组合的晋级率阈值（输入参数原值）}
#'   \item{inferior_promote_rate_thresh}{数值型，劣等组合的晋级率阈值（1 - promote_rate_thresh）}
#'   \item{select_type}{字符型，本次筛选类型（"excellent"或"inferior"）}
#'   \item{target_label}{字符型，目标组合标签（"优良组合"或"劣等组合"）}
#'
#' @details
#' 筛选逻辑说明：
#' 1. 有效组合筛选：首先筛选出配制次数≥min_crosses的组合（排除样本量过少的组合）
#' 2. 基准计算：基于晋级材料的平均亩产和输入参数，计算优良/劣等组合的筛选基准
#' 3. 目标组合筛选：
#'    - 优良组合：晋级率≥promote_rate_thresh 且 平均亩产≥adjusted_yield_threshold
#'    - 劣等组合：晋级率≤inferior_promote_rate_thresh 且 平均亩产≤inferior_yield_threshold
#'
#' @examples
#' \dontrun{
#' # 示例1：筛选优良组合（默认参数，与原函数逻辑一致）
#' # 假设已准备好 preprocessed_data（完整数据）和 promoted_data（晋级数据）
#' excellent_result <- analyze_crosses(
#'   preprocessed_data = preprocessed_data,
#'   promoted_data = promoted_data,
#'   min_crosses = 2,
#'   promote_rate_thresh = 0.5,
#'   yield_threshold_adjust = 1.0
#' )
#' # 查看优良组合
#' print(excellent_result$target_crosses)
#'
#' # 示例2：筛选劣等组合（用于后续杂交规避）
#' inferior_result <- analyze_crosses(
#'   preprocessed_data = preprocessed_data,
#'   promoted_data = promoted_data,
#'   min_crosses = 2,
#'   promote_rate_thresh = 0.6,  # 优良组合晋级率≥0.6，劣等组合≤0.4
#'   yield_threshold_adjust = 1.1,  # 优良组合亩产≥1.1×均值，劣等组合≤均值/1.1
#'   select_type = "inferior"
#' )
#' # 查看劣等组合（建议避免使用）
#' print(inferior_result$target_crosses)
#' }
#'
#' @export
analyze_crosses <- function(preprocessed_data,
                            promoted_data, 
                            min_crosses = 2,
                            promote_rate_thresh = 0.5,
                            yield_threshold_adjust = 1.0,
                            select_type = "excellent") {
  # 输入列名检查
  if (!"亩产_kg" %in% colnames(promoted_data)) {
    stop("❌ 晋级数据缺少必需列！需包含：亩产_kg")
  }
  
  # 输入参数合理性检查
  if (yield_threshold_adjust <= 0) {
    stop("❌ 产量调节系数 yield_threshold_adjust 必须大于0！")
  }
  if (!select_type %in% c("excellent", "inferior")) {
    stop("❌ select_type参数只能是 'excellent'（优良组合） 或 'inferior'（劣等组合）")
  }
  
  # 统计亲本组合表现（与原逻辑一致）
  cross_stats <- preprocessed_data |>
    dplyr::group_by(亲本组合, 母本, 父本) |>
    dplyr::summarise(
      配制品种数 = dplyr::n(),
      晋级品种数 = sum(晋级状态 == "晋级"),
      晋级率 = 晋级品种数 / 配制品种数,
      平均亩产 = mean(亩产_kg, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::filter(配制品种数 >= min_crosses) |>  # 筛选有效组合（配制次数达标）
    dplyr::arrange(dplyr::desc(晋级率), dplyr::desc(平均亩产)) |>  # 排序
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ round(.x, 3)))  # 数值列保留3位小数
  
  # 计算产量基准（保留原有的调节系数逻辑）
  promoted_avg_yield <- mean(promoted_data$亩产_kg, na.rm = TRUE)
  adjusted_yield_threshold <- promoted_avg_yield * yield_threshold_adjust  # 调整后产量基准
  
  # 定义劣等组合的筛选阈值（与优良组合对称）
  inferior_promote_rate_thresh <- 1 - promote_rate_thresh  # 晋级率阈值（例如优良≥0.5，则劣等≤0.5）
  inferior_yield_threshold <- promoted_avg_yield / yield_threshold_adjust  # 产量阈值（与优良对称：若优良是×系数，劣等则是÷系数）
  
  # 根据select_type筛选目标组合
  if (select_type == "excellent") {
    # 优良组合：晋级率≥阈值 且 平均亩产≥调整后产量基准（原逻辑）
    target_crosses <- cross_stats |>
      dplyr::filter(
        晋级率 >= promote_rate_thresh,
        平均亩产 >= adjusted_yield_threshold
      ) |>
      dplyr::mutate(组合等级 = "优良组合")
    target_label <- "优良组合"
  } else {
    # 劣等组合：晋级率≤对称阈值 且 平均亩产≤对称产量基准（与优良条件相反）
    target_crosses <- cross_stats |>
      dplyr::filter(
        晋级率 <= inferior_promote_rate_thresh,
        平均亩产 <= inferior_yield_threshold
      ) |>
      dplyr::mutate(组合等级 = "劣等组合")
    target_label <- "劣等组合"
  }
  
  # 输出统计信息（优化显示，区分两种组合类型）
  cat(sprintf("✅ 组合分析完成！（筛选类型：%s）\n", target_label))
  cat(sprintf(" - 有效组合数（配制次数≥%d）：%d\n", min_crosses, nrow(cross_stats)))
  cat(sprintf(" - %s数：%d\n", target_label, nrow(target_crosses)))
  cat(sprintf(" - 晋级材料平均亩产：%.2f kg\n", promoted_avg_yield))
  cat(sprintf(" - 产量调节系数：%.2f\n", yield_threshold_adjust))
  
  # 分别显示两种组合的筛选基准
  if (select_type == "excellent") {
    cat(sprintf(" - 筛选基准：晋级率≥%.3f，平均亩产≥%.2f kg\n",
                promote_rate_thresh, adjusted_yield_threshold))
  } else {
    cat(sprintf(" - 筛选基准：晋级率≤%.3f，平均亩产≤%.2f kg\n",
                inferior_promote_rate_thresh, inferior_yield_threshold))
  }
  
  # 返回结果（包含所有关键参数和统计数据）
  return(list(
    all_cross_stats = cross_stats,                # 所有有效组合的统计数据
    target_crosses = target_crosses,              # 筛选出的目标组合（优良/劣等）
    promoted_avg_yield = promoted_avg_yield,      # 晋级材料平均亩产
    adjusted_yield_threshold = adjusted_yield_threshold,  # 优良组合产量基准
    inferior_yield_threshold = inferior_yield_threshold,  # 劣等组合产量基准
    promote_rate_thresh = promote_rate_thresh,    # 优良组合晋级率阈值
    inferior_promote_rate_thresh = inferior_promote_rate_thresh,  # 劣等组合晋级率阈值
    select_type = select_type,                    # 本次筛选类型
    target_label = target_label                   # 目标组合标签
  ))
}





