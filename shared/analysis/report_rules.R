# ==============================================================================
# 试验报告口径规则表 —— 机器可判定口径规则的**单一真相源**
# ------------------------------------------------------------------------------
# 口径规则原先住在 skill 文档（39 条陷阱），算数住在管线代码里，两边必然漂。
# 本文件把其中 ~20 条「确定性可判定」的规则搬进代码，与算数同处一地；
# 剩下 19 条真正需要人判断的仍留在 skill 散文里。
#
# 结构：规则 = 一张表（叫什么 / 什么级别 / 怎么写进报告）+ 一组命名谓词（怎么判）
#   · 纯代码分支 → 规则无法被自检器逐条枚举、无法生成 skill 文档
#   · 纯配置     → 判定需要真代码（NA 语义、位次并列、1.5×IQR），DSL 会变成第二个语言
#   · 混合       → 规则可枚举、判定可单测、措辞与判定分离（改措辞不动代码）
#
# 依赖：shared/analysis/analysis_screening.R（control_flag）
# ==============================================================================

# 口径判定上下文（ctx）字段全表 —— 供规则作者对照，避免写错字段名
#   n_plots / n_material / n_ck / n_ck_valid / ck_mean
#   n_with_yield / n_na_yield / n_missing_material / missing_material_ids
#   outlier_ck_ids / outlier_ck_n / n_grade_na_rows / n_increase_na_rows
#   max_avg_ck_rank / screening_threshold / parent_threshold
#   promoted_n / segregated_n / select_plant_n / eliminated_n
#   elim_rank_fail / elim_separated / elim_lodging / elim_unknown / elim_overlap
#   n_no_eval / trait_na_cols / stage_eval_empty / sparse_trait_cols
#   n_grain_traits_na / n_unknown_parent / n_reps / meta_material_n
#   n_plots_dropped / dropped_plots_reason / screening_closed_total

# ------------------------------------------------------------------------------
# 图表注册表：result$plots 的键 ↔ PNG 文件名 ↔ 分诊依赖字段
# ------------------------------------------------------------------------------
# 分诊单元就是 result$plots 的 16 个键，与落盘的 16 张 PNG 一一对应。
# 「逐张打开 PNG 看」这一步完全不需要 —— 判据全部来自数据。
RPT_PLOT_REGISTRY <- data.frame(
  id          = c(
    "fig_yield_dist", "fig_yield_grade", "fig_increase", "fig_growth",
    "fig_scatter_growth", "fig_scatter_height", "fig_scatter_grain",
    "fig_corr_matrix",
    "fig_quality_huase", "fig_quality_yexing", "fig_quality_jiejiaxixing",
    "fig_quality_daofuxing", "fig_quality_rongmaose",
    "fig_comparison", "fig_radar", "fig_parent_plot"),
  plot_key    = c(
    "yield_dist", "yield_grade_dist", "increase_dist", "growth_dist",
    "scatter_growth", "scatter_height", "scatter_grain",
    "corr_matrix",
    "quality_花色", "quality_叶形", "quality_结荚习性",
    "quality_倒伏性", "quality_茸毛色",
    "comparison", "radar", "parent_plot"),
  png_name    = c(
    "亩产分布.png", "产量等级分布.png", "增产分布.png", "生育期分布.png",
    "生育期vs产量.png", "株高vs产量.png", "百粒重vs产量.png",
    "性状相关性矩阵.png",
    "性状分布_花色.png", "性状分布_叶形.png", "性状分布_结荚习性.png",
    "性状分布_倒伏性.png", "性状分布_茸毛色.png",
    "筛选前后性状对比.png", "雷达图.png", "亲本晋级表现.png"),
  title       = c(
    "小区产量分布", "产量等级分布", "增产率分布", "生育期分布",
    "生育期与亩产关系", "株高与亩产关系", "百粒重与亩产关系",
    "性状相关性矩阵",
    "花色分布", "叶形分布", "结荚习性分布", "倒伏性分布", "茸毛色分布",
    "筛选前后性状对比", "晋级材料综合表现雷达图", "亲本晋级表现"),
  # 依赖的源字段（拼音列名）。整列为空 ⇒ 该图必空白
  source_cols = c(
    "MuChan", "MuChan", "JiaoPingJunDuiZhaoZengChan", "ShengYuQi",
    "ShengYuQi", "ZhuGao", "BaiLiZhong",
    "ShengYuQi;ZhuGao;BaiLiZhong",
    "HuaSe", "YeXing", "JieJiaXiXing", "DaoFuXing", "RongMaoSe",
    "MuChan", "MuChan", "JiaoPingJunDuiZhaoWeiCi"),
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------------------------
# 谓词注册表：每个谓词接收 ctx，返回逻辑值
# ------------------------------------------------------------------------------
RPT_PREDICATES <- list(

  # ① 无对照小区 —— 晋级通道整体不成立
  pf_no_control = function(ctx) ctx$n_ck == 0,

  # ② 室内考种字段整列全空 ⇒ 依赖它的图渲染出来是空白（必然剔除，非事故）
  pf_trait_col_all_na = function(ctx) length(ctx$grain_na_cols) > 0,

  # ③ 亲本口径退化：最大位次都够不到阈值 ⇒ 亲本晋级率恒为 1.000，零区分度
  pf_parent_degenerate = function(ctx) {
    !is.null(ctx$max_avg_ck_rank) && is.finite(ctx$max_avg_ck_rank) &&
      ctx$max_avg_ck_rank <= ctx$parent_threshold
  },

  # ④ 亲本口径 ≠ 筛选口径（管线写死 60，不跟滑块走）
  pf_parent_threshold_mismatch = function(ctx) {
    !is.null(ctx$screening_threshold) && !is.null(ctx$parent_threshold) &&
      ctx$screening_threshold != ctx$parent_threshold
  },

  # ⑤ 产量等级分布图分母含无产量小区（case_when 兜底档）
  pf_grade_denominator_na = function(ctx) {
    !is.null(ctx$n_grade_na_rows) && ctx$n_grade_na_rows > 0
  },

  # ⑥ 增产率分布图分母含无增产率小区
  pf_increase_denominator_na = function(ctx) {
    !is.null(ctx$n_increase_na_rows) && ctx$n_increase_na_rows > 0
  },

  # ⑦ 淘汰三类原因计数可重叠（三类之和 > 淘汰数）
  pf_elim_overlap = function(ctx) {
    !is.null(ctx$elim_overlap) && ctx$elim_overlap > 0
  },

  # ⑧ 阈值 ≠ 晋级份数（AND 逻辑 + 并列 + 倒伏排除三重折损）
  pf_threshold_neq_promoted = function(ctx) {
    !is.null(ctx$screening_threshold) && !is.null(ctx$promoted_n) &&
      ctx$promoted_n != ctx$screening_threshold
  },

  # ⑨ 筛选结果不闭合
  pf_screening_not_closed = function(ctx) {
    !is.null(ctx$promoted_n) && !is.null(ctx$eliminated_n) &&
      (ctx$promoted_n + ctx$select_plant_n + ctx$eliminated_n) != ctx$n_plots
  },

  # ⑩ 有材料缺产
  pf_missing_yield = function(ctx) {
    !is.null(ctx$n_missing_material) && ctx$n_missing_material > 0
  },

  # ⑪ 有离群对照（1.5×IQR 置空）
  pf_outlier_control = function(ctx) {
    !is.null(ctx$outlier_ck_n) && ctx$outlier_ck_n > 0
  },

  # ⑫ 室内考种未开展（株高 / 百粒重整列为空）
  pf_indoor_grain_not_done = function(ctx) {
    !is.null(ctx$n_grain_traits_na) && ctx$n_grain_traits_na > 0
  },

  # ⑬ 父本记「未知」⇒ 该组合的晋级率不具亲本评价意义
  pf_unknown_parent = function(ctx) {
    !is.null(ctx$n_unknown_parent) && ctx$n_unknown_parent > 0
  },

  # ⑭ 某期田间评价字段全空
  pf_stage_eval_empty = function(ctx) length(ctx$stage_eval_empty) > 0,

  # ⑮ 有小区无任何一轮田间评价
  pf_no_evaluation = function(ctx) {
    !is.null(ctx$n_no_eval) && ctx$n_no_eval > 0
  },

  # ⑯ 单重复 —— 无重复间误差可估计
  pf_single_replicate = function(ctx) {
    !is.null(ctx$n_reps) && ctx$n_reps <= 1
  },

  # ⑰ 某性状字段稀疏（记录数远少于小区数）⇒ 「未记录」≠「不存在」
  pf_trait_field_sparse = function(ctx) length(ctx$sparse_trait_cols) > 0,

  # ⑱ 晋级材料里带性状分离的份数 > 0
  pf_separation_in_promoted = function(ctx) {
    !is.null(ctx$segregated_n) && ctx$segregated_n > 0
  },

  # ⑲ yaml 手填的参试材料数与管线不符 —— 手抄口径漂移
  pf_material_count_mismatch = function(ctx) {
    !is.null(ctx$meta_material_n) && !is.null(ctx$n_material) &&
      ctx$meta_material_n != ctx$n_material
  },

  # ⑳ 管线出图但报告未采用
  pf_plots_dropped = function(ctx) {
    !is.null(ctx$n_plots_dropped) && ctx$n_plots_dropped > 0
  }
)

# ------------------------------------------------------------------------------
# 规则表
# ------------------------------------------------------------------------------
# severity: blocker（挡住交付）/ warn（必须写进报告）/ info（写进注或附录）
# scope:    trial（整份报告）/ block（某个表图块）
RPT_RULES <- data.frame(
  rule_id = c(
    "no_control", "trait_col_all_na", "parent_rank_degenerate",
    "parent_threshold_mismatch", "grade_denominator_na",
    "increase_denominator_na", "elimination_reasons_overlap",
    "threshold_neq_promoted_count", "screening_not_closed",
    "missing_yield_materials", "outlier_control", "indoor_grain_not_done",
    "unknown_parent", "stage_eval_field_empty", "plots_without_evaluation",
    "single_replicate", "trait_field_sparse", "separation_in_promoted",
    "material_count_mismatch", "plots_dropped"),
  title = c(
    "无对照小区", "关键字段整列为空", "亲本口径退化",
    "亲本口径与筛选口径不一致", "产量等级分布分母含无产量小区",
    "增产率分布分母含无增产率小区", "淘汰原因计数可重叠",
    "阈值不等于晋级份数", "筛选结果不闭合",
    "存在缺产材料", "存在离群对照", "室内考种未开展",
    "父本记为未知", "某期田间评价字段全空", "存在无任何评价的小区",
    "单次重复", "性状字段记录稀疏", "晋级材料中记有性状分离",
    "参试材料数手填值与管线不符", "管线出图但报告未采用"),
  predicate = names(RPT_PREDICATES),
  severity = c(
    "blocker", "warn", "warn", "warn", "info", "info", "info", "info",
    "warn", "info", "info", "info", "info", "info", "info", "info", "info",
    "info", "blocker", "info"),
  # scope=trial 的规则：结论句进「试验级结论」，blocks 不消费（留空）
  # scope=block 的规则：结论句挂到 blocks 各块旁。
  #   **blocks 必须是 primary.yml 里真实存在的块 id** —— 指向不存在的块 ⇒
  #   结论句挂不出去 ⇒ 整条规则静默丢失。
  scope = c(
    "trial", "trial", "block", "block", "block", "block", "block", "block",
    "trial", "block", "block", "trial", "block", "block", "block", "trial",
    "block", "block", "trial", "trial"),
  blocks = c(
    "", "",
    "fig_parent_plot",
    "fig_parent_plot,tbl_parent_stats,tbl_cross_stats",
    "fig_yield_grade",
    "fig_increase",
    "tbl_promoted_no_sep,tbl_promoted_sep,tbl_threshold_scan",
    "tbl_threshold_scan",
    "",
    "tbl_promoted_no_sep,tbl_promoted_sep",
    "fig_yield_dist,fig_yield_grade,fig_increase,tbl_yield_core",
    "",
    "fig_parent_plot",
    "tbl_a4_field_eval",
    "tbl_a4_field_eval",
    "",
    "tbl_trait_dist",
    "tbl_promoted_sep",
    "",
    ""),
  # 文档锚点：指向 skill 里讲这条口径的地方（供人查「为什么这么判」）
  # 「本次新增」= 生成器引入的新规则，skill 散文里本来没有对应条目。
  doc_ref = c(
    "口径与图表分诊.md#三·八 (13) 没有对照小区",
    "口径与图表分诊.md#三 ❌ 不能用（3 张）",
    "口径与图表分诊.md#三·五 10 亲本口径可能整个退化",
    "口径与图表分诊.md#二 亲本分析的「晋级」口径写死为 60",
    "口径与图表分诊.md#三·五 12 增产分布的兜底档吞掉两类行",
    "口径与图表分诊.md#三·五 12 增产分布的兜底档吞掉两类行",
    "口径与图表分诊.md#二 晋级筛选的数要闭合",
    "口径与图表分诊.md#二 晋级筛选的数要闭合",
    "口径与图表分诊.md#二 晋级筛选的数要闭合",
    "口径与图表分诊.md#三·五 11 缺产材料 vs 离群对照置空",
    "口径与图表分诊.md#一.2 对照基准 1.5×IQR 剔除离群",
    "口径与图表分诊.md#三 ❌ 不能用（3 张）",
    "口径与图表分诊.md#四 4 「未知」父本",
    "本次新增（skill 散文里暂无对应条目）",
    "本次新增（skill 散文里暂无对应条目）",
    "SKILL.md#六-B 陷阱 9 默认阈值与「数行不数份」",
    "SKILL.md#六-B 陷阱 8 「无记录 ≠ 不分离」",
    "SKILL.md#六-B 陷阱 8 「分离」也有两套口径",
    "口径与图表分诊.md#三·八 (22) 参试材料数不能数行数",
    "口径与图表分诊.md#三 图表分诊（16 张）"),
  stringsAsFactors = FALSE
)

# --- 结论句 / 注模板 ------------------------------------------------------------
# 结论句：判定为真时写进正文或附录的一句话
# 注模板：判定为真时追加到受影响表图下的「注：」段落
rpt_rule_conclusions <- list(
  no_control = "本试验没有对照小区，晋级/淘汰通道整体不适用，仅作产量描述性分析。",
  trait_col_all_na = paste0(
    "以下字段在全部小区中均为空：{grain_na_cols}。",
    "依赖这些字段的图（性状相关性矩阵、株高与产量、百粒重与产量）渲染后为空白，",
    "无有效信息量，已从正文剔除；室内考种数据入库后重跑管线即可自动生成。"),
  parent_rank_degenerate = paste0(
    "亲本/组合晋级率按「较平均对照位次 ≤ {parent_threshold}」统计，",
    "但本试验最大较平均对照位次仅 {max_avg_ck_rank}，",
    "所有组合晋级率恒为 1.000、零区分度，亲本晋级表现图已剔除。"),
  parent_threshold_mismatch = paste0(
    "亲本口径的晋级判定在管线里写死为「较平均对照位次 ≤ {parent_threshold}」，",
    "不随本次筛选阈值 {screening_threshold} 变化；",
    "亲本统计与组合统计的晋级数对应的是 {parent_threshold} 口径，与晋级名单表不可混读。"),
  grade_denominator_na = paste0(
    "产量等级分布图的分母为全部 {n_plots} 个小区；",
    "其中 {n_grade_na_rows} 个无产量小区被分箱逻辑归入兜底档，",
    "故该档计数含无产量小区。剔除空值后的材料层分布另见下表。"),
  increase_denominator_na = paste0(
    "增产率分布图的分母为全部 {n_plots} 个小区；",
    "其中 {n_increase_na_rows} 个小区无增产率（{n_ck} 个对照小区本身不与对照比增产率，",
    "另加 {n_missing_material} 份缺产材料），被归入兜底档。"),
  elimination_reasons_overlap = paste0(
    "淘汰原因分三类统计，同一小区可能同时命中两类（如位次不达标且重倒），",
    "因此三类计数之和比淘汰数多 {elim_overlap} 份，不可相加闭合。"),
  threshold_neq_promoted_count = paste0(
    "位次阈值 {screening_threshold} 不等于晋级份数 {promoted_n}：",
    "两套位次用 AND 逻辑、位次并列会一并入选、再扣除倒伏排除，三重折损所致。"),
  screening_not_closed = paste0(
    "晋级 {promoted_n} + 分离选单株 {select_plant_n} + 淘汰 {eliminated_n} ",
    "= {screening_closed_total}，与总小区数 {n_plots} 不符；",
    "请核对本次筛选参数与落盘结果。"),
  missing_yield_materials = paste0(
    "{n_missing_material} 份材料无产量（{missing_material_ids}），已从所有产量统计中排除。"),
  outlier_control = paste0(
    "对照小区 {outlier_ck_ids} 的小区产量为离群值，",
    "按 1.5×IQR 规则置空，不计入有效对照基准（基准用 {n_ck_valid} 个对照）。"),
  indoor_grain_not_done = paste0(
    "室内考种未开展（{n_grain_traits_na} 个考种字段全空），",
    "株高、百粒重相关分析本试验不做结论。"),
  unknown_parent = paste0(
    "{n_unknown_parent} 份材料的父本记为「未知」，其晋级率不具亲本评价意义。"),
  stage_eval_field_empty = paste0(
    "以下田间评价字段在本试验全部为空：{stage_eval_empty}；",
    "「未记录」不等于「无异常」，统计时不能拿小区总数当分母。"),
  plots_without_evaluation = paste0(
    "{n_no_eval} 个小区无任何一轮田间评价（清单见附录 A.6）。"),
  single_replicate = "本试验为 1 次重复，无重复间误差可估计，位次比较的偶然性较大。",
  trait_field_sparse = paste0(
    "以下性状字段记录数远少于小区数：{sparse_trait_cols}；",
    "「未记录」不等于「不存在」，统计该性状比例时不能拿小区总数当分母。"),
  separation_in_promoted = paste0(
    "晋级材料中有 {segregated_n} 份记有性状分离（逐份列于晋级名单的分离表）；",
    "晋级判定只看产量位次与倒伏等级，管线未审核性状纯度。"),
  material_count_mismatch = paste0(
    "元信息里手填的参试材料数 {meta_material_n} 与管线算出的 {n_material} 不一致，",
    "请先核对后再交付。"),
  plots_dropped = paste0(
    "管线已生成但本报告不予采用的 {n_plots_dropped} 张图：{dropped_plots_reason}")
)

rpt_rule_notes <- list(
  grade_denominator_na = paste0(
    "图上分母为 {n_plots}（全部小区），其中 {n_grade_na_rows} 个无产量小区",
    "（{n_missing_material} 份缺产材料 + 离群对照 {outlier_ck_ids}）被归入兜底档；",
    "剔除空值后的材料层分布见对应表格。"),
  increase_denominator_na = paste0(
    "图上分母为 {n_plots}（全部小区），其中 {n_increase_na_rows} 个无增产率小区",
    "（{n_ck} 个对照小区本身不与对照比增产率 + {n_missing_material} 份缺产材料）",
    "被归入兜底档，故该档计数含无值小区。"),
  elimination_reasons_overlap = paste0(
    "淘汰三类原因计数可重叠（本次 {elim_overlap} 份同时命中「位次不达标」与「倒伏」），",
    "三类计数之和比淘汰数多 {elim_overlap}，不可相加闭合。"),
  threshold_neq_promoted_count = paste0(
    "八档阈值均由管线对同一试验实跑产出，非手工推算。",
    "阈值与晋级份数不等：AND 逻辑 + 位次并列 + 倒伏排除三重折损，",
    "本报告正文一律采用阈值 {screening_threshold} 的结果。"),
  missing_yield_materials = "缺产材料 {missing_material_ids} 已从产量统计中排除。",
  outlier_control = paste0(
    "离群对照 {outlier_ck_ids} 按 1.5×IQR 判定后置空，不计入有效对照基准（基准用 {n_ck_valid} 个）。"),
  single_replicate = "本表为 1 次重复结果。",
  separation_in_promoted = "本试验晋级材料中 {segregated_n} 份记有性状分离。"
)

# ------------------------------------------------------------------------------
# 求值辅助
# ------------------------------------------------------------------------------
#' 填充 {field} 占位符
#' @param tmpl 模板字符串
#' @param ctx  事实表
#' @return 填充后的字符串；占位符对应的字段缺失时原样保留
#' @export
rpt_fill <- function(tmpl, ctx) {
  if (!is.character(tmpl) || length(tmpl) == 0) return(tmpl)
  out <- tmpl
  for (nm in names(ctx)) {
    v <- ctx[[nm]]
    if (is.null(v) || length(v) == 0) next
    if (length(v) > 1) {
      # 多值事实（字段名列表、位次数值列表）统一折叠成顿号串；
      # 不折叠的话 gsub 只取第一个元素，静默丢数据。
      v <- if (is.numeric(v)) format(v, trim = TRUE, digits = 8) else as.character(v)
      v <- paste(v, collapse = "、")
    } else if (is.numeric(v)) {
      v <- format(v, trim = TRUE, digits = 8)
    }
    out <- gsub(paste0("{", nm, "}"), as.character(v), out, fixed = TRUE)
  }
  out
}

#' 求值单条规则的「结论句」与「注模板」
#'
#' 注模板为函数时直接调用；为字符串时填 {field} 占位符。
#'
#' @param rule 单行规则表
#' @param ctx  事实表
#' @return list(conclusion, note)
#' @export
rpt_rule_text <- function(rule, ctx) {
  concl <- rpt_rule_conclusions[[rule$rule_id]]
  note  <- rpt_rule_notes[[rule$rule_id]]
  if (is.function(concl)) concl <- concl(ctx)
  if (is.function(note))  note  <- note(ctx)
  if (!is.null(concl)) concl <- rpt_fill(concl, ctx)
  if (!is.null(note))  note  <- rpt_fill(note, ctx)
  list(conclusion = concl, note = note)
}
