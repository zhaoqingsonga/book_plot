# ==============================================================================
# 分析功能主调度器 + Modal 渲染
# ==============================================================================

source("shared/analysis/analysis_column_map.R")
source("shared/analysis/analysis_trial_detect.R")
source("shared/analysis/analysis_yield_core.R")
source("shared/analysis/analysis_quality_traits.R")
source("shared/analysis/analysis_screening.R")
source("shared/analysis/analysis_radar.R")
source("shared/analysis/analysis_description.R")
source("shared/analysis/analysis_parent_cross.R")
source("shared/analysis/analysis_gge.R")
source("shared/analysis/analysis_population.R")
source("shared/analysis/analysis_line_selection.R")
source("shared/analysis/analysis_export.R")
source("shared/analysis/analysis_yield_growth_chart.R")
source("shared/analysis/analysis_html_report.R")

# ==============================================================================
# 1. 主调度器
# ==============================================================================

run_analysis <- function(df, module_type = c("yield_test", "population", "line_selection")) {
  module_type <- match.arg(module_type)
  messages <- character()

  if (!is.data.frame(df) || nrow(df) == 0L) {
    return(list(type = "error", tables = list(), plots = list(),
      messages = "数据为空，无法进行分析。请先生成田试记录。", has_traits = FALSE, trial_info = NULL))
  }

  trial_info <- tryCatch(detect_trial_type(df), error = function(e) {
    list(type = "unknown", label = "未知", desc = paste("检测失败:", e$message),
      is_single_site = TRUE, is_multi_site = FALSE,
      is_single_rep = TRUE, is_multi_rep = FALSE,
      can_do_gge = FALSE, n_places = 1L, n_reps = 1L, places = "?", reps = 1L)
  })
  messages <- c(messages, paste0("试验类型：", trial_info$label, " — ", trial_info$desc))

  trait_available <- tryCatch(check_trait_availability(df), error = function(e) {
    list(has_traits = FALSE, available = character(), missing = character())
  })
  has_traits <- trait_available$has_traits
  if (!has_traits) {
    messages <- c(messages,
      "⚠️ 尚未导入田间调查数据（亩产等性状为空），仅展示基础分析。请通过「E智导入」导入性状数据。")
  }

  if (has_traits && module_type == "yield_test") {
    df <- tryCatch(compute_derived_yield_columns(df), error = function(e) {
      messages <<- c(messages, paste("计算增产位次列失败:", e$message)); df
    })
  }

  result <- switch(module_type,
    yield_test     = run_yield_test_analysis(df, trial_info, has_traits),
    population     = run_population_analysis(df, trial_info, has_traits),
    line_selection = run_line_selection_analysis(df, trial_info, has_traits)
  )

  list(type = module_type, trial_info = trial_info, has_traits = has_traits,
    trait_available = trait_available$available,
    tables = result$tables, plots = result$plots,
    per_site_plots = result$per_site_plots,
    per_site_quality = result$per_site_quality,
    messages = c(messages, result$messages),
    capabilities = get_analysis_capabilities(trial_info, has_traits))
}

# ==============================================================================
# 2. 推导列计算
# ==============================================================================

compute_derived_yield_columns <- function(df) {
  if (!"MuChan" %in% colnames(df)) return(df)
  if (!"is_ck" %in% colnames(df)) df$is_ck <- 0L
  df$MuChan <- suppressWarnings(as.numeric(as.character(df$MuChan)))
  df$is_ck  <- as.integer(as.character(df$is_ck))
  df$is_ck[is.na(df$is_ck)] <- 0L

  group_cols <- intersect(c("place", "rp"), colnames(df))
  if (length(group_cols) > 0) {
    df <- df %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) %>%
      dplyr::group_modify(~ soyplant::calculate_MuChan_increase_with_multiple_methods(.x)) %>%
      dplyr::ungroup()
  } else {
    df <- soyplant::calculate_MuChan_increase_with_multiple_methods(df)
  }

  # 清理 Inf/NaN + 保留两位小数
  out_cols <- c("JiaoPingJunDuiZhaoZengChan", "JiaoPingJunDuiZhaoWeiCi",
                "JiaoLinJinDuiZhaoZengChan", "JiaoLinJinDuiZhaoWeiCi")
  for (col in intersect(out_cols, colnames(df))) {
    df[[col]][is.infinite(df[[col]]) | is.nan(df[[col]])] <- NA_real_
    if (col %in% c("JiaoPingJunDuiZhaoZengChan", "JiaoLinJinDuiZhaoZengChan")) {
      df[[col]] <- round(df[[col]], 2)
    }
  }
  df
}

# ==============================================================================
# 3. 产比分析路由（单点和多点合并 — 多点时两种分析都做）
# ==============================================================================

run_yield_test_analysis <- function(df, trial_info, has_traits) {
  tables  <- list()
  plots   <- list()
  per_site_plots   <- NULL
  per_site_quality <- NULL
  messages <- character()

  tables$overview <- build_yield_overview_table(df)
  if (!has_traits) {
    tables$export_data <- df
    return(list(tables = tables, plots = plots, messages = messages))
  }

  # ---- 产量排名 ----
  tables$yield_ranking <- build_yield_ranking_table(df)

  # ====== 单点分析：直方图/散点/品质/筛选/雷达/亲本（所有类型都做） ======
  yc <- tryCatch(analyze_yield_core(df), error = function(e) {
    messages <<- c(messages, paste("产量核心分析失败:", e$message)); NULL })
  if (!is.null(yc)) {
    tables$yield_stats    <- yc$yield_stats
    tables$growth_stats   <- yc$growth_stats
    tables$increase_stats <- yc$increase_stats
    plots$yield_dist      <- yc$plots$yield_dist
    plots$yield_grade     <- yc$plots$yield_grade_dist
    plots$increase_dist   <- yc$plots$increase_dist
    plots$growth_dist     <- yc$plots$growth_dist
    plots$scatter_growth  <- yc$scatter_plots$growth_vs_yield
    plots$scatter_height  <- yc$scatter_plots$height_vs_yield
    plots$scatter_grain   <- yc$scatter_plots$hundred_grain_vs_yield
    plots$corr_matrix     <- yc$corr_plot
  }

  if (length(get_available_traits(df)) > 0) {
    qt <- tryCatch(analyze_quality_traits(df), error = function(e) {
      messages <<- c(messages, paste("质量性状分析失败:", e$message)); NULL })
    if (!is.null(qt)) {
      for (nm in names(qt$plots)) plots[[paste0("quality_", nm)]] <- qt$plots[[nm]]
    }
  }

  # 品种筛选/雷达/评述（需要性状数据提供位次列，不依赖多重复）
  scr <- tryCatch(analyze_screening(df), error = function(e) {
    messages <<- c(messages, paste("品种筛选失败:", e$message)); NULL })
  if (!is.null(scr)) {
    if (!is.null(scr$message)) messages <- c(messages, scr$message)
    tables$promoted   <- scr$promoted
    tables$eliminated <- scr$eliminated
    plots$comparison  <- scr$comparison_plot
    plots$radar       <- scr$radar_plot
    if (nrow(scr$promoted) > 0) {
      desc <- tryCatch(generate_description(scr$promoted), error = function(e) {
        messages <<- c(messages, paste("描述失败:", e$message)); NULL })
      if (!is.null(desc)) tables$description <- desc
    }
  }

  # 亲本与组合分析（不依赖重复数，仅需 ma/pa 列）
  pc <- tryCatch(analyze_parent_cross(df), error = function(e) {
    messages <<- c(messages, paste("亲本分析失败:", e$message)); NULL })
  if (!is.null(pc)) {
    tables$parent_stats <- pc$parent_stats
    tables$cross_stats  <- pc$cross_stats
    plots$parent_plot   <- pc$parent_plot
  }

  # ====== 多点分析：跨地点排名 + GGE（仅多点额外添加） ======
  if (trial_info$is_multi_site) {
    cs <- tryCatch(analyze_cross_site_ranking(df, trial_info), error = function(e) {
      messages <<- c(messages, paste("跨地点排名失败:", e$message)); NULL })
    if (!is.null(cs)) tables$cross_site_ranking <- cs$table

    # 各地点的平均（按品种跨地点汇总）
    cla <- tryCatch(build_cross_location_avg_table(df), error = function(e) {
      messages <<- c(messages, paste("跨地点平均表失败:", e$message)); NULL })
    if (!is.null(cla)) tables$cross_location_avg <- cla

    if (trial_info$can_do_gge) {
      gge <- tryCatch(analyze_gge(df, trial_info), error = function(e) {
        messages <<- c(messages, paste("GGE分析失败:", e$message)); NULL })
      if (!is.null(gge)) {
        if (!is.null(gge$skip) && isTRUE(gge$skip)) {
          messages <<- c(messages, paste("GGE分析跳过：", gge$reason))
        } else {
          plots$gge_biplot    <- gge$biplot
          plots$gge_stability <- gge$stability_scatter
          plots$gge_heatmap   <- gge$heatmap
          plots$gge_ranking   <- gge$ranking
          tables$gge_stable   <- gge$stable_genotypes
          tables$gge_unstable <- gge$unstable_genotypes
          tables$gge_env      <- gge$env_summary

          # 高产品种产量生育期组合图（高产稳定 + 高产不稳，所有高产品种均出图）
          high_yield_genotypes <- dplyr::bind_rows(
            gge$stable_genotypes, gge$unstable_genotypes)
          if (!is.null(high_yield_genotypes) && nrow(high_yield_genotypes) > 0) {
            ygc <- tryCatch(
              generate_yield_growth_chart_shiny(df, high_yield_genotypes),
              error = function(e) {
                messages <<- c(messages,
                  paste("高产品种产量生育期图生成失败:", e$message))
                NULL
              }
            )
            if (!is.null(ygc)) {
              plots$gge_yield_growth  <- ygc$plots
              tables$gge_yield_growth <- ygc$tables
            }
          }
        }
      }
    }

    # 分地点产量统计（所有多点试验均支持，含 MS1R）
    ps <- tryCatch(analyze_per_site(df, trial_info), error = function(e) {
      messages <<- c(messages, paste("分地点分析失败:", e$message)); NULL })
    if (!is.null(ps)) {
      if (!is.null(ps$per_site_yield_stats))    tables$per_site_yield_stats    <- ps$per_site_yield_stats
      if (!is.null(ps$per_site_growth_stats))   tables$per_site_growth_stats   <- ps$per_site_growth_stats
      if (!is.null(ps$per_site_increase_stats)) tables$per_site_increase_stats <- ps$per_site_increase_stats
      if (!is.null(ps$per_site_ck_mean))        tables$per_site_ck_mean        <- ps$per_site_ck_mean
      if (!is.null(ps$per_site_plots))          per_site_plots <- ps$per_site_plots
    }

    # 分地点质量性状分布
    if (!is.null(trial_info) && isTRUE(trial_info$is_multi_site)) {
      qs <- tryCatch(analyze_quality_traits_by_site(df, trial_info), error = function(e) {
        messages <<- c(messages, paste("分地点质量性状分析失败:", e$message)); NULL })
      if (!is.null(qs) && length(qs$site_plots) > 0) {
        per_site_quality <- qs$site_plots
      }
    }
  }

  tables$export_data <- df
  list(tables = tables, plots = plots, per_site_plots = per_site_plots,
       per_site_quality = per_site_quality, messages = messages)
}

# ==============================================================================
# 4. 群体分析路由
# ==============================================================================

run_population_analysis <- function(df, trial_info, has_traits) {
  tables <- list(); plots <- list(); messages <- character()

  pop <- tryCatch(analyze_population(df), error = function(e) {
    messages <<- c(messages, paste("群体分析失败:", e$message)); NULL })
  if (!is.null(pop)) {
    tables$gen_dist       <- pop$gen_table
    tables$gen_track      <- pop$gen_tracking
    tables$cross_top      <- pop$cross_table
    plots$gen_dist_chart  <- pop$gen_dist_plot
    plots$gen_track_chart <- pop$gen_tracking_plot
    plots$cross_chart     <- pop$cross_plot
  }

  if (has_traits) {
    to <- tryCatch(build_population_trait_overview(df), error = function(e) {
      messages <<- c(messages, paste("性状概览失败:", e$message)); NULL })
    if (!is.null(to)) tables$trait_overview <- to$table
  }

  list(tables = tables, plots = plots, messages = messages)
}

# ==============================================================================
# 5. 株行分析路由
# ==============================================================================

run_line_selection_analysis <- function(df, trial_info, has_traits) {
  tables <- list(); plots <- list(); messages <- character()

  ls <- tryCatch(analyze_line_selection(df), error = function(e) {
    messages <<- c(messages, paste("株行分析失败:", e$message)); NULL })
  if (!is.null(ls)) {
    tables$sele_overview  <- ls$overview
    tables$sele_dist      <- ls$sele_dist_table
    tables$progeny_top    <- ls$progeny_table
    plots$sele_dist_chart <- ls$sele_dist_plot
    plots$progeny_chart   <- ls$progeny_plot
  }

  if (has_traits) {
    ms <- tryCatch(build_morphology_stats(df), error = function(e) {
      messages <<- c(messages, paste("形态统计失败:", e$message)); NULL })
    if (!is.null(ms)) tables$morph_stats <- ms$table
  }

  list(tables = tables, plots = plots, messages = messages)
}

# ==============================================================================
# 6. 辅助表
# ==============================================================================

build_yield_overview_table <- function(df) {
  data.frame(
    指标 = c("总记录数", "材料/品种数", "杂交组合数", "地点数", "重复数",
             if ("is_ck" %in% colnames(df)) "对照数" else NULL),
    数值 = c(nrow(df),
      if ("name" %in% colnames(df)) dplyr::n_distinct(df$name, na.rm = TRUE) else nrow(df),
      if (all(c("ma", "pa") %in% colnames(df))) dplyr::n_distinct(paste(df$ma, df$pa, sep = "×"), na.rm = TRUE) else NA_integer_,
      if ("place" %in% colnames(df)) dplyr::n_distinct(df$place, na.rm = TRUE) else 1L,
      if ("rp" %in% colnames(df)) dplyr::n_distinct(df$rp, na.rm = TRUE) else 1L,
      if ("is_ck" %in% colnames(df)) sum(as.integer(df$is_ck) == 1L, na.rm = TRUE) else NA_integer_),
    stringsAsFactors = FALSE)
}

build_yield_ranking_table <- function(df) {
  if (!"MuChan" %in% colnames(df)) return(NULL)
  rank_cols <- intersect(c("name", "stageid", "ma", "pa", "MuChan",
    "JiaoLinJinDuiZhaoZengChan", "JiaoLinJinDuiZhaoWeiCi",
    "JiaoPingJunDuiZhaoZengChan", "JiaoPingJunDuiZhaoWeiCi",
    "ShengYuQi", "ZhuGao", "BaiLiZhong", "is_ck", "place"), colnames(df))
  df %>%
    dplyr::select(dplyr::any_of(rank_cols)) %>%
    dplyr::arrange(dplyr::desc(MuChan)) %>%
    dplyr::rename_with(~ get_trait_display_name(.x), dplyr::everything())
}

build_cross_location_avg_table <- function(df) {
  if (!"MuChan" %in% colnames(df) || !"place" %in% colnames(df)) return(NULL)

  gen_col <- if ("stageid" %in% colnames(df)) "stageid" else "name"
  id_cols <- intersect(c("name", "stageid", "ma", "pa"), colnames(df))

  # 先按品种+地点聚合（取地点内平均），再按品种跨地点聚合
  agg_cols <- intersect(c("MuChan", "JiaoLinJinDuiZhaoZengChan", "JiaoLinJinDuiZhaoWeiCi",
    "JiaoPingJunDuiZhaoZengChan", "JiaoPingJunDuiZhaoWeiCi",
    "ShengYuQi", "ZhuGao", "BaiLiZhong"), colnames(df))

  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(id_cols, "place")))) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(agg_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) %>%
    dplyr::summarise(
      平均亩产               = round(mean(MuChan, na.rm = TRUE), 1),
      增产点次               = if ("JiaoLinJinDuiZhaoZengChan" %in% colnames(.)) {
                                  sum(JiaoLinJinDuiZhaoZengChan > 0, na.rm = TRUE)
                                } else 0L,
      地点数                 = dplyr::n_distinct(place),
      较临近对照增产_pct     = if ("JiaoLinJinDuiZhaoZengChan" %in% colnames(.)) {
                                  round(mean(JiaoLinJinDuiZhaoZengChan, na.rm = TRUE), 1)
                                } else NA_real_,
      较临近对照位次         = if ("JiaoLinJinDuiZhaoWeiCi" %in% colnames(.)) {
                                  round(mean(JiaoLinJinDuiZhaoWeiCi, na.rm = TRUE), 1)
                                } else NA_real_,
      较平均对照增产_pct     = if ("JiaoPingJunDuiZhaoZengChan" %in% colnames(.)) {
                                  round(mean(JiaoPingJunDuiZhaoZengChan, na.rm = TRUE), 1)
                                } else NA_real_,
      较平均对照位次         = if ("JiaoPingJunDuiZhaoWeiCi" %in% colnames(.)) {
                                  round(mean(JiaoPingJunDuiZhaoWeiCi, na.rm = TRUE), 1)
                                } else NA_real_,
      生育期_d               = if ("ShengYuQi" %in% colnames(.)) {
                                  round(mean(ShengYuQi, na.rm = TRUE), 1)
                                } else NA_real_,
      株高_cm                = if ("ZhuGao" %in% colnames(.)) {
                                  round(mean(ZhuGao, na.rm = TRUE), 1)
                                } else NA_real_,
      百粒重_g               = if ("BaiLiZhong" %in% colnames(.)) {
                                  round(mean(BaiLiZhong, na.rm = TRUE), 1)
                                } else NA_real_,
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(平均亩产)) %>%
    dplyr::rename_with(~ get_trait_display_name(.x), dplyr::everything()) %>%
    dplyr::select(dplyr::where(~ !all(is.na(.x))))
}
