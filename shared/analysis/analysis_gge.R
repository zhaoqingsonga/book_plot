# ==============================================================================
# GGE多环境分析（使用metan::gge）
# 必须：≥3环境、基因型在所有环境中都有非CK产量数据
# ==============================================================================

safe_numeric <- function(x) suppressWarnings(as.numeric(as.character(x)))

analyze_gge <- function(df, trial_info) {
  if (!trial_info$can_do_gge) {
    return(list(skip = TRUE, reason = sprintf("GGE需>=3环境，当前%d个", trial_info$n_places)))
  }
  if (!requireNamespace("metan", quietly = TRUE)) {
    return(list(skip = TRUE, reason = "需安装metan包: install.packages('metan')"))
  }
  need <- c("MuChan", "place")
  if (!"stageid" %in% colnames(df) && !"name" %in% colnames(df)) need <- c(need, "name")
  miss <- setdiff(need, colnames(df))
  if (length(miss) > 0) {
    return(list(skip = TRUE, reason = paste("缺少列:", paste(miss, collapse = ", "))))
  }

  gen_col <- if ("stageid" %in% colnames(df)) "stageid" else "name"

  # ==== 1. 清洗 ====
  diag <- list(raw = nrow(df))
  gd <- df

  # 排除CK行
  if ("is_ck" %in% colnames(gd)) {
    ck <- safe_numeric(gd$is_ck)
    gd <- gd[is.na(ck) | ck != 1, , drop = FALSE]
  }
  diag$noCK <- nrow(gd)

  # 完整观测 + 数值化
  ok <- !is.na(gd$place) & nchar(as.character(gd$place)) > 0 &
        !is.na(gd[[gen_col]]) & nchar(as.character(gd[[gen_col]])) > 0
  gd <- gd[ok, , drop = FALSE]
  gd$yield <- safe_numeric(gd$MuChan)
  gd <- gd[!is.na(gd$yield), , drop = FALSE]
  diag$valid <- nrow(gd)

  n_env <- dplyr::n_distinct(gd$place)
  n_gen <- dplyr::n_distinct(gd[[gen_col]])
  if (nrow(gd) < 6 || n_env < 3 || n_gen < 2) {
    return(list(skip = TRUE, reason = sprintf(
      "数据不足：%d行/%d环境/%d基因型（需>=6/>=3/>=2）", nrow(gd), n_env, n_gen)))
  }

  # ==== 2. 全环境公有基因型检查（GGE硬性要求） ====
  ge_dist <- gd %>%
    dplyr::group_by(.data[[gen_col]]) %>%
    dplyr::summarise(n_env = dplyr::n_distinct(place), .groups = "drop")
  n_common <- sum(ge_dist$n_env == n_env)

  if (n_common < 2) {
    top <- ge_dist %>%
      dplyr::arrange(dplyr::desc(n_env)) %>%
      dplyr::slice_head(n = min(10, nrow(ge_dist)))
    msg <- paste0(
      sprintf("全环境公有基因型不足：%d个环境，仅%d个基因型在所有环境中都存在（非CK）。\n",
        n_env, n_common),
      sprintf("(原始%d行 -> 排除CK %d行 -> 清洗 %d行，基因型共%d个)\n\n",
        diag$raw, diag$noCK, diag$valid, n_gen),
      "各基因型覆盖环境数 Top10：\n",
      paste(sprintf("  %s: %d/%d", top[[gen_col]], top$n_env, n_env), collapse = "\n"),
      "\n\n原因：\n",
      "  1) 对照品种(test)在部分地点标记为is_ck=1，过滤后缺失\n",
      "  2) 部分地点未导入产量数据(MuChan空)\n",
      sprintf("  3) %s列在各地点格式不一致\n", gen_col),
      "\n建议：检查各地点is_ck和MuChan是否完整，确认CK与试验品种不重叠。")
    return(list(skip = TRUE, reason = msg))
  }

  # 仅保留全环境基因型
  keep <- ge_dist[[gen_col]][ge_dist$n_env == n_env]
  gd <- gd[gd[[gen_col]] %in% keep, , drop = FALSE]

  # ==== 3. 聚合GxE矩阵 ====
  # 构建 stageid + name 组合标签（所有图表统一使用）
  ge_mat <- gd %>%
    dplyr::group_by(env = place, gen_raw = .data[[gen_col]]) %>%
    dplyr::summarise(yield = round(mean(yield, na.rm = TRUE), 2), .groups = "drop")

  # 拼接 display 标签：stageid\nname（如果 name 列存在且不同于 stageid）
  if ("name" %in% colnames(gd) && gen_col == "stageid") {
    name_lu <- gd %>%
      dplyr::select(gen_raw = !!sym(gen_col), name) %>%
      dplyr::filter(!is.na(name)) %>%
      dplyr::distinct() %>%
      dplyr::group_by(gen_raw) %>%
      dplyr::slice_head(n = 1) %>%
      dplyr::ungroup()
    ge_mat <- ge_mat %>%
      dplyr::left_join(name_lu, by = "gen_raw") %>%
      dplyr::mutate(
        gen = ifelse(!is.na(name) & name != "" & name != gen_raw,
                      paste0(gen_raw, "<", name),
                      gen_raw)
      ) %>%
      dplyr::select(env, gen, yield)
  } else {
    ge_mat <- ge_mat %>% dplyr::rename(gen = gen_raw)
  }

  ge_mat$yield <- as.numeric(as.character(ge_mat$yield))
  ge_mat$env   <- as.factor(ge_mat$env)
  ge_mat$gen   <- as.factor(ge_mat$gen)

  if (!is.numeric(ge_mat$yield) || anyNA(ge_mat$yield) ||
      any(is.infinite(ge_mat$yield)) || any(is.nan(ge_mat$yield))) {
    return(list(skip = TRUE, reason = "产量列含非数值/NA/Inf/NaN"))
  }
  if (dplyr::n_distinct(ge_mat$gen) < 2 || dplyr::n_distinct(ge_mat$env) < 3) {
    return(list(skip = TRUE, reason = "聚合后基因型或环境数不足"))
  }

  # ==== 4. metan::gge ====
  gge_model <- tryCatch({
    metan::gge(.data = ge_mat, env = env, gen = gen, resp = yield,
      centering = "global", scaling = "sd")
  }, error = function(e) {
    return(list(skip = TRUE, reason = paste("metan::gge失败:", e$message)))
  })
  if (is.list(gge_model) && !is.null(gge_model$skip)) return(gge_model)

  # 检查模型结构（兼容新旧 metan API）
  yc <- if (!is.null(gge_model$yield) && is.list(gge_model$yield)) {
    gge_model$yield       # metan >= 1.19
  } else if (!is.null(gge_model$yield_clean) && is.list(gge_model$yield_clean)) {
    gge_model$yield_clean # metan < 1.19
  } else {
    NULL
  }
  if (is.null(yc) || is.null(yc$labelgen) || length(yc$labelgen) == 0) {
    return(list(skip = TRUE, reason = "metan返回空结果(可能数据变异性不足)"))
  }

  # ==== 5. 提取结果(每步标注错误位置) ====
  result <- list()
  step <- "init"

  ok <- tryCatch({
    step <- "labelgen"; nG <- length(yc$labelgen)
    step <- "labelenv"; nE <- length(yc$labelenv)
    step <- "varexpl";  pc1 <- round(yc$varexpl[1], 1); pc2 <- round(yc$varexpl[2], 1)

    step <- "coordgen"; cg <- as.numeric(as.character(yc$coordgen))
    step <- "coordenv"; ce <- as.numeric(as.character(yc$coordenv))
    step <- "mean_gen"; mg <- as.numeric(as.character(yc$mean_gen))
    step <- "mean_env"; me <- as.numeric(as.character(yc$mean_env))
    step <- "ge_mat";   gm <- as.numeric(as.character(yc$ge_mat))

    step <- "gp"; gp <- data.frame(
      Genotype = yc$labelgen[1:nG], PC1 = cg[1:nG],
      PC2 = cg[(nG+1):(2*nG)], Mean_Yield = mg[1:nG], stringsAsFactors = FALSE)
    step <- "ep"; ep <- data.frame(
      Environment = yc$labelenv, PC1 = ce[1:nE],
      PC2 = ce[(nE+1):(2*nE)], Mean_Yield = me[1:nE], stringsAsFactors = FALSE)

    step <- "rank"; gp$Top <- ifelse(rank(-gp$Mean_Yield) <= min(15, nG), "Top", "Other")

    # AEA (Average Environment Axis) — 平均环境轴
    avg_env_pc1 <- mean(ep$PC1, na.rm = TRUE)
    avg_env_pc2 <- mean(ep$PC2, na.rm = TRUE)
    aea_extend <- 1.2  # 轴线向两端延伸比例

    # Biplot
    step <- "biplot"
    if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("ggrepel", quietly = TRUE)) {
      result$biplot <- ggplot2::ggplot() +
        ggplot2::geom_point(data = gp, ggplot2::aes(x = PC1, y = PC2, color = Mean_Yield, size = Top)) +
        ggplot2::geom_segment(data = ep, ggplot2::aes(x = 0, y = 0, xend = PC1 * 0.8, yend = PC2 * 0.8),
          arrow = ggplot2::arrow(length = ggplot2::unit(0.2, "cm")), color = "red", alpha = 0.7) +
        ggrepel::geom_text_repel(data = ep, ggplot2::aes(x = PC1 * 0.9, y = PC2 * 0.9, label = Environment),
          color = "red", size = 3.5, fontface = "bold") +
        ggrepel::geom_text_repel(data = dplyr::filter(gp, Top == "Top"),
          ggplot2::aes(x = PC1, y = PC2, label = Genotype), color = "blue", size = 2.5, max.overlaps = 20) +
        ggplot2::geom_segment(
          data = data.frame(x1 = -avg_env_pc1 * aea_extend, y1 = -avg_env_pc2 * aea_extend,
                            x2 = avg_env_pc1 * aea_extend, y2 = avg_env_pc2 * aea_extend),
          ggplot2::aes(x = x1, y = y1, xend = x2, yend = y2),
          color = "#228B22", linewidth = 1.0, inherit.aes = FALSE) +
        ggplot2::geom_point(
          data = data.frame(x = avg_env_pc1, y = avg_env_pc2),
          ggplot2::aes(x = x, y = y),
          shape = 15, size = 3, color = "#228B22", inherit.aes = FALSE) +
        ggrepel::geom_text_repel(
          data = data.frame(x = avg_env_pc1, y = avg_env_pc2),
          ggplot2::aes(x = x, y = y, label = "平均环境"),
          color = "#228B22", size = 3.5, fontface = "bold", inherit.aes = FALSE) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3) +
        ggplot2::geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3) +
        ggplot2::scale_color_gradientn(colors = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), name = "产量") +
        ggplot2::scale_size_manual(values = c(Top = 3, Other = 2)) +
        ggplot2::labs(title = "GGE双标图",
          subtitle = paste0("PC1:", pc1, "% | PC2:", pc2, "% | 绿线=平均环境轴(AEA)"),
          x = paste0("PC1(", pc1, "%)"), y = paste0("PC2(", pc2, "%)")) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"))
    }

    # Stability
    step <- "gm_mat"; gm_mat <- matrix(gm, nrow = nG, ncol = nE, byrow = FALSE)
    step <- "sd"; sd_vec <- apply(gm_mat, 1, sd, na.rm = TRUE)
    sd_data <- data.frame(Genotype = gp$Genotype, Mean_Yield = gp$Mean_Yield, Stability = sd_vec,
      stringsAsFactors = FALSE)
    step <- "median"
    my <- stats::median(sd_data$Mean_Yield, na.rm = TRUE)
    ms <- stats::median(sd_data$Stability, na.rm = TRUE)
    step <- "category"
    sd_data$Category <- dplyr::case_when(
      sd_data$Mean_Yield >= my & sd_data$Stability <= ms ~ "高产稳定",
      sd_data$Mean_Yield >= my & sd_data$Stability >  ms ~ "高产不稳",
      sd_data$Mean_Yield <  my & sd_data$Stability <= ms ~ "低产稳定",
      TRUE ~ "低产不稳")
    # 稳定/不稳定表：拆分组合标签为 stageid + name
    result$stable_genotypes <- sd_data %>%
      dplyr::filter(Category == "高产稳定") %>%
      dplyr::arrange(dplyr::desc(Mean_Yield))
    result$unstable_genotypes <- sd_data %>%
      dplyr::filter(Category == "高产不稳") %>%
      dplyr::arrange(dplyr::desc(Mean_Yield))

    # 不在这里 add name——已在 gen 标签中自带 "stageid<name"
    # 如需表格拆分两列，在此 split
    for (key in c("stable_genotypes", "unstable_genotypes")) {
      tbl <- result[[key]]
      if (!is.null(tbl) && nrow(tbl) > 0 && "Genotype" %in% names(tbl)) {
        has_pipe <- grepl("<", tbl$Genotype[1], fixed = TRUE)
        if (any(has_pipe)) {
          parts <- strsplit(as.character(tbl$Genotype), "<", fixed = TRUE)
          tbl$stageid <- vapply(parts, `[`, character(1), 1L)
          tbl$name    <- vapply(parts, function(x) if(length(x)>1) x[2] else "", character(1))
          cols_keep <- setdiff(names(tbl), "Genotype")
          tbl <- tbl[, cols_keep, drop = FALSE]
          result[[key]] <- tbl[, c("stageid","name", setdiff(names(tbl), c("stageid","name"))), drop = FALSE]
        }
      }
      # 产量和稳定性保留两位小数
      if (!is.null(result[[key]]) && nrow(result[[key]]) > 0) {
        for (cn in intersect(c("Mean_Yield", "Stability"), names(result[[key]]))) {
          result[[key]][[cn]] <- round(result[[key]][[cn]], 2)
        }
      }
    }

    step <- "stability_plot"
    result$stability_scatter <- ggplot2::ggplot(sd_data,
      ggplot2::aes(x = Mean_Yield, y = Stability, color = Category)) +
      ggplot2::geom_hline(yintercept = ms, linetype = "dashed", color = "gray50", alpha = 0.7) +
      ggplot2::geom_vline(xintercept = my, linetype = "dashed", color = "gray50", alpha = 0.7) +
      ggplot2::geom_point(alpha = 0.7, size = 3) +
      ggrepel::geom_text_repel(data = dplyr::filter(sd_data, Category == "高产稳定"),
        ggplot2::aes(label = Genotype), size = 3.5, max.overlaps = 15) +
      ggrepel::geom_text_repel(data = dplyr::filter(sd_data, Category == "高产不稳"),
        ggplot2::aes(label = Genotype), size = 3.5, max.overlaps = 15, color = "#FF8C00") +
      ggplot2::scale_color_manual(values = c("高产稳定" = "#2E8B57", "高产不稳" = "#FF8C00",
        "低产稳定" = "#1E90FF", "低产不稳" = "#DC143C")) +
      ggplot2::labs(title = "稳定性 x 产量", x = "平均产量", y = "SD(越小越稳定)") +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 16, face = "bold"))

    # Heatmap — 高产品种（高产稳定 + 高产不稳），低产在上
    step <- "heatmap"
    hm_genotypes <- sd_data %>%
      dplyr::filter(Category %in% c("高产稳定", "高产不稳")) %>%
      dplyr::arrange(Mean_Yield)
    if (nrow(hm_genotypes) >= 2 && nE >= 2) {
      hm_rows <- match(hm_genotypes$Genotype, yc$labelgen)
      hm_mat <- gm_mat[hm_rows, 1:nE, drop = FALSE]
      rownames(hm_mat) <- hm_genotypes$Genotype
      colnames(hm_mat) <- yc$labelenv[1:nE]
      hm <- as.data.frame(as.table(hm_mat))
      colnames(hm) <- c("品种", "环境", "中心化产量")
      result$heatmap <- ggplot2::ggplot(hm,
        ggplot2::aes(x = 环境, y = 品种, fill = `中心化产量`)) +
        ggplot2::geom_tile(color = "white", size = 0.5) +
        ggplot2::scale_fill_gradient2(low = "#D73027", mid = "#FFFFBF",
          high = "#1A9850", midpoint = 0, name = "中心化产量") +
        ggplot2::labs(title = "G×E互作热图", x = "环境", y = "品种") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
          axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, size = 12),
          axis.text.y = ggplot2::element_text(size = 11, margin = ggplot2::margin(t = 0, r = 0, b = 0, l = 0),
            lineheight = 1.4))
      result$heatmap_height <- max(300, nrow(hm_genotypes) * 14)
    }

    # Ranking
    step <- "ranking"
    tr <- sd_data %>%
      dplyr::arrange(dplyr::desc(Mean_Yield)) %>%
      dplyr::slice_head(n = min(20, nG))
    result$ranking <- ggplot2::ggplot(tr,
      ggplot2::aes(x = stats::reorder(Genotype, Mean_Yield), y = Mean_Yield)) +
      ggplot2::geom_bar(stat = "identity", fill = "#2E86C1", width = 0.7, alpha = 0.8) +
      ggplot2::geom_text(ggplot2::aes(label = round(Mean_Yield, 1)), hjust = -0.2, size = 3) +
      ggplot2::labs(title = paste("Top", min(20, nG)), x = "", y = "平均产量") +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"))

    # Env summary
    step <- "env"; result$env_summary <- ep %>%
      dplyr::mutate(区分力 = sqrt(PC1^2 + PC2^2)) %>%
      dplyr::arrange(dplyr::desc(区分力))

    TRUE
  }, error = function(e) {
    result$extract_error <<- paste0("[步骤:", step, "] ", e$message)
    FALSE
  })

  if (!ok) {
    return(list(skip = TRUE, reason = paste("结果提取失败:", result$extract_error)))
  }
  result
}

# 跨地点排名
analyze_cross_site_ranking <- function(df, trial_info) {
  # 支持原始列名和适配后列名
  yield_col <- if ("亩产_kg" %in% colnames(df)) "亩产_kg"
               else if ("MuChan" %in% colnames(df)) "MuChan"
               else return(NULL)
  if (!trial_info$is_multi_site) return(NULL)

  df$Mun <- safe_numeric(df[[yield_col]])

  # 确定分组列：优先用 stageid + name，fallback 单独用
  has_stageid <- "stageid" %in% colnames(df)
  has_name    <- "name" %in% colnames(df)
  group_cols  <- if (has_stageid && has_name) c("stageid", "name")
                 else if (has_stageid) "stageid"
                 else if (has_name) "name"
                 else return(NULL)

  # 过滤无效行（替代 dplyr::across 语法）
  df <- df %>%
    dplyr::filter(!is.na(.data$Mun)) %>%
    dplyr::filter(!is.na(place) & nchar(as.character(place)) > 0)
  for (gc in group_cols) {
    df <- df %>% dplyr::filter(!is.na(.data[[gc]]) & nchar(as.character(.data[[gc]])) > 0)
  }

  df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, "place")))) %>%
    dplyr::summarise(v = mean(.data$Mun, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(id_cols = dplyr::all_of(group_cols), names_from = place, values_from = .data$v) -> cs

  vc <- setdiff(colnames(cs), group_cols)
  if (length(vc) > 0) {
    cs$各点平均 <- rowMeans(cs[, vc, drop = FALSE], na.rm = TRUE)
    cs <- cs %>%
      dplyr::mutate(排名 = rank(-.data$各点平均, ties.method = "min")) %>%
      dplyr::arrange(.data$排名) %>%
      { 
        numeric_cols <- names(which(vapply(.[setdiff(names(.), "排名")], is.numeric, logical(1))))
        for (col in numeric_cols) .[[col]] <- round(.[[col]], 1)
        .
      }
  }
  list(table = cs)
}

# 分地点分析（支持 MS1R 和 MSMR，返回综合表）
analyze_per_site <- function(df, trial_info) {
  if (!trial_info$is_multi_site) return(NULL)

  yield_rows    <- list()
  growth_rows   <- list()
  increase_rows <- list()
  ck_rows       <- list()
  site_names    <- character()

  per_site_plots <- list(
    yield_dist    = list(),
    yield_grade   = list(),
    increase_dist = list(),
    growth_dist   = list()
  )

  for (site in trial_info$places) {
    sd <- df[df$place == site, , drop = FALSE]
    if (nrow(sd) < 2) next
    sr <- tryCatch(analyze_yield_core(sd), error = function(e) NULL)
    if (is.null(sr)) next

    site_names <- c(site_names, site)

    if (!is.null(sr$yield_stats)) {
      yield_rows[[length(yield_rows) + 1L]] <- sr$yield_stats
    }
    if (!is.null(sr$growth_stats)) {
      growth_rows[[length(growth_rows) + 1L]] <- sr$growth_stats
    }
    if (!is.null(sr$increase_stats)) {
      increase_rows[[length(increase_rows) + 1L]] <- sr$increase_stats
    }
    if (!is.null(sr$ck_mean)) {
      ck_rows[[length(ck_rows) + 1L]] <- sr$ck_mean
    }

    if (!is.null(sr$plots$yield_dist))       per_site_plots$yield_dist[[site]]    <- sr$plots$yield_dist
    if (!is.null(sr$plots$yield_grade_dist)) per_site_plots$yield_grade[[site]]   <- sr$plots$yield_grade_dist
    if (!is.null(sr$plots$increase_dist))    per_site_plots$increase_dist[[site]] <- sr$plots$increase_dist
    if (!is.null(sr$plots$growth_dist))      per_site_plots$growth_dist[[site]]   <- sr$plots$growth_dist
  }

  if (length(yield_rows) == 0L) return(NULL)

  build_combined <- function(rows, sites) {
    if (length(rows) == 0L) return(NULL)
    tbl <- dplyr::bind_rows(rows)
    tbl[["地点"]] <- sites
    tbl <- tbl[, c("地点", setdiff(colnames(tbl), "地点")), drop = FALSE]
    tbl
  }

  list(
    per_site_yield_stats    = build_combined(yield_rows,    site_names),
    per_site_growth_stats   = build_combined(growth_rows,   site_names),
    per_site_increase_stats = build_combined(increase_rows, site_names),
    per_site_ck_mean        = build_combined(ck_rows,       site_names),
    per_site_plots          = per_site_plots
  )
}
