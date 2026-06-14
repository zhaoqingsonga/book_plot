#' @title 生成增产与位次宽表
#' @description 
#' 对updated_summary数据同时处理增产和位次相关字段，生成合并的宽表，并计算各类统计与中文列名，并在最后追加各地点的田间备注。
#' - 支持多字段补全缺失为NA
#' - 汇总并宽化不同试点的指标
#' - 计算各指标均值、增产点次、试点数量等
#' - 字段名称换成中文，输出结果仅保留有数据的列
#' - 各地点追加TianJianBeiZhu（田间备注）宽表信息，每一列为某地点所有重复拼接的备注内容
#'
#' @param updated_summary 数据框，包含待处理的增产与位次相关数据
#' @return 增产宽表（数据框），字段含义详见列中文名
#' @examples
#' ZengChan_wide <- process_zengchan_wide(updated_summary)
#' # 如需先对updated_summary数值字段四舍五入：
#' # updated_summary <- modify_if(updated_summary, is.numeric, ~round(., 2))
process_ZengChan_wide <- function(updated_summary) {
  #############################################
  # 优化：同时处理增产和位次字段并合并到一个宽表中 
  fields_to_process <- c(
    "MuChan",
    "JiaoPingJunDuiZhaoWeiCi", 
    "JiaoLinJinDuiZhaoWeiCi", 
    "JiaoPingJunDuiZhaoZengChan", 
    "JiaoLinJinDuiZhaoZengChan"
  )
  
  keep_id_fields <- c("stageid", "name", "ma", "pa")
  
  # 补全缺失字段为NA
  for (fld in fields_to_process) {
    if (!(fld %in% names(updated_summary))) updated_summary[[fld]] <- NA
  }
  for (fld in keep_id_fields) {
    if (!(fld %in% names(updated_summary))) updated_summary[[fld]] <- NA
  }
  # --- 补全田间备注字段为NA（如无）
  if (!"TianJianBeiZhu" %in% names(updated_summary)) {
    updated_summary$TianJianBeiZhu <- NA_character_
  }
  
  my_summary <- function(x) {
    if (length(unique(x)) == 1) unique(x) else mean(x, na.rm = TRUE)
  }
  
  ZengChan_wide <- updated_summary %>%
    select(all_of(keep_id_fields), place, rp, treatment, all_of(fields_to_process)) %>%
    group_by(across(all_of(keep_id_fields)), place, rp, treatment) %>%
    summarize(across(all_of(fields_to_process), my_summary), .groups = "drop") %>%
    pivot_wider(
      id_cols = all_of(keep_id_fields),
      names_from = c(place, rp, treatment),
      values_from = all_of(fields_to_process),
      names_glue = "{.value}_{place}"
    )
  
  # 计算增产点次
  zengchan_fields <- c(
    "JiaoPingJunDuiZhaoZengChan" = "较平均对照增点几个点",
    "JiaoLinJinDuiZhaoZengChan" = "较临近对照增产几个点"
  )
  
  avg_prefix <- names(zengchan_fields)[1]
  linjin_prefix <- names(zengchan_fields)[2]
  
  avg_cols <- if (any(grepl(paste0("^", avg_prefix, "_"), names(ZengChan_wide)))) {
    grep(paste0("^", avg_prefix, "_"), names(ZengChan_wide), value = TRUE)
  } else {
    character(0)
  }
  
  linjin_cols <- if (any(grepl(paste0("^", linjin_prefix, "_"), names(ZengChan_wide)))) {
    grep(paste0("^", linjin_prefix, "_"), names(ZengChan_wide), value = TRUE)
  } else {
    character(0)
  }
  
  ZengChan_wide <- ZengChan_wide %>%
    mutate(
      共几个试点 = if (length(c(avg_cols, linjin_cols)) == 0) {
        0L
      } else {
        all_cols <- c(avg_cols, linjin_cols)
        extract_suffix <- function(x, prefix) sub(paste0("^", prefix, "_"), "", x)
        avg_suffix <- if(length(avg_cols) > 0) extract_suffix(avg_cols, avg_prefix) else character(0)
        linjin_suffix <- if(length(linjin_cols) > 0) extract_suffix(linjin_cols, linjin_prefix) else character(0)
        all_suffixes <- union(avg_suffix, linjin_suffix)
        apply(select(., all_of(all_cols)), 1, function(row) {
          count <- 0L
          for (suffix in all_suffixes) {
            avg_col <- if (suffix %in% avg_suffix) paste0(avg_prefix, "_", suffix) else NULL
            linjin_col <- if (suffix %in% linjin_suffix) paste0(linjin_prefix, "_", suffix) else NULL
            vals <- c()
            if (!is.null(avg_col)) vals <- c(vals, row[which(names(row) == avg_col)])
            if (!is.null(linjin_col)) vals <- c(vals, row[which(names(row) == linjin_col)])
            if (any(!is.na(vals))) count <- count + 1L
          }
          count
        })
      }
    ) %>%
    {
      df <- .
      for (i in seq_along(zengchan_fields)) {
        fld <- names(zengchan_fields)[i]
        new_col <- zengchan_fields[i]
        cols <- if (i == 1) avg_cols else linjin_cols
        
        if (length(cols) == 0) {
          df[[new_col]] <- 0L
        } else {
          df[[new_col]] <- rowSums(select(df, all_of(cols)) > 0, na.rm = TRUE)
        }
      }
      df
    }
  
  ZengChan_wide <- ZengChan_wide %>%
    relocate(共几个试点, 较平均对照增点几个点, 较临近对照增产几个点, .after = pa)
  
  for (fld in fields_to_process) {
    cols <- grep(paste0("^", fld, "_"), names(ZengChan_wide), value = TRUE)
    
    if (length(cols) > 0) {
      ZengChan_wide <- ZengChan_wide %>%
        mutate(
          "{fld}_平均" := rowMeans(select(., all_of(cols)), na.rm = TRUE)
        )
    } else {
      ZengChan_wide <- ZengChan_wide %>%
        mutate("{fld}_平均" := NA_real_)
    }
  }
  
  ZengChan_wide <- rank_by_place(ZengChan_wide, c("JiaoLinJinDuiZhaoWeiCi_平均", "JiaoPingJunDuiZhaoWeiCi_平均"), by_place = FALSE)
  
  ZengChan_wide <- ZengChan_wide %>%
    relocate(ends_with("_平均"), .after = last_col())
  
  ZengChan_wide <- modify_if(ZengChan_wide, is.numeric, ~round(., 2))
  
  ###################换成中文#########################
  name_map <- c(
    "MuChan" = "亩产",
    "JiaoPingJunDuiZhaoWeiCi" = "较平均对照位次",
    "JiaoLinJinDuiZhaoWeiCi" = "较临近对照位次",
    "JiaoPingJunDuiZhaoZengChan" = "较平均对照增产",
    "JiaoLinJinDuiZhaoZengChan" = "较临近对照增产"
  )
  ZengChan_wide <- ZengChan_wide %>%
    rename_with(
      \(nm) {
        for (old in names(name_map)) {
          new <- name_map[[old]]
          nm <- gsub(paste0("^", old), new, nm)
        }
        nm
      }
    )
  #####################################################    
  ZengChan_wide <- ZengChan_wide %>%
    dplyr::select(
      all_of(keep_id_fields),
      dplyr::where(~ sum(!is.na(.x)) > 0)
    )
  
  # ------ 追加各地点的TianJianBeiZhu宽表 -------
  # 补全田间备注缺失为""
  tmp_bz <- updated_summary
  for (fld in keep_id_fields) {
    if (!(fld %in% names(tmp_bz))) tmp_bz[[fld]] <- NA
  }
  if (!"TianJianBeiZhu" %in% names(tmp_bz)) tmp_bz$TianJianBeiZhu <- NA_character_
  tmp_bz$TianJianBeiZhu[is.na(tmp_bz$TianJianBeiZhu)] <- ""
  
  # 汇总方式：按 stageid, name, ma, pa, place 排列，rp重复备注拼接（用中文逗号）
  tianjian_remarks <- tmp_bz %>%
    group_by(across(all_of(keep_id_fields)), place) %>%
    arrange(across(all_of(keep_id_fields)), place, rp) %>%
    summarise(TianJianBeiZhu = paste(TianJianBeiZhu[TianJianBeiZhu != ""], collapse = "，"), .groups = "drop") %>%
    mutate(colname = paste0("TianJianBeiZhu_", place)) %>%
    select(all_of(keep_id_fields), colname, TianJianBeiZhu)
  
  # 宽表展开，多地点生成 TianJianBeiZhu_地点1、TianJianBeiZhu_地点2 ...
  tianjian_remarks_wide <- tianjian_remarks %>%
    tidyr::pivot_wider(id_cols = all_of(keep_id_fields), names_from = colname, values_from = TianJianBeiZhu)
  
  # 合并到主表，确保stageid, name, ma, pa为主键左连接
  ZengChan_wide <- ZengChan_wide %>%
    left_join(tianjian_remarks_wide, by = keep_id_fields)
  
  # ---------- END REMARKS ---------------
  ZengChan_wide
}

# 使用方式：ZengChan_wide <- process_zengchan_wide(updated_summary)
# 如有需要，还可将 updated_summary 的数值字段四舍五入处理如下（按原逻辑）：
# updated_summary <- modify_if(updated_summary, is.numeric, ~round(., 2))



#' 亩产宽表预处理函数
#' 
#' 该函数用于将包含亩产数据的长表转换为宽表，按stageid分组，并添加每行的均值列，最终保留数值列两位小数
#' 
#' @param updated_summary 数据框，包含亩产分析所需的核心列：stageid, place, rp, treatment, MuChan（若MuChan列不存在会自动添加NA）
#' @return 返回处理后的亩产宽表数据框，包含stageid、各组合列（place_rp_treatment）、平均列，所有数值列保留两位小数
#' @examples
#' # 示例使用（假设有updated_summary数据框）
#' # yield_wide_result <- prepare_yield_wide_table(updated_summary)
#' @import dplyr
#' @import tidyr
process_yield_wide<- function(updated_summary) {
  # 检查并添加MuChan列（若不存在）
  if (!"MuChan" %in% names(updated_summary)) {
    updated_summary$MuChan <- NA
  }
  
  # 选择核心列
  yield_wide <- updated_summary[c("stageid", "place", "rp", "treatment", "MuChan")]
  
  # 定义自定义汇总函数：若唯一值只有1个则取该值，否则取均值（忽略NA）
  my_summary <- function(x) {
    if (length(unique(x)) == 1) {
      unique(x)
    } else {
      mean(x, na.rm = TRUE)
    }
  }
  
  # 数据处理主流程：分组汇总 -> 转宽表 -> 添加平均列 -> 保留两位小数
  yield_wide <- yield_wide %>%
    group_by(stageid, place, rp, treatment) %>%
    summarize(MuChan = my_summary(MuChan), .groups = "drop") %>%
    pivot_wider(
      id_cols = stageid,
      names_from = c(place, rp, treatment),
      values_from = MuChan
    ) %>%
    # 添加平均列：计算每行（每个stageid）所有产量列的均值（忽略NA）
    mutate(
      平均 = rowMeans(select(., -stageid), na.rm = TRUE)  # 排除stageid列，计算其他列的均值
    )
  
  # 所有数值列保留两位小数
  yield_wide <- modify_if(yield_wide, is.numeric, ~round(., 2))
  
  # 返回处理后的宽表
  return(yield_wide)
}




#' 按地点分组（可选）对指定字段进行排名
#' 
#' 该函数用于对数据框中的指定数值字段执行排名操作，支持两种模式：按place字段分组排名（默认）或全局排名。
#' 排名时会自动忽略NA值，并列值采用"min"方法（即并列值取最小排名），排名结果会替换原字段值。
#' 
#' @param data 数据框，输入的原始数据（对应原逻辑中的updated_summary）。当by_place=TRUE时，需包含place字段；
#' 必须包含vars参数指定的所有字段。
#' @param vars 字符向量，指定需要进行排名的字段名称（如c("MuChan", "ZhuGao")）。
#' @param by_place 逻辑值，可选，默认为TRUE。TRUE表示按place字段分组后对指定字段排名；FALSE表示全局排名（不分组）。
#' 
#' @return 数据框，在原始数据基础上，将vars指定的字段替换为对应的排名值（NA值位置仍为NA）。
#' 
#' @examples
#' # 构造测试数据
#' test_data <- data.frame(
#'   place = rep(c("北京", "上海"), each = 5),
#'   MuChan = c(800, 750, 800, NA, 700, 900, 850, 900, 880, NA),
#'   ZhuGao = c(120, 115, 120, 110, NA, 130, 125, 130, 128, 122)
#' )
#' 
#' # 按place分组对MuChan字段排名
#' result1 <- rank_by_place(test_data, vars = "MuChan")
#' 
#' # 全局对MuChan和ZhuGao字段排名（不按place分组）
#' result2 <- rank_by_place(test_data, vars = c("MuChan", "ZhuGao"), by_place = FALSE)
#' 
#' @import dplyr
rank_by_place <- function(data, vars, by_place = TRUE) {
  # 定义内部排名函数：对指定字段执行排名（忽略NA，并列取最小排名）
  ranking_fun <- function(df) {
    mutate(df, across(
      all_of(vars),
      ~{
        non_na_pos <- !is.na(.x)
        ranks <- rank(.x[non_na_pos], ties.method = "min")
        res <- rep(NA, length(.x))
        res[non_na_pos] <- ranks
        res
      }
    ))
  }
  
  # 根据by_place参数决定是否按place分组
  if (by_place) {
    data %>%
      group_by(place) %>%
      ranking_fun() %>%
      ungroup()
  } else {
    data %>%
      ranking_fun()
  }
}


