# ============ 解析田间布局字符串，提取水沟/观察道/保护行列位置 ============
getWaterLane<-function(mstr="w/23/2r/w/2p/w/2p/23/w") {
  if(is.null(mstr) || nchar(trimws(mstr))==0){
    stop("田间布局不能为空")
  }
  tokens<-unlist(strsplit(mstr,"/"))
  layout_vec<-NULL
  for(i in 1:length(tokens)){
    if(tokens[i]=='w'){
      layout_vec<-c(layout_vec,CODE_WATER)
    }else if(tokens[i]=='r'){
      layout_vec<-c(layout_vec,CODE_LANE)
    }else if(length((grep('r',tokens[i])==1))==1){
      lane_n<-suppressWarnings(as.numeric(sub('r','',tokens[i])))
      if(is.na(lane_n) || lane_n<=0) stop(paste0("田间布局格式错误：'", tokens[i], "' 不是有效的观察道写法"))
      layout_vec<-c(layout_vec,rep(CODE_LANE,lane_n))
    }else if(tokens[i]=='p'){
      layout_vec<-c(layout_vec,CODE_PROTECT_COL)
    }else if(length((grep('p',tokens[i])==1))==1){
      protect_n<-suppressWarnings(as.numeric(sub('p','',tokens[i])))
      if(is.na(protect_n) || protect_n<=0) stop(paste0("田间布局格式错误：'", tokens[i], "' 不是有效的保护行写法"))
      layout_vec<-c(layout_vec,rep(CODE_PROTECT_COL,protect_n))
    }else{
      lane_width<-suppressWarnings(as.numeric(tokens[i]))
      if(is.na(lane_width) || lane_width<=0) stop(paste0("田间布局格式错误：'", tokens[i], "' 不是有效的列宽数字"))
      layout_vec<-c(layout_vec,rep(1,lane_width))
    }
  }
  return(list(
    total_cols   = length(layout_vec),
    water_cols   = which(layout_vec==CODE_WATER),
    lane_cols    = which(layout_vec==CODE_LANE),
    protect_cols = which(layout_vec==CODE_PROTECT_COL)
  ))
}

# ============ 解析各条（block）宽度配置字符串 ============
modibridges<-function(bridges){
  if(is.null(bridges) || nchar(trimws(bridges))==0) stop("各条宽度配置不能为空")
  bri<-NULL
  ori<-unlist(strsplit(bridges,","))
  for(i in 1:length(ori)){
    unit1<-as.numeric(unlist(strsplit(ori[i],"/")))
    if(any(is.na(unit1))) stop(paste0("各条宽度配置格式错误：'", ori[i], "'"))
    ti<-length(unit1)
    if(ti==1){
      bri<-c(bri,unit1)
    }else{
      bri<-c(bri,rep(unit1[1],unit1[2]))
    }
  }
  return(bri)
}

# ============ 解析田间规划参数TXT文件 ============
parseParamTxt<-function(file_path){
  if(!file.exists(file_path)) stop("参数文件不存在: ", file_path)
  lines<-readLines(file_path, warn=FALSE, encoding="UTF-8")
  lines<-lines[nchar(trimws(lines))>0]
  lines<-lines[!grepl("^#", trimws(lines))]
  allowed_keys<-c(
    "field_len",
    "no_plant",
    "field_layout",
    "strip_width",
    "protect_strip",
    "cross_path_width",
    "row_gap",
    "group_rows",
    "plant_start_pos",
    "plant_end_pos",
    "plant_start_row",
    "plant_start_col",
    "plant_end_col",
    "plan_left",
    "plant_left"
  )
  parse_bool<-function(x){
    xv<-toupper(trimws(x))
    if(xv %in% c("TRUE","T","1","YES","Y")) return(TRUE)
    if(xv %in% c("FALSE","F","0","NO","N")) return(FALSE)
    return(as.logical(x))
  }
  params<-list()
  for(line in lines){
    eq_pos<-regexpr("=", line, fixed=TRUE)[1]
    if(eq_pos<=0) next
    k<-trimws(substr(line, 1, eq_pos-1))
    if(!(k %in% allowed_keys)){
      stop(
        paste0(
          "参数键不支持: '", k, "'。请仅使用新英文键名：",
          paste(allowed_keys, collapse=", ")
        )
      )
    }
    v<-trimws(substr(line, eq_pos+1, nchar(line)))
    v<-trimws(sub("\\s+#.*$", "", v))
    if(v=="") next
    if(k %in% c("field_len","cross_path_width","row_gap","group_rows","plant_start_row","plant_start_col","plant_end_col")){
      params[[k]]<-as.numeric(v)
    }else if(k %in% c("plan_left","plant_left")){
      params[[k]]<-parse_bool(v)
    }else{
      params[[k]]<-v
    }
  }
  return(params)
}

# ============ 解析并校验种植列范围（纯函数） ============
parsePlantingColumnRange<-function(start_col_text, end_col_text, total_cols){
  parse_one <- function(x){
    if(is.null(x)) return(NULL)
    if(length(x) == 0) return(NULL)
    if(length(x) > 1) x <- x[1]
    if(is.numeric(x) && is.na(x)) return(NULL)
    if(is.character(x) && (is.na(x) || !nzchar(trimws(x)) || identical(trimws(x), "NA"))) return(NULL)
    xv <- trimws(as.character(x))
    if(length(xv) == 0 || is.na(xv) || !nzchar(xv) || identical(xv, "NA")) return(NULL)
    num <- suppressWarnings(as.numeric(xv))
    if(length(num) != 1 || is.na(num) || num != as.integer(num)){
      stop("种植列范围必须为整数")
    }
    as.integer(num)
  }

  start_col <- parse_one(start_col_text)
  end_col <- parse_one(end_col_text)

  if(is.null(start_col)) start_col <- 1L
  if(is.null(end_col)) end_col <- as.integer(total_cols)

  if(start_col < 1 || start_col > total_cols){
    stop(paste0("种植起始列必须在 1 到 ", total_cols, " 之间"))
  }
  if(end_col < 1 || end_col > total_cols){
    stop(paste0("种植终止列必须在 1 到 ", total_cols, " 之间"))
  }
  if(start_col > end_col){
    stop("种植起始列不能大于终止列")
  }

  list(start_col = start_col, end_col = end_col)
}

sanitizePlantingColumnRange<-function(start_col_text, end_col_text, total_cols){
  parse_one_relaxed <- function(x){
    if(is.null(x) || length(x) == 0) return(NULL)
    if(length(x) > 1) x <- x[1]
    if(is.numeric(x) && is.na(x)) return(NULL)
    if(is.character(x)){
      if(is.na(x)) return(NULL)
      x <- trimws(x)
      if(!nzchar(x) || identical(x, "NA")) return(NULL)
    }
    suppressWarnings(as.numeric(as.character(x)))
  }

  start_col <- parse_one_relaxed(start_col_text)
  end_col <- parse_one_relaxed(end_col_text)

  if(is.na(start_col) || is.null(start_col)) start_col <- 1
  if(is.na(end_col) || is.null(end_col)) end_col <- total_cols

  start_col <- floor(start_col)
  end_col <- floor(end_col)
  start_col <- max(1, min(total_cols, start_col))
  end_col <- max(1, min(total_cols, end_col))

  if(start_col > end_col){
    end_col <- max(start_col, min(total_cols, end_col))
    start_col <- min(start_col, end_col)
  }

  list(start_col = as.integer(start_col), end_col = as.integer(end_col))
}

parsePlantingStartRow<-function(start_row_text){
  if(is.null(start_row_text) || length(start_row_text) == 0) return(1L)
  x <- start_row_text[1]
  if(is.numeric(x) && is.na(x)) return(1L)
  xv <- trimws(as.character(x))
  if(length(xv) == 0 || is.na(xv) || !nzchar(xv) || identical(xv, "NA")) return(1L)
  num <- suppressWarnings(as.numeric(xv))
  if(length(num) != 1 || is.na(num) || num != as.integer(num) || num < 1){
    stop("种植起始行号必须为大于等于 1 的整数")
  }
  as.integer(num)
}

parsePlantingCoordinateRange <- function(start_pos_text, end_pos_text, max_row, max_col){
  normalize_text <- function(x){
    if(is.null(x) || length(x) == 0) return("")
    x <- x[1]
    if(is.numeric(x) && is.na(x)) return("")
    xv <- trimws(as.character(x))
    if(is.na(xv) || !nzchar(xv) || identical(xv, "NA")) return("")
    xv
  }

  parse_piece <- function(piece, label){
    p <- trimws(piece)
    if(!nzchar(p)) return(NA_integer_)
    num <- suppressWarnings(as.numeric(p))
    if(length(num) != 1 || is.na(num) || num != as.integer(num) || num < 1){
      stop(paste0("种植", label, "必须为大于等于 1 的整数"))
    }
    as.integer(num)
  }

  parse_pos <- function(text, default_row, default_col, name){
    x <- normalize_text(text)
    if(!nzchar(x)) return(list(row = as.integer(default_row), col = as.integer(default_col)))
    parts <- strsplit(x, ",", fixed = TRUE)[[1]]
    if(length(parts) == 1){
      row <- parse_piece(parts[1], paste0(name, "行号"))
      col <- as.integer(default_col)
      return(list(row = row, col = col))
    }
    if(length(parts) != 2){
      stop(paste0("种植", name, "位置格式错误，应为 n,n / n, / ,n / n"))
    }
    row <- parse_piece(parts[1], paste0(name, "行号"))
    col <- parse_piece(parts[2], paste0(name, "列号"))
    if(is.na(row)) row <- as.integer(default_row)
    if(is.na(col)) col <- as.integer(default_col)
    list(row = row, col = col)
  }

  start <- parse_pos(start_pos_text, default_row = 1L, default_col = 1L, name = "起始")
  end <- parse_pos(end_pos_text, default_row = max_row, default_col = max_col, name = "终止")

  if(start$row < 1 || start$row > max_row) stop(paste0("种植起始行必须在 1 到 ", max_row, " 之间"))
  if(start$col < 1 || start$col > max_col) stop(paste0("种植起始列必须在 1 到 ", max_col, " 之间"))
  if(end$row < 1 || end$row > max_row) stop(paste0("种植终止行必须在 1 到 ", max_row, " 之间"))
  if(end$col < 1 || end$col > max_col) stop(paste0("种植终止列必须在 1 到 ", max_col, " 之间"))
  if(start$row > end$row) stop("种植起始行不能大于终止行")
  if(start$col > end$col) stop("种植起始列不能大于终止列")

  list(
    start_row = as.integer(start$row),
    start_col = as.integer(start$col),
    end_row = as.integer(end$row),
    end_col = as.integer(end$col)
  )
}

sanitizePlantingStartRow<-function(start_row_text){
  if(is.null(start_row_text) || length(start_row_text) == 0) return(1L)
  x <- start_row_text[1]
  if(is.numeric(x) && is.na(x)) return(1L)
  xv <- trimws(as.character(x))
  if(length(xv) == 0 || is.na(xv) || !nzchar(xv) || identical(xv, "NA")) return(1L)
  num <- suppressWarnings(as.numeric(xv))
  if(length(num) != 1 || is.na(num)) return(1L)
  as.integer(max(1, floor(num)))
}

# ============ 解析并校验 datasetInput 的关键参数（纯函数） ============
parseDesignInputParams<-function(protected_blocks_text, p_a_text){
  if(is.null(protected_blocks_text)) protected_blocks_text <- ""
  protected_blocks_raw<-trimws(unlist(strsplit(protected_blocks_text,',')))
  protected_blocks_raw<-protected_blocks_raw[nchar(protected_blocks_raw)>0]
  if(length(protected_blocks_raw)==0){
    protected_blocks <- numeric(0)
  } else {
    protected_blocks<-suppressWarnings(as.numeric(protected_blocks_raw))
  }

  if(length(protected_blocks)>0 && any(is.na(protected_blocks))){
    stop("横向保护行必须为数字，逗号分隔")
  }
  if(length(protected_blocks)>0 && any(protected_blocks < 0)){
    stop("横向保护行不能为负数")
  }
  # 约定：留空或输入 0 表示无横向保护行
  if(length(protected_blocks)>0){
    protected_blocks <- protected_blocks[protected_blocks > 0]
  }

  if(nchar(trimws(p_a_text))>0){
    p_a_raw<-trimws(unlist(strsplit(p_a_text,',')))
    p_a_raw<-p_a_raw[nchar(p_a_raw)>0]
    p_a<-suppressWarnings(as.numeric(p_a_raw))
    if(length(p_a)==0){
      stop("不能种植地块参数不能为空")
    }
    if(any(is.na(p_a))){
      stop("不能种植地块必须全部为数字")
    }
    if(length(p_a)%%BLOCK_RANGE_PARAM_GROUP_SIZE!=0){
      stop(paste0("不能种植地块参数个数必须是", BLOCK_RANGE_PARAM_GROUP_SIZE, "的倍数"))
    }
  }else{
    p_a<-NULL
  }

  list(
    protected_blocks = protected_blocks,
    p_a = p_a
  )
}
