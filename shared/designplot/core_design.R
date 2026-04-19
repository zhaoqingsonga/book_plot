# ============ 统计各条（block）可种植行数 ============
safeSeq <- function(from, to, by = 1){
  if(any(is.na(c(from, to, by))) || by == 0) return(integer(0))
  if((by > 0 && from > to) || (by < 0 && from < to)) return(integer(0))
  seq(from, to, by)
}

getSta<-function(field=designPlot()){
  num_cols<-ncol(field)
  num_rows<-nrow(field)
  even_idx <- safeSeq(2, num_rows, 2)
  if(length(even_idx) == 0){
    return(data.frame(排长度 = numeric(0), 排数 = numeric(0), 行数 = numeric(0)))
  }
  even_rows<-field[even_idx,]
  data_cols<-even_rows[,1:(num_cols-STAT_COL_COUNT)]
  data_cols[data_cols<0]=0
  data_cols[data_cols>0]=1
  block_sums<-cbind(even_rows[,(num_cols-STAT_COL_OFFSET_INTERVAL)],apply(data_cols,1,sum))
  block_agg<-aggregate(block_sums,by=list(name=block_sums[,1]),FUN=sum)
  block_agg$V1=block_agg[,2]/block_agg[,1]
  colnames(block_agg)<-c("排长度","排数","行数")
  return(block_agg)
}

# ============ 提取指定种植行列范围内的可种植位点（按序号排序） ============
extractPlantingTargets<-function(field, start_row=1, start_col=1, end_row=NULL, end_col=NULL){
  data_cols<-ncol(field)-STAT_COL_COUNT
  rowno_vec <- suppressWarnings(as.numeric(field[, ncol(field)-STAT_COL_OFFSET_ROWNO]))
  valid_rows <- rowno_vec[!is.na(rowno_vec)]
  max_row <- if(length(valid_rows) > 0) max(valid_rows) else nrow(field)
  if(is.null(end_col)) end_col<-data_cols
  if(is.null(end_row)) end_row<-max_row
  if(any(is.na(start_row)) || start_row!=as.integer(start_row)){
    stop("种植起始行号必须为大于等于 1 的整数")
  }
  if(any(is.na(end_row)) || end_row!=as.integer(end_row)){
    stop("种植终止行号必须为大于等于起始行的整数")
  }
  if(start_row < 1) stop("种植起始行号必须为大于等于 1 的整数")
  if(end_row < start_row) stop("种植起始行号不能大于终止行号")
  if(any(is.na(c(start_col, end_col))) || start_col!=as.integer(start_col) || end_col!=as.integer(end_col)){
    stop("种植列范围必须为整数")
  }
  if(start_col < 1 || start_col > data_cols) stop(paste0("种植起始列必须在 1 到 ", data_cols, " 之间"))
  if(end_col < 1 || end_col > data_cols) stop(paste0("种植终止列必须在 1 到 ", data_cols, " 之间"))
  if(start_col > end_col) stop("种植起始列不能大于终止列")

  positions<-which(field[,start_col:end_col, drop=FALSE] > 0, arr.ind=TRUE)
  if(nrow(positions)==0){
    return(data.frame(
      seq_no=integer(),
      field_row_index=integer(),
      field_row=numeric(),
      field_col=integer(),
      stringsAsFactors=FALSE
    ))
  }

  cell_vals <- field[cbind(positions[, 1], positions[, 2] + start_col - 1)]
  seq_vals <- suppressWarnings(as.numeric(cell_vals))
  seq_no <- as.integer(seq_vals)
  result <- data.frame(
    seq_no = seq_no,
    field_row_index = positions[, 1],
    field_row = suppressWarnings(as.numeric(field[positions[, 1], ncol(field) - STAT_COL_OFFSET_ROWNO])),
    field_col = positions[, 2] + start_col - 1,
    stringsAsFactors = FALSE
  )
  ok_seq <- !is.na(result$seq_no) & result$seq_no > 0L
  result <- result[ok_seq, , drop = FALSE]
  result <- result[result$field_row >= start_row & result$field_row <= end_row, , drop = FALSE]
  if(nrow(result)==0){
    return(data.frame(
      seq_no=integer(),
      field_row_index=integer(),
      field_row=numeric(),
      field_col=integer(),
      stringsAsFactors=FALSE
    ))
  }
  result[order(result$seq_no), , drop=FALSE]
}

# ============ 将材料名称填入规划矩阵的编号位置 ============
plant<-function(field=designPlot(),seed=makelist(), start_row=1, start_col=1, end_row=NULL, end_col=NULL){
  targets<-extractPlantingTargets(field, start_row=start_row, start_col=start_col, end_row=end_row, end_col=end_col)
  max_assign<-min(nrow(seed), nrow(targets))
  if(max_assign<=0) return(field)
  for (j in 1:max_assign){
    field[targets$field_row_index[j], targets$field_col[j]]<-paste(seed[j,2], seed[j,1], targets$seq_no[j], sep="|")
  }
  return(field)
}

# ============ 判断向量中是否包含特殊编码（水沟/观察道/保护行等） ============
myAny<-function(vec1){
  return(any(vec1%in%SPECIAL_CODES))
}

# ============ 提取简表：只保留含特殊编码的列及相邻列 ============
selectedCol<-function(fi=designPlot()){
  nc<-ncol(fi)
  result<-which(apply(fi,2,myAny)==TRUE)
  result1<-result+1
  result_1<-result-1
  result2<-sort(c(1,result,result1,result_1,(nc-STAT_COL_COUNT):nc))
  result2<-unique(result2[result2 >= 1 & result2 <= nc])
  return(fi[,result2])
}

# ============ 规范化左右镜像相关输入 ============
normalizeMirroredColumns <- function(y, protected_columns, water_columns, lane_columns, p_a, design_from_left){
  if (isTRUE(design_from_left)) {
    return(list(
      protected_columns = protected_columns,
      water_columns = water_columns,
      lane_columns = lane_columns,
      p_a = p_a
    ))
  }

  protected_columns <- mirrorColumns(protected_columns, y)
  water_columns <- mirrorColumns(water_columns, y)
  lane_columns <- mirrorColumns(lane_columns, y)

  if (!is.null(p_a)) {
    sele <- c(
      (0:(length(p_a) / BLOCK_RANGE_PARAM_GROUP_SIZE - 1)) * BLOCK_RANGE_PARAM_GROUP_SIZE + 3,
      (0:(length(p_a) / BLOCK_RANGE_PARAM_GROUP_SIZE - 1)) * BLOCK_RANGE_PARAM_GROUP_SIZE + 4
    )
    p_a[sele] <- mirrorColumns(p_a[sele], y)

    # 镜像后起止列可能发生前后颠倒，这里规范为 [min, max]
    p_a_mat <- matrix(p_a, ncol = BLOCK_RANGE_PARAM_GROUP_SIZE, byrow = TRUE)
    start_cols <- p_a_mat[, 3]
    end_cols <- p_a_mat[, 4]
    p_a_mat[, 3] <- pmin(start_cols, end_cols)
    p_a_mat[, 4] <- pmax(start_cols, end_cols)
    p_a <- as.vector(t(p_a_mat))
  }

  list(
    protected_columns = protected_columns,
    water_columns = water_columns,
    lane_columns = lane_columns,
    p_a = p_a
  )
}

# ============ 规范化各条宽度向量 ============
normalizeBridges <- function(bridges, blocks){
  if(length(bridges) == 1){
    bridges <- rep(bridges, blocks)
  }
  rev(c(bridges, rep(0, blocks - length(bridges))))
}

# ============ 收尾：修正统计列并统一列名 ============
finalizeFieldColumns <- function(field, y){
  field[, y + STAT_COL_OFFSET_INTERVAL][field[, y + STAT_COL_OFFSET_INTERVAL] == CODE_GROUP_PAD] <- NA
  field[, y + STAT_COL_COUNT - STAT_COL_OFFSET_INTERVAL][field[, y + STAT_COL_COUNT - STAT_COL_OFFSET_INTERVAL] == CODE_GROUP_PAD] <- 0
  colnames(field) <- c(paste0("V", 1:y), STAT_COL_NAMES)
  field
}

# ============ 根据材料清单（品种+行数）生成逐行种植列表 ============
makelist<-function(myd=data.frame(num=1:3,name=c('JD12','JD17','五星1'),re=c(3,10,6))){
  if(is.null(myd) || nrow(myd)==0) stop("材料清单不能为空")
  if(ncol(myd)<3) stop("材料清单至少需要3列（编号、名称、行数）")
  result<-NULL
  number<-NULL
  for (i in 1:nrow(myd)){
    result<-c(result,as.character(rep(myd[i,2],myd[i,3])))
    number<-c(number,1:myd[i,3])
  }
  return(data.frame(number,result))
}

# ============ 生成带田间行列位置的逐行种植列表 ============
makelistWithPos<-function(myd, field, start_row=1, start_col=1, end_row=NULL, end_col=NULL){
  seeds_list<-makelist(myd)
  targets<-extractPlantingTargets(field, start_row=start_row, start_col=start_col, end_row=end_row, end_col=end_col)
  field_row<-rep(NA, nrow(seeds_list))
  field_col<-rep(NA, nrow(seeds_list))
  assign_n<-min(nrow(seeds_list), nrow(targets))
  if(assign_n>0){
    field_row[1:assign_n]<-targets$field_row[1:assign_n]
    field_col[1:assign_n]<-targets$field_col[1:assign_n]
  }
  seeds_list$field_row<-field_row
  seeds_list$field_col<-field_col
  return(seeds_list)
}

fillUnplantedWithMaterial <- function(field, material_name, start_row=1, start_col=1, end_row=NULL, end_col=NULL){
  if(is.null(field) || !is.matrix(field) || ncol(field) <= STAT_COL_COUNT){
    stop("种植地块矩阵不合法")
  }
  mat_name <- trimws(as.character(material_name))
  if(!nzchar(mat_name)) stop("补种材料不能为空")

  data_cols <- ncol(field) - STAT_COL_COUNT
  rowno_idx <- ncol(field) - STAT_COL_OFFSET_ROWNO
  rowno_vec <- suppressWarnings(as.numeric(field[, rowno_idx]))
  valid_rows <- rowno_vec[!is.na(rowno_vec)]
  max_row <- if(length(valid_rows) > 0) max(valid_rows) else nrow(field)
  if(is.null(end_row)) end_row <- max_row
  if(is.null(end_col)) end_col <- data_cols

  if(any(is.na(start_row)) || start_row != as.integer(start_row) || start_row < 1) stop("种植起始行号必须为大于等于 1 的整数")
  if(any(is.na(end_row)) || end_row != as.integer(end_row) || end_row < start_row) stop("种植终止行号必须为大于等于起始行的整数")
  if(any(is.na(c(start_col, end_col))) || start_col != as.integer(start_col) || end_col != as.integer(end_col)) stop("种植列范围必须为整数")
  if(start_col < 1 || start_col > data_cols) stop(paste0("种植起始列必须在 1 到 ", data_cols, " 之间"))
  if(end_col < 1 || end_col > data_cols) stop(paste0("种植终止列必须在 1 到 ", data_cols, " 之间"))
  if(start_col > end_col) stop("种植起始列不能大于终止列")

  parse_seq_no <- function(cell_value){
    text <- trimws(as.character(cell_value))
    if(!nzchar(text)) return(NA_integer_)
    if(grepl("^[0-9]+$", text)) return(as.integer(text))
    parts <- strsplit(text, "\\|", fixed = FALSE)[[1]]
    if(length(parts) == 3){
      seq_no <- suppressWarnings(as.integer(parts[3]))
      if(!is.na(seq_no) && seq_no > 0) return(seq_no)
    }
    NA_integer_
  }

  targets <- list()
  t_idx <- 1L
  for(r in seq_len(nrow(field))){
    row_no <- suppressWarnings(as.numeric(field[r, rowno_idx]))
    if(is.na(row_no) || row_no < start_row || row_no > end_row) next
    for(c in seq_len(data_cols)){
      if(c < start_col || c > end_col) next
      seq_no <- parse_seq_no(field[r, c])
      if(is.na(seq_no) || seq_no <= 0) next
      targets[[t_idx]] <- list(field_row_index = r, field_col = c, seq_no = seq_no)
      t_idx <- t_idx + 1L
    }
  }
  if(length(targets) == 0){
    return(list(matrix = field, filled_count = 0L, total_targets = 0L))
  }

  updated <- field
  filled <- 0L
  for(i in seq_along(targets)){
    r <- targets[[i]]$field_row_index
    c <- targets[[i]]$field_col
    current <- trimws(as.character(updated[r, c]))
    if(grepl("^[0-9]+$", current)){
      updated[r, c] <- paste(mat_name, 1, targets[[i]]$seq_no, sep = "|")
      filled <- filled + 1L
    }
  }

  list(matrix = updated, filled_count = as.integer(filled), total_targets = as.integer(length(targets)))
}

# ============ 从位置j开始取subg长度的子向量（分组种植辅助函数） ============
getSub<-function(j=1,vec=c(0,0,-77,0,0,0),subg=3){
  vec_len<-length(vec)
  if((j+subg-1)<=vec_len){
    sub_vec<-vec[j:(j+subg-1)]
  }else if(j<=vec_len){
    sub_vec<-vec[j:vec_len]
  }else{
    sub_vec<-vec[vec_len]
  }
  return(sub_vec)
}

# ============ 按组种植：连续subg个空行为一组，不足一组的补保护行标记 ============
plantByGroup<-function(ma,subg=3,mysign=-6){
  myncol<-ncol(ma)
  for(i in 1:nrow(ma)){
    j<-1
    while(j<=myncol){
      mysub<-getSub(j,ma[i,1:myncol],subg)
      mylen<-length(mysub)
      is0<-all(mysub==0)
      if(mylen==subg&is0){
        j<-j+subg
      }else{
        if(ma[i,j]==0){ma[i,j]<-mysign}
        j<-j+1
      }
    }
  }
  return(ma)
}

# ============ 镜像列位置（从左侧规划改为从右侧） ============
mirrorColumns<-function(cols, total_cols){
  return((1+total_cols)-cols)
}

# ============ 初始化田间矩阵：填充间隔行和观察道行 ============
initFieldMatrix<-function(total_rows, total_cols, water_columns, lane_columns,
                          ww, w, bridges, blocks){
  field<-matrix(rep(0,total_rows*(total_cols+1)),c(total_rows,total_cols+1))
  field[,water_columns]<-CODE_WATER
  row_1_4 <- safeSeq(1, total_rows, 4)
  row_3_4 <- safeSeq(3, total_rows, 4)
  row_even <- safeSeq(2, total_rows, 2)
  if((total_rows/2)%%2==0){
    field[row_1_4,]<-CODE_INTERVAL
    field[row_3_4,]<-CODE_ROAD
    field[row_1_4,total_cols+1]<-w
    field[row_3_4,total_cols+1]<-ww
  }else{
    field[row_1_4,]<-CODE_ROAD
    field[row_3_4,]<-CODE_INTERVAL
    field[row_1_4,total_cols+1]<-ww
    field[row_3_4,total_cols+1]<-w
  }
  field[,lane_columns]<-CODE_LANE
  field[row_even,total_cols+1]<-bridges
  return(field)
}

# ============ 填充保护行/列及附加统计列 ============
fillProtection<-function(field, total_cols, total_rows, blocks,
                         protected_columns, protected_blocks){
  protected_rows <- integer(0)
  if(length(protected_blocks) > 0){
    protected_rows <- (blocks - protected_blocks + 1) * 2
  }
  field[,protected_columns][field[,protected_columns]==0]<-CODE_PROTECT_COL
  if(length(protected_rows) > 0){
    field[protected_rows,1:total_cols][field[protected_rows,1:total_cols]==0]<-CODE_PROTECT_ROW
  }
  field<-cbind(field,rev(cumsum(rev(field[,total_cols+1]))))
  field<-cbind(field,rep(0,total_rows))
  even_idx <- safeSeq(2, total_rows, 2)
  if(length(even_idx) > 0){
    field[,total_cols+3][even_idx]<-blocks:1
  }
  field<-cbind(field,1:total_rows)
  return(field)
}

# ============ 标记不能种植的地块 ============
markBlockedAreas<-function(field, total_cols, p_a){
  if(is.null(p_a)) return(field)
  cum_widths<-rev(cumsum(rev(field[,total_cols+1])))
  cum_next<-c(cum_widths[2:length(cum_widths)],0)
  cum_range<-cbind(cum_next,cum_widths)
  block_defs<-matrix(p_a,c(length(p_a)/BLOCK_RANGE_PARAM_GROUP_SIZE, BLOCK_RANGE_PARAM_GROUP_SIZE),byrow=TRUE)
  for(bi in 1:nrow(block_defs)){
    affected_rows<-(cum_range[,2]>block_defs[bi,1]&block_defs[bi,1]>cum_range[,1])|
      (cum_range[,2]>block_defs[bi,2]&block_defs[bi,2]>cum_range[,1])|
      (block_defs[bi,2]>cum_range[,1]&cum_range[,1]>block_defs[bi,1])|
      (block_defs[bi,2]>cum_range[,2]&cum_range[,2]>block_defs[bi,1])
    field[affected_rows,block_defs[bi,3]:block_defs[bi,4]]<-CODE_BLOCKED
  }
  return(field)
}

# ============ 蛇形编号：从底部向上交替左右方向填入材料序号 ============
applySerpentineNumbering<-function(field, total_cols, total_rows, marker1){
  direction<-1
  num<-1
  for(i in rev(safeSeq(2,total_rows,2))){
    if(direction%%2==marker1){
      filled_count<-0
      for(j in 1:total_cols){
        if(field[i,j]==0){field[i,j]<-num; num<-num+1; filled_count<-filled_count+1}
      }
      if(filled_count!=0){direction<-direction+1}
    }else{
      filled_count<-0
      for(j in total_cols:1){
        if(field[i,j]==0){field[i,j]<-num; num<-num+1; filled_count<-filled_count+1}
      }
      if(filled_count!=0){direction<-direction+1}
    }
  }
  return(field)
}

# ============ 核心函数：生成田间规划矩阵（含蛇形编号） ============
designPlot<-function(blocks=20,
                     y=10,
                     bridges=c(9,rep(6,10),rep(3,9)),
                     ww=1,
                     w=.2,
                     protected_columns=c(3),
                     protected_blocks=c(1,3,4,6),
                     water_columns=c(5),
                     lane_columns=c(9),
                     p_a=c(56,65,6,8,
                           34,38,2,5),
                     design_from_left=TRUE,
                     plant_from_left=TRUE,
                     subg=3
){
  if(is.null(blocks) || is.na(blocks) || blocks<=0 || blocks!=as.integer(blocks)){
    stop("blocks 必须是大于0的整数")
  }
  if(is.null(y) || is.na(y) || y<=0 || y!=as.integer(y)){
    stop("总列数必须是大于0的整数")
  }
  if(is.null(subg) || is.na(subg) || subg<=0 || subg!=as.integer(subg)){
    stop("分组大小必须是大于0的整数")
  }
  if(any(!is.finite(c(ww,w))) || ww<0 || w<0){
    stop("横向观察道宽度和材料间隔必须为非负数")
  }

  norm_cols <- normalizeMirroredColumns(
    y = y,
    protected_columns = protected_columns,
    water_columns = water_columns,
    lane_columns = lane_columns,
    p_a = p_a,
    design_from_left = design_from_left
  )
  protected_columns <- norm_cols$protected_columns
  water_columns <- norm_cols$water_columns
  lane_columns <- norm_cols$lane_columns
  p_a <- norm_cols$p_a

  checkColsInRange<-function(cols, total_cols, name){
    if(length(cols)==0) return(invisible(TRUE))
    if(any(is.na(cols)) || any(cols!=as.integer(cols)) || any(cols<1) || any(cols>total_cols)){
      stop(paste0(name, "列索引必须是 1 到 ", total_cols, " 的整数"))
    }
    invisible(TRUE)
  }
  checkColsInRange(protected_columns, y, "保护")
  checkColsInRange(water_columns, y, "水沟")
  checkColsInRange(lane_columns, y, "观察道")

  if(length(protected_blocks)>0 && (any(is.na(protected_blocks)) ||
     any(protected_blocks!=as.integer(protected_blocks)) ||
     any(protected_blocks<1) || any(protected_blocks>blocks))){
    stop(paste0("横向保护行必须是 1 到 ", blocks, " 的整数"))
  }

  if(!is.null(p_a)){
    if(length(p_a)%%BLOCK_RANGE_PARAM_GROUP_SIZE!=0){
      stop(paste0("不能种植地块参数个数必须是", BLOCK_RANGE_PARAM_GROUP_SIZE, "的倍数"))
    }
    p_a_matrix<-matrix(p_a, ncol=BLOCK_RANGE_PARAM_GROUP_SIZE, byrow=TRUE)
    if(any(p_a_matrix[,1]>=p_a_matrix[,2])) stop("不能种植地块的起始位置必须小于终止位置")
    if(any(p_a_matrix[,3]>p_a_matrix[,4])) stop("不能种植地块的起始行不能大于终止行")
    if(any(p_a_matrix[,3]<1) || any(p_a_matrix[,4]>y)) stop(paste0("不能种植地块的行范围必须在 1 到 ", y, " 之间"))
  }

  serpentine_marker<-if(isTRUE(plant_from_left)) 1 else 0
  total_rows<-blocks*2
  bridges<-normalizeBridges(bridges, blocks)
  field<-initFieldMatrix(total_rows, y, water_columns, lane_columns, ww, w, bridges, blocks)
  field<-fillProtection(field, y, total_rows, blocks, protected_columns, protected_blocks)
  field<-markBlockedAreas(field, y, p_a)
  field<-plantByGroup(field, subg, CODE_GROUP_PAD)
  field<-applySerpentineNumbering(field, y, total_rows, serpentine_marker)
  field<-finalizeFieldColumns(field, y)
  return(field)
}
