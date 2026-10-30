# ==============================================================================
# 报告附录 · 田间评价（田间人工评价）提取与合并
# ==============================================================================
# 用途：把「田间评价」这类人工巡田评价从三个来源合并成「一行一个田间 ID」的评价串，喂给报告附录 A：
#         · 表 A-2「各来源覆盖情况」→ summarize_field_note_coverage()；表 A-3/A-4 → build_field_eval_table()
# 三个来源：① md 巡田评价时间线（主来源，单元格内按轮次记录，用「；」分隔，每轮以「MM-DD｜」起头）
#           ② 数据库田间备注 TianJianBeiZhu（一条一句，可带日期 8.30 / 9.17，也可能是时间线的逐字拷贝）
#           ③ 数据库分期田间评价 MiaoQi / HuaQi / ChengShuQi TianJianPingJia
# 口径：评价串用「；」连接，每条形如「标签｜正文」；巡田轮次标签用 MM-DD，数据库来源用「库-」前缀；
#       排序为巡田轮次（按日期）→ 数据库田间备注 → 数据库分期评价；同一小区内同标签同正文只留一次。
# ==============================================================================

# ---- 内部常量 ---------------------------------------------------------------

# 轮次分隔符：整角分号 + 后面紧跟 MM-DD 日期标签（PCRE 前瞻）
.FN_ROUND_SPLIT <- "；(?=\\d{2}-\\d{2}｜)"

# 日期标签前缀：MM-DD + 整角竖线
.FN_DATE_TAG_RE <- "\\d{2}-\\d{2}｜"

# 分组排序用的「大数」：排在所有日期之后
.FN_ORDER_DB <- 1e6
#'默认字误订正表 
#'
#'巡田口述里的同音字/别字，按「旧词 → 新词」逐条做固定字面替换（fixed = TRUE）。 
#'来源：Obsidian 仓库 `scripts/append_n42_timeline_to_field_notes.py` 的 CORRECTIONS， 
#'以及 9/24 巡田笔记 §五「字误/口误清单」、10 月笔记的字音误还原。 
#'
#'⚠️ 与原 Python 脚本的一处差异：原表把 `树势` 订正成 `熟期`，但脚本自己的 
#'   docstring 写的是「树势 → 株势」。「树势」是植株长势，与熟期无关， 
#'   这里按 docstring 订正为「株势」。 
#'
#'@export
FIELD_NOTE_CORRECTIONS <- list(
  c("数据合适", "熟期合适"),
  c("数据格式", "熟期合适"),
  c("数据分离", "熟期分离"),
  c("牙有线", "亚有限"),
  c("亚有线", "亚有限"),
  c("以亚有限", "亚有限"),
  c("鸭有限", "亚有限"),
  c("落花好", "落黄好"),
  c("漏黄", "落黄"),
  c("洛黄", "落黄"),
  c("收期", "熟期"),
  c("初期", "熟期"),
  c("树势", "株势"),
  c("树形", "株型"),
  c("甲有病", "荚有病"),
  c("甲部", "基部"),
  c("绒毛色", "茸毛色"),
  c("株高。70", "株高 70"),
  c("对照比熟期晚两天", "熟期比对照晚两天"),
  c("类似株高", "类似，株高"),
  c("「早一两天」", "「比对照早 1 至 2 天」"),
  c("302 这两个都晚熟。302 感觉。感觉芽挺多的，但下面的叶子烂的很多",
    "301、302 都晚熟。302 荚多，但下部叶片烂得多"),
  c("飞虱", "大豆飞虱")
)

# ---- 内部工具 ---------------------------------------------------------------
#'把一段评价文本拆成「轮次」列表 
#'
#'一个单元格里可能有多轮评价，轮次之间用整角分号「；」分隔，且每轮以 
#'「MM-DD｜」起头。**只有后面紧跟日期标签的分号才算轮次分隔符**——正文里 
#'自己也可能有「；」（如「R1，优；苗期抗病性好。」），不能一刀切。 
#'
#'拆分用「先定位所有日期标签位置、再切片」实现，不依赖 UTF-8 环境下 
#'`strsplit(perl = TRUE)` 对多字节字符的处理。 
#'
#'@param cell 单条评价文本 
#'@return list(date_tag = character, text = character)；date_tag 为 "" 表示该段没有日期标签 
.fn_parse_round_texts <- function(cell) {
  if (is.null(cell) || length(cell) != 1L || is.na(cell)) {
    return(list(date_tag = character(0), text = character(0)))
  }
  cell <- trimws(as.character(cell))
  if (!nzchar(cell)) {
    return(list(date_tag = character(0), text = character(0)))
  }
  m <- gregexpr(.FN_DATE_TAG_RE, cell, perl = TRUE)
  starts <- as.integer(m[[1]])
  if (!length(starts) || starts[1L] == -1L) {
    # 整段没有日期标签 → 当作一条无标签评价
    return(list(date_tag = "", text = cell))
  }
  tags <- regmatches(cell, m)[[1]]
  n <- length(starts)
  n_ch <- nchar(cell)
  date_tag <- character(n)
  text <- character(n)
  for (i in seq_len(n)) {
    s <- starts[i]
    e <- if (i < n) starts[i + 1L] - 1L else n_ch
    seg <- substr(cell, s, e)
    if (i < n) {
      # 去掉轮次之间的分隔符「；」（可能带空格）
      seg <- sub("[[:space:]]*；[[:space:]]*$", "", seg)
    }
    date_tag[i] <- substr(tags[i], 1L, 5L)          # "MM-DD｜" 的前 5 个字符
    text[i] <- trimws(substr(seg, 7L, nchar(seg)))   # 去掉 "MM-DD｜" 前缀
  }
  # 首个日期标签之前若还有文字，说明这段文字没有日期标签，单独成条
  pre <- trimws(sub("[[:space:]]*；[[:space:]]*$", "",
                    substr(cell, 1L, starts[1L] - 1L)))
  if (nzchar(pre)) {
    date_tag <- c("", date_tag)
    text <- c(pre, text)
  }
  list(date_tag = date_tag, text = text)
}
#'校验并规范化字误订正表 
.fn_check_corrections <- function(corrections) {
  if (is.null(corrections)) return(list())
  if (!is.list(corrections)) {
    stop("corrections 必须是 list（每个元素为 c(旧词, 新词)），当前类型：", class(corrections)[1])
  }
  out <- list()
  for (i in seq_along(corrections)) {
    p <- corrections[[i]]
    if (!is.character(p) || length(p) != 2L || any(is.na(p))) {
      stop(sprintf("corrections[[%d]] 必须是长度为 2 的字符向量 c(旧词, 新词)", i))
    }
    if (!nzchar(p[1L])) {
      stop(sprintf("corrections[[%d]] 的旧词不能为空字符串", i))
    }
    out[[length(out) + 1L]] <- p
  }
  out
}
#'按字误订正表逐条做固定字面替换 
.fn_apply_corrections <- function(text, corrections) {
  if (!length(text)) return(text)
  if (!length(corrections)) return(text)
  for (p in corrections) {
    text <- gsub(p[1L], p[2L], text, fixed = TRUE)
  }
  text
}
#'把「MM-DD」标签换成可排序的数值 
.fn_date_key <- function(tag) {
  if (is.null(tag) || length(tag) == 0L) return(integer(0))
  vapply(tag, function(t) {
    if (is.na(t) || !nzchar(t)) return(0L)
    m <- regmatches(t, regexec("^(\\d{1,2})-(\\d{1,2})$", t))[[1]]
    if (length(m) != 3L) {
      # 只有月份（如日期别名把 "10-07" 写成 "10月"）→ 按月初排，同月内排在前面
      m2 <- regmatches(t, regexec("^(\\d{1,2})月", t))[[1]]
      if (length(m2) == 2L) return(as.integer(m2[2L]) * 100L)
      return(0L)
    }
    as.integer(m[2L]) * 100L + as.integer(m[3L])
  }, integer(1), USE.NAMES = FALSE)
}
#'归一化正文，用于去重比较（去首尾空白 + 连续空白压成一个） 
.fn_norm_text <- function(x) {
  if (!length(x)) return(character(0))
  trimws(gsub("[[:space:]]+", " ", as.character(x)))
}
#'从「备注」正文里认出内嵌日期（如 8.30 / 9.17 / 8月30日） 
#'
#'只认「日必须是两位」的形式，避免把「早 4.3 天」这种天数误判成日期。 
#'
.fn_detect_memo_date <- function(text) {
  if (is.null(text) || length(text) != 1L || is.na(text)) return("")
  m <- regmatches(text, regexec("(?<!\\d)([0-9]{1,2})[.．]([0-9]{2})(?!\\d)", text, perl = TRUE))[[1]]
  if (length(m) == 3L) {
    mo <- as.integer(m[2L]); d <- as.integer(m[3L])
    if (mo >= 1L && mo <= 12L && d >= 1L && d <= 31L) {
      return(sprintf("%02d-%02d", mo, d))
    }
  }
  m <- regmatches(text, regexec("(?<!\\d)([0-9]{1,2})月([0-9]{1,2})日?", text, perl = TRUE))[[1]]
  if (length(m) == 3L) {
    mo <- as.integer(m[2L]); d <- as.integer(m[3L])
    if (mo >= 1L && mo <= 12L && d >= 1L && d <= 31L) {
      return(sprintf("%02d-%02d", mo, d))
    }
  }
  ""
}
#'判断是否对照小区（兼容 0/1、TRUE/FALSE、"是"/"否" 等写法） 
.fn_is_control <- function(x) {
  if (is.null(x) || !length(x)) return(logical(0))
  if (is.logical(x)) return(x %in% TRUE)
  if (is.numeric(x)) return(!is.na(x) & x == 1)
  ch <- trimws(as.character(x))
  ch[is.na(ch)] <- ""
  ch %in% c("1", "是", "对照", "Y", "y", "TRUE", "true", "T")
}
#'空骨架 
.fn_empty <- function(...) as.data.frame(list(...), stringsAsFactors = FALSE)
.fn_empty_timeline <- function() .fn_empty(id = character(0), date_tag = character(0), text = character(0), round_idx = integer(0))
.fn_empty_db <- function() .fn_empty(id = character(0), source_tag = character(0), text = character(0), kind = character(0))
.fn_empty_notes <- function() .fn_empty(id = character(0), notes = character(0))

# ==============================================================================
# ① 巡田评价时间线（md）
# ==============================================================================
#'从田间调查评价时间线 md 中提取逐轮评价 
#'
#'md 形状（已对 N-4.2 等 14 份时间线核对）： 
#'``` 
#'| 田间ID | 田间调查评价 | 
#'|---|---| 
#'| N26E001 | 08-08｜⚠️ 叶形分离（需提纯）；09-01｜✅ 优异；09-24｜「1 号，熟期与对照相近」｜＝对照 | 
#'``` 
#'单元格内多个**轮次**用整角分号「；」分隔，每轮以「MM-DD｜」日期标签起头； 
#'正文里也可能出现「｜」（分隔「用户原话」与「信号/评级」）与「；」。 
#'因此：**只有后面紧跟日期标签的分号才算轮次分隔符**，且每轮只把**第一个**「｜」 
#'当作日期分隔符。 
#'
#'@param md_path 时间线 md 文件路径（Obsidian 仓库里 2026年田间调查/B. 调查·初选·评价/ 下） 
#'@param id_pattern 田间 ID 正则，默认 `^[A-Z]\d{2}[A-Z]\d{3,4}$`（如 N26E001） 
#'@param corrections 字误订正表，list of c(旧词, 新词)，按顺序做 fixed 替换；默认不改写 
#'@return data.frame(id, date_tag, text, round_idx)，一行一轮 
#'@export
extract_timeline_notes <- function(md_path, id_pattern = "^[A-Z]\\d{2}[A-Z]\\d{3,4}$", corrections = list()) {
  # ---- 参数校验 ----
  if (!is.character(md_path) || length(md_path) != 1L || is.na(md_path) ||
      !nzchar(trimws(md_path))) {
    stop("md_path 必须是非空字符路径")
  }
  if (!is.character(id_pattern) || length(id_pattern) != 1L || is.na(id_pattern)) {
    stop("id_pattern 必须是单个正则字符串")
  }
  corrections <- .fn_check_corrections(corrections)
  # ---- 文件不存在：告警并返回空表，不中断整条流水线 ----
  if (!file.exists(md_path)) {
    warning(sprintf("田间调查评价时间线不存在：%s；该来源按 0 条处理", md_path))
    return(.fn_empty_timeline())
  }
  lines <- tryCatch(
    readLines(md_path, encoding = "UTF-8", warn = FALSE),
    error = function(e) {
      warning(sprintf("读取时间线失败（%s）：%s", md_path, conditionMessage(e)))
      character(0)
    }
  )
  if (!length(lines)) return(.fn_empty_timeline())
  # ---- 逐行匹配「| ID | 评价 |」 ----
  # 用贪婪的 (.*) 取到行尾最后一个「|」之前的内容，正文里含 ASCII 竖线也不怕
  m <- regexec("^\\|\\s*([^|]+?)\\s*\\|\\s*(.*)\\s*\\|\\s*$", lines, perl = TRUE)
  hits <- regmatches(lines, m)
  hits <- hits[vapply(hits, length, integer(1)) == 3L]
  if (!length(hits)) {
    warning(sprintf("时间线里没匹配到「| 田间ID | 评价 |」表格行：%s", md_path))
    return(.fn_empty_timeline())
  }
  raw_id <- trimws(vapply(hits, `[[`, character(1), 2L))
  raw_cell <- trimws(vapply(hits, `[[`, character(1), 3L))
  # 只保留符合 ID 口径的行（md 里还有轮次说明表等其它表格）
  keep <- grepl(id_pattern, raw_id)
  raw_id <- raw_id[keep]
  raw_cell <- raw_cell[keep]
  if (!length(raw_id)) {
    warning(sprintf("时间线里没有符合 id_pattern（%s）的行：%s", id_pattern, md_path))
    return(.fn_empty_timeline())
  }
  # ---- 逐 ID 拆轮次 ----
  out_id <- character(0)
  out_tag <- character(0)
  out_txt <- character(0)
  out_idx <- integer(0)
  for (i in seq_along(raw_id)) {
    rounds <- .fn_parse_round_texts(raw_cell[i])
    if (!length(rounds$text)) next
    for (k in seq_along(rounds$text)) {
      tag <- rounds$date_tag[k]
      out_id <- c(out_id, raw_id[i])
      out_tag <- c(out_tag, tag)
      out_txt <- c(out_txt, .fn_apply_corrections(rounds$text[k], corrections))
      out_idx <- c(out_idx, k)
    }
  }
  if (!length(out_id)) return(.fn_empty_timeline())
  res <- data.frame(id = out_id, date_tag = out_tag, text = out_txt,
                    round_idx = out_idx, stringsAsFactors = FALSE)
  # 同一 ID 内按轮次先后排序（原文就是时间先后，排序只为稳妥）
  res[order(res$id, res$round_idx), , drop = FALSE]
}

#' 从 md 巡田时间线里解析「评价轮次」小节
#'
#' 时间线 md 里有一张 `## 评价轮次` 表（轮次 / 日期 / 生育期 / 评价材料数），
#' 是报告附录 A.1「田间评价」表的**直接来源**——比「按日期标签聚合条目」口径更准，
#' 因为 md 里的「评价材料数」是编者按轮次核过的口径。
#'
#' ⚠️ 与 `extract_timeline_notes()` 的区别：后者返回**长表**
#'   （一行 = 一个 ID × 一轮评价，列 id/date_tag/text/round_idx），
#'   **不能**当轮次汇总用——早先 `rpt_blk_tbl_a4_rounds()` 正是这么用的，
#'   结果把 ID 填进了「日期」列、把内容整列填成「—」。
#'
#' @param md_path 时间线 md 路径
#' @return data.frame(轮次, 日期, 生育期, 评价材料数)；解析不到时返回 NULL
#' @export
extract_rounds_summary <- function(md_path) {
  if (!is.character(md_path) || length(md_path) != 1L || is.na(md_path) ||
      !nzchar(trimws(md_path))) return(NULL)
  if (!file.exists(md_path)) return(NULL)
  lines <- tryCatch(readLines(md_path, encoding = "UTF-8", warn = FALSE),
                    error = function(e) character(0))
  if (!length(lines)) return(NULL)

  # ---- 定位「评价轮次」小节 ----
  head_line <- grep("^#{1,6}\\s*评价轮次\\s*$", lines)
  if (!length(head_line)) return(NULL)
  i0 <- head_line[1] + 1L
  if (i0 > length(lines)) return(NULL)
  next_head <- grep("^#{1,6}\\s", lines[i0:length(lines)])
  i1 <- if (length(next_head)) i0 + next_head[1] - 2L else length(lines)
  if (i1 < i0) return(NULL)
  seg <- lines[i0:i1]

  # ---- 表格行：列数**不固定**，按表头自适应 ----
  # N-4.2 的轮次表是 4 列：| 轮次 | 日期 | 生育期 | 评价材料数 |
  # N-9.9 的轮次表是 3 列：| 轮次 | 日期 |          | 评价材料数 |   ← 没有「生育期」
  # 原先硬编码 4 列正则 ⇒ 3 列表格一条都匹配不到 ⇒ 返回 NULL
  # ⇒ 上层 rpt_field_notes_bundle 的 tryCatch 吞掉错误 ⇒ A.4 整段留白。
  # 现在按「按 | 切分后取单元数」自适应：列数取表头行的单元数。
  cell_split <- function(l) {
    # ⚠️ sub() 的 pattern 是正则：`^\\|` 会被当成「锚点 或 空」，
    #    要把 | 当字面量必须 fixed=TRUE 或用 "[|]"。
    l <- gsub("^[|]", "", l)
    l <- gsub("[|][[:space:]]*$", "", l)
    trimws(strsplit(l, "[|]", perl = TRUE)[[1]])
  }
  row_cells <- lapply(seg, cell_split)
  is_tbl <- vapply(seg, function(l) grepl("^[|]", trimws(l)), logical(1))
  row_cells <- row_cells[is_tbl]
  if (!length(row_cells)) return(NULL)

  # 找表头行（含「轮次」）
  head_i <- which(vapply(row_cells, function(cs)
    length(cs) >= 3L && any(cs == "轮次"), logical(1)))
  if (!length(head_i)) return(NULL)
  hdr <- row_cells[[head_i[1]]]
  ncol_tbl <- length(hdr)

  # 只留与表头同列数的数据行（跳过表头、分隔行）
  body <- row_cells[vapply(row_cells, length, integer(1)) == ncol_tbl]
  body <- body[vapply(body, function(cs) !grepl("^-+$", cs[1]) && cs[1] != "轮次",
                      logical(1))]
  if (!length(body)) return(NULL)

  df <- as.data.frame(do.call(rbind, body), stringsAsFactors = FALSE)
  names(df) <- hdr
  # 「生育期」列可能不存在 ⇒ 补空列，保持下游模板取用不出错
  if (!"生育期" %in% names(df)) df$生育期 <- NA_character_
  if (!"评价材料数" %in% names(df)) return(NULL)

  df$轮次 <- suppressWarnings(as.integer(df$轮次))
  df$评价材料数 <- suppressWarnings(as.integer(df$评价材料数))
  df <- df[!is.na(df$轮次), , drop = FALSE]
  if (!nrow(df)) return(NULL)
  df[order(df$轮次), , drop = FALSE]
}

# ==============================================================================
# ② + ③ 数据库田间备注 / 分期田间评价
# ==============================================================================
#'从数据库导出表里提取带来源标签的田间评价 
#'
#'两类列： 
#'- 田间备注列（`TianJianBeiZhu`）：一条一句。**既可能是纯人工备注** 
#'  （如「R1，优。」「成熟重点关注8.30」），**也可能是整段巡田时间线的拷贝** 
#'  （N-4.2 里 237 个小区的备注就是时间线单元格的逐字拷贝）。 
#'  备注正文里内嵌日期（8.30 / 9.17）会被认成 `库-08-30` / `库-09-17` 这样的标签。 
#'- 分期评价列（苗期 / 花期 / 成熟期）：整格即一条评价。 
#'
#'⚠️ `export_data` 用**拼音**列名（stageid / TianJianBeiZhu / MiaoQiTianJianPingJia…）， 
#'   不要用 `yield_ranking` 的中文列名，这是本项目里一个已知的坑。 
#'
#'@param export_data 分析管线的 `res$result$tables$export_data` 
#'@param memo_col 田间备注列名 
#'@param stage_cols 分期评价列，名字为中文阶段名，值为拼音列名 
#'@param id_col 小区 ID 列名 
#'@return data.frame(id, source_tag, text, kind)，kind ∈ {db_memo, db_memo_date, db_stage} 
#'@export
extract_db_notes <- function(export_data,
                             memo_col = "TianJianBeiZhu",
                             stage_cols = c(苗期 = "MiaoQiTianJianPingJia",
                                            花期 = "HuaQiTianJianPingJia",
                                            成熟期 = "ChengShuQiTianJianPingJia"),
                             id_col = "stageid") {
  # ---- 参数校验 ----
  if (!is.data.frame(export_data)) {
    stop("export_data 必须是 data.frame（分析管线 result$tables$export_data）")
  }
  # 空表：结构可能不完整（如只有 ID 列），按 0 条处理，不做列名校验
  if (nrow(export_data) == 0L) {
    warning("export_data 是空表（0 行）；田间备注与分期评价按 0 条处理")
    return(.fn_empty_db())
  }
  if (!is.character(id_col) || length(id_col) != 1L || is.na(id_col)) {
    stop("id_col 必须是单个列名字符串")
  }
  if (!id_col %in% names(export_data)) {
    stop(sprintf("export_data 里没有 ID 列「%s」", id_col))
  }
  if (!is.null(memo_col) && (!is.character(memo_col) || length(memo_col) != 1L)) {
    stop("memo_col 必须是单个列名字符串（或 NULL 表示不取田间备注）")
  }
  if (!is.null(memo_col) && !memo_col %in% names(export_data)) {
    stop(sprintf("export_data 里没有田间备注列「%s」", memo_col))
  }
  if (length(stage_cols) && (is.null(names(stage_cols)) ||
                              any(!nzchar(names(stage_cols))) ||
                              any(is.na(names(stage_cols))))) {
    stop("stage_cols 必须是有名字的字符向量（名字为中文阶段名，如 苗期）")
  }
  miss_stage <- setdiff(unname(stage_cols), names(export_data))
  if (length(miss_stage)) {
    stop(sprintf("export_data 里没有分期评价列：%s", paste(miss_stage, collapse = ", ")))
  }
  ids <- trimws(as.character(export_data[[id_col]]))
  ids[is.na(ids)] <- ""
  out_id <- character(0)
  out_tag <- character(0)
  out_txt <- character(0)
  out_kind <- character(0)
  # ---- ② 田间备注 ----
  if (!is.null(memo_col)) {
    memo <- as.character(export_data[[memo_col]])
    memo[is.na(memo)] <- ""
    for (i in seq_along(memo)) {
      if (!nzchar(trimws(memo[i]))) next
      parts <- .fn_parse_round_texts(memo[i])
      if (!length(parts$text)) next
      for (k in seq_along(parts$text)) {
        tag <- parts$date_tag[k]
        txt <- trimws(parts$text[k])
        if (!nzchar(txt)) next
        if (nzchar(tag)) {
          # 备注整段带着 MM-DD｜日期标签 → 这是巡田时间线的拷贝
          out_id <- c(out_id, ids[i]); out_tag <- c(out_tag, tag)
          out_txt <- c(out_txt, txt); out_kind <- c(out_kind, "db_memo_date")
        } else {
          # 纯人工备注：正文里可能内嵌日期（8.30 / 9.17）
          out_id <- c(out_id, ids[i])
          out_tag <- c(out_tag, .fn_detect_memo_date(txt))
          out_txt <- c(out_txt, txt); out_kind <- c(out_kind, "db_memo")
        }
      }
    }
  }
  # ---- ③ 分期评价 ----
  for (j in seq_along(stage_cols)) {
    stage_name <- names(stage_cols)[j]
    col <- unname(stage_cols)[j]
    v <- as.character(export_data[[col]])
    v[is.na(v)] <- ""
    for (i in seq_along(v)) {
      txt <- trimws(v[i])
      if (!nzchar(txt)) next
      out_id <- c(out_id, ids[i]); out_tag <- c(out_tag, stage_name)
      out_txt <- c(out_txt, txt); out_kind <- c(out_kind, "db_stage")
    }
  }
  if (!length(out_id)) return(.fn_empty_db())
  data.frame(id = out_id, source_tag = out_tag, text = out_txt,
             kind = out_kind, stringsAsFactors = FALSE)
}

# ==============================================================================
# 合并
# ==============================================================================
#'把巡田时间线与数据库评价合并成「一行一个 ID」的评价串 
#'
#'标签口径（`tag_map` 可覆盖）： 
#'- `timeline`：巡田轮次前缀，默认空串 → 标签就是 `08-08` 这样的日期。 
#'- `db_memo`：数据库田间备注前缀，默认 `库-备注`；备注里带日期时按 
#'  「前缀去掉『备注』二字 + 日期」拼出 `库-08-30`。 
#'- `db_stage`：数据库分期评价前缀，默认 `库-{stage}` → `库-苗期` 等。 
#'
#'排序：`order` 给出分组先后（"date" = 巡田轮次按日期先后，"db" = 数据库来源）， 
#'组内数据库来源按「田间备注 → 分期评价（按 stage_cols 顺序）」排。 
#'
#'去重（`dedup = TRUE`）分两步： 
#'1. **同轮次去重**：同一小区的同一轮日期，**巡田时间线优先**——数据库备注里 
#'   逐字拷贝的时间线整段（N-4.2 有 237 个小区）整条丢弃；数据库备注若与时间线 
#'   措辞不同（时间线是原话、数据库里是订正过的），也以时间线为准。 
#'2. **同标签同正文去重**：同一 ID 内「标签 + 正文」完全相同的只留一条。 
#'
#'@param timeline_df extract_timeline_notes() 的结果 
#'@param db_df extract_db_notes() 的结果 
#'@param tag_map 标签模板 
#'@param order 分组先后 
#'@param dedup 是否去重 
#'@param date_aliases 日期标签别名，如 `c("10-07" = "10月")`；默认保持来源原样。 
#'  别名只影响**显示**，去重仍按原始日期比对。 
#'@return data.frame(id, notes) 
#'@export
merge_field_notes <- function(timeline_df, db_df,
                              tag_map = c(timeline = "", db_memo = "库-备注",
                                          db_stage = "库-{stage}"),
                              order = c("date", "db"),
                              dedup = TRUE,
                              date_aliases = NULL) {
  # ---- 参数校验 ----
  if (is.null(timeline_df)) timeline_df <- .fn_empty_timeline()
  if (is.null(db_df)) db_df <- .fn_empty_db()
  if (!is.data.frame(timeline_df)) {
    stop("timeline_df 必须是 data.frame（extract_timeline_notes 的结果）")
  }
  if (!is.data.frame(db_df)) {
    stop("db_df 必须是 data.frame（extract_db_notes 的结果）")
  }
  miss_tl <- setdiff(c("id", "date_tag", "text"), names(timeline_df))
  if (length(miss_tl)) {
    stop(sprintf("timeline_df 缺少列：%s", paste(miss_tl, collapse = ", ")))
  }
  miss_db <- setdiff(c("id", "source_tag", "text"), names(db_df))
  if (length(miss_db)) {
    stop(sprintf("db_df 缺少列：%s", paste(miss_db, collapse = ", ")))
  }
  if (!is.character(tag_map) || is.null(names(tag_map))) {
    stop("tag_map 必须是带名字的字符向量（timeline / db_memo / db_stage）")
  }
  miss_tag <- setdiff(c("timeline", "db_memo", "db_stage"), names(tag_map))
  if (length(miss_tag)) {
    stop(sprintf("tag_map 缺少项：%s", paste(miss_tag, collapse = ", ")))
  }
  if (!is.character(order) || !length(order) ||
      !all(order %in% c("date", "db"))) {
    stop("order 只能取 \"date\" 与 \"db\" 的组合")
  }
  if (!is.null(date_aliases) &&
      (!is.character(date_aliases) || is.null(names(date_aliases)))) {
    stop("date_aliases 必须是带名字的字符向量，如 c(\"10-07\" = \"10月\")")
  }
  if (!nrow(timeline_df) && !nrow(db_df)) return(.fn_empty_notes())
  # ---- 收集条目 ----
  ent_id <- character(0); ent_tag <- character(0)
  ent_txt <- character(0); ent_grp <- character(0); ent_key <- numeric(0)
  ent_date <- character(0); ent_copy <- logical(0)
  if (nrow(timeline_df)) {
    tid <- trimws(as.character(timeline_df$id))
    ttag <- as.character(timeline_df$date_tag)
    ttxt <- trimws(as.character(timeline_df$text))
    keep <- nzchar(tid) & nzchar(ttxt)
    tid <- tid[keep]; ttag <- ttag[keep]; ttxt <- ttxt[keep]
    if (length(tid)) {
      ent_id <- c(ent_id, tid)
      ent_tag <- c(ent_tag, paste0(tag_map[["timeline"]], ttag))
      ent_txt <- c(ent_txt, ttxt)
      ent_grp <- c(ent_grp, rep("date", length(tid)))
      ent_key <- c(ent_key, .fn_date_key(ttag))
      ent_date <- c(ent_date, ttag)
      ent_copy <- c(ent_copy, rep(FALSE, length(tid)))
    }
  }
  if (nrow(db_df)) {
    did <- trimws(as.character(db_df$id))
    dtag <- as.character(db_df$source_tag)
    dtxt <- trimws(as.character(db_df$text))
    dkind <- if (!is.null(db_df$kind)) as.character(db_df$kind) else
      rep(ifelse(dtag %in% c("苗期", "花期", "成熟期"), "db_stage", "db_memo"),
          length(dtag))
    keep <- nzchar(did) & nzchar(dtxt)
    did <- did[keep]; dtag <- dtag[keep]; dtxt <- dtxt[keep]; dkind <- dkind[keep]
    if (length(did)) {
      is_date <- dkind == "db_memo_date"
      is_memo <- dkind == "db_memo"
      is_stage <- dkind == "db_stage"
      # 备注里带 MM-DD 标签的整段（巡田时间线拷贝）→ 用巡田标签渲染
      if (any(is_date)) {
        ent_id <- c(ent_id, did[is_date])
        ent_tag <- c(ent_tag, paste0(tag_map[["timeline"]], dtag[is_date]))
        ent_txt <- c(ent_txt, dtxt[is_date])
        ent_grp <- c(ent_grp, rep("date", sum(is_date)))
        ent_key <- c(ent_key, .fn_date_key(dtag[is_date]))
        ent_date <- c(ent_date, dtag[is_date])
        ent_copy <- c(ent_copy, rep(TRUE, sum(is_date)))
      }
      # 纯人工备注
      if (any(is_memo)) {
        memo_prefix <- tag_map[["db_memo"]]
        dated <- nzchar(dtag[is_memo])
        tg <- rep(memo_prefix, sum(is_memo))
        if (any(dated)) {
          base <- sub("备注$", "", memo_prefix)
          tg[dated] <- paste0(base, dtag[is_memo][dated])
        }
        ent_id <- c(ent_id, did[is_memo])
        ent_tag <- c(ent_tag, tg)
        ent_txt <- c(ent_txt, dtxt[is_memo])
        ent_grp <- c(ent_grp, rep("db", sum(is_memo)))
        ent_key <- c(ent_key, rep(.FN_ORDER_DB, sum(is_memo)))
        ent_date <- c(ent_date, rep("", sum(is_memo)))
        ent_copy <- c(ent_copy, rep(FALSE, sum(is_memo)))
      }
      # 分期评价
      if (any(is_stage)) {
        ent_id <- c(ent_id, did[is_stage])
        ent_tag <- c(ent_tag, vapply(dtag[is_stage], function(s) {
          gsub("{stage}", s, tag_map[["db_stage"]], fixed = TRUE)
        }, character(1), USE.NAMES = FALSE))
        ent_txt <- c(ent_txt, dtxt[is_stage])
        ent_grp <- c(ent_grp, rep("db", sum(is_stage)))
        ent_key <- c(ent_key, .FN_ORDER_DB + seq_len(sum(is_stage)))
        ent_date <- c(ent_date, rep("", sum(is_stage)))
        ent_copy <- c(ent_copy, rep(FALSE, sum(is_stage)))
      }
    }
  }
  if (!length(ent_id)) return(.fn_empty_notes())
  ent <- data.frame(id = ent_id, tag = ent_tag, text = ent_txt,
                    grp = ent_grp, key = ent_key, date = ent_date,
                    copy = ent_copy, stringsAsFactors = FALSE)
  # ---- 去重 ①：同轮次去重，巡田时间线优先 ----
  # 数据库备注里逐字拷贝的时间线整段（同一小区、同一轮日期）整条丢弃； 即便措辞不同（时间线是现场原话、库里是订正过的），也以时间线为准。
  if (dedup && nrow(ent)) {
    tl_rows <- ent$grp == "date" & !ent$copy
    tl_keys <- paste(ent$id[tl_rows], ent$date[tl_rows], sep = "\r")
    copy_rows <- ent$copy & nzchar(ent$date)
    if (any(copy_rows)) {
      k <- paste(ent$id[copy_rows], ent$date[copy_rows], sep = "\r")
      drop_rows <- rep(FALSE, nrow(ent))
      drop_rows[copy_rows] <- k %in% tl_keys
      ent <- ent[!drop_rows, , drop = FALSE]
    }
  }
  # ---- 日期别名（只影响显示，去重仍按原始日期比过） ----
  if (!is.null(date_aliases) && length(date_aliases) && nrow(ent)) {
    d <- ent$date
    hit <- nzchar(d) & (d %in% names(date_aliases))
    if (any(hit)) d[hit] <- unname(date_aliases[d[hit]])
    ent$date <- d
    dated <- nzchar(ent$date)
    if (any(dated)) {
      ent$tag[dated] <- paste0(tag_map[["timeline"]], ent$date[dated])
    }
  }
  # 分组先后：order 里排前面的组先出
  grp_rank <- match(ent$grp, order)
  ent <- ent[order(grp_rank, ent$key, ent$id), , drop = FALSE]
  # ---- 逐 ID 拼串 + 去重 ----
  ids <- sort(unique(ent$id))
  notes <- vapply(ids, function(one_id) {
    sub_ent <- ent[ent$id == one_id, , drop = FALSE]
    if (dedup) {
      key <- paste(sub_ent$tag, .fn_norm_text(sub_ent$text), sep = "\r")
      sub_ent <- sub_ent[!duplicated(key), , drop = FALSE]
    }
    if (!nrow(sub_ent)) return("")
    paste0(
      ifelse(nzchar(sub_ent$tag), paste0(sub_ent$tag, "｜"), ""),
      sub_ent$text,
      collapse = "；"
    )
  }, character(1), USE.NAMES = FALSE)
  data.frame(id = ids, notes = notes, stringsAsFactors = FALSE)
}

# ==============================================================================
# 覆盖情况（表 A-2）
# ==============================================================================
#'统计各来源覆盖了多少个小区（报告表 A-2） 
#'
#'口径： 
#'- 巡田轮次：按日期标签逐轮统计**去重后的 ID 数**（一轮一个小区算一次）。 
#'- 数据库田间备注：只算**纯人工备注**（正文里带 MM-DD 标签的整段是巡田时间线的 
#'  拷贝，不重复计入），并按「带日期 / 无日期」拆开。 
#'- 数据库分期评价：按阶段分别统计。 
#'- 合并去重后：最终有评价的小区总数。 
#'
#'@param timeline_df extract_timeline_notes() 的结果 
#'@param db_df extract_db_notes() 的结果 
#'@param n_total 小区总数（用于算百分比），可为 NULL 
#'@return data.frame(source, n_ids, pct) 
#'@export
summarize_field_note_coverage <- function(timeline_df, db_df, n_total = NULL,
                                         stage_names = c("苗期", "花期", "成熟期")) {
  if (is.null(timeline_df)) timeline_df <- .fn_empty_timeline()
  if (is.null(db_df)) db_df <- .fn_empty_db()
  if (!is.data.frame(timeline_df) || !is.data.frame(db_df)) {
    stop("timeline_df / db_df 必须是 data.frame")
  }
  if (!is.null(n_total) && (!is.numeric(n_total) || length(n_total) != 1L || is.na(n_total))) {
    stop("n_total 必须是单个数值（或 NULL）")
  }
  src <- character(0); cnt <- integer(0)
  # ---- 巡田轮次 ----
  if (nrow(timeline_df)) {
    tags <- unique(as.character(timeline_df$date_tag))
    tags <- tags[nzchar(tags)]
    tags <- tags[order(.fn_date_key(tags))]
    for (tg in tags) {
      ids <- unique(trimws(as.character(
        timeline_df$id[timeline_df$date_tag == tg]
      )))
      src <- c(src, paste0("巡田 ", tg))
      cnt <- c(cnt, sum(nzchar(ids)))
    }
  }
  # ---- 数据库田间备注（纯人工备注部分） ----
  if (nrow(db_df)) {
    kind <- if (!is.null(db_df$kind)) as.character(db_df$kind) else
      rep("db_memo", nrow(db_df))
    memo_rows <- kind == "db_memo"
    if (any(memo_rows)) {
      ids <- trimws(as.character(db_df$id[memo_rows]))
      tags <- as.character(db_df$source_tag[memo_rows])
      dated <- nzchar(tags)
      undated_ids <- unique(ids[!dated & nzchar(ids)])
      src <- c(src, "库·田间备注（无日期）")
      cnt <- c(cnt, length(undated_ids))
      for (tg in sort(unique(tags[dated]))) {
        dids <- unique(ids[dated & tags == tg & nzchar(ids)])
        # "08-30" → "8.30"（报告表 A-2 的写法）
        mm <- sub("^0([0-9])-", "\\1-", tg)
        mm <- sub("-", ".", mm)
        src <- c(src, paste0("库·田间备注（", mm, "）"))
        cnt <- c(cnt, length(dids))
      }
    }
  }
  # ---- 数据库分期评价（没有数据的阶段也要列出 0，与报告表 A-2 口径一致） ----
  stage_rows <- if (nrow(db_df) && !is.null(db_df$kind)) db_df$kind == "db_stage" else rep(FALSE, 0)
  for (st in stage_names) {
    sids <- if (any(stage_rows)) {
      unique(trimws(as.character(db_df$id[stage_rows & db_df$source_tag == st])))
    } else {
      character(0)
    }
    src <- c(src, paste0("库·", st, "评价"))
    cnt <- c(cnt, sum(nzchar(sids)))
  }
  # ---- 合并去重后 ----
  all_ids <- c(
    if (nrow(timeline_df)) trimws(as.character(timeline_df$id)) else character(0),
    if (nrow(db_df)) trimws(as.character(db_df$id)) else character(0)
  )
  all_ids <- unique(all_ids[nzchar(all_ids)])
  src <- c(src, "合并去重后")
  cnt <- c(cnt, length(all_ids))
  res <- data.frame(source = src, n_ids = cnt, stringsAsFactors = FALSE)
  # 按覆盖小区数从多到少排，「合并去重后」永远在最后——与报告表 A-2 的排法一致
  res <- res[order(-res$n_ids, res$source == "合并去重后", method = "radix"),
             , drop = FALSE]
  res$pct <- if (!is.null(n_total) && n_total > 0) {
    round(100 * res$n_ids / n_total, 1)
  } else {
    NA_real_
  }
  rownames(res) <- NULL
  res
}

# ==============================================================================
# 表 A-4 材料田间评价表
# ==============================================================================
#'生成「材料田间评价表」（报告表 A-3 / A-4） 
#'
#'口径： 
#'- 一行一个小区，**按亩产从高到低**排，**无产量的排在最后**。 
#'- 对照小区的 name 标为「中黄301（对照位）」（便于与材料区分）， 
#'  两套位次列标「—（对照基准）」——对照是产比基准，不与自己的基准比增产率。 
#'- 亩产与两套位次**直接取自 `export_data`，不做二次计算**。 
#'  ⚠️ `export_data` 用拼音列名（MuChan / JiaoLinJinDuiZhaoWeiCi / 
#'     JiaoPingJunDuiZhaoWeiCi），**不是** `yield_ranking` 的中文列名。 
#'- 默认只输出**有田间评价**的小区（`drop_empty = TRUE`）。 
#'
#'@param export_data 分析管线的 export_data 
#'@param field_notes merge_field_notes() 的结果（id, notes） 
#'@param ck_variety 对照品种名 
#'@param drop_empty 是否丢掉没有任何田间评价的小区 
#'@return data.frame(stageid, name, 亩产 (kg), 较临近对照位次, 较平均对照位次, 田间评价) 
#'@export
build_field_eval_table <- function(export_data, field_notes, ck_variety = "中黄301", drop_empty = TRUE) {
  # ---- 参数校验 ----
  if (!is.data.frame(export_data)) {
    stop("export_data 必须是 data.frame（分析管线 result$tables$export_data）")
  }
  need <- c("stageid", "name", "MuChan",
            "JiaoLinJinDuiZhaoWeiCi", "JiaoPingJunDuiZhaoWeiCi")
  miss <- setdiff(need, names(export_data))
  if (length(miss)) {
    stop(sprintf(
      "export_data 缺少列：%s（本模块用拼音列名的 export_data，不要用 yield_ranking 的中文列名）",
      paste(miss, collapse = ", ")
    ))
  }
  if (is.null(field_notes)) field_notes <- .fn_empty_notes()
  if (!is.data.frame(field_notes)) {
    stop("field_notes 必须是 data.frame（merge_field_notes 的结果）")
  }
  if (!all(c("id", "notes") %in% names(field_notes))) {
    stop("field_notes 必须含 id 与 notes 两列")
  }
  if (!is.character(ck_variety) || length(ck_variety) != 1L || is.na(ck_variety)) {
    stop("ck_variety 必须是单个对照品种名字符串")
  }
  n <- nrow(export_data)
  stageid <- trimws(as.character(export_data$stageid))
  stageid[is.na(stageid)] <- ""
  nm <- as.character(export_data$name)
  nm[is.na(nm)] <- ""
  is_ck <- if ("is_ck" %in% names(export_data)) {
    .fn_is_control(export_data$is_ck)
  } else {
    rep(FALSE, n)
  }
  if (length(is_ck) != n) is_ck <- rep(FALSE, n)
  # 对照位：name 换成「{对照品种}（对照位）」
  nm[is_ck] <- paste0(ck_variety, "（对照位）")
  yield_kg <- suppressWarnings(as.numeric(as.character(export_data$MuChan)))
  r1 <- suppressWarnings(as.numeric(as.character(export_data$JiaoLinJinDuiZhaoWeiCi)))
  r2 <- suppressWarnings(as.numeric(as.character(export_data$JiaoPingJunDuiZhaoWeiCi)))
  fmt_rank <- function(x, is_ck_v) {
    out <- rep(NA_character_, length(x))
    for (i in seq_along(x)) {
      if (is_ck_v[i]) {
        out[i] <- "—（对照基准）"
      } else if (!is.na(x[i])) {
        out[i] <- format(x[i], trim = TRUE, scientific = FALSE)
      } else {
        out[i] <- "—"
      }
    }
    out
  }
  # ---- 田间评价 ----
  notes_map <- stats::setNames(as.character(field_notes$notes),
                               trimws(as.character(field_notes$id)))
  notes <- unname(notes_map[stageid])
  notes[is.na(notes)] <- ""
  res <- data.frame(
    stageid = stageid,
    name = nm,
    yield_kg = yield_kg,
    rank_near = fmt_rank(r1, is_ck),
    rank_avg = fmt_rank(r2, is_ck),
    notes = notes,
    stringsAsFactors = FALSE
  )
  # 列名里含空格与括号，data.frame() 的 check.names 会把它们改写， 这里显式指定最终列名，保证与报告表 A-3/A-4 完全一致。
  names(res) <- c("stageid", "name", "亩产 (kg)",
                  "较临近对照位次", "较平均对照位次", "田间评价")
  if (drop_empty) {
    res <- res[nzchar(trimws(res[["田间评价"]])), , drop = FALSE]
  }
  # 亩产从高到低，无产量的排最后（radix 排序保证同产值的相对顺序稳定）
  y <- res[["亩产 (kg)"]]
  res <- res[order(-y, na.last = TRUE, method = "radix"), , drop = FALSE]
  rownames(res) <- NULL
  res
}

# ==============================================================================
# 便捷入口
# ==============================================================================
#'一站式跑完「时间线 → 数据库 → 合并 → 覆盖情况」 
#'
#'@param export_data 分析管线的 export_data 
#'@param md_path 田间调查评价时间线 md 路径（可为 NULL，表示没有时间线） 
#'@param corrections 字误订正表，默认不改写 
#'@param date_aliases 日期标签别名，作用在合并后的显示标签上 
#'@param n_total 小区总数（用于百分比） 
#'@return list(timeline, db, notes, coverage) 
#'@export
extract_field_notes <- function(export_data, md_path = NULL, corrections = list(), date_aliases = NULL, n_total = NULL) {
  tl <- if (is.null(md_path)) {
    .fn_empty_timeline()
  } else {
    extract_timeline_notes(md_path, corrections = corrections)
  }
  db <- extract_db_notes(export_data)
  notes <- merge_field_notes(tl, db, date_aliases = date_aliases)
  cov <- summarize_field_note_coverage(tl, db, n_total = n_total)
  list(timeline = tl, db = db, notes = notes, coverage = cov)
}
