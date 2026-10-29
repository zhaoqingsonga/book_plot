# ==============================================================================
# 试验报告交付自检引擎（report check engine）
#
# 移植自 Python 版 check_report.py（对照 trial-report-writer skill §七 交付清单），
# 并修复原版两个缺陷：
#
#   ① 表/图题正则只认「裸写」式（`^表 4-1`），而交付稿绝大多数写成加粗式
#      （`**表 4-1 …**`），导致表数恒为 0，图/表统计与后续检查全部失真。
#      → 改为单次 `(?m)^\*{0,2}(表|图)\s+(\d+)-(\d+)` 扫描：加粗式与裸写式
#        一次命中、只数一次，不做两套正则再相加（相加会重复计数）；
#        并断言「加粗式计数 + 裸写式计数 == 合计」，防止两式互相重叠。
#
#   ② 「表号连续」检查写成 `tn == list(range(1, len(tn) + 1))`：
#      表数为空时是 `[] == []`，恒真，形同虚设；且只比「集合」不比「顺序」，
#      G-5.2 的 5-1 / 5-3 / 5-2 这类乱序从未被拦下。
#      → 拆成三步：① 先断言识别数 > 0（堵死空集）；② 查无重号无跳号；
#        ③ 查正文内编号严格升序。失败时打印实际序列与期望序列。
#
# 口径说明：
# - 「图/表编号」= `阶段号-流水号`，阶段号取自报告自身的试验阶段（如 4.2 → 4），
#   同一份报告内图、表共用一个阶段号前缀。
# - 「注：」紧邻口径：表 —— 表题行之后的 Markdown 表格块结束后的第一个非空行
#   必须含「注：」；图 —— 图题行之后的第一个非空行必须含「注：」。
#   原 Python 版用的是 900 / 2000 字符的宽松窗口，会把隔了好几段的注也算数。
#
# 本引擎只依赖 base R（不加载 dplyr / ggplot2 / shiny），可脱离 Shiny 应用独立运行。
# ==============================================================================

# ---- 交付清单骨架（与 skill §七 一一对应）----
RC_SECTIONS <- c(
  "## 一、试验目的", "## 二、试验方法", "## 三、试验材料", "## 四、试验设计",
  "## 五、性状调查项目及收获方法", "## 六、试验地选择及田间管理措施",
  "## 七、主要结果", "## 八、结论与建议", "## 附录 A"
)
RC_SUBSECTIONS <- c("### 7.1", "### 7.2", "### 7.3", "### 7.4", "### 7.5", "### 7.6")
RC_APPENDIX_SUBS <- c("### A.1", "### A.2", "### A.3", "### A.4")
RC_FRONTMATTER_KEYS <- c("title:", "type: report", "tags:", "sources:",
                          "created:", "updated:", "置信度:")

# 空白占位图：分析管线产出的空白图，报告里不应引用
RC_BLANK_FIGS <- c("性状相关性矩阵.png", "株高vs产量.png", "百粒重vs产量.png")

# 附件照片目录标记：附件照片用「图 A-N」编号，不参与正文图/表编号口径
RC_ATTACHMENT_MARK <- "06-附件"

# ------------------------------------------------------------------------------
# 基础工具
# ------------------------------------------------------------------------------

#' 读取报告全文（按字节读，显式 UTF-8 解码）
#'
#' @param path 报告 markdown 路径
#' @return list(lines = 字符向量按行, text = 全文字符串)
#' @export
rc_read_report <- function(path) {
  if (!file.exists(path)) {
    stop("报告文件不存在：", path, call. = FALSE)
  }
  size <- file.size(path)
  con <- file(path, open = "rb")
  on.exit(close(con))
  raw <- readBin(con, what = "raw", n = size)
  txt <- if (length(raw) == 0L) "" else rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  txt <- gsub("\r\n", "\n", txt, fixed = TRUE)
  txt <- gsub("\r", "\n", txt, fixed = TRUE)
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  if (length(lines) > 0L && grepl("^﻿", lines[1])) {
    lines[1] <- sub("^﻿", "", lines[1])
    txt <- sub("^﻿", "", txt)
  }
  list(lines = lines, text = txt)
}

#' 在全文里找出全部正则命中（返回位置、长度与命中文本）
#'
#' @param pattern 正则（PCRE）
#' @param txt 全文
#' @return data.frame(start, len, text)，无命中时返回 0 行空表
#' @export
rc_grep_all <- function(pattern, txt) {
  empty <- data.frame(start = integer(0), len = integer(0), text = character(0),
                      stringsAsFactors = FALSE)
  if (length(txt) == 0L) return(empty)
  gl <- gregexpr(pattern, txt, perl = TRUE)
  m <- gl[[1]]
  if (length(m) == 1L && m[1] == -1L) return(empty)
  starts <- as.integer(m)
  lens <- as.integer(attr(m, "match.length"))
  data.frame(start = starts, len = lens,
             text = substring(txt, starts, starts + lens - 1),
             stringsAsFactors = FALSE)
}

#' 字符偏移 → 行号
#'
#' @param start 字符偏移（1 起）
#' @param line_starts 每行起始偏移（cumsum 得到）
#' @return 整数行号向量
#' @export
rc_line_index <- function(start, line_starts) {
  as.integer(findInterval(start, line_starts))
}

#' 取出全文中首个正则命中的完整片段
#'
#' @param pattern 正则（PCRE）
#' @param txt 全文
#' @return 命中的完整片段；未命中返回 character(0)
#' @export
rc_capture <- function(pattern, txt) {
  if (length(txt) == 0L) return(character(0))
  m <- regexpr(pattern, txt, perl = TRUE)
  if (length(m) == 1L && m[1] == -1L) return(character(0))
  start <- as.integer(m[1])
  len <- as.integer(attr(m, "match.length"))
  substring(txt, start, start + len - 1)
}

#' 追加一条检查结果（不改原列表，返回新列表）
#'
#' @param results 已有结果列表
#' @param label 检查项名称
#' @param passed 是否通过
#' @param detail 补充说明
#' @param severity "error" 失败项 / "warn" 警告项
#' @return 新的结果列表
#' @export
rc_push <- function(results, label, passed, detail = "", severity = "error") {
  c(results, list(list(label = label, passed = isTRUE(passed),
                       detail = detail, severity = severity)))
}

#' 轻量路径归一化（消除 `.` / `..`，不要求路径存在）
#'
#' @param p 路径
#' @return 归一化后的路径
#' @export
rc_norm_path <- function(p) {
  if (length(p) == 0L) return(character(0))
  vapply(p, function(one) {
    absolute <- grepl("^/", one)
    segs <- strsplit(one, "/", fixed = TRUE)[[1]]
    out <- character(0)
    for (s in segs) {
      if (s == "" || s == ".") next
      if (s == "..") {
        if (length(out) > 0L) out <- out[-length(out)]
        next
      }
      out <- c(out, s)
    }
    paste0(if (absolute) "/" else "", paste(out, collapse = "/"))
  }, character(1), USE.NAMES = FALSE)
}

#' 下一行非空内容的行号（找不到则返回 NA）
#'
#' @param lines 行向量
#' @param idx 当前行号
#' @return 整数行号或 NA_integer_
#' @export
rc_next_nonblank <- function(lines, idx) {
  i <- idx + 1L
  n <- length(lines)
  while (i <= n && !nzchar(trimws(lines[i]))) i <- i + 1L
  if (i > n) return(NA_integer_)
  as.integer(i)
}

#' 判断某行是否为图/表标题行
#'
#' @param line 行文本
#' @param kind "表" / "图" / NULL（两者皆可）
#' @return 逻辑值
#' @export
rc_is_title_line <- function(line, kind = NULL) {
  # 前缀既可能是阶段号（4、5、3），也可能是附录标记（A）
  pat <- if (is.null(kind)) {
    "^[\\*]{0,2}[表图][ \\t]*[0-9A-Za-z]+-[0-9]+"
  } else {
    sprintf("^[\\*]{0,2}%s[ \\t]*[0-9A-Za-z]+-[0-9]+", kind)
  }
  grepl(pat, line, perl = TRUE)
}

# ------------------------------------------------------------------------------
# 图 / 表题识别
# ------------------------------------------------------------------------------

#' 识别全文中的图题 / 表题
#'
#' 单次扫描 `(?m)^\*{0,2}<kind>\s+(\w+)-(\d+)`，加粗式（`**表 4-1 …**`）与
#' 裸写式（`表 5-1 …`）一次命中、各数一次，不做两套正则再相加。
#'
#' 前缀 `\w+` 同时容纳**阶段号**（产比 4 / 多点 5 / 群体 3）与**附录标记**（A）。
#'
#' @param kind "表" 或 "图"
#' @param txt 全文
#' @param line_starts 每行起始偏移
#' @return data.frame(kind, stage, num, bold, line, text)
#' @export
rc_parse_titles <- function(kind, txt, line_starts) {
  pat <- sprintf("(?m)^\\*{0,2}%s\\s+([0-9A-Za-z]+)-([0-9]+)", kind)
  d <- rc_grep_all(pat, txt)
  if (nrow(d) == 0L) {
    return(data.frame(kind = character(0), stage = character(0), num = integer(0),
                      bold = logical(0), line = integer(0), text = character(0),
                      stringsAsFactors = FALSE))
  }
  bold <- grepl("^\\*\\*", d$text)
  body <- sub("^\\*{0,2}", "", d$text)
  body <- sub(sprintf("^%s\\s+", kind), "", body)
  mm <- regmatches(body, regexec("^([0-9A-Za-z]+)-([0-9]+)", body))
  stage <- vapply(mm, function(x) if (length(x) >= 3L) x[2] else NA_character_, character(1))
  num <- vapply(mm, function(x) if (length(x) >= 3L) as.integer(x[3]) else NA_integer_, integer(1))
  data.frame(kind = kind, stage = stage, num = num, bold = bold,
             line = rc_line_index(d$start, line_starts), text = d$text,
             stringsAsFactors = FALSE)
}

#' 校验图/表编号的「识别数 → 无重号无跳号 → 正文升序」
#'
#' 口径（对应 Python 版被 Bug ①/② 破坏的两步检查）：
#' 1. 识别数必须 > 0，否则报「未识别到任何表题/图题」并提示正确写法；
#' 2. `sort(unique(num))` 必须等于 `1..max(num)`：无重号、无跳号；
#' 3. `num` 必须严格等于 `1..n`：编号在正文中按升序出现（捕捉 5-1,5-3,5-2）。
#'
#' ⚠️ **按前缀分组校验**：正文用阶段号（4/5/3）、附录用 A，两套编号各自独立，
#'    混在一起算会误报（「表 4-10」和「表 A-3」放一起必然不连续）。
#'
#' @param titles rc_parse_titles() 的结果
#' @param label "表" 或 "图"
#' @return 结果列表
#' @export
rc_check_numbering <- function(titles, label) {
  res <- list()
  n <- nrow(titles)
  if (n == 0L) {
    res <- rc_push(res, sprintf("识别到 %d 个%s题", 0, label), FALSE,
                   sprintf("未识别到任何%s题 —— %s题格式应为 `**%s 4-1 标题**` 或 `%s 4-1 标题`",
                           label, label, label, label))
    return(res)
  }
  res <- rc_push(res, sprintf("识别到 %d 个%s题", n, label), TRUE)

  if (any(is.na(titles$num)) || any(is.na(titles$stage))) {
    res <- rc_push(res, sprintf("%s号连续不重号", label), FALSE, "存在无法解析的编号")
    return(res)
  }

  # —— 按前缀分组（正文阶段号 / 附录 A），逐组校验 ——
  for (st in unique(titles$stage)) {
    grp <- titles[titles$stage == st, , drop = FALSE]
    num <- grp$num
    max_num <- max(num)
    gtag <- if (grepl("^[0-9]+$", st)) sprintf("%s %s 号", label, st) else
      sprintf("%s %s 号", label, st)

    # ② 无重号、无跳号
    observed <- sort(unique(num))
    expected <- as.integer(seq_len(max_num))
    if (!identical(observed, expected)) {
      miss <- setdiff(expected, observed)
      dup <- observed[duplicated(observed)]
      detail <- sprintf("实际 %s；期望 %s", paste(observed, collapse = ","),
                        paste(expected, collapse = ","))
      if (length(miss) > 0L) detail <- paste0(detail, "；缺号 ", paste(miss, collapse = ","))
      if (length(dup) > 0L) detail <- paste0(detail, "；重号 ", paste(dup, collapse = ","))
      res <- rc_push(res, sprintf("%s 1~%d 连续不重号", gtag, max_num), FALSE, detail)
    } else {
      res <- rc_push(res, sprintf("%s 1~%d 连续不重号", gtag, max_num), TRUE)
    }

    # ③ 正文内严格升序
    asc_expected <- as.integer(seq_len(nrow(grp)))
    if (!identical(num, asc_expected)) {
      detail <- sprintf("实际 %s；期望 %s", paste(num, collapse = ","),
                        paste(asc_expected, collapse = ","))
      res <- rc_push(res, sprintf("%s在正文中按升序出现", gtag), FALSE, detail)
    } else {
      res <- rc_push(res, sprintf("%s在正文中按升序出现", gtag), TRUE)
    }
  }
  res
}

#' 编号前缀一致性（图与表共用一个阶段号；附录统一用 A）
#'
#' 合法组合：正文阶段号**恰好一个**（4/5/3…），可选再出现附录前缀 **A**。
#' 出现别的前缀（如表 4-0 那种 0 号表）→ 判失败。
#'
#' @param tables 表题表
#' @param figs 图题表
#' @return 结果列表
#' @export
rc_check_prefix <- function(tables, figs) {
  stages <- unique(c(tables$stage, figs$stage))
  stages <- stages[!is.na(stages)]
  if (length(stages) == 0L) {
    return(rc_push(list(), "图/表编号前缀唯一", FALSE, "未识别到任何图/表题，无法判定前缀"))
  }
  is_num  <- grepl("^[0-9]+$", stages)
  is_app  <- grepl("^[A-Za-z]$", stages)
  num_st  <- stages[is_num]
  app_st  <- stages[is_app]
  other   <- stages[!is_num & !is_app]

  if (length(other) > 0L) {
    return(rc_push(list(), "图/表编号前缀唯一", FALSE,
                   paste0("出现非法前缀：", paste(other, collapse = ","))))
  }
  if (length(num_st) != 1L) {
    return(rc_push(list(), "图/表编号前缀唯一", FALSE,
                   sprintf("正文阶段号应恰好一个，实际 %d 个（%s）",
                           length(num_st), paste(num_st, collapse = ","))))
  }
  if (length(app_st) > 1L) {
    return(rc_push(list(), "图/表编号前缀唯一", FALSE,
                   paste0("附录前缀应统一为 A，实际：", paste(app_st, collapse = ","))))
  }
  detail <- if (length(app_st) == 1L)
    sprintf("正文阶段号 %s，附录前缀 %s", num_st, app_st) else paste0("正文阶段号 ", num_st)
  rc_push(list(), "图/表编号前缀唯一", TRUE, detail)
}

#' 识别口径自洽性断言：加粗式计数 + 裸写式计数 == 合计，且两者不重叠
#'
#' @param kind "表" 或 "图"
#' @param txt 全文
#' @param total rc_parse_titles() 得到的总计数
#' @return 结果列表
#' @export
rc_check_title_count_selfconsistent <- function(kind, txt, total) {
  # 前缀同时容纳阶段号与附录标记 A
  bold_n <- nrow(rc_grep_all(sprintf("(?m)^\\*\\*%s\\s+[0-9A-Za-z]+-[0-9]+", kind), txt))
  bare_n <- nrow(rc_grep_all(sprintf("(?m)^%s\\s+[0-9A-Za-z]+-[0-9]+", kind), txt))
  ok <- (bold_n + bare_n == total)
  rc_push(list(), sprintf("%s题识别口径自洽（加粗式 + 裸写式 = 合计，无重叠）", kind),
          ok, sprintf("加粗式 %d + 裸写式 %d = 合计 %d", bold_n, bare_n, total))
}

# ------------------------------------------------------------------------------
# 「注：」紧邻性
# ------------------------------------------------------------------------------

#' 判定表题之后是否紧邻「注：」
#'
#' 口径：表题行 →（可空行）→ Markdown 表格块（连续以 `|` 开头的行）→
#' （可空行）→ 第一个非空行必须含「注：」。
#'
#' @param lines 行向量
#' @param title_idx 表题所在行号
#' @return list(ok, note_idx)
#' @export
rc_table_has_note <- function(lines, title_idx) {
  i <- rc_next_nonblank(lines, title_idx)
  if (is.na(i)) return(list(ok = FALSE, note_idx = NA_integer_))
  if (grepl("^[ \t]*\\|", lines[i])) {
    n <- length(lines)
    while (i <= n && grepl("^[ \t]*\\|", lines[i])) i <- i + 1L
    j <- rc_next_nonblank(lines, i - 1L)
    if (is.na(j)) return(list(ok = FALSE, note_idx = NA_integer_))
    i <- j
  }
  if (grepl("注：", lines[i], fixed = TRUE)) {
    return(list(ok = TRUE, note_idx = i))
  }
  list(ok = FALSE, note_idx = NA_integer_)
}

#' 判定图题之后是否紧邻「注：」
#'
#' 口径：图题行之后的第一个非空行必须含「注：」。
#'
#' @param lines 行向量
#' @param title_idx 图题所在行号
#' @return list(ok, note_idx)
#' @export
rc_figure_has_note <- function(lines, title_idx) {
  i <- rc_next_nonblank(lines, title_idx)
  if (is.na(i)) return(list(ok = FALSE, note_idx = NA_integer_))
  if (grepl("注：", lines[i], fixed = TRUE)) {
    return(list(ok = TRUE, note_idx = i))
  }
  list(ok = FALSE, note_idx = NA_integer_)
}

# ------------------------------------------------------------------------------
# 交叉引用（正文提到的表/图必须真实存在）
# ------------------------------------------------------------------------------

#' 校验正文里提到的「表 N-N」「图 N-N」都指向真实存在的编号
#'
#' 口径：逐行扫出所有 `表/图 + 数字-数字`；若该行是标题行，则第一个命中
#' 视为标题本身（不算引用），其余命中才是正文引用；非标题行的命中全部算引用。
#'
#' @param lines 行向量
#' @param kind "表" 或 "图"
#' @param titles rc_parse_titles() 的结果（对应 kind）
#' @return list(results = 结果列表, dangling = 失效引用字符向量)
#' @export
rc_check_xrefs <- function(lines, kind, titles) {
  known <- paste0(titles$stage, "-", titles$num)
  known <- known[!is.na(titles$num)]
  known <- unique(known)

  pat <- sprintf("(%s)[ \\t]*([0-9A-Za-z]+)-([0-9]+)", kind)
  dangling <- character(0)
  n_ref <- 0L
  for (ln in lines) {
    if (!nzchar(trimws(ln))) next
    hits <- regmatches(ln, gregexpr(pat, ln, perl = TRUE))[[1]]
    if (length(hits) == 0L) next
    is_title <- rc_is_title_line(ln, kind)
    for (k in seq_along(hits)) {
      if (is_title && k == 1L) next          # 标题本身不算交叉引用
      n_ref <- n_ref + 1L
      parsed <- regmatches(hits[k], regexec(sprintf("^(%s)[ \\t]*([0-9A-Za-z]+)-([0-9]+)$", kind),
                                            hits[k], perl = TRUE))[[1]]
      if (length(parsed) < 4L) next
      key <- paste0(parsed[3], "-", parsed[4])
      if (!(key %in% known)) {
        dangling <- c(dangling, sprintf("%s %s", parsed[2], key))
      }
    }
  }
  res <- list()
  if (n_ref == 0L) {
    res <- rc_push(res, sprintf("正文 %s 交叉引用均指向存在的%s", kind, kind), TRUE,
                   "正文未出现交叉引用")
  } else {
    res <- rc_push(res, sprintf("正文 %d 处%s交叉引用均指向存在的%s", n_ref, kind, kind),
                   length(dangling) == 0L,
                   if (length(dangling) == 0L) "" else paste(unique(dangling), collapse = "、"))
  }
  list(results = res, dangling = dangling)
}

#' 取正则命中的上下文（行号 + 该行截断片段），便于人工判断是「脏值泄漏」
#' 还是「报告里在讨论这个脏值」
#'
#' @param pattern 正则（PCRE）
#' @param txt 全文
#' @param line_starts 每行起始偏移
#' @param lines 行向量
#' @param k 最多展示几行
#' @return 字符串；无命中返回 ""
#' @export
rc_hit_context <- function(pattern, txt, line_starts, lines, k = 3L) {
  d <- rc_grep_all(pattern, txt)
  if (nrow(d) == 0L) return("")
  snippets <- vapply(seq_len(nrow(d)), function(i) {
    s <- d$start[i]
    e <- s + d$len[i] - 1L
    from <- max(1L, s - 60L)
    to <- min(nchar(txt), e + 60L)
    sprintf("L%d: …%s…", rc_line_index(s, line_starts),
            gsub("[\r\n]+", " ", substr(txt, from, to)))
  }, character(1))
  paste(head(unique(snippets), k), collapse = " ｜ ")
}

# ------------------------------------------------------------------------------
# 主检查入口
# ------------------------------------------------------------------------------

#' 对一份成品试验报告跑完整交付自检
#'
#' @param path 报告 markdown 路径
#' @param strict TRUE 时警告项也计入失败
#' @return list(path, summary, results, n_pass, n_fail, n_warn, n_total, ok)
#' @export
rc_check_report <- function(path, strict = FALSE) {
  rep <- rc_read_report(path)
  lines <- rep$lines
  txt <- rep$text
  base <- dirname(path)
  line_starts <- cumsum(c(1, nchar(lines) + 1))

  res <- list()

  # ---------- 1. frontmatter ----------
  fm <- NULL
  if (length(lines) >= 3L && identical(trimws(lines[1]), "---")) {
    rest <- which(trimws(lines[-1]) == "---")
    if (length(rest) > 0L) {
      fm_body <- lines[2:(rest[1])]
      fm <- paste(fm_body, collapse = "\n")
    }
  }
  res <- rc_push(res, "frontmatter 存在", !is.null(fm))
  if (!is.null(fm)) {
    for (k in RC_FRONTMATTER_KEYS) {
      res <- rc_push(res, sprintf("frontmatter 含 %s", k), grepl(k, fm, fixed = TRUE))
    }
  }

  # ---------- 2. 章节骨架 ----------
  for (s in RC_SECTIONS) {
    res <- rc_push(res, sprintf("章节 %s", s), grepl(s, txt, fixed = TRUE))
  }
  for (sub in RC_SUBSECTIONS) {
    res <- rc_push(res, sprintf("小节 %s", sub), grepl(sub, txt, fixed = TRUE))
  }

  # ---------- 3. 图/表编号 ----------
  tables <- rc_parse_titles("表", txt, line_starts)
  figs <- rc_parse_titles("图", txt, line_starts)

  res <- c(res, rc_check_title_count_selfconsistent("表", txt, nrow(tables)))
  res <- c(res, rc_check_title_count_selfconsistent("图", txt, nrow(figs)))
  res <- c(res, rc_check_prefix(tables, figs))
  res <- c(res, rc_check_numbering(tables, "表"))
  res <- c(res, rc_check_numbering(figs, "图"))

  # ---------- 4. 每张图 / 表后有紧邻的「注：」 ----------
  tab_missing <- character(0)
  for (i in seq_len(nrow(tables))) {
    nt <- rc_table_has_note(lines, tables$line[i])
    if (!nt$ok) {
      tab_missing <- c(tab_missing, sprintf("%s %s-%d", tables$kind[i],
                                           tables$stage[i], tables$num[i]))
    }
  }
  res <- rc_push(res, sprintf("%d 张表后紧邻「注：」", nrow(tables) - length(tab_missing)),
                 length(tab_missing) == 0L,
                 if (length(tab_missing) == 0L) "" else paste(tab_missing, collapse = "、"))

  fig_missing <- character(0)
  for (i in seq_len(nrow(figs))) {
    nt <- rc_figure_has_note(lines, figs$line[i])
    if (!nt$ok) {
      fig_missing <- c(fig_missing, sprintf("%s %s-%d", figs$kind[i],
                                            figs$stage[i], figs$num[i]))
    }
  }
  res <- rc_push(res, sprintf("%d 张图后紧邻「注：」", nrow(figs) - length(fig_missing)),
                 length(fig_missing) == 0L,
                 if (length(fig_missing) == 0L) "" else paste(fig_missing, collapse = "、"))

  # ---------- 4b. 正文图片 → 图题 → 注 的结构 ----------
  img_hits <- rc_grep_all("!\\[[^\\]]*\\]\\(([^)]+)\\)", txt)
  img_paths <- if (nrow(img_hits) == 0L) character(0) else
    sub("^!\\[[^\\]]*\\]\\((.*)\\)$", "\\1", img_hits$text, perl = TRUE)
  img_lines <- if (nrow(img_hits) == 0L) integer(0) else
    rc_line_index(img_hits$start, line_starts)

  is_body <- !grepl(RC_ATTACHMENT_MARK, img_paths, fixed = TRUE)
  body_img <- img_paths[is_body]
  body_img_line <- img_lines[is_body]

  no_title <- character(0)
  struct_ok <- 0L
  for (k in seq_along(body_img)) {
    j <- rc_next_nonblank(lines, body_img_line[k])
    if (is.na(j) || !rc_is_title_line(lines[j], "图")) {
      no_title <- c(no_title, basename(body_img[k]))
      next
    }
    if (rc_figure_has_note(lines, j)$ok) {
      struct_ok <- struct_ok + 1L
    }
  }
  res <- rc_push(res, sprintf("%d/%d 张正文图为「图片→图题→注」结构",
                              struct_ok, length(body_img)),
                 length(no_title) == 0L && struct_ok == length(body_img),
                 if (length(no_title) == 0L) "" else paste(no_title, collapse = "、"))
  res <- rc_push(res, "每张正文图后紧跟图题", length(no_title) == 0L,
                 if (length(no_title) == 0L) "" else paste(no_title, collapse = "、"))

  # ---------- 5. 图片文件都存在 / 无孤儿 PNG / 未引用空白图 ----------
  missing_img <- img_paths[!file.exists(rc_norm_path(file.path(base, img_paths)))]
  res <- rc_push(res, sprintf("正文引用的 %d 张图全部存在", length(img_paths)),
                 length(missing_img) == 0L,
                 if (length(missing_img) == 0L) "" else paste(missing_img, collapse = "、"))

  stem <- sub("\\.[A-Za-z0-9]+$", "", basename(path))
  img_dir <- file.path(base, paste0(stem, "_images"))
  if (dir.exists(img_dir)) {
    files <- list.files(img_dir, pattern = "\\.png$")
    used <- basename(img_paths)
    orphan <- setdiff(files, used)
    res <- rc_push(res, sprintf("图目录无孤儿 PNG（%d 张）", length(files)),
                   length(orphan) == 0L,
                   if (length(orphan) == 0L) "" else paste(sort(orphan), collapse = "、"),
                   severity = "warn")
    usedblank <- intersect(used, RC_BLANK_FIGS)
    res <- rc_push(res, "未引用空白图", length(usedblank) == 0L,
                   if (length(usedblank) == 0L) "" else paste(usedblank, collapse = "、"),
                   severity = "warn")
  }

  # ---------- 6. wikilink 指向的页面真实存在 ----------
  wl <- rc_grep_all("\\[\\[([^\\]|#]+)", txt)
  wl_links <- if (nrow(wl) == 0L) character(0) else
    sub("^\\[\\[([^\\]|#]+)$", "\\1", wl$text, perl = TRUE)
  wl_missing <- character(0)
  for (l in unique(wl_links)) {
    p <- rc_norm_path(file.path(base, l))
    if (!(file.exists(paste0(p, ".md")) || file.exists(p))) {
      wl_missing <- c(wl_missing, l)
    }
  }
  res <- rc_push(res, sprintf("%d 条 wikilink 指向的页面均存在", length(unique(wl_links))),
                 length(wl_missing) == 0L,
                 if (length(wl_missing) == 0L) "" else paste(wl_missing, collapse = "、"))

  # ---------- 7. 口径标注 ----------
  res <- rc_push(res, "标注材料层口径", grepl("材料层", txt, fixed = TRUE))
  res <- rc_push(res, "出现小区层/小区口径说明", grepl("小区", txt, fixed = TRUE))
  res <- rc_push(res, "标注较临近对照口径", grepl("较临近对照", txt, fixed = TRUE))
  res <- rc_push(res, "标注较平均对照口径", grepl("较平均对照", txt, fixed = TRUE))
  res <- rc_push(res, "标注含对照口径", grepl("含对照", txt, fixed = TRUE))

  # ---------- 8. 管线同源声明 + 盖章行 ----------
  res <- rc_push(res, "含管线同源声明", grepl("run_analysis", txt, fixed = TRUE))
  res <- rc_push(res, "含承试单位盖章行", grepl("承试单位（盖章）", txt, fixed = TRUE))
  res <- rc_push(res, "含 ## 关联 段", grepl("## 关联", txt, fixed = TRUE))

  # ---------- 9. 摘要 ----------
  abs_raw <- rc_capture("(?s)\\*\\*摘要：\\*\\*(.+?)\\n\\n## 一、", txt)
  res <- rc_push(res, "含摘要段", length(abs_raw) > 0L)
  if (length(abs_raw) > 0L) {
    abstract <- sub("(?s)^\\*\\*摘要：\\*\\*(.+?)\\n\\n## 一、$", "\\1", abs_raw, perl = TRUE)
    res <- rc_push(res, sprintf("摘要长度 %d 字符 > 500", nchar(abstract)),
                   nchar(abstract) > 500)
    n_num <- nrow(rc_grep_all("[0-9]+\\.?[0-9]*[ \\t]*(?:kg|%|份|天|个)", abstract))
    res <- rc_push(res, sprintf("摘要含密集数字（%d 处，要求 > 15）", n_num), n_num > 15)
  }

  # ---------- 10. 结论与建议 ----------
  con_raw <- rc_capture("(?s)## 八、结论与建议(.+?)\\n## 附录", txt)
  res <- rc_push(res, "结论段存在", length(con_raw) > 0L)
  if (length(con_raw) > 0L) {
    concl <- sub("(?s)^## 八、结论与建议(.+?)\\n## 附录$", "\\1", con_raw, perl = TRUE)
    n_items <- nrow(rc_grep_all("(?m)^[0-9]+\\.[ \\t]", concl))
    res <- rc_push(res, sprintf("结论 %d 条（要求 5~8）", n_items),
                   n_items >= 5L && n_items <= 8L)
    n_num <- nrow(rc_grep_all("[0-9]+\\.?[0-9]*[ \\t]*(?:kg|%|份|天)", concl))
    res <- rc_push(res, sprintf("结论每条带数字（%d 处，要求 > 10）", n_num), n_num > 10)
  }

  # ---------- 11. 附录 A 结构 ----------
  for (s in RC_APPENDIX_SUBS) {
    res <- rc_push(res, sprintf("附录 %s", s), grepl(s, txt, fixed = TRUE))
  }
  res <- rc_push(res, "含待确认事项", grepl("待确认事项", txt, fixed = TRUE))

  # ---------- 12. 无 Inf / NaN / TODO / XXX ----------
  # Inf / NaN 用「非字母数字边界」匹配：既能抓住 R 输出的 Inf / -Inf / NaN，
  # 又不会把 Infirm、NaNometer 之类的英文单词误判为数值残留。
  for (tok in c("Inf", "-Inf", "NaN")) {
    pat <- if (tok == "-Inf") {
      "(?<![A-Za-z0-9])-Inf(?![A-Za-z0-9])"
    } else {
      sprintf("(?<![A-Za-z0-9])%s(?![A-Za-z0-9])", tok)
    }
    hits <- rc_grep_all(pat, txt)
    detail <- if (nrow(hits) == 0L) "" else paste0(
      sprintf("命中 %d 处 —— ", nrow(hits)),
      rc_hit_context(pat, txt, line_starts, lines))
    res <- rc_push(res, sprintf("无 %s", tok), nrow(hits) == 0L, detail)
  }
  for (tok in c("TODO", "XXX")) {
    res <- rc_push(res, sprintf("无 %s", tok), !grepl(tok, txt, fixed = TRUE))
  }

  # ---------- 13. 新增：占位符残留 ----------
  n_tpl_slots <- length(strsplit(txt, "{{", fixed = TRUE)[[1]]) - 1L
  res <- rc_push(res, "无 {{ }} 未填充模板槽", n_tpl_slots == 0L,
                 if (n_tpl_slots == 0L) "" else sprintf("残留 %d 处", n_tpl_slots))
  n_narr <- nrow(rc_grep_all("<<[^>]+>>", txt))
  res <- rc_push(res, "无 <<...>> 叙述占位标记", n_narr == 0L,
                 if (n_narr == 0L) "" else sprintf("残留 %d 处", n_narr))

  # ---------- 14. 新增：正文交叉引用 ----------
  xt <- rc_check_xrefs(lines, "表", tables)
  res <- c(res, xt$results)
  xf <- rc_check_xrefs(lines, "图", figs)
  res <- c(res, xf$results)

  # ---------- 汇总 ----------
  ok_rows <- Filter(function(r) isTRUE(r$passed), res)
  bad_rows <- Filter(function(r) !isTRUE(r$passed) && identical(r$severity, "error"), res)
  warn_rows <- Filter(function(r) !isTRUE(r$passed) && identical(r$severity, "warn"), res)

  n_pass <- length(ok_rows)
  n_fail <- length(bad_rows)
  n_warn <- length(warn_rows)
  n_total <- n_pass + n_fail + n_warn

  list(
    path = path,
    summary = list(
      chars = nchar(txt),
      lines = nchar(txt) - nchar(gsub("\n", "", txt, fixed = TRUE)),
      n_fig = nrow(figs),
      n_tab = nrow(tables),
      n_img = length(img_paths)
    ),
    results = res,
    n_pass = n_pass,
    n_fail = n_fail,
    n_warn = n_warn,
    n_total = n_total,
    ok = (n_fail == 0L) && (!strict || n_warn == 0L)
  )
}
