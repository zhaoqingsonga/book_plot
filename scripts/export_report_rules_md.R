#!/usr/bin/env Rscript
# ==============================================================================
# 从 report_rules.R 导出 skill 文档「口径规则表.md」
# ------------------------------------------------------------------------------
# 为什么要这个脚本：
#   机器可判定的口径规则（~20 条）住在 shared/analysis/report_rules.R 里，
#   人工判断的规则住在 skill 散文里。两处都写全量 = 必然漂移。
#   本脚本保证「代码里的规则表」和「skill 里给人看的表」永远是同一份。
#
# 用法：
#   Rscript scripts/export_report_rules_md.R            # 生成/覆盖
#   Rscript scripts/export_report_rules_md.R --check    # 漂移检查（不一致则退出码 1）
#   Rscript scripts/export_report_rules_md.R --out <path>
# ==============================================================================

suppressWarnings(suppressMessages({
  args <- commandArgs(trailingOnly = TRUE)
}))

# --- 定位仓库根 -------------------------------------------------------------
script_path <- grep("^--file=", commandArgs(FALSE), value = TRUE)
script_dir <- if (length(script_path) > 0) {
  dirname(normalizePath(sub("^--file=", "", script_path[1])))
} else {
  getwd()
}
project_root <- normalizePath(file.path(script_dir, ".."))

rules_file <- file.path(project_root, "shared", "analysis", "report_rules.R")
if (!file.exists(rules_file)) {
  stop("找不到规则表源文件：", rules_file)
}
source(rules_file)

# --- 参数 -------------------------------------------------------------------
opt_out <- NULL
opt_check <- FALSE
i <- 1
while (i <= length(args)) {
  a <- args[i]
  if (a == "--check") {
    opt_check <- TRUE
  } else if (a == "--out") {
    i <- i + 1
    if (i > length(args)) stop("--out 需要一个路径参数")
    opt_out <- args[i]
  } else if (a %in% c("-h", "--help")) {
    cat("用法: Rscript scripts/export_report_rules_md.R [--check] [--out <path>]\n")
    quit(save = "no", status = 0)
  } else {
    stop("未知参数：", a)
  }
  i <- i + 1
}

default_out <- file.path(project_root, ".dsh", "skills", "trial-report-writer",
                         "references", "口径规则表.md")
out_path <- if (!is.null(opt_out)) opt_out else default_out

# --- 渲染辅助 ---------------------------------------------------------------

# 表格单元转义：竖线与换行会破坏 markdown 表格
esc <- function(x) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) return("—")
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("\r?\n", "<br>", x)
  trimws(x)
}

SEVERITY_CN <- c(blocker = "阻断", warn = "警告", info = "提示")
SCOPE_CN <- c(trial = "试验级", block = "块级")

# 规则表里没写结论/注模板时，文档里要如实说明，不要编
raw_conclusion <- function(rule) {
  v <- rpt_rule_conclusions[[rule$rule_id]]
  if (is.null(v)) return(NA_character_)
  if (is.function(v)) return("<由 ctx 动态生成>")
  as.character(v)
}

raw_note <- function(rule) {
  v <- rpt_rule_notes[[rule$rule_id]]
  if (is.null(v)) return(NA_character_)
  if (is.function(v)) return("<由 ctx 动态生成>")
  as.character(v)
}

# 规则表里没有的谓词函数要报出来 —— 文档不能替代码撒谎
missing_predicates <- RPT_RULES$predicate[!(RPT_RULES$predicate %in% names(RPT_PREDICATES))]
unused_predicates <- setdiff(names(RPT_PREDICATES), RPT_RULES$predicate)

# --- 生成 markdown ----------------------------------------------------------
sev_rank <- c(blocker = 1, warn = 2, info = 3)
ord <- order(sev_rank[RPT_RULES$severity], RPT_RULES$rule_id)
R <- RPT_RULES[ord, ]

lines <- character()
add <- function(...) lines <<- c(lines, paste0(...))

add("# 口径规则表（机器判定部分）")
add("")
add("**本文件由 `scripts/export_report_rules_md.R` 生成，请勿手改。**")
add("")
add("改规则请改 `shared/analysis/report_rules.R`，然后重跑：")
add("")
add("```bash")
add("Rscript scripts/export_report_rules_md.R           # 重新生成")
add("Rscript scripts/export_report_rules_md.R --check   # 漂移检查（CI 用）")
add("```")
add("")
add("## 这张表是什么")
add("")
add("skill 文档里的口径陷阱分两类：")
add("")
add("- **机器判定**（本表）：判据全部来自管线已有数据，确定性可复算 ⇒ 写成代码。")
add("  规则表是数据不是 `if` 分支，所以可被逐条枚举、可被导出成文档、")
add("  可在报告里声明「本次触发了哪几条」。")
add("- **人工判断**：怎么写、怎么判矛盾、待确认事项怎么措辞 ⇒ 留在 skill 散文里。")
add("")
add("判定式一列给出的是**谓词函数名**，实现在 `RPT_PREDICATES`；")
add("结论句与注模板是**带 `{占位符}` 的模板**，由 `rpt_fill()` 在运行时填入事实。")
add("")

n_block <- sum(RPT_RULES$severity == "blocker")
n_warn  <- sum(RPT_RULES$severity == "warn")
n_info  <- sum(RPT_RULES$severity == "info")
add(sprintf("当前共 **%d 条**规则：阻断 %d 条、警告 %d 条、提示 %d 条。",
            nrow(RPT_RULES), n_block, n_warn, n_info))
add("")

add("## 规则一览")
add("")
add("| 规则 ID | 名称 | 级别 | 作用域 | 判定式 | 影响块 | 文档锚点 |")
add("|---|---|---|---|---|---|---|")
for (i in seq_len(nrow(R))) {
  r <- R[i, ]
  blocks <- r$blocks
  if (is.na(blocks) || !nzchar(blocks)) blocks <- "—"
  add(sprintf("| `%s` | %s | %s | %s | `%s` | %s | %s |",
              r$rule_id, r$title,
              SEVERITY_CN[[r$severity]], SCOPE_CN[[r$scope]],
              r$predicate, blocks, r$doc_ref))
}
add("")

add("## 规则明细")
add("")
for (i in seq_len(nrow(R))) {
  r <- R[i, ]
  add(sprintf("### %s `%s`", r$title, r$rule_id))
  add("")
  add(sprintf("- **级别**：%s（%s）", SEVERITY_CN[[r$severity]], r$severity))
  add(sprintf("- **作用域**：%s（%s）", SCOPE_CN[[r$scope]], r$scope))
  add(sprintf("- **判定式**：`%s`", r$predicate))
  add(sprintf("- **影响块**：`%s`", if (is.na(r$blocks) || !nzchar(r$blocks)) "（无）" else r$blocks))
  add(sprintf("- **文档锚点**：%s", r$doc_ref))
  concl <- raw_conclusion(r)
  add(sprintf("- **结论句**：%s", if (is.na(concl)) "（无）" else paste0("`", concl, "`")))
  nt <- raw_note(r)
  add(sprintf("- **注模板**：%s", if (is.na(nt)) "（无）" else paste0("`", nt, "`")))
  add("")
}

add("## 级别含义")
add("")
add("| 级别 | 含义 | 触发后果 |")
add("|---|---|---|")
add("| 阻断 blocker | 报告的核心口径不成立 | `report_preflight()` 返回 `ok=FALSE`；生成器退出码 1，不产出交付件 |")
add("| 警告 warn | 口径有瑕疵但可交付 | 在正文/附录插入披露句 + 表图注 |")
add("| 提示 info | 需要读者知道的口径细节 | 写进表图下的「注：」 |")
add("")

if (length(missing_predicates) > 0) {
  add(paste0("> ⚠️ **规则表里有谓词在 `RPT_PREDICATES` 中不存在**：",
             paste(missing_predicates, collapse = "、"),
             " —— 这些规则永远不会触发，请修规则表。"))
  add("")
}
if (length(unused_predicates) > 0) {
  add(paste0("> ℹ️ `RPT_PREDICATES` 中有未被任何规则引用的谓词：",
             paste(unused_predicates, collapse = "、"), "。"))
  add("")
}

md <- paste0(paste(lines, collapse = "\n"), "\n")

# --- 输出 / 检查 ------------------------------------------------------------
# 真相源是「行数组」而不是拼好的字符串：writeLines 会给每行补一个 \n，
# 若先拼字符串再写就会多出一个空行，导致写完立刻 --check 报漂移。
if (opt_check) {
  if (!file.exists(out_path)) {
    cat("[FAIL] 规则表文档不存在：", out_path, "\n", sep = "")
    cat("       请运行: Rscript scripts/export_report_rules_md.R\n")
    quit(save = "no", status = 1)
  }
  cur <- readLines(out_path, warn = FALSE)
  if (!identical(cur, lines)) {
    cat("[FAIL] 规则表文档与代码不一致（漂移）：", out_path, "\n", sep = "")
    cat("       修复: Rscript scripts/export_report_rules_md.R\n")
    quit(save = "no", status = 1)
  }
  cat("[OK] 规则表文档与代码一致：", out_path, "\n", sep = "")
  quit(save = "no", status = 0)
}

dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
writeLines(lines, out_path, useBytes = TRUE)
cat("[OK] 已生成：", out_path, "\n", sep = "")
cat("     规则 ", nrow(RPT_RULES), " 条（阻断 ",
    n_block, " / 警告 ", n_warn, " / 提示 ", n_info, "）\n", sep = "")
