# =============================================================================
# 模块: 其它试验（上传试验 | 分析）
# =============================================================================

regional_import_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(class = "tab-panel",
      h3(class = "panel-title", span(class = "icon", icon("file-import")), "其它试验"),
      tabsetPanel(id = ns("subtab"),
        # ===== 子页1: 上传试验 =====
        tabPanel("上传试验", icon = icon("upload"),
          fluidRow(
            column(5,
              div(id = ns("card_step1"),
                div(class = "card",
                  div(class = "card-header", icon("upload"), " Step 1: 上传文件"),
                  div(class = "card-body",
                    fileInput(ns("file_excel"), "选择 Excel 文件", accept = c(".xlsx", ".xls"),
                      buttonLabel = "浏览...", placeholder = "未选择文件"),
                    selectInput(ns("sheet_name"), "选择 Sheet", choices = "Sheet1", selected = "Sheet1", width = "100%"),
                    textInput(ns("trial_name"), "试验名称", value = "", placeholder = "如：北组区域试验2024", width = "100%"),
                    textInput(ns("group_label"), "组别标记（可选）", value = "", placeholder = "如：北组、南组", width = "100%"),
                    hr(),
                    actionButton(ns("btn_next_mapping"), "下一步：列映射", icon = icon("arrow-right"), class = "btn-primary")))),
              shinyjs::hidden(div(id = ns("card_step2"),
                div(class = "card",
                  div(class = "card-header", icon("exchange-alt"), " Step 2: 列映射确认"),
                  div(class = "card-body",
                    p(class = "text-muted small", "系统已自动识别列名, 请检查调整。", tags$b("品种、地点、亩产为必选项。")),
                    uiOutput(ns("mapping_ui")), hr(),
                    div(class = "d-flex gap-2",
                      actionButton(ns("btn_back_step1"), "返回", icon = icon("arrow-left"), class = "btn-outline-secondary btn-sm"),
                      actionButton(ns("btn_preview"), "预览数据", icon = icon("eye"), class = "btn-outline-primary btn-sm")))))),
              shinyjs::hidden(div(id = ns("card_step3"),
                div(class = "card",
                  div(class = "card-header", icon("check-circle"), " Step 3: 预览确认与导入"),
                  div(class = "card-body",
                    uiOutput(ns("preview_summary")), hr(),
                    div(class = "d-flex gap-2",
                      actionButton(ns("btn_back_step2"), "返回调整", icon = icon("arrow-left"), class = "btn-outline-secondary btn-sm"),
                      actionButton(ns("btn_import"), "确认导入", icon = icon("play"), class = "btn-success btn-sm"),
                      actionButton(ns("btn_new_import"), "继续导入", icon = icon("plus"), class = "btn-outline-primary btn-sm")))))),
              div(class = "card mt-3",
                div(class = "card-header", icon("spinner"), " 导入进度"),
                div(class = "card-body", verbatimTextOutput(ns("import_log"))))),
            column(7,
              div(class = "card",
                div(class = "card-header", div(class = "d-flex justify-content-between align-items-center",
                  span(icon("table"), " 数据预览"), uiOutput(ns("preview_badge")))),
                div(class = "card-body p-0", DT::dataTableOutput(ns("preview_table"))))))),

        # ===== 子页2: 分析 =====
        tabPanel("分析", icon = icon("chart-bar"),
          p(class = "text-muted fb-panel-intro", "数据保存在 ", tags$code("data/other_trials.sqlite")),
          fluidRow(
            column(6, selectInput(ns("selected_batch"), "选择试验", choices = NULL, width = "100%")),
            column(6, div(class = "d-flex gap-1 pt-4",
              actionButton(ns("btn_analyze_batch"), "分析", icon=icon("chart-bar"), class="btn-outline-primary btn-sm"),
              downloadButton(ns("btn_download_result"), "下载", class="btn-outline-secondary btn-sm"),
              actionButton(ns("btn_delete_batch"), "删除", icon=icon("trash"), class="btn-outline-danger btn-sm")))),
          uiOutput(ns("batch_info")),
          div(class = "card", div(class = "card-body p-0", DT::dataTableOutput(ns("batch_table")))),
          shinyjs::hidden(downloadLink(ns("other_do_export"), "hidden")))
      )))
}

regional_import_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rv <- reactiveValues(current_step="step1", raw_data=NULL,
      user_mapping=NULL, processed=NULL, import_log=character(0), is_importing=FALSE)
    refresh_trigger <- reactiveVal(0)

    showStep <- function(step) {
      rv$current_step <- step
      for (i in c("card_step1","card_step2","card_step3")) shinyjs::hide(i)
      shinyjs::show(paste0("card_", step))
    }
    add_log <- function(msg) {
      rv$import_log <- c(rv$import_log, paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", msg))
    }

    observeEvent(input$file_excel, {
      req(input$file_excel)
      tryCatch({
        sheets <- openxlsx::getSheetNames(input$file_excel$datapath)
        updateSelectInput(session, "sheet_name", choices=sheets, selected=sheets[1])
      }, error=function(e) updateSelectInput(session, "sheet_name", choices="Sheet1"))
    })

    observeEvent(input$btn_next_mapping, {
      req(input$file_excel, input$sheet_name); add_log("正在读取文件...")
      tryCatch({
        raw <- readRegionalRaw(input$file_excel$datapath, input$sheet_name)
        rv$raw_data <- raw; rv$user_mapping <- autoDetectColumns(raw$raw_cols)
        add_log(sprintf("完成: %d行, %d列", nrow(raw$raw_df), length(raw$raw_cols))); showStep("step2")
      }, error=function(e) { add_log(sprintf("错误: %s", e$message)); showNotification(paste("读取失败:", e$message), type="error") })
    })

    output$mapping_ui <- renderUI({
      req(rv$raw_data, rv$user_mapping)
      TRAIT_DISPLAY_NAMES <- c(
        "品种(name)"="name","地点(place)"="place","亩产(MuChan)"="MuChan",
        "小区产量"="XiaoQuChanLiang","小区实收面积"="XiaoQuShiShouMianJi","含水量"="HanShuiLiang",
        "序号(stageid)"="stageid","母本(ma)"="ma","父本(pa)"="pa",
        "播种期"="BoZhongQi","出苗期"="ChuMiaoQi","出苗良否"="ChuMiaoLiangFou","苗期田间评价"="MiaoQiTianJianPingJia",
        "开花期"="KaiHuaQi","花色(HuaSe)"="HuaSe","花期田间评价"="HuaQiTianJianPingJia","叶形(YeXing)"="YeXing",
        "茸毛色(RongMaoSe)"="RongMaoSe","生长习性"="ShengZhangXiXing","结荚习性"="JieJiaXiXing",
        "倒伏性(DaoFuXing)"="DaoFuXing","早衰性"="ZaoShuaiXing","株型(ZhuXing)"="ZhuXing",
        "落叶性"="LuoYeXing","裂荚性"="LieJiaXing","成熟期"="ChengShuQi","活秆成熟"="HuoGanChengShu",
        "成熟期田间评价"="ChengShuQiTianJianPingJia","收获期"="ShouHuoQi","小区收获株数"="XiaoQuShouHuoZhuShu",
        "生育期(ShengYuQi)"="ShengYuQi","田间备注"="TianJianBeiZhu",
        "花叶病毒病"="HuaYeBingDuBing","拟茎点种腐病"="NiJingDianZhongFuBing","霜霉病"="ShuangMeiBing",
        "灰斑病"="HuiBanBing","细菌性斑点病"="XiJunXingBanDianBing","锈病"="XiuBing","根腐病"="GenFuBing",
        "孢囊线虫病"="BaoNangXianChongBing","其他病害"="QiTaBingHai",
        "豆秆黑潜蝇"="DouGanHeiQianYing","豆荚螟"="DouJiaMing","蚜虫"="YaChong","食叶性害虫"="ShiYeXingHaiChong",
        "考种株数"="KaoZhongZhuShu","株高(ZhuGao)"="ZhuGao","底荚高(DiJiaGao)"="DiJiaGao",
        "分枝数"="FenZhiShu","主茎节数"="ZhuJingJieShu","荚形(JiaXing)"="JiaXing","荚熟色"="JiaShuSe",
        "有效荚"="YouXiaoJia","无效荚"="WuXiaoJia","单株荚数"="DanZhuJiaShu","单株粒数"="DanZhuLiShu",
        "单株粒重"="DanZhuLiZhong","每荚粒数"="MeiJiaLiShu","粒型(LiXing)"="LiXing","种皮色(ZhongPiSe)"="ZhongPiSe",
        "脐色(QiSe)"="QiSe","子叶色"="ZiYeSe","种皮光泽"="ZhongPiGuangZe","百粒重(BaiLiZhong)"="BaiLiZhong",
        "完好粒率"="WanHaoLiLv","破碎粒率"="PoSuiLiLv","病粒率"="BingLiLv","紫斑粒率"="ZiBanLiLv",
        "褐斑粒率"="HeBanLiLv","霜霉粒率"="ShuangMeiLiLv","灰斑粒率"="HuiBanLiLv","虫蚀粒率"="ChongShiLiLv",
        "籽粒评价"="ZiLiPingJia","蛋白(DanBai)"="DanBai","脂肪(ZhiFang)"="ZhiFang","蛋脂和"="DanZhiHe",
        "草甘膦抗性"="CaoGanLinKangXing","试纸检测"="ShiZhiJianCe","含基因"="HanJiYin",
        "播种盆数"="BoZhongPenShu","播种粒数"="BoZhongLiShu","出苗数"="ChuMiaoShu","出苗粒数"="ChuMiaoLiShu",
        "耐盐性"="NaiYanXing","耐旱性"="NaiHanXing","始花期"="ShiHuaQi","杂交花数"="ZaJiaoHuaShu",
        "成活荚数"="ChengHuoJiaShu","杂交粒数"="ZhaJiaoliShu","初熟期"="ChuShuQi","完熟期"="WanShuQi",
        "恢复率"="HuiFuLv","SSR不合格"="SSRBuHeGeWeiDian","忽略"="ignore")
      raw_cols <- rv$raw_data$raw_cols; mapping <- rv$user_mapping
      tagList(lapply(seq_along(raw_cols), function(i) {
        col <- raw_cols[i]; sugg <- mapping[[col]]
        if (is.null(sugg) || is.na(sugg)) sugg <- "ignore"
        fluidRow(column(5, p(class="text-end pt-2", tags$code(col))),
          column(1, p(class="text-center pt-2", icon("arrow-right"))),
          column(6, selectInput(ns(paste0("map_",i)), label=NULL,
            choices=TRAIT_DISPLAY_NAMES, selected=sugg, width="100%")))}))
    })

    observeEvent(rv$raw_data, {
      req(rv$raw_data)
      for (i in seq_along(rv$raw_data$raw_cols)) local({
        idx <- i
        observeEvent(input[[paste0("map_",idx)]], {
          val <- input[[paste0("map_",idx)]]
          if (!is.null(val) && !is.null(rv$raw_data)) rv$user_mapping[[ rv$raw_data$raw_cols[idx] ]] <- val
        }, ignoreInit=TRUE, ignoreNULL=TRUE)
      })
    }, once=TRUE)

    observeEvent(input$btn_preview, {
      req(rv$raw_data, rv$user_mapping); add_log("正在标准化...")
      tryCatch({
        meta <- list(trial_name=if(nchar(input$trial_name)>0)input$trial_name else"未命名试验", group_label=input$group_label)
        rv$processed <- processRegionalImport(file=input$file_excel$datapath, sheet=input$sheet_name, mapping=rv$user_mapping, metadata=meta)
        add_log(sprintf("完成: %d行, %d地点, %d品种", rv$processed$stats$n_rows, rv$processed$stats$n_sites, rv$processed$stats$n_varieties))
    if (!is.null(rv$processed$stats$dropped_rows) && rv$processed$stats$dropped_rows > 0)
      add_log(sprintf("注意: 剔除了 %d 行无效数据（地点或品种为空）", rv$processed$stats$dropped_rows))
        showStep("step3")
      }, error=function(e) { add_log(sprintf("错误: %s", e$message)); showNotification(paste("标准化失败:", e$message), type="error") })
    })

    output$preview_summary <- renderUI({
      req(rv$processed); s <- rv$processed$stats; df <- rv$processed$data
      ck <- unique(df$name[df$is_ck==1]); unresolved <- rv$processed$unresolved
      has_unr <- !is.null(unresolved) && length(unresolved) > 0
      tagList(
        div(h5("导入概览"), tags$ul(
          tags$li(tags$b("行数:"), s$n_rows), tags$li(tags$b("地点:"), s$n_sites),
          tags$li(tags$b("品种:"), s$n_varieties), tags$li(tags$b("产量:"), if(s$has_yield)"有"else"无"),
          tags$li(tags$b("CK:"), if(length(ck)>0)paste(ck,collapse=",")else"无"))),
        if (has_unr) tagList(hr(), div(class="alert alert-warning", icon("exclamation-triangle"), tags$b("以下质量性状值无法自动匹配，请手动指定：")),
          lapply(names(unresolved), function(tcol) {
            u <- unresolved[[tcol]]
            fluidRow(column(12, tags$b(u$name_cn," (",u$name_lib,"):"),
              lapply(u$values, function(v) fluidRow(
                column(3, tags$code(v), icon("arrow-right")),
                column(9, selectInput(ns(paste0("res_",u$name_lib,"_",gsub("[^A-Za-z0-9]","_",v))), label=NULL,
                  choices=c("空值(NA)"="__NA__", setNames(u$levels, paste0(sub("^\\d+-","",u$levels)," (",u$levels,")"))),
                  selected="__NA__", width="100%"))))))
          })) else div(class="text-success", icon("check"), "所有性状值均已成功匹配"))
    })

    output$preview_table <- DT::renderDataTable({
      req(rv$processed); df <- rv$processed$data
      sc <- intersect(c("name","place","MuChan","is_ck"), names(df))
      DT::datatable(head(df[,sc,drop=FALSE],30), options=list(pageLength=10,scrollX=TRUE,dom="tp"), rownames=FALSE, class="cell-border stripe hover")
    })

    output$preview_badge <- renderUI({
      req(rv$processed); s <- rv$processed$stats
      span(class="badge bg-info", sprintf("%d行|%d地点|%d品种", s$n_rows, s$n_sites, s$n_varieties))
    })

    observeEvent(input$btn_import, {
      req(rv$processed, !rv$is_importing); rv$is_importing <- TRUE; shinyjs::disable("btn_import")
      add_log("========== 开始导入 ==========")
      tryCatch({
        df <- rv$processed$data; unresolved <- rv$processed$unresolved
        if (!is.null(unresolved) && length(unresolved) > 0) {
          add_log("正在应用性状值处理决策...")
          for (tcol in names(unresolved)) { u <- unresolved[[tcol]]
            for (v in u$values) {
              choice <- input[[paste0("res_", u$name_lib, "_", gsub("[^A-Za-z0-9]", "_", v))]]
              if (is.null(choice)) choice <- "__NA__"
              mask <- !is.na(df[[tcol]]) & as.character(df[[tcol]]) == v
              if (choice == "__NA__") { df[[tcol]][mask] <- NA; add_log(sprintf("  %s: '%s' → 空值", u$name_cn, v)) }
              else { df[[tcol]][mask] <- choice; add_log(sprintf("  %s: '%s' → %s", u$name_cn, v, choice)) }
            }
          }
        }
        bid <- generateBatchId(); tn <- if(nchar(input$trial_name)>0)input$trial_name else"未命名试验"
        mc <- rv$user_mapping; mc[is.na(mc)] <- "ignore"
        stat <- saveOtherTrialData(df, bid, input$file_excel$name, input$sheet_name, tn, input$group_label, mc)
        add_log(sprintf("%d行, %d地点, %d品种 → 成功", stat$row_count, stat$site_count, stat$variety_count))
        refresh_trigger(runif(1))
        showNotification(sprintf("导入成功: %d行, %d地点, %d品种", stat$row_count, stat$site_count, stat$variety_count), type="message", duration=3)
        add_log("可继续导入下一个试验，点击下方「继续导入」按钮")
      }, error=function(e) { add_log(sprintf("失败: %s", e$message)); showNotification(paste("失败:", e$message), type="error")
      }, finally={rv$is_importing<-FALSE; shinyjs::enable("btn_import")})
    })

    observeEvent(input$btn_new_import, { rv$processed <- NULL; showStep("step1"); add_log("-------- 准备导入下一个试验 --------") })
    observeEvent(input$btn_back_step2, showStep("step2"))
    observeEvent(input$btn_back_step1, { rv$raw_data<-NULL; rv$user_mapping<-NULL; rv$processed<-NULL; showStep("step1") })
    output$import_log <- renderText({ paste(rev(rv$import_log), collapse="\n") })

    # ====================================================================
    # 子页2: 分析
    # ====================================================================
    getBatches <- reactive({ refresh_trigger(); tryCatch(listOtherTrialBatches(), error=function(e)data.frame()) })

    observe({
      batches <- getBatches()
      if (nrow(batches)==0) { updateSelectInput(session, "selected_batch", choices=c("暂无数据"="")) }
      else {
        ch <- setNames(batches$import_batch_id, paste0(batches$trial_name," | ",batches$group_label," | ",batches$row_count,"行 | ",batches$import_time))
        cur <- isolate(input$selected_batch)
        if (!cur %in% batches$import_batch_id) cur <- batches$import_batch_id[1]
        updateSelectInput(session, "selected_batch", choices=ch, selected=cur)
      }
    })

    selectedData <- reactive({ req(input$selected_batch); getOtherTrialData(input$selected_batch) })

    output$batch_info <- renderUI({
      req(input$selected_batch); df <- selectedData()
      sites <- unique(df$place); vars <- unique(df$name); ck_vars <- unique(df$name[df$is_ck==1])
      yr <- if(all(is.na(df$MuChan))) "无" else paste0(round(range(df$MuChan, na.rm=TRUE),0), collapse=" ~ ")
      div(class="row mb-2",
        lapply(list(c("行数",nrow(df)),c("地点数",length(sites)),c("品种数",length(vars)),
          c("CK",paste(ck_vars,collapse="/")),c("亩产范围",yr),c("地点",paste(sites,collapse=", "))),
          function(x) div(class="col-md-2", tags$small(class="text-muted", x[1]), br(), tags$b(x[2]))))
    })

    output$batch_table <- DT::renderDataTable({
      df <- selectedData()
      if (is.null(df) || nrow(df)==0) return(DT::datatable(data.frame(提示="请选择试验"), options=list(dom="t"), rownames=FALSE))
      all_show <- c("stageid","name","place","MuChan","is_ck","XiaoQuChanLiang","XiaoQuShiShouMianJi","HanShuiLiang",
        "ShengYuQi","KaiHuaQi","ChengShuQi","ShouHuoQi","BoZhongQi","ChuMiaoQi","ZhuGao","DiJiaGao","FenZhiShu",
        "ZhuJingJieShu","BaiLiZhong","DanBai","ZhiFang","DanZhiHe","HuaSe","YeXing","RongMaoSe","JieJiaXiXing",
        "DaoFuXing","ShengZhangXiXing","ZhuXing","LuoYeXing","LieJiaXing","JiaXing","JiaShuSe","LiXing","ZhongPiSe",
        "QiSe","ZiYeSe","ZhongPiGuangZe","YouXiaoJia","WuXiaoJia","DanZhuJiaShu","DanZhuLiShu","DanZhuLiZhong",
        "MeiJiaLiShu","KaoZhongZhuShu","XiaoQuShouHuoZhuShu","WanHaoLiLv","PoSuiLiLv","BingLiLv","ZiBanLiLv",
        "HeBanLiLv","ShuangMeiLiLv","HuiBanLiLv","ChongShiLiLv","HuaYeBingDuBing","ShuangMeiBing","HuiBanBing",
        "XiuBing","GenFuBing","DouGanHeiQianYing","DouJiaMing","YaChong","ShiYeXingHaiChong","NaiYanXing",
        "NaiHanXing","CaoGanLinKangXing","ZaoShuaiXing","HuoGanChengShu","ZiLiPingJia","ma","pa")
      show_cols <- Filter(function(cn) any(!is.na(df[[cn]])), intersect(all_show, names(df)))
      cn_map <- c(name="品种",place="地点",MuChan="亩产(kg)",is_ck="CK",stageid="序号",
        XiaoQuChanLiang="小区产量",XiaoQuShiShouMianJi="面积",HanShuiLiang="含水量",ShengYuQi="生育期",
        KaiHuaQi="开花期",ChengShuQi="成熟期",ShouHuoQi="收获期",BoZhongQi="播种期",ChuMiaoQi="出苗期",
        ZhuGao="株高(cm)",DiJiaGao="底荚高(cm)",FenZhiShu="分枝数",ZhuJingJieShu="主茎节数",
        BaiLiZhong="百粒重(g)",DanBai="蛋白%",ZhiFang="脂肪%",DanZhiHe="蛋脂和",HuaSe="花色",YeXing="叶形",
        RongMaoSe="茸毛色",JieJiaXiXing="结荚习性",DaoFuXing="倒伏性",ShengZhangXiXing="生长习性",
        ZhuXing="株型",LuoYeXing="落叶性",LieJiaXing="裂荚性",JiaXing="荚形",JiaShuSe="荚熟色",
        LiXing="粒型",ZhongPiSe="种皮色",QiSe="脐色",ZiYeSe="子叶色",ZhongPiGuangZe="光泽",
        YouXiaoJia="有效荚",WuXiaoJia="无效荚",DanZhuJiaShu="单株荚数",DanZhuLiShu="单株粒数",
        DanZhuLiZhong="单株粒重",MeiJiaLiShu="每荚粒数",KaoZhongZhuShu="考种株数",
        XiaoQuShouHuoZhuShu="收获株数",WanHaoLiLv="完好粒率",PoSuiLiLv="破碎粒率",BingLiLv="病粒率",
        ZiBanLiLv="紫斑粒率",HeBanLiLv="褐斑粒率",ShuangMeiLiLv="霜霉粒率",HuiBanLiLv="灰斑粒率",
        ChongShiLiLv="虫蚀粒率",HuaYeBingDuBing="花叶病毒病",ShuangMeiBing="霜霉病",HuiBanBing="灰斑病",
        XiuBing="锈病",GenFuBing="根腐病",DouGanHeiQianYing="豆秆黑潜蝇",DouJiaMing="豆荚螟",YaChong="蚜虫",
        ShiYeXingHaiChong="食叶害虫",NaiYanXing="耐盐性",NaiHanXing="耐旱性",CaoGanLinKangXing="草甘膦抗性",
        ZaoShuaiXing="早衰性",HuoGanChengShu="活秆成熟",ZiLiPingJia="籽粒评价",ma="母本",pa="父本")
      display <- df[, show_cols, drop=FALSE]; names(display) <- unname(cn_map[show_cols])
      round_cols <- intersect(show_cols, c("MuChan","XiaoQuChanLiang","XiaoQuShiShouMianJi","HanShuiLiang",
        "ShengYuQi","ZhuGao","DiJiaGao","FenZhiShu","ZhuJingJieShu","BaiLiZhong","DanBai","ZhiFang",
        "DanZhiHe","YouXiaoJia","WuXiaoJia","DanZhuJiaShu","DanZhuLiShu","DanZhuLiZhong","MeiJiaLiShu",
        "KaoZhongZhuShu","XiaoQuShouHuoZhuShu","WanHaoLiLv","PoSuiLiLv","BingLiLv","ZiBanLiLv",
        "HeBanLiLv","ShuangMeiLiLv","HuiBanLiLv","ChongShiLiLv","BoZhongPenShu","BoZhongLiShu",
        "ChuMiaoShu","ChuMiaoLiShu","ZaJiaoHuaShu","ChengHuoJiaShu","ZhaJiaoliShu","HuiFuLv"))
      dt <- DT::datatable(display, filter="top", options=list(pageLength=25,
        lengthMenu=list(c(10,25,50,100,-1),c("10","25","50","100","全部")), scrollX=TRUE, scrollY="500px",
        autoWidth=TRUE, dom="lfrtip", language=list(url="//cdn.datatables.net/plug-ins/1.13.7/i18n/zh-Hans.json")),
        rownames=FALSE, class="cell-border stripe hover compact")
      for (rc in round_cols) { cn_d <- unname(cn_map[rc]); if (cn_d %in% names(display)) dt <- DT::formatRound(dt, columns=cn_d, digits=2) }
      dt
    })

    observeEvent(input$btn_analyze_batch, {
      req(input$selected_batch)

      # 预清洗 + 弹窗
      df <- getOtherTrialData(input$selected_batch)
      drop_cols <- intersect(c("id","import_batch_id","import_time","trial_name","group_label","extra_cols"), names(df))
      if(length(drop_cols)>0) df <- df[,setdiff(names(df),drop_cols),drop=FALSE]
      for(cn in names(df)) if(is.character(df[[cn]])) { df[[cn]][df[[cn]] %in% c("/","-","—")] <- NA_character_ }
      batches <- getBatches(); info <- batches[batches$import_batch_id==input$selected_batch,]
      tname <- if(nrow(info)>0) info$trial_name[1] else "其它试验"

      n_rows <- nrow(df); n_sites <- length(unique(df$place))
      showNotification(
        sprintf("正在分析 %s（%d行, %d地点），请稍候...", tname, n_rows, n_sites),
        type = "message", duration = NULL, id = "other_analysis_working")

      # 用 future/promise 做不到同步——用 showModal 占位，跑完后替换
      showModal(modalDialog(
        div(class="text-center p-5",
          tags$div(class="spinner-border text-primary mb-3", role="status",
            style="width:4rem;height:4rem;",
            tags$span(class="visually-hidden", "分析中...")),
          h5("正在执行分析管道..."),
          p(class="text-muted", sprintf("%s — %d 行, %d 地点, %d 品种",
            tname, n_rows, n_sites, length(unique(df$name)))),
          p(class="text-muted small", "产量统计 · 分布图 · 品种筛选 · GGE双标图 · 亲本分析\n请耐心等待，分析完成后自动弹出结果")
        ),
        title = NULL, size = "m", easyClose = FALSE,
        footer = NULL))

      # 延迟一小会儿让 modal 渲染出来，然后跑计算
      shinyjs::delay(100, {
        analysis <- tryCatch({
          other_analysis_show_ui(df, tname, ns, input, output)
        }, error = function(e) {
          removeModal()
          removeNotification(id = "other_analysis_working")
          showNotification(paste("分析失败:", e$message), type = "error", duration = 10)
          NULL
        })
        if (is.null(analysis)) return()
        rv$other_analysis_result <- analysis$result

        # 替换占位弹窗为结果弹窗
        removeModal()
        showModal(modalDialog(
          title = div(icon("chart-bar"), paste("数据分析 —", tname)),
          size = "xl", easyClose = TRUE, footer = modalButton("关闭"),
          do.call(tabsetPanel, c(list(id = ns("other_analysis_tabs")), unname(analysis$tabs)))))
        removeNotification(id = "other_analysis_working")
      })
    })

    output$btn_download_result <- downloadHandler(
      filename=function() { b <- getBatches(); i <- b[b$import_batch_id==isolate(input$selected_batch),]; paste0(if(nrow(i)>0) enc2utf8(as.character(i$trial_name[1])) else "export","_",format(Sys.time(),"%Y%m%d"),".xlsx") },
      content=function(file) { df <- getOtherTrialData(input$selected_batch); df <- df[,setdiff(names(df),c("id","import_batch_id","import_time","trial_name","group_label","extra_cols")),drop=FALSE]; for(cn in names(df)) if(is.numeric(df[[cn]])) df[[cn]]<-round(df[[cn]],2); openxlsx::write.xlsx(df, file, rowNames=FALSE) })

    observeEvent(input$btn_delete_batch, {
      req(input$selected_batch)
      showModal(modalDialog(title="确认删除","确定要删除该试验数据吗？不可撤销。", footer=tagList(modalButton("取消"), actionButton(ns("confirm_delete"),"确认删除", class="btn-danger")), size="s")) })
    observeEvent(input$confirm_delete, {
      req(input$selected_batch); tryCatch({ deleteOtherTrialBatch(input$selected_batch); refresh_trigger(runif(1)); removeModal(); showNotification("已删除", type="message") }, error=function(e) showNotification(paste("删除失败:",e$message), type="error")) })

    rv$other_export_path <- NULL

        output$other_do_export <- downloadHandler(
      filename = function() {
        batch_id <- isolate(input$selected_batch)
        batches <- getBatches()
        info <- batches[batches$import_batch_id == batch_id, ]
        exp_name <- if (nrow(info) > 0 && nzchar(trimws(info$trial_name[1]))) {
          trimws(as.character(info$trial_name[1]))
        } else {
          "其它试验"
        }
        exp_name <- enc2utf8(exp_name)
        exp_name <- gsub("[:*?\"<>|]", "_", exp_name)
        exp_name <- trimws(exp_name)
        if (!nzchar(exp_name)) exp_name <- "其它试验"
        paste0("其它试验分析_", exp_name, ".zip")
      },
      content = function(file) {
        req(rv$other_export_path)
        file.copy(rv$other_export_path, file)
      })
    outputOptions(output, "other_do_export", suspendWhenHidden = FALSE)

    observeEvent(input$other_export_btn, {
      req(rv$other_analysis_result)
      showModal(modalDialog(
        div(class="text-center p-5",
          tags$div(class="spinner-border text-primary mb-3", role="status",
            style="width:3rem;height:3rem;",
            tags$span(class="visually-hidden", "打包中...")),
          h5("正在生成压缩包..."),
          p(class="text-muted small", "PNG图表 + Excel + HTML + Markdown 报告")
        ),
        title = NULL, size = "s", easyClose = FALSE, footer = NULL))
      shinyjs::delay(100, {
        tmp <- tempfile(fileext = ".zip")
        tryCatch({
          build_analysis_zip(rv$other_analysis_result, tmp)
          rv$other_export_path <- tmp
          removeModal()
          session$sendCustomMessage("trigger_download", list(id = ns("other_do_export")))
        }, error = function(e) {
          removeModal()
          showNotification(paste("打包失败:", e$message), type = "error")
        })
      })
    })

    return(reactive(rv$import_log))
  })
}
