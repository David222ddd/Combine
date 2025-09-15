# app.R ----

# 可选：TSHRC (Two-stage), KMsurv (寿命表), riskRegression (扩展评估), gtsummary (排版)

# ========== 0) 加载包与工具函数 ----------
pkgs <- c("shiny","bslib","thematic","survival","survminer","cmprsk","flexsurv","broom","dplyr","ggplot2","DT")
sapply(pkgs, function(p) if (!requireNamespace(p, quietly = TRUE)) message("建议安装：", p))

library(shiny)
library(bslib)
library(thematic)
library(survival)
library(survminer)
library(cmprsk)
library(flexsurv)
library(broom)
library(dplyr)
library(ggplot2)
library(DT)
library(TSHRC)
library(KMsurv)

pkg_avail <- function(p) requireNamespace(p, quietly = TRUE)

# 生成一个简单的竞争风险模拟数据（事件码：1=感兴趣事件；2=竞争事件；0=删失）
sim_cr_data <- function(n = 300, seed = 123){
  set.seed(seed)
  x <- rbinom(n, 1, 0.5)              # 分组/二值自变量
  t1 <- rexp(n, rate = 0.08 * (1 + 0.7*x))  # 事件1
  t2 <- rexp(n, rate = 0.06 * (1 + 0.2*x))  # 竞争事件
  c  <- rexp(n, rate = 0.03)               # 删失
  ftime   <- pmin(t1, t2, c)
  fstatus <- ifelse(ftime == t1, 1, ifelse(ftime == t2, 2, 0))
  data.frame(time = ftime, status = fstatus, group = factor(x, labels=c("A","B")))
}

# 统一构建 Surv 对象的帮助函数（status_event_code 为“终点事件”的编码）
make_Surv <- function(dat, time_col, status_col, status_event_code = 1){
  time <- dat[[time_col]]
  status_raw <- dat[[status_col]]
  # 二元事件：将感兴趣事件映射为1，其它（包含竞争事件）映射为0（KM/Cox 情景）
  status <- as.integer(status_raw == status_event_code)
  Surv(time, status)
}

# ========== 1) UI ----------
ui <- page_navbar(
  title = "生存数据分析",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  sidebar = sidebar(
    width = 320,
    h5("① 数据与变量"),
    radioButtons("data_src", "数据来源", inline = FALSE,
                 choices = c("上传CSV/TSV"="upload",
                             "示例：survival::lung（Cox/KM）"="lung",
                             "示例：模拟竞争风险（CIF/Fine-Gray）"="simcr")),
    conditionalPanel(
      "input.data_src == 'upload'",
      fileInput("file", "选择文件", accept = c(".csv",".tsv",".txt")),
      radioButtons("sep", "分隔符", choices = c(逗号="," , 制表符="\\t", 自动="auto"), inline = TRUE)
    ),
    uiOutput("var_map_ui"),
    hr(),
    h5("② 方法与下载"),
    selectInput("scenario", "分析场景/方法", width = "100%",
                choices = list(
                  "单因素 - KM + Log-rank" = "km",
                  "单因素 - 交叉曲线：Breslow / Two-stage*" = "cross",
                  "单因素 - 竞争风险：Gray + Fine-Gray" = "cr_one",
                  "寿命表法*" = "lifetable",
                  "多因素 - Cox (PH)" = "cox",
                  "多因素 - 竞争风险：Fine-Gray" = "cr_multi",
                  "多因素 - Weibull (AFT)" = "weibull"
                )),
    conditionalPanel("input.scenario == 'km' || input.scenario == 'cross'",
                     checkboxInput("show_risk_table","KM图显示 at-risk 表", TRUE),
                     checkboxInput("show_ci","KM图显示置信区间", TRUE)),
    conditionalPanel("input.scenario == 'cox'",
                     checkboxInput("show_ph_diag","显示PH检验图 (cox.zph)", TRUE)),
    hr(),
    downloadButton("download_tbl", "下载结果表（CSV）"),
    downloadButton("download_plot", "下载图（PNG）")
  ),
  nav_panel(
    "结果",
    layout_columns(
      col_widths = c(6,6),
      card(full_screen = TRUE, card_header("图形输出"),
           plotOutput("main_plot", height = "480px")),
      card(card_header("结果表/模型摘要"),
           DTOutput("main_table"))
    )
  ),
  nav_panel(
    "说明",
    card(
      p("这是一个用于生存分析的网页工具，已包含全部主要方法。"),
      tags$ul(
        tags$li("按左侧步骤：选择数据 → 选择变量 → 选择方法。"),
        tags$li("竞争风险：status 需包含 0（删失）、1（终点事件）、2（竞争事件）等编码。"),
        tags$li("寿命表法仍待完善")
      )
    )
  )
)

# ========== 2) Server ----------
server <- function(input, output, session){
  
  thematic::thematic_shiny()
  
  # ---- 2.1 数据读取与示例数据 ----
  dat_raw <- reactive({
    src <- input$data_src
    req(src)
    if (src == "lung"){
      df <- survival::lung %>%
        dplyr::mutate(status = ifelse(status == 2, 1, 0)) # 2=death → 1
      attr(df, "._note") <- "示例：survival::lung，status=1 表示死亡事件"
      return(df)
    } else if (src == "simcr"){
      df <- sim_cr_data()
      attr(df, "._note") <- "示例：模拟竞争风险数据，status: 1=感兴趣事件, 2=竞争事件, 0=删失"
      return(df)
    } else if (src == "upload"){
      f <- input$file
      req(f)
      sep <- switch(input$sep, ","=",", "\\t"="\t", "auto"=NULL)
      df <- tryCatch({
        if (is.null(sep)) {
          utils::read.table(f$datapath, header = TRUE, sep = "", check.names = FALSE)
        } else {
          utils::read.table(f$datapath, header = TRUE, sep = sep, check.names = FALSE)
        }
      }, error = function(e) { validate(need(FALSE, paste("读取失败：", e$message))); NULL })
      return(df)
    }
    NULL
  })
  
  # ---- 2.2 动态变量映射 UI ----
  output$var_map_ui <- renderUI({
    df <- dat_raw(); req(df)
    nms <- names(df)
    tagList(
      selectInput("time_var","时间变量（必选）", choices = nms),
      selectInput("status_var","结局/状态变量（必选）", choices = nms),
      selectInput("group_var","分组/单个自变量（可选）", choices = c("<无>"="", nms)),
      selectizeInput("covars","多因素协变量（可多选）", choices = nms, multiple = TRUE),
      numericInput("event_code","终点事件编码（用于KM/Cox/Weibull 等二分类分析）", value = 1, step = 1),
      numericInput("cr_event_code","竞争风险：感兴趣事件编码", value = 1, step = 1)
    )
  })
  
  # ---- 2.3 核心分析反应式 ----
  analysis_result <- reactive({
    df <- dat_raw(); req(df, input$time_var, input$status_var, input$scenario)
    time_col   <- input$time_var
    status_col <- input$status_var
    grp <- input$group_var
    covars <- input$covars
    
    scenario <- input$scenario
    res <- list(type = scenario, plot = NULL, table = NULL, note = attr(df, "._note"))
    
    # 通用：数据/变量基本校验
    validate(need(is.numeric(df[[time_col]]), "时间变量需为数值"))
    validate(need(all(!is.na(df[[time_col]])), "时间变量存在缺失，请处理"))
    validate(need(all(!is.na(df[[status_col]])), "状态变量存在缺失，请处理"))
    
    # -------- A) 单因素 - KM + Log-rank --------
    if (scenario == "km") {
      
      ## 0. 工具函数 ----
      .fmt_p <- function(p) {
        if (is.na(p)) return(NULL)
        paste0("Log-rank p = ", format.pval(p, digits = 3, eps = 1e-4))
      }
      
      ## 1. 整理数据：统一列名 time/event/group ----
      km_data <- reactive({
        req(input$time_var, input$status_var)
        
        df <- dat_raw()
        
        # 去掉时间/状态缺失（注意这里都用 [[ ]]）
        df <- df %>%
          dplyr::filter(!is.na(.data[[ input$time_var ]]),
                        !is.na(.data[[ input$status_var ]]))
        
        # 固定列名：time / event / group
        df$time  <- df[[ input$time_var ]]
        df$event <- as.integer(df[[ input$status_var ]] == input$event_code)
        
        if (nzchar(input$group_var) && input$group_var %in% names(df)) {
          df$group <- factor(df[[ input$group_var ]])
        } else {
          df$group <- factor("All")
        }
        
        # 清理不可用行
        df <- df[!is.na(df$time) & !is.na(df$event) & !is.na(df$group), , drop = FALSE]
        df
      })
      
      ## 2. 拟合 KM（固定公式，无动态列名）----
      fit <- reactive({
        df <- km_data()
        survival::survfit(survival::Surv(time, event) ~ group, data = df)
      })
      
      ## 3. 计算 log-rank p（我们自己算，传给 ggsurvplot 的 pval 字符串）----
      pval_label <- reactive({
        df <- km_data()
        if (nlevels(df$group) <= 1L) return(NULL)  # 单组不显示
        
        sdiff <- survival::survdiff(survival::Surv(time, event) ~ group, data = df)
        # 自由度 = 组数 - 1
        p <- stats::pchisq(sdiff$chisq, df = length(sdiff$n) - 1L, lower.tail = FALSE)
        .fmt_p(p)
      })
      
      ## 4. 绘图与表格（注意：pval 传字符串或 NULL，而不是 TRUE）----
      res$plot <- survminer::ggsurvplot(
        fit(),
        data         = km_data(),                    # 给风险表等用
        conf.int     = isTRUE(input$show_ci),
        risk.table   = isTRUE(input$show_risk_table),
        risk.table.height  = 0.25,      # at-risk 表占图的相对高度
        risk.table.y.text  = FALSE,     # 不重复显示分组标签
        pval         = pval_label(),                 # 关键：避免 survminer 内部再 eval
        legend.title = if (nzchar(input$group_var)) input$group_var else "All",
        ggtheme      = ggplot2::theme_minimal()
      )$plot
      
      res$table <- broom::tidy(fit())
      
      return(res)
    }
    
    
    
    
    
    # -------- B) 单因素 - 交叉曲线：Breslow / Two-stage --------
    if (scenario == "cross") {
      
      ## 0. 工具 —— P 值格式化 ----
      .fmt_p <- function(p) {
        if (is.na(p) || is.null(p)) return(NULL)
        paste0("Breslow p = ", format.pval(p, digits = 3, eps = 1e-4))
      }
      
      ## 1. 整理数据：统一列名 time / event / group ----
      cross_data <- reactive({
        df2 <- df |>
          dplyr::filter(!is.na(.data[[ time_col ]]),
                        !is.na(.data[[ status_col ]]),
                        !is.na(.data[[ grp ]])) |>
          dplyr::mutate(
            time  = .data[[ time_col ]],
            event = as.integer(.data[[ status_col ]] == input$event_code),
            group = factor(.data[[ grp ]])
          )
        validate(need(nlevels(df2$group) > 1, "分组变量需 ≥2 组才能做交叉曲线检验"))
        df2
      })
      
      ## 2. 统计检验 ----
      bres_p <- reactive({
        sdiff <- survdiff(Surv(time, event) ~ group,
                          data = cross_data(), rho = 1)
        stats::pchisq(sdiff$chisq,
                      df = length(sdiff$n) - 1, lower.tail = FALSE)
      })
      
      ts_p <- reactive({
        df2 <- cross_data()
        # TSHRC 要求 group 为 0/1 数值向量
        g_num <- as.numeric(df2$group) - min(as.numeric(df2$group))
        
        tryCatch({
          p_vec <- TSHRC::twostage(
            time   = df2$time,
            delta  = df2$event,   # 已是 0 = 删失, 1 = 事件
            group  = g_num,       # 0/1
            nboot  = 1000         # 自定，也可做成 UI 选项
          )
          p_vec["TSPV"]           # 取整体两阶段检验的 p 值
        }, error = function(e) NA_real_)
      })
      
      ## 3. 拟合 KM 并绘图 ----
      fit <- reactive(
        survfit(Surv(time, event) ~ group, data = cross_data())
      )
      
      g <- survminer::ggsurvplot(
        fit(),
        data               = cross_data(),
        conf.int           = isTRUE(input$show_ci),
        risk.table         = isTRUE(input$show_risk_table),
        risk.table.height  = 0.25,
        risk.table.y.text  = FALSE,
        pval               = .fmt_p(bres_p()),
        legend.title       = grp,
        ggtheme            = ggplot2::theme_minimal()
      )
      
      ## 4. 输出 ----
      res$plot  <- g$plot
      res$table <- tibble::tibble(
        method = c("Breslow (rho = 1)", "Two-stage"),
        p_value = c(bres_p(), ts_p()),
        note = c(
          "",
          ifelse(is.na(ts_p()), "需要安装 TSHRC 包或计算失败", "")
        )
      )
      return(res)
    }
    
    
    # -------- C) 单因素 - 竞争风险：Gray + Fine-Gray --------
    if (scenario == "cr_one"){
      validate(need(nzchar(grp), "Gray 检验需要分组变量"))
      ftime   <- df[[time_col]]
      fstatus <- df[[status_col]]
      event_code <- input$cr_event_code
      
      # cuminc + Gray
      ci <- tryCatch(cmprsk::cuminc(ftime, fstatus, group = df[[grp]], cencode = 0), error = function(e) NULL)
      
      # 图：优先使用 survminer::ggcompetingrisks（若可用）
      if (pkg_avail("survminer") && !is.null(ci)){
        res$plot <- survminer::ggcompetingrisks(ci) + theme_minimal()
      } else if (!is.null(ci)) {
        # 简单基础图
        graphics::plot(ci, main = "Cumulative Incidence (Gray)", xlab = "Time", ylab = "CIF")
      }
      
      # Gray P 值
      gray_p <- tryCatch({
        # cmprsk::cuminc 的 $Tests 包含 Gray 的统计量/ P 值
        test_tbl <- as.data.frame(ci$Tests)
        pf <- if ("pv" %in% names(test_tbl)) test_tbl$pv[1] else NA
        pf
      }, error = function(e) NA)
      
      # Fine-Gray 单因素（group 作为回归变量）
      fg <- tryCatch(cmprsk::crr(ftime, fstatus, cov1 = model.matrix(~ df[[grp]])[, -1, drop = FALSE],
                                 failcode = event_code, cencode = 0), error = function(e) NULL)
      coef_tbl <- if (!is.null(fg)) broom::tidy(fg) else NULL
      
      res$table <- tibble::tibble(method = c("Gray (CIF)","Fine-Gray 回归"),
                                  p_value = c(gray_p, if (!is.null(coef_tbl)) coef_tbl$p.value[1] else NA))
      return(res)
    }
    
    # -------- D) 寿命表法 --------
    if (scenario == "lifetable"){
      if (!pkg_avail("KMsurv")){
        res$table <- tibble::tibble(msg = "寿命表法需要安装 KMsurv 包：install.packages('KMsurv')")
        return(res)
      } else {
        time     <- df[[time_col]]
        status01 <- as.integer(df[[status_col]] == input$event_code)
        # 这里做一个等宽分组（也可提供 UI 设定区间）
        brks <- pretty(range(time, na.rm = TRUE), n = 10)
        lt <- KMsurv::lifetab(brks, time, status01)
        res$table <- as.data.frame(lt) %>% tibble::rownames_to_column("interval")
        return(res)
      }
    }
    
    # -------- E) 多因素 - Cox --------
    if (scenario == "cox"){
      validate(need(length(covars) >= 1 || nzchar(grp), "请至少选择一个协变量或分组变量"))
      # 构造公式：Surv(time, status==event_code) ~ covars (+ group)
      rhs <- c(covars, if (nzchar(grp)) grp else NULL)
      form <- as.formula(paste0("Surv(", time_col, ", ", status_col, "==", input$event_code, ") ~ ",
                                paste(rhs, collapse = " + ")))
      fit <- coxph(form, data = df, x = TRUE, y = TRUE)
      tt  <- broom::tidy(fit, exponentiate = TRUE, conf.int = TRUE)
      res$table <- tt
      
      # 可选：PH 诊断图
      if (isTRUE(input$show_ph_diag)){
        zph <- tryCatch(cox.zph(fit), error = function(e) NULL)
        if (!is.null(zph)){
          # 将 zph 图存为 ggplot 对象不直观，这里直接画到当前设备并捕获
          # 模板化：返回一个简单的系数森林图替代，供骨架展示
          res$plot <- survminer::ggforest(fit, data = df)
        } else {
          res$plot <- NULL
        }
      }
      return(res)
    }
    
    # -------- F) 多因素 - 竞争风险：Fine-Gray --------
    if (scenario == "cr_multi"){
      validate(need(length(covars) >= 1 || nzchar(grp), "请至少选择一个协变量或分组变量"))
      ftime   <- df[[time_col]]
      fstatus <- df[[status_col]]
      rhs <- c(covars, if (nzchar(grp)) grp else NULL)
      X <- model.matrix(as.formula(paste("~", paste(rhs, collapse = " + "))), data = df)[, -1, drop = FALSE]
      fg <- cmprsk::crr(ftime, fstatus, cov1 = X, failcode = input$cr_event_code, cencode = 0)
      res$table <- broom::tidy(fg)
      # TODO: 可添加基于 riskRegression::Score 的评估/校准
      res$plot <- NULL
      return(res)
    }
    
    # -------- G) 多因素 - Weibull (AFT) --------
    if (scenario == "weibull"){
      validate(need(length(covars) >= 1 || nzchar(grp), "请至少选择一个协变量或分组变量"))
      rhs <- c(covars, if (nzchar(grp)) grp else NULL)
      form <- as.formula(paste0("Surv(", time_col, ", ", status_col, "==", input$event_code, ") ~ ",
                                paste(rhs, collapse = " + ")))
      fit <- survival::survreg(form, data = df, dist = "weibull")
      # AFT 系数（对数时间比）；可根据需要转换为“近似 HR”
      tt <- broom::tidy(fit, conf.int = TRUE)
      res$table <- tt
      # 简单预测曲线（单一协变量时可视化更直观；此处留空骨架）
      res$plot <- NULL
      return(res)
    }
    
    res
  })
  
  # ---- 2.4 输出：图与表 ----
  output$main_plot <- renderPlot({
    ar <- analysis_result()
    validate(need(!is.null(ar), "尚无可绘制结果"))
    if (inherits(ar$plot, "ggplot")) {
      print(ar$plot)
    } else if (is.null(ar$plot)) {
      plot.new(); title("（本方法当前未提供图形输出或条件不足）")
    }
  })
  
  output$main_table <- renderDT({
    ar <- analysis_result(); req(ar)
    tbl <- ar$table
    if (is.null(tbl)) tbl <- data.frame(消息="暂无结果")
    datatable(tbl, rownames = FALSE, options = list(scrollX = TRUE, pageLength = 10))
  })
  
  # ---- 2.5 下载 ----
  output$download_tbl <- downloadHandler(
    filename = function(){ paste0("results_", input$scenario, ".csv") },
    content = function(file){
      ar <- analysis_result()
      utils::write.csv(ar$table, file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  output$download_plot <- downloadHandler(
    filename = function(){ paste0("plot_", input$scenario, ".png") },
    content = function(file){
      ar <- analysis_result()
      png(file, width = 1200, height = 900, res = 144)
      if (inherits(ar$plot, "ggplot")) print(ar$plot) else { plot.new(); title("无图可导出") }
      dev.off()
    }
  )
}

# ========== 3) Run ----------
shinyApp(ui, server)
