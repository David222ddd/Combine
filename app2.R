# app.R
library(shiny)
library(bs4Dash)
library(thematic)
library(waiter)
library(DT)
library(readr)
library(dplyr)
library(bregr)
library(visreg)
library(survival)

thematic_shiny()




# UI 部分
ui <- dashboardPage(
  #preloader = list(html = tagList(spin_1(), "Loading ..."), color = "#343a40"),
  #dark = TRUE,

  header = dashboardHeader(
    title = "我的 Shiny 工具",
    fixed = TRUE
  ),
  

  
  sidebar = dashboardSidebar(
    id = "tabs",         # 用于 input$tabs 监听当前选中的菜单
    skin = "light",
    status = "primary",
    sidebarMenu(
      menuItem("Home 主页",         tabName = "home",         icon = icon("home")),
      menuItem("About 关于页",      tabName = "about",        icon = icon("info-circle")),
      menuItem("Help 帮助页",      tabName = "help",         icon = icon("question-circle")),
      menuItem("描述性统计",        tabName = "descriptive",  icon = icon("chart-bar")),
      menuItem("定性资料分析",      tabName = "qualitative",  icon = icon("clipboard-list")),
      menuItem("定量资料分析",      tabName = "quantitative", icon = icon("calculator")),
      menuItem("相关性分析",        tabName = "correlation",  icon = icon("project-diagram")),
      menuItem("一致性检验",        tabName = "consistency",  icon = icon("check-circle")),
      menuItem("回归分析",          tabName = "regression",   icon = icon("chart-line")),
      menuItem("生存数据分析",      tabName = "survival",    icon = icon("hourglass"))
    )
  ),
  controlbar = dashboardControlbar(
    skinSelector(),    # ← 关键：插入皮肤选择器组件 :contentReference[oaicite:0]{index=0}
    skin = "light",
    pinned = TRUE
  ),
  footer = dashboardFooter(
    fixed = FALSE,
    left = "© 2025",
    right = "Powered by bs4Dash"
  ),
  body = dashboardBody(
    tabItems(
      ## —— UI：Home 模块 —— ##
      tabItem(
        tabName = "home",
        fluidRow(
          # 欢迎卡片
          bs4Card(
            title = tagList(icon("smile"), "欢迎使用我的 Shiny 工具"),
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            p("在左侧菜单中选择对应的功能模块，或点击下方按钮快速跳转："),
            fluidRow(
              column(3, actionButton("go_descriptive",  "描述性统计",        icon = icon("chart-bar"),       class = "btn-block btn-outline-primary")),
              column(3, actionButton("go_qualitative", "定性资料分析",      icon = icon("clipboard-list"), class = "btn-block btn-outline-success")),
              column(3, actionButton("go_quantitative","定量资料分析",      icon = icon("calculator"),     class = "btn-block btn-outline-info")),
              column(3, actionButton("go_correlation","相关性分析",        icon = icon("project-diagram"),class = "btn-block btn-outline-warning"))
            )
          )
        ),
        fluidRow(
          # 四个关键指标示例
          valueBoxOutput("vb_total_obs", width = 3),
          valueBoxOutput("vb_num_vars",  width = 3),
          valueBoxOutput("vb_last_update",width = 3),
          valueBoxOutput("vb_version",    width = 3)
        ),
        fluidRow(
          # 可以放置一个欢迎图或 logo
          bs4Card(
            width = 12,
            img(src = "www/logo.png", height = "200px"),
            footer = "© 2025 我的团队"
          )
        )
      ),
      # 2. About
      tabItem(
        tabName = "about",
        h2("关于我们"),
        p("在此描述项目背景、团队或联系方式。")
      ),
      # 3. Help
      tabItem(
        tabName = "help",
        h2("使用帮助"),
        p("FAQ、使用说明或示例。")
      ),
      # 4. 描述性统计
      tabItem(
        tabName = "descriptive",
        h2("描述性统计"),
        # 这里可以放 valueBox、plotOutput、dataTableOutput 等
        fluidRow(
          valueBoxOutput("vb_mean"),
          valueBoxOutput("vb_median")
        ),
        plotOutput("plot_descriptive")
      ),
      # 5. 定性资料分析
      tabItem(
        tabName = "qualitative",
        h2("定性资料分析"),
        # 例如 wordcloud2Output、DT::dataTableOutput 等
        dataTableOutput("tbl_qualitative")
      ),
      # 6. 定量资料分析
      tabItem(
        tabName = "quantitative",
        h2("定量资料分析"),
        plotOutput("plot_quantitative"),
        verbatimTextOutput("summary_quantitative")
      ),
      # 7. 相关性分析
      tabItem(
        tabName = "correlation",
        h2("相关性分析"),
        plotOutput("plot_corr"),
        tableOutput("tbl_corr")
      ),
      # 8. 一致性检验
      tabItem(
        tabName = "consistency",
        h2("一致性检验"),
        verbatimTextOutput("test_consistency")
      ),
        # 9. 回归分析
        tabItem(
          tabName = "regression",
          h2("回归分析"),
          plotOutput("plot_regression"),
          verbatimTextOutput("summary_regression")
        ),
        # 10. 生存数据分析
        tabItem(
          tabName = "survival",
          fluidRow(
            column(
              width = 4,
              bs4Card(
                title = "参数设置",
                width = 12,
                fileInput("file", "上传 CSV/TSV 数据", accept = c(".csv", ".tsv", ".txt")),
                checkboxInput("use_example", "使用内置示例", value = TRUE),
                helpText("示例数据仅用于演示："),
                helpText("coxph, survreg, cch -> survival::lung;"),
                helpText("clogit -> survival::infert;"),
                helpText("gamma, inverse.gaussian -> datasets::ChickWeight"),
                helpText("nls -> datasets::Puromycin"),
                helpText("aov -> datasets::PlantGrowth"),
                helpText("其他 -> mtcars"),
                hr(),
                uiOutput("method_ui"),
                uiOutput("y_block_ui"),
                uiOutput("x_ui"),
                uiOutput("x2_ui"),
                uiOutput("group_ui"),
                uiOutput("strata_ui"),
                uiOutput("xvar_ui"),
                numericInput("n_workers", "并行工作线程 n_workers", value = 1, min = 1, step = 1),
                actionButton("run", "开始建模", class = "btn-primary"),
                hr(),
                downloadButton("download_csv", "下载 Tidy 结果（CSV）")
              )
            ),
            column(
              width = 8,
              bs4Card(
                title = "结果输出",
                width = 12,
                tabsetPanel(
                  id = "br_tabs",
                  tabPanel("数据概览", DTOutput("head")),
                  tabPanel("Tidy 结果表", DTOutput("tbl")),
                  tabPanel("发表型森林图", plotOutput("forest", height = 520)),
                  tabPanel("环形森林图", plotOutput("forest_circle", height = 520)),
                  tabPanel("拟合线（visreg）", plotOutput("fitline", height = 520)),
                  tabPanel("残差诊断", plotOutput("residuals", height = 520)),
                  tabPanel("风险网络", plotOutput("risk_net", height = 620)),
                  tabPanel("列线图（Nomogram）", plotOutput("nomogram", height = 620)),
                  tabPanel(
                    "生存曲线（Cox）",
                    conditionalPanel("input.method == 'coxph'", plotOutput("surv", height = 520)),
                    conditionalPanel("input.method != 'coxph'",
                      div(style = \"padding:1rem;color:#666;\",
                          \"提示：将方法切换为 CoxPH 才会绘制生存曲线。\")
                    )
                  ),
                  tabPanel(
                    "Cox 诊断",
                    conditionalPanel("input.method == 'coxph'", plotOutput("cox_diag", height = 520)),
                    conditionalPanel("input.method != 'coxph'",
                      div(style = \"padding:1rem;color:#666;\",
                          \"提示：将方法切换为 CoxPH 才会显示诊断图。\")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )  # end ui

# Server 部分
server <- function(input, output, session) {
  # 描述性统计示例
  data <- mtcars
  output$vb_mean <- renderValueBox({
    valueBox(
      value = round(mean(data$mpg), 2),
      subtitle = "平均 MPG",
      icon = icon("tachometer-alt")
    )
  })
  output$vb_median <- renderValueBox({
    valueBox(
      value = median(data$mpg),
      subtitle = "中位数 MPG",
      icon = icon("tachometer-alt")
    )
  })
  output$plot_descriptive <- renderPlot({
    hist(data$mpg, main = "MPG 分布", xlab = "MPG")
  })
  
  # 后续模块的 server stub（做类似处理）
  output$tbl_qualitative <- renderDataTable({
    # placeholder
    data.frame(类别 = c("A", "B"), 频次 = c(10, 15))
  })
  
  output$plot_quantitative <- renderPlot({
    plot(data$wt, data$mpg, main = "重量 vs MPG",
         xlab = "重量", ylab = "MPG")
  })
  output$summary_quantitative <- renderPrint({
    summary(data)
  })
  
  output$plot_corr <- renderPlot({
    corr <- cor(data)
    corrplot::corrplot(corr, method = "circle")
  })
  output$tbl_corr <- renderTable({
    cor(data)
  })
  
  output$test_consistency <- renderPrint({
    # 这里可以调用例如 Cronbach's alpha 等
    paste("一致性检验结果（示例）")
  })
  
  output$plot_regression <- renderPlot({
    fit <- lm(mpg ~ wt + cyl, data = data)
    plot(fit, which = 1)
  })
  output$summary_regression <- renderPrint({
    summary(lm(mpg ~ wt + cyl, data = data))
  })

  # ---- 生存数据分析 (回归模块) ----
  # 1) 数据
  dataset <- reactive({
    if (!is.null(input$file)) {
      ext <- tolower(tools::file_ext(input$file$name))
      if (ext %in% c("csv","txt")) readr::read_csv(input$file$datapath, guess_max = 10000)
      else if (ext == "tsv") readr::read_tsv(input$file$datapath, guess_max = 10000)
      else validate("仅支持 .csv/.tsv/.txt")
    } else if (isTRUE(input$use_example)) {
      if (!is.null(input$method)) {
        if (input$method %in% c("coxph", "survreg", "cch")) {
          if (!requireNamespace("survival", quietly = TRUE))
            validate("请先安装 survival 包：install.packages('survival')")
          d <- survival::lung
          d$status <- ifelse(d$status == 2, 1L, 0L)
          d$sex <- factor(d$sex, levels = c(1,2), labels = c("Male","Female"))
          d$ph.ecog <- factor(d$ph.ecog)
          d
        } else if (identical(input$method, "clogit")) {
          if (!requireNamespace("survival", quietly = TRUE))
            validate("请先安装 survival 包：install.packages('survival')")
          data("infert", package = "survival")
          d <- infert
          d$status <- as.integer(d$case)
          d$time <- 1
          d
        } else if (input$method %in% c("gamma", "inverse.gaussian")) {
          datasets::ChickWeight
        } else if (identical(input$method, "nls")) {
          datasets::Puromycin
        } else if (identical(input$method, "aov")) {
          datasets::PlantGrowth
        } else {
          mtcars
        }
      } else {
        mtcars
      }
    } else {
      validate("请上传数据或勾选示例数据")
    }
  })

  output$head <- renderDT({
    req(dataset())
    datatable(head(dataset(), 10), options = list(pageLength = 10), rownames = FALSE)
  })

  # 2) 方法
  output$method_ui <- renderUI({
    sel <- tryCatch(br_avail_methods(), error = function(e) c("gaussian","binomial","poisson","coxph"))
    method_desc <- c(
      gaussian = "连续因变量线性回归",
      binomial = "0/1 因变量逻辑回归",
      poisson  = "计数数据泊松回归",
      coxph    = "生存数据 Cox 比例风险模型",
      survreg  = "生存数据参数回归",
      clogit   = "匹配病例对照条件逻辑回归",
      cch      = "病例-队列设计生存模型"
    )
    desc <- method_desc[names(method_desc) %in% sel]
    help <- paste(names(desc), desc, sep = "：", collapse = "； ")
    tagList(
      selectInput("method", "建模方法", choices = sel, selected = sel[1]),
      helpText(help)
    )
  })

  # 3) 动态 UI
  observe({
    req(dataset())
    cols <- names(dataset())

    if (!is.null(input$method) && input$method %in% c("coxph","survreg","clogit","cch")) {
      output$y_block_ui <- renderUI({
        tagList(
          selectInput("y_time", "生存时间列（time）", choices = cols),
          selectInput("y_status", "结局状态列（0=截尾, 1=事件）", choices = cols)
        )
      })
      if (identical(input$method, "clogit")) {
        output$strata_ui <- renderUI({
          selectInput("y_strata", "匹配分层变量（strata）", choices = cols)
        })
      } else {
        output$strata_ui <- renderUI({ NULL })
      }
    } else {
      output$y_block_ui <- renderUI({
        selectInput("y", "因变量 Y", choices = cols)
      })
      output$strata_ui <- renderUI({ NULL })
    }

    output$x_ui <- renderUI({
      selectizeInput("x", "焦点自变量 X（批量）", choices = cols, multiple = TRUE)
    })
    output$x2_ui <- renderUI({
      selectizeInput("x2", "控制变量（可选）", choices = cols, multiple = TRUE)
    })
    output$group_ui <- renderUI({
      selectInput("group_by", "组变量（可选）", choices = c("无"="__none__", cols), selected = "__none__")
    })
    output$xvar_ui <- renderUI({
      selectInput("xvar", "拟合线横轴变量（visreg）", choices = cols)
    })
  })

  # 4) 建模
  breg_obj <- reactiveVal(NULL)

  observeEvent(input$run, {
    req(dataset(), input$method)
    dat <- dataset()
    group_by <- if (!is.null(input$group_by) && input$group_by != "__none__") input$group_by else NULL

    y_vec <- if (!is.null(input$method) && input$method %in% c("coxph","survreg","clogit","cch")) {
      req(input$y_time, input$y_status)
      st <- dat[[input$y_status]]
      if (!all(na.omit(unique(st)) %in% c(0,1)))
        showNotification("提醒：生存状态列应为 0/1（0=截尾,1=事件）。", type = "warning")
      if (identical(input$method, "clogit")) {
        req(input$y_strata)
        list(c(input$y_time, input$y_status), strata = input$y_strata)
      } else {
        c(input$y_time, input$y_status)
      }
    } else {
      req(input$y); input$y
    }

    validate(need(length(input$x) >= 1, "请至少选择一个自变量 X"))

    obj <- tryCatch({
      br_pipeline(
        data      = dat,
        y         = y_vec,
        x         = input$x,
        x2        = input$x2,
        method    = input$method,
        group_by  = group_by,
        n_workers = input$n_workers
      )
    }, error = function(e) {
      showNotification(paste("建模失败：", e$message), type = "error"); NULL
    })

    breg_obj(obj)
    if (!is.null(obj)) updateTabsetPanel(session, "br_tabs", selected = "Tidy 结果表")
  })

  # 5) 结果表 & 下载
  output$tbl <- renderDT({
    req(breg_obj())
    res <- br_get_results(breg_obj(), tidy = TRUE)
    datatable(res, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("bregr_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      req(breg_obj()); write.csv(br_get_results(breg_obj(), tidy = TRUE), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # 6) 图形
  output$forest <- renderPlot({ req(breg_obj()); br_show_forest(breg_obj()) })
  output$forest_circle <- renderPlot({ req(breg_obj()); br_show_forest_circle(breg_obj()) })

  output$fitline <- renderPlot({
    req(breg_obj(), input$xvar)
    if (!requireNamespace("visreg", quietly = TRUE)) {
      plot.new(); text(0.5, 0.5, "请安装 visreg 包以显示拟合线：install.packages('visreg')")
    } else {
      fit <- if (exists("br_get_model")) br_get_model(breg_obj()) else breg_obj()
      visreg::visreg(fit, xvar = input$xvar, data = dataset())
    }
  })

  output$residuals <- renderPlot({ req(breg_obj()); br_show_residuals(breg_obj()) })
  output$risk_net  <- renderPlot({ req(breg_obj()); br_show_risk_network(breg_obj()) })

  output$nomogram <- renderPlot({
    req(breg_obj())
    tryCatch({
      br_show_nomogram(breg_obj())
    }, error = function(e) {
      plot.new(); text(0.5, 0.5, paste("无法生成列线图：", e$message))
    })
  })

  output$surv <- renderPlot({
    req(breg_obj(), identical(input$method, "coxph"))
    br_show_survival_curves(breg_obj())
  })

  output$cox_diag <- renderPlot({
    req(breg_obj(), identical(input$method, "coxph"))
    br_show_coxph_diagnostics(breg_obj())
  })
}

# 启动 App
shinyApp(ui, server)




#ui <- fluidPage(
 # tags$iframe(
  #  src = "index.html",  # 这里的 index.html 应该位于 www 文件夹中
  #  style = "width:100%; height:100vh; border:none;"
  #)
#)

#server <- function(input, output, session) { }

#shinyApp(ui, server)
