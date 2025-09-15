# app.R
library(shiny)
library(bs4Dash)
library(thematic)
library(waiter)

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
          h2("生存数据分析"),
          p("此处将展示生存数据分析相关内容。")
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
