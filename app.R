library(shiny)
library(tidyverse)
library(shinythemes)
library(shinyjs)
library(ggdark)
library(ggthemes)
library(shinydashboard)
library(DT)

a = "#004AAD"
b = "#C0C1C5"

shinyjs::useShinyjs()

ui <- dashboardPage(
  skin="purple",
  dashboardHeader(title = "圖表生成器",titleWidth = 200),
  dashboardSidebar(
    width=300,
    sidebarMenu(
      menuItem("介紹",tabName="介紹1"),
      menuItem("輸入你的數據", tabName = "inputData", icon = icon("database")),
      menuItem("繪製你的圖表", tabName = "drawChart", icon = icon("chart-line")),
      menuItem("下載你的圖表", tabName = "downloadChart", icon = icon("download"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "介紹1",
              div(includeMarkdown("uuu.Rmd"))),
      tabItem(tabName = "inputData",
              box(
                title="上傳csv檔案",width =4,
                br(),
                fileInput("data", "請提供你的數據", accept = ".csv")),
              box(
                title="查看你的表格",width =8,
                DT::dataTableOutput("output")
              )
      ),
      tabItem(tabName = "drawChart",
              box(
                width=4,
                title = "控制圖表", status = "warning", solidHeader = TRUE, collapsible = TRUE,
                selectInput("y", "選擇Ｙ軸", choices = names(data)),
                selectInput("x", "選擇Ｘ軸", choices = names(data)),
                selectInput("geom", "選擇圖表類型", c("點子圖", "直線圖")),
                textInput("title", "輸入標題"),
                selectInput("theme1", "選擇主題", c("白" = "白", "黑" = "黑", "藍" = "藍")),
                numericInput("文字大小", "文字大小", value = 17, min = 1, max = 50)
              ),
              box(
                width=8,
                title = "圖表", status ="success", solidHeader = TRUE, collapsible = TRUE,
                plotOutput("plot")
              )
      ),
      tabItem(tabName = "downloadChart",
              h1("Shiny is a product of ",
                 span("RStudio", style = "color:blue")),
              box(
                width=8,height = 8,
                title ="控制圖表", status = "info", solidHeader = TRUE, collapsible = TRUE,
                numericInput("height", "高", min = 2, max = 10, value = 5, step = 0.5),
                numericInput("width", "寬", min = 2, max = 10, value = 5, step = 0.5),
                actionButton("previewButton", "預覽圖片"),
                uiOutput("previewUI")),

              box(
                width=4,height = 8,
                title = "控制圖表", status="primary", solidHeader = TRUE, collapsible = TRUE,
                radioButtons('format', '圖片形式', c('PNG', 'JPG'), inline = TRUE),
                downloadButton('downloadImage')
              )
      )
    )
  )
)



server <- function(input, output, session) {
  data <- reactive({
    req(input$data)
    vroom::vroom(input$data$datapath)
  })

  output$output <- DT::renderDataTable(
    {data()},editable="cell"
  )

  observe({
    y_choices <- names(data())
    updateSelectInput(session, "y", choices = y_choices, selected = y_choices[1])
  })

  observe({
    x_choices <- names(data())
    updateSelectInput(session, "x", choices = x_choices, selected = x_choices[1])
  })

  plot_geom <- reactive({
    switch(input$geom,
           點子圖 = geom_point(color = a,size=2),
           直線圖 = geom_line(group = 1, linewidth = 1, color = a)
    )
  })

  theme1<- reactive({
    switch(input$theme1,
           白 = theme_classic(),
           黑 =dark_theme_classic(),
           藍=theme_economist()
    )
  })

  output$plot <- renderPlot({
    x_data <- data()[[input$x]]
    x_levels <- unique(x_data)
    x_factor <- factor(x_data, levels = x_levels)

    ggplot(data(), aes(x = x_factor, y = .data[[input$y]])) +
      plot_geom() +
      labs(title = input$title, x = " ", y = " ") +
      theme1()+
      theme(text = element_text(family = "黑體-繁 中黑", size=input$文字大小)) +
      coord_cartesian(ylim = c(0, 100))
  }, res = 96)

  output$downloadImage <- downloadHandler(
    filename = function() {
      paste('my-plot', sep = '.', switch(
        input$format, PNG = 'png', JPG = 'jpg'
      ))
    },
    content = function(file) {
      extension <- switch(input$format, PNG = 'png', JPG = 'jpg')
      ggsave(file, plot = last_plot(), device = extension, dpi = 300,
             width = input$width, height = input$height, units = 'in')
    }
  )

  observeEvent(input$previewButton, {
    shinyjs::disable("downloadImage")
    output$previewUI <- renderUI({
      plotOutput("previewPlot", width = input$width * 100, height = input$height * 100)
    })
  })

  output$previewPlot <- renderPlot({
    ggplot(data(), aes(.data[[input$x]], .data[[input$y]])) +
      plot_geom() +
      labs(title = input$title, x = " ", y = " ") +
      theme1() +
      theme(text = element_text(family = "黑體-繁 中黑", size=input$文字大小)) +
      coord_cartesian(ylim = c(0, 100))
  }, res = 96)

  output$plot2 <- renderPlot(width = 500,
                             height = 350,
                             res = 92,
                             {
                               last_plot()
                             })

}

shinyApp(ui = ui, server = server)
