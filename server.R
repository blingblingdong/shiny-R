
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
