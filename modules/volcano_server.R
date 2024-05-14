volcano_server <- function(id, input, output, session) ({
  ns <- session$ns
  
  processed_deg.tables <- reactiveVal(NULL)
  
  processed_deg.tables <- reactive({
    if (is.null(deg_table.final()) && input$filetype != "Count Matrix") {
      dataSet1()
    } else if (!is.null(deg_table.final())){
      format_change(deg_table.final())
    } else {
      verbatimTextOutput("No DEG tables uploaded")
    }
  })
  
  observeEvent(input$link_to_VP1, {
    updateNavbarPage(inputId="preprocess", selected = "volcano_plots")
  })
  
  output$RS_plot1 <- renderPlotly({
    geneDiff <- processed_deg.tables()
    geneDiff <- process_diffs(diff = geneDiff, pval = as.numeric(input$RS_pval), logfc = as.numeric(input$RS_logfc))
    shapes <- get_lines(diff = geneDiff, pval = as.numeric(input$RS_pval), logfc = as.numeric(input$RS_logfc))
    up <- subset(geneDiff, geneDiff$group == 'up-regulated')
    not <- subset(geneDiff, geneDiff$group == 'unchanged')
    down <- subset(geneDiff, geneDiff$group == 'down-regulated')
    
    plot_ly(data = up, 
            x = not$`Log2(FC)`, 
            y = -log10(not$`P-value`), 
            type = 'scatter', 
            mode = 'markers', 
            height = 480, 
            alpha = 0.3,
            marker = list(size = 10, color = 'red', opacity = 0.5), 
            text = not$Gene, 
            color = not$group,
            hovertemplate = paste("<b>Gene Name: %{text}</b><br>", 
                                  "<b>ENSEMBL ID:<b>", 
                                  not$ENSG, 
                                  "<br>", 
                                  "%{yaxis.title.text}: %{y:.3f}<br>", 
                                  "%{xaxis.title.text}: %{x:.2f}<br>", 
                                  "Mean of Normalized Counts: ", 
                                  not$BaseAvg, "<extra></extra>")) %>%
      add_trace(x = up$`Log2(FC)`, 
                y = -log10(up$`P-value`), 
                type = 'scatter', 
                mode = 'markers', 
                marker = list(size = 10, color = '#39FF14', opacity = 0.5), 
                text = up$Gene, 
                color = up$group,
                hovertemplate = paste("<b>Gene Name: %{text}</b><br>", 
                                      "<b>ENSEMBL ID:<b>", 
                                      up$ENSG, 
                                      "<br>", 
                                      "%{yaxis.title.text}: %{y:.3f}<br>", 
                                      "%{xaxis.title.text}: %{x:.2f}<br>", 
                                      "Mean of Normalized Counts: ", 
                                      up$BaseAvg, "<extra></extra>")) %>%
      add_trace(x = down$`Log2(FC)`, 
                y = -log10(down$`P-value`), 
                type = 'scatter', 
                mode = 'markers', 
                marker= list(size = 10, color = 'yellow', opacity = 0.5),
                text = down$Gene,
                color = down$group,
                
                hovertemplate = paste(
                  "<b>Gene Name: %{text}</b><br>",
                  "<b>ENSEMBL ID:<b>", down$ENSG, "<br>",
                  "%{yaxis.title.text}: %{y:.3f}<br>",
                  "%{xaxis.title.text}: %{x:.2f}<br>",
                  "Mean of Normalized Counts: ", 
                  down$BaseAvg, "<extra></extra>")) %>%
      layout(xaxis = my_data$x,
             yaxis = my_data$y,
             shapes = shapes,
             legend = list(orientation = 'h',
                           xanchor = 'center',
                           x = 0.5,
                           y = -0.2,
                           font = list(size = 16))) %>%
      config(toImageButtonOptions = my_data$conf) %>%
      # makes background black and font white-
      layout(
        font = list(color = "white"),
        paper_bgcolor='rgba(0,0,0,0)',
        plot_bgcolor='rgba(0,0,0,0)'
      )
  })
})
                
                
                
                