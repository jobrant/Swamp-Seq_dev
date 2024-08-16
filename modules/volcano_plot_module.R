# modules/volcano_plot_module.R

library(shiny)
library(plotly)
library(shinydashboard)  # For box function, if you still want to use it

volcanoPlotUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        awesomeRadio(
          inputId = ns("pval"),
          label = 'Filter Expression Data Tables by Adjusted p-value', 
          choices = list(1, 0.05, 0.01),
          selected =  0.05,
          status = "success"
        ),
        awesomeRadio(
          inputId = ns('logfc'),
          label = 'Filter Expression Data Tables by log2(FC)',
          choices = list(0, 0.58, 1),
          selected = 0.58,
          status = "success"
        )
      ),
      mainPanel(
        h4("Volcano Plots", style = "text-align: center"),
        plotlyOutput(ns("plot"), width = "100%", height = "600px"),
        h6("Use the options available on the right side corner to navigate your way on the figure.")
      )
    )
  )
}

volcanoPlotServer <- function(id, processed_deg.tables) {
  moduleServer(id, function(input, output, session) {
    my_data <- reactiveValues(
      x = list(title = list(text = '<b>log2(fold change)<b>', 
                            color = 'black', 
                            standoff = 20L),
               titlefont = list(size = 22), 
               tickfont = list(size = 18)),
      
      y = list(title = list(text = '<b>-log10(adjusted p-value)<b>', color = 'black'), 
               titlefont = list(size = 22), 
               tickfont = list(size = 18)),
      
      conf = list(format = 'svg', width = 800, height = 800)
    )
    
    output$plot <- renderPlotly({
      geneDiff <- processed_deg.tables()
      geneDiff <- process_diffs(diff = geneDiff,
                                pval = as.numeric(input$pval),
                                logfc = as.numeric(input$logfc))
      shapes <- get_lines(diff = geneDiff,
                          pval = as.numeric(input$pval),
                          logfc = as.numeric(input$logfc))
      up <- subset(geneDiff, geneDiff$group == 'up-regulated')
      not <- subset(geneDiff, geneDiff$group == 'unchanged')
      down <- subset(geneDiff, geneDiff$group == 'down-regulated')
      
      plot_ly(data = up,
              x = not$`Log2(FC)` ,
              y = -log10(not$`P-value`),
              type = 'scatter',
              mode = 'markers',
              height = 480,
              alpha = 0.3,
              marker = list(size = 10, color = 'red', opacity = 0.5),
              text = not$Gene,
              color = not$group,
              
              hovertemplate = paste(
                "<b>Gene Name: %{text}</b><br>",
                "<b>ENSEMBL ID:<b>", not$ENSG, "<br>",
                "%{yaxis.title.text}: %{y:.3f}<br>",
                "%{xaxis.title.text}: %{x:.2f}<br>",
                "Mean of Normalized Counts: ", not$BaseAvg,
                "<extra></extra>"
              )) %>%
        add_trace(x = up$`Log2(FC)` ,
                  y = -log10(up$`P-value`),
                  type = 'scatter',
                  mode = 'markers',
                  marker = list(size = 10, color = '#39FF14', opacity = 0.5),
                  text = up$Gene,
                  color = up$group,
                  
                  hovertemplate = paste(
                    "<b>Gene Name: %{text}</b><br>",
                    "<b>ENSEMBL ID:<b>", up$ENSG, "<br>",
                    "%{yaxis.title.text}: %{y:.3f}<br>",
                    "%{xaxis.title.text}: %{x:.2f}<br>",
                    "Mean of Normalized Counts: ", up$BaseAvg,
                    "<extra></extra>"
                  )) %>%
        add_trace(x = down$`Log2(FC)` ,
                  y = -log10(down$`P-value`),
                  type = 'scatter',
                  mode = 'markers',
                  marker = list(size = 10, color = 'yellow', opacity = 0.5),
                  text = down$Gene,
                  color = down$group,
                  
                  hovertemplate = paste(
                    "<b>Gene Name: %{text}</b><br>",
                    "<b>ENSEMBL ID:<b>", down$ENSG, "<br>",
                    "%{yaxis.title.text}: %{y:.3f}<br>",
                    "%{xaxis.title.text}: %{x:.2f}<br>",
                    "Mean of Normalized Counts: ", down$BaseAvg,
                    "<extra></extra>"
                  )) %>%
        layout(xaxis = my_data$x,
               yaxis = my_data$y,
               shapes = shapes,
               legend = list(orientation = 'h',
                             xanchor = 'center',
                             x = 0.5,
                             y = -0.2,
                             font = list(size = 16))) %>%
        config(toImageButtonOptions = my_data$conf) %>%
        layout(
          font = list(color = "white"),
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)'
        )
    })
  })
}