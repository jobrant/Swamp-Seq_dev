process_diffs <- function(diff, pval, logfc) {
  colnames(diff)[1] <- 'ENSG'
  diff <- diff %>% dplyr::select(ENSG, Gene, `Log2(FC)`, `P-value`, BaseAvg)
  diff$BaseAvg <- as.numeric(format(round(diff$BaseAvg, 0)))
  diff['group'] <- 'unchanged'
  diff$group[diff$`P-value` <= pval & diff$`Log2(FC)` > logfc] <- 'up-regulated'
  diff$group[diff$`P-value` <= pval & diff$`Log2(FC)` < (logfc * - 1)] <- 'down-regulated'
  return(diff)
}

get_lines <- function(diff, pval, logfc) {
  shapes <- list(list(type = 'line', 
                      x0 = logfc, 
                      x1 = logfc, 
                      y0 = min(-log10(diff$`P-value`)) -2, 
                      y1 = max(-log10(diff$`P-value`)), 
                      line = list(dash = 'dot', width = 1)), 
                 list(type = 'line', 
                      x0 = (logfc * - 1), 
                      x1 = (logfc * - 1), 
                      y0 = min(-log10(diff$`P-value`)) -2, 
                      y1 = max(-log10(diff$`P-value`)), 
                      line = list(dash = 'dot', width = 1)), 
                 list(type = 'line', 
                      x0 = min(diff$`Log2(FC)`), 
                      x1 = max(diff$`Log2(FC)`), 
                      y0 = -log10(pval), 
                      y1 = -log10(pval),
                      line = list(dash = 'dot', width = 1)))
  return(shapes)
}


dataSet1 <- reactive({
  req(input$RS_contrast)
  allgenediff <- paste(project_path, input$RS_projects, input$RS_experiment, 
                       paste(input$RS_contrast, 'allGeneDiff.csv', sep = '.'), sep = '/')
  fread(file = paste(allgenediff), sep = "\t", data.table = F)
})



output$RS_plot1 <- renderPlotly({ 
  geneDiff <- dataSet1()
  geneDiff <- process_diffs(diff = geneDiff, 
                            pval = as.numeric(input$RS_pval), 
                            logfc = as.numeric(input$RS_logfc)) 
  shapes <- get_lines(diff = geneDiff, 
                      pval = as.numeric(input$RS_pval), 
                      logfc = as.numeric(input$RS_logfc))
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
          marker = list(size = 10, color = 'gray', opacity = 0.5),
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
              marker = list(size = 10, color = 'red', opacity = 0.5),
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
              marker = list(size = 10, color = 'blue', opacity = 0.5),
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
    config(toImageButtonOptions = my_data$conf)
})