# dimensionality_reduction_module.R

library(shiny)
library(plotly)

dimReductionUI <- function(id) {
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
        fluidRow(
          box(
            headerBorder = TRUE,
            title = h6('Principle Component Analysis-Norm', style = 'font-size:28px'),
            height = 540,
            width = 6,
            id = 'PCA-Norm',
            plotlyOutput(ns('pcaNorm'))
          )
        ),
        fluidRow(
          box(
            headerBorder = TRUE,
            title = h6('Multidimentional Scaling-Norm', style = 'font-size:28px'),
            height = 540,
            width = 6,
            id = 'MDS-Norm',
            plotlyOutput(ns('mdsNorm'))
          )
        )
      )
    )
  )
}

dimReductionServer <- function(id, processed.norm_data, processed_metadata) {
  moduleServer(id, function(input, output, session) {
    
    mds_data <- reactiveValues(
      conf = list(format = 'svg', width = 800, height = 800)
    )
    
    output$pcaNorm <- renderPlotly({
      conditions <- get_conditions(processed_metadata())
      counts_norm <- processed.norm_data()
      counts_norm <- counts_norm %>% 
        mutate_all(as.numeric)
      comp <- process_counts(counts_norm)
      
      plot_PCA(comp_data = comp, 
               metadata.1 = processed_metadata(), 
               conditions.1 = conditions, 
               mds_data.1 = mds_data)
    })
    
    output$mdsNorm <- renderPlotly({
      conditions <- get_conditions(processed_metadata())
      
      counts_norm <- processed.norm_data()
      counts_norm <- counts_norm %>% 
        mutate_all(as.numeric)
      
      counts_norm_df <- plotMDS(counts_norm, plot = F)
      
      counts_norm_dist <- as.data.frame(counts_norm_df$distance.matrix)  
      counts_norm_scaled <- as.data.frame(cmdscale(counts_norm_dist))
      counts_norm_scaled$Sample_ID <- rownames(counts_norm_scaled)
      
           counts_norm_scaled <- add_groups(counts_norm_scaled, processed_metadata(), conditions)
      
      x <- counts_norm_scaled$V1
      y <- counts_norm_scaled$V2
      
      plot_ly(data = counts_norm_scaled[,2:3], 
              x = counts_norm_scaled$V1,
              y = counts_norm_scaled$V2, 
              type = 'scatter', 
              mode = 'markers', 
              text = counts_norm_scaled$Sample_ID,
              color = counts_norm_scaled$Group,
              # colors = c('blue', 'gray40', 'red', 'green'),
              marker = list(size = 15),
              hovertemplate = paste(
                "<b>Sample Name: %{text}</b><br>",
                "<b>Group Name: ", counts_norm_scaled$Group, "<br>",
                "<b>PC1: %{x:.2f}<br>",
                "<b>PC2: %{y:.2f}<br>"
              )) %>% 
        layout(xaxis = list(title = list(text = paste('<b>Leading log2(FC) dim 1<b>'),
                                         color = 'black', 
                                         standoff = 20L),
                            titlefont = list(size = 20), 
                            tickfont = list(size = 18)),  
               yaxis = list(title = list(text = paste('<b>Leading log(FC) dim 2<b>'),
                                         color = 'black'), 
                            titlefont = list(size = 20), 
                            tickfont = list(size = 18)),
               legend = list(orientation = 'h', 
                             xanchor = 'center',
                             x = 0.5, 
                             y = -0.2, 
                             font = list(size = 16)), 
               height = 480) %>% 
        config(toImageButtonOptions = mds_data$conf) %>%
        layout(
          font = list(color = "white"),
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)'
        )
    })
  })
}
