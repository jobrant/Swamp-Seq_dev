# heatmap_module.R

library(shiny)
library(plotly)
library(heatmaply)
library(dplyr)

heatmapUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        awesomeRadio(
          inputId = ns("pval_hm"),
          label = 'Filter Expression Data Tables by Adjusted p-value',
          choices = list(1, 0.05, 0.01),
          selected =  0.05,
          status = "success"
        ),
        awesomeRadio(
          inputId = ns('logfc_hm'),
          label = 'Filter Expression Data Tables by log2(FC)',
          choices = list(0, 0.58, 1),
          selected = 0.58,
          status = "success"
        )
      ),
      mainPanel(
        h3("Heatmaps here"),
        plotlyOutput(ns('pheatmap'), width = "100%")
      )
    )
  )
}

heatmapServer <- function(id, processed_deg.tables, processed.norm_data, processed_metadata, table1_deg) {
  moduleServer(id, function(input, output, session) {
    
    heatmaply_conf <- reactiveValues(
      conf = list(format = 'svg', width = 800, height = 800)
    )
    
    # Reactive expression for data processing
    heatmap_data <- reactive({
      req(input$pval_hm, input$logfc_hm)
      
      conditions <- get_conditions(processed_metadata())
      counts_raw <- as.data.frame(processed.norm_data())
      counts_raw <- rownames_to_column(counts_raw, var = 'ENSG')
      diff <- table1_deg()$x$data
      filtered <- counts_raw[counts_raw$ENSG %in% diff$ENSG, ]
      
      filtered_tf <- setNames(data.frame(t(filtered[,-1])), filtered[,1])
      rownames(filtered) <- NULL
      filtered <- column_to_rownames(filtered, var = 'ENSG')
      
      filtered_tf$Sample_ID <- rownames(filtered_tf)
      filtered_tf <- add_groups(filtered_tf, processed_metadata(), conditions)
      filtered_tf <- filtered_tf %>% dplyr::select(Sample_ID, Group)
      
      list(filtered = filtered, filtered_tf = filtered_tf, diff = diff)
    })
    
    output$pheatmap <- renderPlotly({
      data <- heatmap_data()
      
      if (max(data$diff$`P-value` > 0.05)) {
        return(NULL)  # or return a placeholder plot
      }
      
      heatmaply(x = data$filtered, 
                k_row = 2, 
                k_col = 3, 
                scale = 'row',
                colors = hm_colors,
                showticklabels = c(T,F), 
                col_side_colors = data$filtered_tf, 
                side_color_colorbar_len = 0.5,
                plot_method = 'plotly',
                scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                  low = 'cyan', 
                  mid = 'black', 
                  high = 'red',
                  midpoint = 0)) %>% 
        layout(height = 580) %>% 
        config(toImageButtonOptions = heatmaply_conf$conf)   
    })
  })
}
