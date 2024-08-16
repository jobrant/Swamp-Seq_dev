# enrichment_analysis_module.R

library(shiny)
library(plotly)
library(clusterProfiler)
library(org.Hs.eg.db)
library(ggplot2)
library(qs)

enrichmentAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        awesomeRadio(
          inputId = ns("pval_ea"),
          label = 'Filter Expression Data Tables by Adjusted p-value', 
          choices = list(1, 0.05, 0.01),
          selected =  0.05,
          status = "success"
        ),
        awesomeRadio(
          inputId = ns('logfc_ea'),
          label = 'Filter Expression Data Tables by log2(FC)',
          choices = list(0, 0.58, 1),
          selected = 0.58,
          status = "success"
        )
      ),
      mainPanel(
        h3("Enrichment Analysis: Tree Plots"),
        plotlyOutput(ns('GO'), height = "900px")
      )
    )
  )
}

enrichmentAnalysisServer <- function(id, processed_deg.tables) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for data processing
    go_data <- reactive({
      req(input$pval_ea, input$logfc_ea)
      
      geneDiff <- processed_deg.tables()
      colnames(geneDiff)[1] <- 'ENSG'
      universe <- geneDiff$ENSG
      geneDiff <- geneDiff[geneDiff$`P-value` <= as.numeric(input$pval_ea) & 
                           abs(geneDiff$`Log2(FC)`) >= as.numeric(input$logfc_ea), ]
      
      sem <- qs::qread("APP_DATA/UCSC.homo.sapiens.sem.qs")
      
      GO <- enrichGO(gene = geneDiff$ENSG,
                     OrgDb = 'org.Hs.eg.db',
                     keyType = 'ENSEMBL',
                     ont = 'BP',
                     pAdjustMethod = 'BH',
                     pvalueCutoff = 0.05,
                     qvalueCutoff = 0.05,
                     readable = TRUE,
                     universe = universe)
      
      pairwise_termsim(GO, method = 'Wang', semData = sem)
    })
    
    # Render the plot
    output$GO <- renderPlotly({
      GO <- go_data()
      ggplotly(barplot(GO, showCategory = 20))
    })
  })
}
