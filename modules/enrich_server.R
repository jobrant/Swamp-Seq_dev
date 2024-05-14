enrich_server <- function(id, input, output, session) {
  ns <- session$ns
  
  # Reactive expression to fetch and process DEG data
  processed_deg_data <- reactive({
    # Replace with the actual logic to fetch or process DEG data
    # For example, reading from a file or a reactiveVal
    deg_data <- data.frame(
      ENSG = c("ENSG000001", "ENSG000002", "ENSG000003"),
      Gene = c("Gene1", "Gene2", "Gene3"),
      `P-value` = c(0.01, 0.02, 0.03),
      `Log2(FC)` = c(1.5, -2.0, 0.5)
    )
    return(deg_data)
  })
  
  # Reactive expression to perform GO enrichment analysis for up-regulated genes
  GO_up <- reactive({
    req(processed_deg_data())
    
    genes_data <- processed_deg_data()
    colnames(genes_data)[1] <- 'ENSG'
    universe <- genes_data$ENSG
    
    genes_up <- genes_data[genes_data$`P-value` <= input$pval_cutoff & 
                             genes_data$`Log2(FC)` >= input$logfc_cutoff, ]$ENSG
    
    GO <- enrichGO(gene = genes_up,
                   OrgDb = org.Hs.eg.db,
                   keyType = 'ENSEMBL',
                   ont = 'BP',
                   pAdjustMethod = 'BH',
                   pvalueCutoff = 0.05,
                   qvalueCutoff = 0.05,
                   readable = TRUE,
                   universe = universe)
    
    simplify(GO)
  })
  
  # Reactive expression to perform GO enrichment analysis for down-regulated genes
  GO_down <- reactive({
    req(processed_deg_data())
    
    genes_data <- processed_deg_data()
    colnames(genes_data)[1] <- 'ENSG'
    universe <- genes_data$ENSG
    
    genes_down <- genes_data[genes_data$`P-value` <= input$pval_cutoff & 
                               genes_data$`Log2(FC)` <= -input$logfc_cutoff, ]$ENSG
    
    GO <- enrichGO(gene = genes_down,
                   OrgDb = org.Hs.eg.db,
                   keyType = 'ENSEMBL',
                   ont = 'BP',
                   pAdjustMethod = 'BH',
                   pvalueCutoff = 0.05,
                   qvalueCutoff = 0.05,
                   readable = TRUE,
                   universe = universe)
    
    simplify(GO)
  })
  
  # Render GO enrichment plot for up-regulated genes
  output$GO_up_plot <- renderPlotly({
    req(GO_up())
    ggplotly(barplot(GO_up(), showCategory = 10))
  })
  
  # Render GO enrichment plot for down-regulated genes
  output$GO_down_plot <- renderPlotly({
    req(GO_down())
    ggplotly(barplot(GO_down(), showCategory = 10))
  })
  
  # Render table of enriched GO terms for up-regulated genes
  output$GO_up_table <- renderDT({
    req(GO_up())
    datatable(as.data.frame(GO_up()))
  })
  
  # Render table of enriched GO terms for down-regulated genes
  output$GO_down_table <- renderDT({
    req(GO_down())
    datatable(as.data.frame(GO_down()))
  })
}
