# deg_tables_module.R

library(shiny)
library(DT)
library(dplyr)

degTablesUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Differentially Expressed Gene Tables"),
    sidebarLayout(
      sidebarPanel(
        width = 4,
        awesomeRadio(
          inputId = ns("pval_deg"),
          label = 'Filter Expression Data Tables by Adjusted p-value', 
          choices = list(1, 0.05, 0.01),
          selected =  0.05,
          status = "success"
        ),
        awesomeRadio(
          inputId = ns('logfc_deg'),
          label = 'Filter Expression Data Tables by log2(FC)',
          choices = list(0, 0.58, 1),
          selected = 0.58,
          status = "success"
        )
      ),
      mainPanel(
        width = 8,
        navset_card_tab(
          height = 650,
          full_screen = FALSE,
          title = "View your data",
          nav_panel('Table', DTOutput(ns('table_deg')))
        )
      )
    )
  )
}

degTablesServer <- function(id, processed_deg.tables) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive expression for filtered DEG table
    filtered_deg_table <- reactive({
      req(input$pval_deg, input$logfc_deg)
      
      tmp1 <- processed_deg.tables() %>%
        filter(`P-value` <= as.numeric(input$pval_deg) &
               abs(`Log2(FC)`) >= as.numeric(input$logfc_deg))
      
      colnames(tmp1)[1] <- 'ENSG'
      
      tmp1 %>% 
        select(Gene, `Log2(FC)`, `P-value`, 'ENSG', BaseAvg, everything())
    })
    
    # Render the DEG table
    output$table_deg <- renderDT({
      datatable(filtered_deg_table(), 
                style = "bootstrap4",
                extensions = c('FixedColumns', 'FixedHeader', 'Scroller'),
                options = list(
                  scrollX = TRUE,
                  scrollY = "500px",
                  scroller = TRUE,
                  dom = 'Bfrtip',
                  buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                ),
                class = 'display nowrap', 
                rownames = TRUE)
    })
  })
}
