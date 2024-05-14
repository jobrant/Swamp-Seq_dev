library(shiny)
library(DT)
library(clusterProfiler)
library(org.Hs.eg.db)

source('modules/preprocess_server.R')
source('modules/volcano_server.R')
source('modules/dim_red_server.R')
source('modules/enrich_server.R')

server <- function(input, output, session){
  preprocess_server("upload", input, output, session)
  preprocess_server("server", input, output, session)
  volcano_server("volcano_plots", input, output, session)
  dim_red_server("dimentionality_reduction", input, output, session)
  enrich_server("enrich_analysis", input, output, session)
  
  # Other global observeEvent or reactive code
  observeEvent(input$link_to_PP, {
    updateNavbarPage(inputId="MainNavBar", selected = "upload")
  })
  
  observeEvent(input$link_to_VP1, {
    updateNavbarPage(inputId="preprocess", selected = "volcano_plots")
  })
}

shinyApp(ui, server)
