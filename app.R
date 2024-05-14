library(shiny)
library(shinyWidgets)
library(plotly)
library(DT)
library(bslib)
library(shinydashboard)
library(shinydashboardPlus)
library(clusterProfiler)
library(org.Hs.eg.db)

# Assume these modules will handle specific functionalities related to different UI and server logic
source('modules/preprocess_ui.R')
source('modules/preprocess_server.R')
source('modules/volcano_ui.R')
source('modules/volcano_server.R')
source('modules/dim_red_ui.R')
source('modules/dim_red_server.R')
source('modules/enrich_ui.R')
source('modules/enrich_server.R')

# Main UI
ui <- navbarPage(
  id = "MainNavBar",
  position = "fixed-top",
  theme = bs_theme(bootswatch = "darkly", base_font = font_google("Inter")),
  title = tags$strong("SWAMP-SEQ dev version"),
  br(), br(), br(), br(),
  tabPanel(value = "home", title = tags$strong("Home"),
           homepageUI()  # Assume homepageUI is defined in one of your sourced files or modules
  ),
  navbarMenu(title = div(tags$strong("Process Data"), tags$i(style = "color: rgb(0,166,90)")),
             tabPanel(preprocess_ui("upload")),
             tabPanel(preprocess_ui("server"))
  ),
  tabPanel(volcano_ui("volcano_plots")),
  tabPanel(dim_red_ui("dimensionality_reduction")),
  tabPanel(enrich_ui("enrich_analysis")),
  tabPanel(value = "info", title = div(tags$strong("About")),
           aboutPageUI()  # Assume aboutPageUI is defined in one of your sourced files or modules
  )
)

# Main server function
server <- function(input, output, session) {
  preprocess_server("upload", input, output, session)
  preprocess_server("server", input, output, session)
  volcano_server("volcano_plots", input, output, session)
  dim_red_server("dimensionality_reduction", input, output, session)
  enrich_server("enrich_analysis", input, output, session)
  
  observeEvent(input$link_to_PP, {
    updateNavbarPage(inputId = "MainNavBar", selected = "upload")
  })
  
  observeEvent(input$link_to_VP1, {
    updateNavbarPage(inputId = "MainNavBar", selected = "volcano_plots")
  })
}

# Run the application
shinyApp(ui, server)
