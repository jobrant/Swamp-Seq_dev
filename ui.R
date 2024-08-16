# ui.R

library(shiny)
library(bslib)
library(shinyWidgets)

# Determine the directory of the current script
current_dir <- tryCatch({
  # First, try to get the directory of the current script
  dirname(sys.frame(1)$ofile)
}, error = function(e) {
  # If that fails, use the current working directory
  getwd()
})


source(file.path(current_dir, 'modules', 'homepage_module.R'))
source(file.path(current_dir, 'modules', 'volcano_plot_module.R'))
source(file.path(current_dir, 'modules', 'dimensionality_reduction_module.R'))
source(file.path(current_dir, 'modules', 'heatmap_plot_module.R'))
source(file.path(current_dir, 'modules', 'enrichment_analysis_module.R'))
source(file.path(current_dir, 'modules', 'deg_tables_module.R'))

ui <- navbarPage(
  id = "MainNavBar",
  position = "fixed-top",
  theme = bs_theme(bootswatch = "darkly", base_font = font_google("Inter")),
  tags$script(HTML("
    var header = $('.navbar > .container-fluid');
    var navbarHeight = $('.navbar').height();
    header.append('<div style=\"float:right; height:' + navbarHeight + 'px;\"><a href=\"https://cancer.ufl.edu/\"><img src=\"UFH_CancerCenter.jpg\" alt=\"alt\" style=\"width:100%; height:100%;\"></a></div>');
  ")),
  title = tags$strong("SWAMP-SEQ"),
  
  nav_panel(
    title = tags$strong("Home"),
    homepageUI("homepage")
  ),
  
  navbarMenu(
    title = div(tags$strong("Process-Data"), tags$i(style = "color: rgb(0,166,90)")),
    menuName = "preprocess",
    nav_panel(
      title = "Upload",
      # Your upload UI here
    ),
    nav_panel(
      title = "Server",
      # Your server data loading UI here
    )
  ),
  
  nav_panel(
    title = div(tags$strong("Volcano Plots"), tags$i(style = "color: rgb(0,166,90)")),
    volcanoPlotUI("volcano")
  ),
  
  nav_panel(
    title = div(tags$strong("Dimensionality Reduction")),
    dimReductionUI("dimreduction")
  ),
  
  nav_panel(
    title = div(tags$strong("Heatmaps")),
    heatmapUI("heatmap")
  ),
  
  nav_panel(
    title = div(tags$strong("DEG Tables")),
    degTablesUI("deg_tables")
  ),
  
  nav_panel(
    title = div(tags$strong("Enrichment Analysis")),
    enrichmentAnalysisUI("enrichment")
  ),
  
  nav_panel(
    title = div(tags$strong("About")),
    # Your about page content here
  )
)
