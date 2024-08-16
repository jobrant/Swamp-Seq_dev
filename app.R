# app.R 

# Determine the directory of the current script
current_dir <- tryCatch({
  # First, try to get the directory of the current script
  dirname(sys.frame(1)$ofile)
}, error = function(e) {
  # If that fails, use the current working directory
  getwd()
})

print(paste("Current directory:", current_dir))

# Load and install required packages
packages <- c('dplyr', 'qs', 'Matrix', 'shiny', 'DT', 'shinyWidgets', 'plotly', 'openxlsx',
              'ggplot2', "limma", 'shinydashboard', "edgeR", 'stringr', 'pheatmap', 'tidyverse',
              'heatmaply', 'bslib', 'shinydashboardPlus', 'thematic', 'leaflet', 'bsicons', 'data.table',
              'enrichplot', 'clusterProfiler', 'org.Hs.eg.db', 'GOSemSim')

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

lapply(packages, install_if_missing)
lapply(packages, library, character.only = TRUE)

print("Packages loaded successfully")

# Source helper functions
tryCatch({
  source(file.path(current_dir, 'functions', 'helper_functions.R'))
  source(file.path(current_dir, 'functions', 'global_options.R'))
  source(file.path(current_dir, 'functions', 'plotting_functions.R'))
  source(file.path(current_dir, 'functions', 'heatmaps.R'))
  source(file.path(current_dir, 'functions', 'process_upload.R'))
  source(file.path(current_dir, 'functions', 'rna-seq-process.R'))
  source(file.path(current_dir, 'functions', 'volcano.R'))
  print("Helper functions sourced successfully")
}, error = function(e) {
  print(paste("Error sourcing helper functions:", e$message))
})

# Source modules
tryCatch({
  print("Attempting to source homepage_module.R")
  source(file.path(current_dir, 'modules', 'homepage_module.R'))
  print("homepage_module.R sourced successfully")
  
  print("Attempting to source volcano_plot_module.R")
  source(file.path(current_dir, 'modules', 'volcano_plot_module.R'))
  print("volcano_plot_module.R sourced successfully")
  
  source(file.path(current_dir, 'modules', 'dimensionality_reduction_module.R'))
  source(file.path(current_dir, 'modules', 'heatmap_plot_module.R'))
  source(file.path(current_dir, 'modules', 'enrichment_analysis_module.R'))
  source(file.path(current_dir, 'modules', 'deg_tables_module.R'))
  print("All modules sourced successfully")
}, error = function(e) {
  print(paste("Error sourcing modules:", e$message))
})

# Source the UI and server files
tryCatch({
  source(file.path(current_dir, 'ui.R'))
  source(file.path(current_dir, 'server.R'))
  print("UI and server files sourced successfully")
}, error = function(e) {
  print(paste("Error sourcing UI or server file:", e$message))
})

# Run the application
shinyApp(ui, server)