# load the required packages
x <- c('dplyr', 'qs', 'Matrix', 'shiny', 'DT', 'shinyWidgets', 'plotly', 'openxlsx',
       'ggplot2', "limma", 'shinydashboard', "edgeR", 'stringr', 'pheatmap', 'tidyverse',
       'heatmaply', 'bslib', 'shinydashboardPlus', 'thematic', 'leaflet', 'bsicons', 'data.table',
       'enrichplot', 'clusterProfiler', 'org.Hs.eg.db', 'GOSemSim')

lapply(x, require, character.only = T)

# Source the helper functions and modules
source('functions/helper_functions.R')
source('functions/global_options.R')
source('functions/plotting_functions.R')
source('functions/homepage.R')


# Source the UI and server files
source('ui.R')
source('server.R')

# Run the application
shinyApp(ui, server)
