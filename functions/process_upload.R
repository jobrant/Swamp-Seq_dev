
source('FUNCTIONS/helper_functions.R')
source('FUNCTIONS/global_options.R')
source('FUNCTIONS/plotting_functions.R')
source('FUNCTIONS/homepage.R')
source('FUNCTIONS/dim_red.R')
source('FUNCTIONS/volcano.R')


z <- c("limma", "edgeR", "tidyverse", "openxlsx", 'pheatmap', 'data.table',
       'org.Hs.eg.db', 'Homo.sapiens', 'RColorBrewer', 'ggrepel', 'hues',
       'Biobase', 'stringr', 'ggfortify', 'qs', 'kableExtra', 'patchwork', 'openxlsx', 'knitr',
       'gtools')
aa <- lapply(z, require, character.only = T)

# Define sidebar_panel

process_upload_sidebar <- sidebarPanel(
  width = 3,
  style = "position:fixed;width:22%;",
  h4("Upload your data here!"),
  awesomeRadio(
    inputId = "fileexttype",
    label = "Choose your file separator", 
    choices = c(".csv" = ",", ".tsv" = "\t"),
    selected = ",",
    status = "success"
  ),
  tags$hr,
  awesomeRadio(
    inputId = "filetype",
    label = "Choose your data file type", 
    choices = c("RSEM", "Counts.csv (Filtered and Normalized CPM)"),
    selected = "RSEM",
    status = "success"
  ),
  tags$hr,
  fileInput("file1", "Upload here",
            multiple = TRUE),
            # accept = c(".csv", ".tsv", ".txt")),
  tags$hr,
  actionButton("viewDataBtn", "Raw Counts Data"),

)

process_load_sidebar <- sidebarPanel(
  width = 3,
  style = "position:fixed;width:22%;",
  
  h4("Load your data from the server!"),
  pickerInput(
    inputId = "RS_projects",
    label = "Select Project",
    choices = c('kalyanee', 'Hermione'), 
    selected = 'kalyanee',
    options = list(
      title = "Select dataset")
  ), 
  uiOutput('RS_experiment'),
  uiOutput('RS_contrast'),
  br(),
  actionButton("toggleSidebar", "Toggle sidebar"),
)

process_upload_mainpanel <- mainPanel(
  width = 8,
  
)