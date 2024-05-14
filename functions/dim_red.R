# Sidebar layout for Dimentionality Reduction

mainpanel_dr <- mainPanel(
  fluidRow(
    tabBox(title = 'Dimensional Reduction Plots', 
           id = 'Expression', 
           # width = 6, 
           # height = 540,
           tabPanel('PCA',
                    plotlyOutput('RS_pcaRaw')),
           tabPanel('PCA-Norm', 
                    plotlyOutput('RS_pcaNorm')), 
           tabPanel('MDS',
                    plotlyOutput('RS_mdsRaw')),
           tabPanel('MDS-Norm', 
                    plotlyOutput('RS_mdsNorm'))
  )
)
)

sidebar_dr <- sidebarPanel(
  # sidebar volcano ----
  width = 3,
  uiOutput('RS_experiment'),
  uiOutput('RS_contrast'),
  awesomeRadio(
    inputId = "RS_pval",
    label = 'Filter Expression Data Tables by Adjusted p-value', 
    choices = list(1, 0.05, 0.01),
    selected =  0.05,
    status = "success"),
  
  awesomeRadio(
    inputId = 'RS_logfc',
    label = 'Filter Expression Data Tables by log2(FC)',
    choices = list(0, 0.58, 1),
    selected = 0.58,
    status = "success"),
  
)