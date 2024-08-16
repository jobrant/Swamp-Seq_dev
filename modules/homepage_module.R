# homepage_module.R

library(shiny)
library(bslib)
library(shinyWidgets)
library(bsicons)

homepageUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(12, 
        h2("Welcome to SEQU-VIZ", style = "text-align: center;"),
        p(intro_content(), style = "text-align: center;")
      )
    ),
    br(),
    fluidRow(
      column(3, value_box_ui(ns("de_box"))),
      column(3, value_box_ui(ns("tables_box"))),
      column(3, value_box_ui(ns("viz_box"))),
      column(3, value_box_ui(ns("downstream_box")))
    ),
    br(),
    fluidRow(
      column(12, actionLink(ns("get_started"), "Get Started", icon("arrow-right")))
    ),
    br(),
    fluidRow(
      column(12, nav_card_home_ui(ns("nav_card")))
    )
  )
}

homepageServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Value boxes
    value_box_server("de_box", "Differential Expression", 
                     "Perform DE analysis on your counts data or load datasets from the server",
                     icon = "bar-chart-steps", color = "#3498DB")
    
    value_box_server("tables_box", "Interactive Tables", 
                     "Obtain list of top differentially expressed genes",
                     icon = "table")
    
    value_box_server("viz_box", "Visualizations", 
                     "Plot volcano plots, heatmaps, dimensionality reduction plots and more",
                     icon = "bar-chart-line-fill", color = "#3498DB")
    
    value_box_server("downstream_box", "Downstream Analysis", 
                     "Conduct downstream analysis such as enrichment analysis",
                     icon = "diagram-3-fill")
    
    # Navigation card
    nav_card_home_server("nav_card")
    
    # Get Started action
    observeEvent(input$get_started, {
      # Navigate to the upload page or trigger the appropriate action
    })
  })
}

# Helper functions

intro_content <- function() {
  "SEQU-VIZ provides an interactive and user-friendly interface to analyse RNA-Seq Datasets. 
   The data can be accessed directly from the Licht lab server or uploaded externally. 
   There are numerous visualizations available to perform quality control, filtering and 
   normalization of the data. You can also perform dimensionality reduction using principal 
   component analysis and multidimensional scaling analysis. Furthermore, the app also allows the 
   user to conduct their own enrichment analysis."
}

value_box_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("box"))
}

value_box_server <- function(id, title, content, icon, color = "success") {
  moduleServer(id, function(input, output, session) {
    output$box <- renderUI({
      value_box(
        title = title,
        value = "",
        showcase = bs_icon(icon),
        p(content),
        theme_color = color,
        height = 250,
        full_screen = FALSE
      )
    })
  })
}

nav_card_home_ui <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    height = 450,
    full_screen = FALSE,
    title = div(img(src="help.png", width= 15), tags$strong("Information Center", style = "color:white")),
    nav_panel("Volcano Plots", nav_content_ui(ns("volcano"))),
    nav_panel("Dimensionality Reduction", nav_content_ui(ns("dim_reduction"))),
    nav_panel("Heatmaps", nav_content_ui(ns("heatmaps"))),
    nav_panel("Gene Ontology Analysis", nav_content_ui(ns("go_analysis"))),
    nav_panel(shiny::icon("circle-info"),
              markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)"))
  )
}

nav_card_home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    nav_content_server("volcano", "Volcano Plots", "Volcanoplot.PNG",
                       "A Volcano plot is a common way to visualize differential gene expression in RNA-Seq data. 
                       It displays log-fold changes on the x-axis and -log10 p-values on the y-axis. 
                       A volcano plot combines a measure of statistical significance from a statistical test 
                       with the magnitude of the change, enabling quick visual identification of those data-points (genes, etc.) 
                       that display large magnitude changes that are also statistically significant.")
    
    nav_content_server("dim_reduction", "Dimensionality Reduction", c("pca0.png", "mds.png"),
                       c("Principal Component Analysis (PCA) is a dimensionality reduction technique 
                         used to visualize the global structure and variability in the data. It helps 
                         identify sample groupings and outliers. A PCA plot shows clusters of samples 
                         based on their similarity. PCA does not discard any samples or characteristics (variables). 
                         Instead, it reduces the overwhelming number of dimensions by constructing 
                         principal components (PCs)",
                         "The MDS plot is an unsupervised clustering technique based on the 
                         eigenvalue decomposition of euclidean distances between samples based 
                         on their gene expression profiles. MDS plots are a means of visualizing
                         the level of similarity of individual cases of a dataset. In RNA-Seq experiments
                         MDS plots can be used to identify batch effects, patterns in correlation
                         between metadata and gene expression levels."))
    
    nav_content_server("heatmaps", "Heatmaps", "heatmap.png",
                       "Heatmaps are used to visualize the expression levels of genes across multiple samples. 
                       They help identify patterns and clusters in the data. Heatmaps can cluster together the 
                       genes with similar expression patterns. Thus, a cluster of up regulated genes can be 
                       distinguished from the set of down regulated genes.")
    
    nav_content_server("go_analysis", "GO Analysis", NULL, "Place holder text")
  })
}

nav_content_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("content"))
  )
}

nav_content_server <- function(id, title, image, description) {
  moduleServer(id, function(input, output, session) {
    output$content <- renderUI({
      tagList(
        h3(title),
        if (!is.null(image)) {
          if (length(image) > 1) {
            lapply(image, function(img) {
              tagList(
                div(img(src = img, height = 200, width = 250), style = "text-align: center"),
                br()
              )
            })
          } else {
            div(img(src = image, height = 200, width = 250), style = "text-align: center")
          }
        },
        if (length(description) > 1) {
          lapply(description, p)
        } else {
          p(description)
        }
      )
    })
  })
}
