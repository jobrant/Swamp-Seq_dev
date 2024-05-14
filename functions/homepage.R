

# sidebarpanel for the home page
# Either upload the count matrix or rsem files or load your data from the VM

sidepanel_home <- sidebarPanel(width = 3,
                               h4("Upload your data here!"),
                               awesomeRadio(
                                 inputId = "filetype",
                                 label = "Choose your data file type", 
                                 choices = c("RSEM", "Counts.csv (Filtered and Normalized)"),
                                 selected = "RSEM",
                                 status = "primary"
                               ),
                               fileInput("file1", "Upload here",
                                         multiple = TRUE),
                               tags$hr(),
                               h4("Load your data from the server!"),
                               pickerInput(
                                 inputId = "RS_projects",
                                 label = "Select Project",
                                 choices = c('kalyanee', 'Hermione'), 
                                 selected = 'kalyanee',
                                 options = list(
                                   title = "Select dataset")
                                 # choicesOpt = list(style = colors)
                               ), 
                               
                               uiOutput('RS_experiment'),
                               uiOutput('RS_contrast'),
                               
                               # To be added to the next tab-
                               # radioButtons(
                               #   inputId = 'RS_pval', 
                               #   label = 'Filter Expression Data Tables by Adjusted p-value',
                               #   choiceNames = list('No Filter', '0.05', '0.01'),
                               #   choiceValues = list(1, 0.05, 0.01), 
                               #   selected = 0.05,
                               #   inline = T), 
                               # 
                               # radioButtons(
                               #   inputId = 'RS_logfc', 
                               #   label = 'Filter Expression Data Tables by log2(FC)',
                               #   choiceNames = list('No Filter', '0.58', '1.0'),
                               #   choiceValues = list(0, 0.58, 1), 
                               #   inline = T),
                               br(),
                               actionButton("toggleSidebar", "Toggle sidebar"),
)



nav_card_home <- bslib::navset_card_tab(
  height = 450,
  full_screen = FALSE,
  # width = 6,
  title = div(img(src="help.png", width= 15), tags$strong("Information Center", style = "color:white")),
  nav_panel(
    "Volcano Plots",
    card_title("Volcano plots of Count matrix"),
    div(img(src = "Volcanoplot.PNG", 
            height = 200, 
            width = 250), style = "text-align: center"),
    p("A Volcano plot is a common way to visualize 
                   differential gene expression in RNA-Seq data. 
                   It displays log-fold changes on the x-axis and
                   -log10 p-values on the y-axis.
                   A volcano plot combines a measure of statistical 
                   significance from a statistical test 
                   with the magnitude of the change, enabling quick 
                   visual identification of those data-points (genes, etc.) 
                   that display large magnitude changes that are 
                   also statistically significant."),
    # actionLink("link_to_PP", label = "Get Started", icon("circle-xmark"))
  ),
  nav_panel(
    # nav panel for MDS PCA --------
    "Dimentionality Reduction",
    card_title("PCA plot"),
    div(img(src = "pca0.png", 
            height = 200, 
            width = 250), style = "text-align: center"),
    p("Principal Component Analysis (PCA) is a dimensionality reduction technique 
                   used to visualize the global structure and variability in the data. It helps 
                   identify sample groupings and outliers. A PCA plot shows clusters of samples 
                   based on their similarity. 
                   PCA does not discard any samples or characteristics (variables). 
                   Instead, it reduces the overwhelming number of dimensions by constructing 
                   principal components (PCs)"),
    br(),
    div(img(src = "mds.png", 
            height = 200, 
            width = 250), style = "text-align: center"),
    p("The MDS plot is an unsupervised clustering technique based on the 
                   eigenvalue decomposition of euclidean distances between samples based 
                   on their gene expression profiles. MDS plots are a means of visualizing
                   the level of similarity of individual cases of a dataset. In RNA-Seq experiments
                   MDS plots can be used to identify batch effects, patterns in correlation
                   between metadata and gene expression levels."),
  ),
  nav_panel(
    # nav panel for heatmaps -----
    "Heatmaps",
    card_title("Heatmaps"),
    div(img(src = "heatmap.png", 
            height = 200, 
            width = 250), style = "text-align: center"),
    p("Heatmaps are used to visualize the expression levels of genes across multiple samples. 
                   They help identify patterns and clusters in the data. Heatmaps can cluster together the 
                   genes with similar expression patters. Thus, a cluster of up regulated genes can be 
                   distinguished from the set of down regulated genes.")
  ),
  nav_panel(
    "Gene Ontology Analysis",
    card_title("GO analysis"),
    p("Place holder text")
  ),
  nav_panel(
    shiny::icon("circle-info"),
    markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
  )
)


homepage_info <- fluidRow(
  
  column(
    3,
    value_box(
      width = 3, 
      height = 250,
      title = " ",
      value = "Differential Expression",
      showcase = bs_icon("bar-chart-steps"),
      theme_color = "success",
      p("Perform DE analysis on your counts data or load datasets from the server"),
      # showcase_left_center(width = 0.3, max_height = "15")
      style = "background-color: #3498DB !important;" 
    )
  ),
  
  column(
    3,
    value_box(
      width = 3, 
      height = 250,
      title = " ",
      value = "Interactive Tables",
      showcase = bs_icon("table"),
      theme_color = "success",
      p("Obtain list of top differentially expressed genes"),
      # p("The 3rd detail")
    )),
  
  column(
    3,
    value_box(
      width = 3, 
      height = 250,
      title = " ",
      value = "Visualizations",
      showcase = bs_icon("bar-chart-line-fill"),
      # theme_color = "success",
      p("Plot volcano plots, heatmaps, dimentionality reduction plots and more"),
      # p("The 3rd detail")
      style = "background-color: #3498DB !important;"
    )
  ),
  
  column(
    3,
    value_box(
      width = 3, 
      height = 250,
      title = " ",
      value = "Downstream Analysis",
      showcase = bs_icon("diagram-3-fill"),
      theme_color = "success",
      p("Conduct downstream analysis such as enrichment analysis"),
      # p("The 3rd detail")
      # style = "background-color: purple !important;"
    )
    
  )
  
  
)


intro_content <- h5("SEQU-VIZ provides an interactive and user-friendly interface to analyse RNA-Seq Datasets. 
                    The data can be accessed directly from the Licht lab server or uploaded externally. 
                    There are numerous vizzualizations available to perform quality control, filtering and 
                    normalization of the data. You can also perform dimentionality reduction using principle 
                    component analysis and multidimensional scaling analysis. Furthermore, the app also allows the 
                    user to conduct your own enrichment analysis.")


## Unused content
# vbs,
#                 tags$head(
#                   tags$style(HTML("
#                 .1st value {
# background-color: purple !important;  /* Set a soft pastel blue background color */
# color: black !important;  /* Set a custom text color */
# width: 250px !important;   /* Set a custom width */
# height: 200px !important;  /* Set a custom height */
#                 }"))) ,

# showcase_left_center(
#   width = 0.3,
#   max_height = "100px",
#   max_height_full_screen = 0.67
# ),
# layout_column_wrap(
#   width = "100px",
#   !!!vbs
# ),
# Add custom theme to the boxes


tags$head(
  tags$style(HTML("
                          .soft-blue-box {
          background-color: orange !important;  /* Set a soft pastel blue background color */
          color: black !important;  /* Set a custom text color */
          width: 250px !important;   /* Set a custom width */
          height: 200px !important;  /* Set a custom height */
          border: 2px grey !important;  /* Set a white border with 2px width */
          border-radius: 10px !important;  /* Set border-radius for curved edges */
          display: flex;
          flex-direction: column;
          align-items: center;
          justify-content: center;
        }
        .soft-blue-box h3, .soft-blue-box p, .soft-blue-box .icon {
          text-align: center !important;  /* Center-align the h3, p, and icon elements inside the box */
        }
                          
                                          "))
)


# h2("Welcome to our RNA-Seq Analysis App!"),

# fluidRow(
#   column(3,
# value_box(
#   title = "1st value",
#   value = "123",
#   width = 3, 
#   height = 200,
#   full_screen = FALSE,
#   showcase = bs_icon("bar-chart"),
#   theme_color = "success",
#   p("The 1st detail")
# ))),

# layout_column_wrap(
#   width = "20px"),
# Feature Highlights
# fluidRow(
#   column(
#     4,
#     div(class = "soft-blue-box", icon("chart-bar"), h3("Differential Expression"), p("Perform DE analysis with ease."))
#   ),
#   column(
#     4,
#     div(class = "soft-blue-box", icon("table"), h3("Interactive Tables"), p("Explore your data using interactive tables."))
#   ),
#   column(
#     4,
#     div(class = "soft-blue-box", icon("chart-line"), h3("Visualization"), p("Visualize results with interactive plots."))
#   )
# ),

