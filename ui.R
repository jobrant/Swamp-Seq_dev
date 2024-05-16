source("functions/homepage.R")

ui <- navbarPage(id="MainNavBar", position = "fixed-top",
                 theme = bs_theme(bootswatch = "darkly",
                                  base_font = font_google("Inter")),
                 tags$script(HTML("
                  var header = $('.navbar > .container-fluid');
                  var navbarHeight = $('.navbar').height();
                  header.append('<div style=\"float:right; height:' + 
                  navbarHeight + 
                  'px;\"><a href=\"https://cancer.ufl.edu/\"><img src=\"UFH_CancerCenter.jpg\" alt=\"alt\" style=\"width:100%; height:100%;\"></a></div>');
")),
                 title = tags$strong("SWAMP-SEQ"),
                 br(), br(), br(), br(),
                 
                 # Home Tab ----
                 nav_panel(value = "home", 
                          title = tags$strong("Home"),
                          wellPanel(style = "background-color: rgb(34, 34, 34) !important;text-align: center;",
                                    HTML('<h2 style="font-size: 40px;">Welcome to the SWAMP-SEQ Visualization Toolkit <img src="cropped.gif" style="width:100px;height:85px;"></h2>')),
                          br(),
                          homepage_info,
                          br(), br(),
                          intro_content,
                          br(),
                          actionLink("link_to_PP", label = "Get Started", icon("circle-xmark")),
                          br(), br(), br(),
                          h5("Instructions"),
                          h6("This is an automated do your own RNA-Seq Visualization toolkit.
                             Please follow the guidelines below to use the toolkit."),
                          br(),
                          h6("1. Process-Data Tab:"),
                          h6("There are two tabs in the 'Process Data' tab: upload your data or
                             load your data from the VM. Both these data sources work on a different
                             RNA Seq pipeline. The data obtained directly from the VM has been analysed
                             using Alberto's pipeline which uses DeSeq2 package
                             whereas the RNA-Seq analysis provided by this toolkit uses edgeR. Both the methods
                             have the same assumptions to begin with that 'No gene is differentially
                             expressed' but offer a different take in distribution and normalization methods."),
                          h6("a. Upload"),
                          h6("This tab can take normalized and filtered counts matrix or RSEM files. RSEM is a software package for 
                             estimating gene levels from single-end or paired-end RNA-Seq data. You can upload
                             multiple RSEM files across the conditions. These files can be generated using 
                             nf-core/rnaseq pipeline. For more information, please contact BCB @ UFHCC. '
                             Please upload either multiple RSEM files or a single count matrix file. Select the seperator. 
                             If any of the filters are missed, the app will give an error."),
                          h6("Select RSEM files that are clearly named. The name of the file can be 'SampleID1_Condition.results'
                             and 'SampleID1_Treatment.results. You can view the list of the selected files and click on one
                             file to see the contents of the file below"),
                          
                          h6("Next select sample keys, demographic information to be used in the design matrix. 
                                Ensure that there is one column named as 'Sample_Name' which matches the names of the 
                                samples uploaded and another column named 'Conditions' which has the 'Control' and the 
                                'Treatment' as the permissible values. The remaining features can be anything however 
                                they have to be converted as binary factors to be used in the calculations"),
                          h6("Next you can 'Start Analysis'. This will create a DGEList object in the backend used
                             by edgeR to proceed with filtering, normalization and building a negative binomial model using voom.
                             Once the analysis is complete, you can view the number of differentially expressed genes.
                             Click on 'View Normalized Count Data' to see the matrix or download it as csv"),
                          h6("b. Load"),
                          h6("This tab provides the user with an ability to load the RNA-Seq data from Alberto's
                             pipeline in the toolkit to visualize various plots. Select the project, experiment and contrast
                             and proceed to the plot of desire. There are Volcano plots, heatmaps, dimentionality reduction plots and 
                             gene enrichment analysis."),
                          br(), br(),
                          nav_card_home
                 ),
                 # Preprocess Tab ----
                 navbarMenu(
                   
                   title = div(tags$strong("Process-Data"), tags$i(style = "color: rgb(0,166,90)")),
                   menuName = "preprocess",
                   nav_panel(
                     # upload ----
                     value = "upload",
                     title = "Upload",
                     
                     sidebarLayout(
                       
                       sidebarPanel(
                         width = 3,
                         style = "position:fixed;width:22%;",
                         h4("Upload your data here!"),
                         pickerInput("filetype", "Choose your data file type",
                                     choices = c("RSEM", "Count Matrix")),
                         conditionalPanel(
                           condition = "input.filetype == 'RSEM'",
                           awesomeRadio(
                             inputId = "fileexttype",
                             label = "Choose your file separator",
                             choices = c(".csv" = ",", ".tsv" = "\t"),
                             selected = ",",
                             status = "success"
                           ),
                           tags$hr(),
                           fileInput("file1", "Upload RSEM Files for all the samples",
                                     multiple = TRUE),
                           tags$hr(),
                           fileInput("file2", "Upload Sample Key File here (.csv)",
                                     multiple = FALSE, accept = c(".csv")),
                           tags$hr(),
                           actionButton("startanalysis", "Start Analysis"),
                           tags$hr(),
                           actionButton("viewNormDataBtn", "Normalized Counts Data"),
                           tags$hr(),
                           downloadButton(outputId = "downloadDataBtn", label = "Download Norm Counts Data")
                         ),
                         conditionalPanel(
                           condition = "input.filetype == 'Count Matrix'",
                           awesomeRadio(
                             inputId = "fileexttype2",
                             label = "Choose your file separator",
                             choices = c(".csv" = ",", ".tsv" = "\t"),
                             selected = ",",
                             status = "success"
                           ),
                           fileInput("file", "Choose a count matrix file", multiple = FALSE),
                           tags$hr(),
                           fileInput("metafile", "Choose a Metadata file", multiple = FALSE),
                           downloadButton("templateDownload", "Download Template")
                         )
                       ),
                       mainPanel(
                         h4("Start your differential gene expression analysis here:"),
                         br(),
                         
                         conditionalPanel(
                           condition = "input.filetype == 'RSEM'",
                           h5("Step 1:"),
                           h6("Upload your RSEM files and sample key files using the left panel to perform your own differential gene expression analysis.
                         View the raw count matrix once all the files are uploaded. Scroll below to view your sample key information."),
                           br(),
                           div(
                             box(h5("Raw Count Matrix Table")),
                             DTOutput('raw.dataTable')
                           ),
                           tags$hr(),
                           h5("Step 2:"),
                           p("Upload your sample key information using the left side panel.
                           Please follow the instructions regarding the sample key files regarding the column headers and format.
                             The column in 'Condition' will be considered to create a contrast matrix initially"),
                           br(),
                           div(
                             box(h5("Sample Key Information")),
                             DTOutput('samplekeys')
                           ),
                           tags$hr(),
                           br(),
                           verbatimTextOutput('outputmessage1'),
                           br(),
                           tags$hr(),
                           h5("Step 3:"),
                           h6("Click on the View Design to see the contrast matrix. To begin with, the design consists only of the condition. 
                              You can perform the analysis by adding another feature to the design matrix if required"),
                           br(),
                           div(
                             box(h5("Contrast Matrix")),
                             DTOutput('contr.mtrx')
                           ),
                           tags$hr(),
                           br(),
                           h5("Step 4:"),
                           h6("Click on the Start Analysis button to perform DEG Analysis using the
                           limma-voom method. TMM Normalization is performed on the data. The analysis takes
                           a short period of time to be completed. You can view the filtered and normalized data below
                           by clicking on the View Normalized Counts Data button."),
                           
                           br(),
                           verbatimTextOutput('outputmessage2'),
                           br(), br(),
                           tags$hr(),
                           h5("Step 5:"),
                           h6("Click on the Normalized Count Data button to see the normalized counts data.
                              You can even download the csv file using the download Norm Counts Data button"),
                           
                           div(
                             box(h5("Normalized Count Matrix")),
                             DTOutput('norm.dataTable1')
                           ),
                           br(), br(),
                           p("The differential gene expression tables can be found on the DEG tables tab. Click below to
                             navigate to the tab. The MDS plot can be found in the Dimentionality Reduction tab."),
                           br(),
                           actionLink("link_to_VP1", label = "View Plots", icon("arrow-right")),
                           br(), br()
                         ),
                         conditionalPanel(
                           condition = "input.filetype == 'Count Matrix'",
                           p("Upload a count matrix file in the following format:"),
                           p("First column should be ENSEMBL. Remaining columns are samples where the columns are named in the following
                             manner: 'SampleID_Condition'. For example: NS2201_Control, NS2201_Treatment. 
                             The condition can be anything that you are comparing. It should be consistent throughout the samples and the metafile"),
                           div(
                             box(h5("Norm Count Matrix Table")),
                             DTOutput('norm.dataTable2')
                           ),
                           tags$hr(),
                           br(),
                           p("Please upload the file in the following format: \n 
                            The file should be a tab separated file with 2 columns without column headers.
                             You can find the template in the sidebar."),
                           
                           div(
                             box(h5("Metadata")),
                             DTOutput('metafile.dataTable3')
                           )
                           
                         )
                       )
                     )
                     
                   ),
                   nav_panel(
                     # server -----
                     value = "server",
                     title = "Server",
                     
                     sidebarLayout(
                       sidebarPanel(
                         h4("Load your data from the server!"),
                         pickerInput(
                           inputId = "RS_projects",
                           label = "Select Project",
                           choices = c('UTX', 'H2B', 'NSD2', 'SINGLE_CELL', 'KMT2C'),
                           selected = 'UTX',
                           options = list(
                             title = "Select dataset")
                         ),
                         tags$hr(),
                         uiOutput('RS_experiment'),
                         tags$hr(),
                         uiOutput('RS_contrast'),
                         br()
                       ),
                       
                       mainPanel(
                         # Display normalized counts-
                         h4("View Normalized Count Matrix"),
                         navset_card_tab(
                           height = 550,
                           full_screen = TRUE,
                           # width = 6,
                           title = "Normalized Count Matrix",
                           nav_panel('Table', DT::DTOutput('dataTable2'))
                         ),
                       )
                     )
                     
                   )
                   
                   
                 ),
                 
                 nav_panel(
                   # Volcano tab ----
                   value = "volcano_plots",
                   title = div(tags$strong("Volcano Plots"), tags$i(style = "color: rgb(0,166,90)")),
                   h6("Use the left side panel to change the filters"),
                   br(), br(),
                   sidebarLayout(
                     position = "left",
                     fluid = TRUE,
                     sidebarPanel(
                       
                       # sidebar volcano ----
                       width = 4,
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
                     ),
                     mainPanel(
                       # main Panel -----
                       h4("Volcano Plots", style = "text-align: center"),
                       box(width = 11,
                           status = "warning",
                           solidHeader = TRUE,
                           plotlyOutput(width = "100%", height = "600px", 'RS_plot1')
                       ),
                       br(), br(), br(), br(), br(), br(), br(), br(), br(), br(),
                       h6("Use the options available on the right side corner to 
           navigate your way on the figure.")
                     )
                   )
                   
                 ),
                 nav_panel(
                   # DR tab ----
                   value = "dimentionality_reduction", 
                   title = div(tags$strong("Dimentionality Reduction")),
                   br(),
                   sidebarLayout(
                     position = "left",
                     fluid = TRUE,
                     sidebarPanel(
                       # sidebar PCA ----
                       width = 4,
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
                         status = "success")
                     ),
                     mainPanel(
                       fluidRow(
                         box(
                           headerBorder = TRUE,
                           title = h6('Principle Component Analysis-Norm', style = 'font-size:28px'),
                           height = 540,
                           width = 6,
                           id = 'PCA-Norm',
                           plotlyOutput('RS_pcaNorm')
                         )),
                       br(), br(), br(), br(), br(), br(), br(),
                       fluidRow(
                         box(
                           headerBorder = TRUE,
                           title = h6('Multidimentional Scaling-Norm', style = 'font-size:28px'),
                           height = 540,
                           width = 6,
                           id = 'MDS-Norm',
                           plotlyOutput('RS_mdsNorm')
                         )
                       )
                     )
                   )
                   
                 ),
                 nav_panel(
                   # Heatmaps tab ----
                   value = "heatmaps",
                   title = div(tags$strong("Heatmaps")),
                   
                   # Heatmaps ----
                   sidebarLayout(
                     sidebarPanel(
                       width = 4,
                       
                       awesomeRadio(
                         inputId = "RS_pval_hm",
                         label = 'Filter Expression Data Tables by Adjusted p-value',
                         choices = list(1, 0.05, 0.01),
                         selected =  0.05,
                         status = "success"),
                       
                       awesomeRadio(
                         inputId = 'RS_logfc_hm',
                         label = 'Filter Expression Data Tables by log2(FC)',
                         choices = list(0, 0.58, 1),
                         selected = 0.58,
                         status = "success")
                     ),
                     mainPanel = mainPanel(
                       h3("Heatmaps here"),
                       width = 8,
                       status = "warning",
                       solidHeader = TRUE,
                       plotlyOutput(outputId = 'RS_pheatmap1.1', width = "100%")
                     )
                   )
                   
                 ),
                 nav_panel(
                   # DEG tab ----
                   value = "deg_tables",
                   title = div(tags$strong("DEG Tables")),
                   h4("Different Gene Expression Tables"),
                   sidebarLayout(
                     
                     sidebarPanel(
                       br(),
                       br(),
                       width = 4,
                       awesomeRadio(
                         inputId = "RS_pval_deg",
                         label = 'Filter Expression Data Tables by Adjusted p-value', 
                         choices = list(1, 0.05, 0.01),
                         selected =  0.05,
                         status = "success"),
                       
                       awesomeRadio(
                         inputId = 'RS_logfc_deg',
                         label = 'Filter Expression Data Tables by log2(FC)',
                         choices = list(0, 0.58, 1),
                         selected = 0.58,
                         status = "success")
                     ),
                     
                     mainPanel(
                       width = 8,
                       # mainpanel---------
                       navset_card_tab(
                         height = 650,
                         full_screen = FALSE,
                         title = "View your data",
                         nav_panel('Table', DT::DTOutput('RS_table1.1_deg'))
                       )
                     )
                   )
                 ),
                 nav_panel(
                   # Enrichment Analysis ----
                   value = "enrich_analysis",
                   title = div(tags$strong("Enrichment Analysis")),
                   br(),
                   sidebarLayout(
                     sidebarPanel(
                       br(), br(),
                       width = 4,
                       awesomeRadio(
                         inputId = "RS_pval_ea",
                         label = 'Filter Expression Data Tables by Adjusted p-value', 
                         choices = list(1, 0.05, 0.01),
                         selected =  0.05,
                         status = "success"),
                       
                       awesomeRadio(
                         inputId = 'RS_logfc_ea',
                         label = 'Filter Expression Data Tables by log2(FC)',
                         choices = list(0, 0.58, 1),
                         selected = 0.58,
                         status = "success")
                       
                     ),
                     mainPanel(
                       h3("Enrichment Analysis: Tree Plots"),
                       br(),
                       plotlyOutput(outputId = 'RS_GO', height = "900px"),
                       )
                   ),
                 ),
                 nav_panel(value = "info",
                          title = div(tags$strong("About")),
                          sidebarLayout(
                            sidebarPanel(width = 1 , id = "mySidebar"),
                            mainPanel(width = 8, offset = 1,
                                      h3("About this toolkit:", style = "color:white; font-weight: 50"),
                                      p("This is an interactive visualization toolkit for reports generated by DIBIG TOOLs pipeline on the Lichtlab server.
                       A brief summary of all the functionalities offered by the toolkit are provided below."),
                                      h3("Contact:", style = "color:white; font-weight: 50"),
                                      p("This toolkit is developed by  Dr. Jason O Brant and 
                     Kalyanee Shirlekar, BCB-UF Health Cancer Center. 
                     If you have any trouble accessing the content on this
                       application or have any questions or feedback related to
                       the functionality, you can contact us here: "),
                                      tags$li("jobrant@ufl.edu"),
                                      tags$li("kshirlekar@ufl.edu"))))
)