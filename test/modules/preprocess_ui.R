source("functions/homepage.R")

preprocess_ui <- function(id) {
  ns <- NS(id)
  tabPanel(
    value = id,
    title = "Upload",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        style = "position:fixed;width:22%;",
        h4("Upload your data here!"),
        pickerInput(ns("filetype"), "Choose your data file type", choices = c("RSEM")),
        conditionalPanel(
          condition = "input.filetype == 'RSEM'",
          awesomeRadio(ns("fileexttype"), "Choose your file separator", choices = c(".csv" = ",", ".tsv" = "\t"), selected = ",", status = "success"),
          tags$hr(),
          fileInput(ns("file1"), "Upload RSEM Files for all the samples", multiple = TRUE),
          tags$hr(),
          fileInput(ns("file2"), "Upload Sample Key File here (.csv)", multiple = FALSE, accept = c(".csv")),
          tags$hr(),
          actionButton(ns("startanalysis"), "Start Analysis"),
          tags$hr(),
          actionButton(ns("viewNormDataBtn"), "Normalized Counts Data"),
          tags$hr(),
          downloadButton(ns("downloadDataBtn"), "Download Norm Counts Data")
        ),
        conditionalPanel(
          condition = "input.filetype == 'Count Matrix'",
          awesomeRadio(ns("fileexttype2"), "Choose your file separator", choices = c(".csv" = ",", ".tsv" = "\t"), selected = ",", status = "success"),
          fileInput(ns("file"), "Choose a count matrix file", multiple = FALSE),
          tags$hr(),
          fileInput(ns("metafile"), "Choose a Metadata file", multiple = FALSE),
          downloadButton(ns("templateDownload"), "Download Template")
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
            DTOutput(ns('raw.dataTable'))
          ),
          tags$hr(),
          h5("Step 2:"),
          p("Upload your sample key information using the left side panel.
                           Please follow the instructions regarding the sample key files regarding the column headers and format.
                             The column in 'Condition' will be considered to create a contrast matrix initially"),
          br(),
          div(
            box(h5("Sample Key Information")),
            DTOutput(ns('samplekeys'))
          ),
          tags$hr(),
          br(),
          verbatimTextOutput(ns('outputmessage1')),
          br(),
          tags$hr(),
          h5("Step 3:"),
          h6("Click on the View Design to see the contrast matrix. To begin with, the design consists only of the condition. 
                              You can perform the analysis by adding another feature to the design matrix if required"),
          br(),
          div(
            box(h5("Contrast Matrix")),
            DTOutput(ns('contr.mtrx'))
          ),
          tags$hr(),
          br(),
          h5("Step 4:"),
          h6("Click on the Start Analysis button to perform DEG Analysis using the
                           limma-voom method. TMM Normalization is performed on the data. The analysis takes
                           a short period of time to be completed. You can view the filtered and normalized data below
                           by clicking on the View Normalized Counts Data button."),
          br(),
          verbatimTextOutput(ns('outputmessage2')),
          br(),
          br(),
          tags$hr(),
          h5("Step 5:"),
          h6("Click on the Normalized Count Data button to see the normalized counts data.
                              You can even download the csv file using the download Norm Counts Data button"),
          div(
            box(h5("Normalized Count Matrix")),
            DTOutput(ns('norm.dataTable1'))
          ),
          br(),
          br(),
          p("The differential gene expression tables can be found on the DEG tables tab. Click below to
                             navigate to the tab. The MDS plot can be found in the Dimentionality Reduction tab."),
          br(),
          actionLink(ns("link_to_VP1"), label = "View Plots", icon("arrow")),
          br(),
          br()
        ),
        conditionalPanel(
          condition = "input.filetype == 'Count Matrix'",
          p("Upload a count matrix file in the following format:"),
          p("First column should be ENSEMBL. Remaining columns are samples where the columns are named in the following
                             manner: 'SampleID_Condition'. For example: NS2201_Control, NS2201_Treatment. 
                             The condition can be anything that you are comparing. It should be consistent throughout the samples and the metafile"),
          div(
            box(h5("Norm Count Matrix Table")),
            DTOutput(ns('norm.dataTable2'))
          ),
          tags$hr(),
          br(),
          p("Please upload the file in the following format: The file should be a tab separated file with 2 columns without column headers. You can find the template in the sidebar."),
          div(
            box(h5("Metadata")),
            DTOutput(ns('metafile.dataTable3'))
          )
        )
      )
    )
  )
}
