preprocess_server <- function(id, input, output, session) {
  ns <- session$ns
  
  raw.DGE <- reactiveVal(NULL)
  uploaded_norm.data <- reactiveVal(NULL)
  sample.key_data <- reactiveVal(NULL)
  final.dge <- reactiveVal(NULL)
  norm.counts <- reactiveVal(NULL)
  deg_table.final <- reactiveVal(NULL)
  
  raw.DGE <- reactive({
    req(input$file1)
    file_paths <- input$file1$datapath
    valid_files <- grep("results$", input$file1$name, value = TRUE)
    if (length(valid_files) == 0) {
      return(showAlert("No valid files selected."))
    }
    sample.names <- input$file1$name
    readDGE(files = file_paths, columns = c(1,5), labels = sample.names)
  })
  
  uploaded_norm.data <- reactive({
    if (is.null(input$file)) return(NULL)
    fileexttype2 <- input$fileexttype2
    if (is.null(fileexttype2)) return(NULL)
    data <- read.delim2(input$file$datapath, sep = fileexttype2, header = TRUE)
    data <- data %>% remove_rownames %>% column_to_rownames(var="ENSEMBL")
  })
  
  output$templateDownload <- downloadHandler(
    filename = function() {
      "template.tsv"
    },
    content = function(file) {
      write_tsv(templatedata, file)
    }
  )
  
  uploaded_metadata <- reactive({
    if (is.null(input$metafile)) return(NULL)
    data <- read.delim2(input$metafile$datapath, sep = '\t', header = FALSE)
  })
  
  output$raw.dataTable <- renderDT({
    if (!is.null(raw.DGE())) {
      raw.counts <- as.data.frame(raw.DGE()$counts)
      datatable(raw.counts, options = list(
        lengthMenu = c(5, 10, 15), 
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "400px",
        searching = TRUE
      ))
    }
  })
  
  output$norm.dataTable2 <- renderDT({
    if (!is.null(uploaded_norm.data())) {
      datatable(uploaded_norm.data(), options = list(
        lengthMenu = c(5, 10, 15),
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "400px",
        searching = TRUE
      ))
    }
  })
  
  output$metafile.dataTable3 <- renderDT({
    if (!is.null(uploaded_metadata())) {
      datatable(uploaded_metadata(), options = list(
        lengthMenu = c(5, 10, 15),
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "400px",
        searching = TRUE
      ))
    }
  })
  
  sample.key_data <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath, sep = ",", header = TRUE)
  })
  
  output$samplekeys <- renderDT({
    datatable(sample.key_data(), options = list(
      lengthMenu = c(5, 10, 15),
      pageLength = 10,
      scrollX = TRUE,
      scrollY = "400px",
      searching = TRUE
    ))
  })
  
  observeEvent(input$file2, {
    final.dge(validate_datasets(raw.DGE(), sample.key_data()))
    output$outputmessage1 <- renderText(paste("Raw DGE object built, ready to launch the analysis"))
  })
  
  observeEvent(input$startanalysis, {
    data <- process_dge(final.dge())
    norm.counts(as.data.frame(cpm(data)))
    deg_table.final(fit_a_model(final.dge()))
    n <- length(which(deg_table.final()$adj.P.Val < 0.05))
    output$outputmessage2 <- renderText(paste("Analysis complete", n, " genes have p value less than 0.05"))
  })
  
  observeEvent(input$viewNormDataBtn, {
    output$norm.dataTable1 <- renderDT({
      if (!is.null(norm.counts)){
        datatable(norm.counts(), options = list(
          lengthMenu = c(5, 10, 15),
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "400px",
          searching = TRUE
        ))
      }
    })
  })
  
  output$downloadDataBtn <- downloadHandler(
    filename =  function() {
      'norm.counts.csv'
    },
    content = function(file) {
      write.csv(as.data.frame(norm.counts()), file)
    } 
  )
  
  # More observeEvent or reactive code specific to preprocess module...
}
