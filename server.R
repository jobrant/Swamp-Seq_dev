# server.R

server <- function(input, output, session){
  
  # Call the module servers
  volcanoPlotServer("volcano", processed_deg.tables)
  dimReductionServer("dimreduction", processed.norm_data, processed_metadata)
  heatmapServer("heatmap", processed_deg.tables, processed.norm_data, processed_metadata)
  enrichmentAnalysisServer("enrichment", processed_deg.tables)
  degTablesServer("deg_tables", processed_deg.tables)
  homepageServer("homepage")
  
  # Action Links to Volcano Plots
  observeEvent(input$link_to_PP, {
    updateNavbarPage(inputId="MainNavBar", selected = "upload")
  })
  
  # Take data from the server ----
  
  # Works on server
  output$RS_experiment <- renderUI({
    req(input$RS_projects)
    choices = get_experiments(paste(project_path, input$RS_projects, sep = '/'))
    pickerInput('RS_experiment', 
                'Select Experiment', 
                selected = 'GE5667',
                choices = choices, 
                multiple = TRUE, 
                options = pickerOptions(maxOptions = 1))
  })
  
  output$RS_contrast <- renderUI({
    req(input$RS_experiment)
    choices = getSheetNames(paste(project_path, input$RS_projects, input$RS_experiment, paste(input$RS_experiment, 'allgenediff.xlsx', sep = '-'), sep = '/'))
    choices = str_sub(choices, end = -13)
    pickerInput('RS_contrast',
                'Select Contrast',
                choices = choices,
                multiple = F, 
    )
  })
  
  # Take data from the user ----
  
  # Choose whether uploaded data is RSEM files or filtered/normalized counts
  raw.DGE <- reactiveVal(NULL)
  # uploaded_norm.data <- reactiveVal(NULL)
  
  raw.DGE <- reactive({
    req(input$file1)
    # Extract file paths
    file_paths <- input$file1$datapath
    
    # Filter files with names ending in 'results'
    valid_files <- grep("results$", input$file1$name, value = TRUE)
    
    if (length(valid_files) == 0) {
      return(showAlert("No valid files selected."))
    }
    
    sample.names <- input$file1$name
    # Create DGEList object from all files
    readDGE(files = file_paths, columns = c(1,5), labels = sample.names)
    
  })
  
  uploaded_norm.data <- reactive({
    # Process data for Count Matrix
    if (is.null(input$file)) return(NULL)  # Return NULL if no file is uploaded
    
    fileexttype2 <- input$fileexttype2
    if (is.null(fileexttype2)) return(NULL)  # Return NULL if fileType is not selected
    data <- read.delim2(input$file$datapath, sep = fileexttype2, header = TRUE)
    data <- data %>% remove_rownames %>% column_to_rownames(var="ENSEMBL")
    
  })
  
  output$templateDownload <- downloadHandler(
    filename = function() {
      templatedata
    },
    content = function(template) {
      write_tsv(templatedata, "template.tsv")
    }
  )
  
  uploaded_metadata <- reactive({
    if (is.null(input$metafile)) return(NULL)  # Return NULL if no file is uploaded
    data <- read.delim2(input$metafile$datapath, sep = '\t', header = FALSE)
  })
  
  # For outputs-
  
  output$raw.dataTable <- renderDT({
    # print(str(raw.DGE()))
    
    if (!is.null(raw.DGE())) {
      raw.counts <- as.data.frame(raw.DGE()$counts)
      # print(dim(raw.counts))
      datatable(raw.counts, options = list(
        lengthMenu = c(5, 10, 15),  # Set adjustable length options
        pageLength = 10,            # Set default page length
        scrollX = TRUE,             # Enable horizontal scrolling
        scrollY = "400px",          # Set vertical scrolling height
        searching = TRUE            # Enable searching
      ))
    }
  })
  
  output$norm.dataTable2 <- renderDT({
    if (!is.null(uploaded_norm.data())) {
      datatable(uploaded_norm.data(), options = list(
        lengthMenu = c(5, 10, 15),  # Set adjustable length options
        pageLength = 10,            # Set default page length
        scrollX = TRUE,             # Enable horizontal scrolling
        scrollY = "400px",          # Set vertical scrolling height
        searching = TRUE            # Enable searching
      ))
    }
  })
  
  output$metafile.dataTable3 <- renderDT({
    if (!is.null(uploaded_metadata())) {
      datatable(uploaded_metadata(), options = list(
        lengthMenu = c(5, 10, 15),  # Set adjustable length options
        pageLength = 10,            # Set default page length
        scrollX = TRUE,             # Enable horizontal scrolling
        scrollY = "400px",          # Set vertical scrolling height
        searching = TRUE            # Enable searching
      ))
    }
  })
  
  
  sample.key_data <- reactiveVal(NULL)
  
  sample.key_data <- reactive({
    req(input$file2)
    read.csv(input$file2$datapath, sep = ",", header = TRUE)
  })
  
  output$samplekeys <- DT::renderDT({
    datatable(sample.key_data(),  options = list(
      lengthMenu = c(5, 10, 15),  # Set adjustable length options
      pageLength = 10,            # Set default page length
      scrollX = TRUE,             # Enable horizontal scrolling
      scrollY = "400px",          # Set vertical scrolling height
      searching = TRUE            # Enable searching
    )
    )
  })
  
  final.dge <- reactiveVal(NULL)
  norm.counts <- reactiveVal(NULL)
  deg_table.final <- reactiveVal(NULL)
  
  observeEvent(input$file2,{
    final.dge(validate_datasets(raw.DGE(), sample.key_data()))
    output$outputmessage1 <- renderText(paste("Raw DGE object built, ready to launch the analysis"))
  })
  
  # Proceed with the analysis
  observeEvent(input$startanalysis, {
    # Obtain filtered and normalized object back
    data <- process_dge(final.dge())
    # print(str(final.dge))
    
    norm.counts(as.data.frame(cpm(data)))
    # print(dim(norm.counts()))
    # print(head(norm.counts()))
    
    deg_table.final(fit_a_model(final.dge()))
    
    # print(length(which(deg_table.final()$adj.P.Val < 0.05)))
    n <- length(which(deg_table.final()$adj.P.Val < 0.05))
    # print(head(deg_table.final())) # This needs to be directed to DEG table tab
    
    output$outputmessage2 <- renderText(paste("Analysis complete", n, " genes have p value less than 0.05"))
  })
  
  observeEvent(input$viewNormDataBtn, {
    output$norm.dataTable1 <- renderDT({
      # print("Hello")
      # print(dim(norm.counts()))
      if (!is.null(norm.counts)){
        datatable(norm.counts(), options = list(
          lengthMenu = c(5, 10, 15),  # Set adjustable length options
          pageLength = 10,            # Set default page length
          scrollX = TRUE,             # Enable horizontal scrolling
          scrollY = "400px",          # Set vertical scrolling height
          searching = TRUE            # Enable searching
        )
        )
      }
    })
  })
  
  # Download processed data as CSV
  output$downloadDataBtn <- downloadHandler(
    filename =  function() {
      paste('norm.counts.csv')
    },
    # content is a function with argument file. content writes the plot to the device
    content = function(filename) {
      print(head(as.data.frame(norm.counts())))
      write.csv(as.data.frame(norm.counts()), "norm.counts.csv")
    } 
    
  )
  
  # Action Links to Volcano Plots
  observeEvent(input$link_to_VP1, {
    updateNavbarPage(inputId="preprocess", selected = "volcano_plots")
  })
  
  # Fetch datasets from the server
  dataSet1 <- reactiveVal(NULL)
  dataSet5 <- reactiveVal(NULL)
  metadata5 <- reactiveVal(NULL)
  
  dataSet1 <- reactive({
    req(input$RS_contrast)
    allgenediff <- paste(project_path, input$RS_projects,
                         input$RS_experiment, paste(input$RS_contrast, 'allGeneDiff.csv', sep = '.'), sep = '/')
    fread(file = paste(allgenediff), sep = "\t", data.table = F)
  })
  
  
  dataSet5 <- reactive({
    req(input$RS_experiment)
    norm <- paste(project_path, input$RS_projects, 
                  input$RS_experiment, paste(input$RS_experiment, 'g.deseq2norm.csv', sep = '.'), sep = '/')
    read.table(file = paste(norm), header = T, sep = '\t')
  })
  
  metadata5 <- reactive({
    print(paste(project_path, input$RS_projects, 
                input$RS_experiment, 'METADATA', sep = '/'))
    read.table(paste(project_path, input$RS_projects, 
                     input$RS_experiment, 'METADATA', sep = '/'), 
               sep = '\t', 
               header = F)
  })
  
  output$dataTable2 <- DT::renderDT(
    datatable(dataSet5(), options = list(
      lengthMenu = c(5, 10, 15),  # Set adjustable length options
      pageLength = 10,            # Set default page length
      scrollX = TRUE,             # Enable horizontal scrolling
      scrollY = "400px",          # Set vertical scrolling height
      searching = TRUE            # Enable searching
    )
    )
  )
  
  processed_deg.tables <- reactive({
    if (is.null(deg_table.final()) && input$filetype != "Count Matrix") {
      dataSet1()
    } else if (!is.null(deg_table.final())){
      format_change(deg_table.final())
    } else {
      NULL  # Changed from verbatimTextOutput to NULL
    }
  })
  
  processed.norm_data <- reactive({
    if (is.null(norm.counts()) && input$filetype != "Count Matrix") {
      dataSet5()
    } else if (!is.null(norm.counts())){
      norm.counts()
    } else {
      uploaded_norm.data()
    }
  })
  
  processed_metadata <- reactive({
    if (is.null(deg_table.final()) && input$filetype != "Count Matrix") {
      metadata5()
    } else if (!is.null(deg_table.final())){
      create.metadata(sample.key_data())
    } else {
      uploaded_metadata()
    }
  })
  
} # close server