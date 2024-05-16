
server <- function(input, output, session){
  
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
  
  processed_deg.tables <- reactiveVal(NULL)
  # Select the dataset for plotting:
  processed.norm_data <- reactive({
    if (is.null(norm.counts()) && input$filetype != "Count Matrix") {
      dataSet5()
    } else if (!is.null(norm.counts())){
      # write.table(norm.counts(), "analysednormcounts.csv", sep = ',')
      norm.counts()
      
    } else {
      uploaded_norm.data()
    }
  })
  
  processed_deg.tables <- reactive({
    if (is.null(deg_table.final()) && input$filetype != "Count Matrix") {
      dataSet1()
    } else if (!is.null(deg_table.final())){
      format_change(deg_table.final())
    } else {
      verbatimTextOutput("No DEG tables uploaded")
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
  
  
  # observe the input data from server -----
  
  observe({
    # All plots will be under this bracket
    # Define all datasets first by pulling them based on the filters 
    
    # Plots as per the tabs
    my_data <- reactiveValues(
      x = list(title = list(text = '<b>log2(fold change)<b>', 
                            color = 'black', 
                            standoff = 20L),
               titlefont = list(size = 22), 
               tickfont = list(size = 18)),
      
      y = list(title = list(text = '<b>-log10(adjusted p-value)<b>', color = 'black'), 
               titlefont = list(size = 22), 
               tickfont = list(size = 18)),
      
      conf = list(format = 'svg', width = 800, height = 800)
    )
    
    output$RS_plot1 <- renderPlotly(
      {
        geneDiff <- processed_deg.tables()
        geneDiff <- process_diffs(diff = geneDiff,
                                  pval = as.numeric(input$RS_pval),
                                  logfc = as.numeric(input$RS_logfc))
        shapes <- get_lines(diff = geneDiff,
                            pval = as.numeric(input$RS_pval),
                            logfc = as.numeric(input$RS_logfc))
        up <- subset(geneDiff, geneDiff$group == 'up-regulated')
        not <- subset(geneDiff, geneDiff$group == 'unchanged')
        down <- subset(geneDiff, geneDiff$group == 'down-regulated')
        
        plot_ly(data = up,
                x = not$`Log2(FC)` ,
                y = -log10(not$`P-value`),
                type = 'scatter',
                mode = 'markers',
                height = 480,
                alpha = 0.3,
                marker = list(size = 10, color = 'red', opacity = 0.5),
                text = not$Gene,
                color = not$group,
                
                hovertemplate = paste(
                  "<b>Gene Name: %{text}</b><br>",
                  "<b>ENSEMBL ID:<b>", not$ENSG, "<br>",
                  "%{yaxis.title.text}: %{y:.3f}<br>",
                  "%{xaxis.title.text}: %{x:.2f}<br>",
                  "Mean of Normalized Counts: ", not$BaseAvg,
                  "<extra></extra>"
                )) %>%
          add_trace(x = up$`Log2(FC)` ,
                    y = -log10(up$`P-value`),
                    type = 'scatter',
                    mode = 'markers',
                    marker = list(size = 10, color = '#39FF14', opacity = 0.5),
                    text = up$Gene,
                    color = up$group,
                    
                    hovertemplate = paste(
                      "<b>Gene Name: %{text}</b><br>",
                      "<b>ENSEMBL ID:<b>", up$ENSG, "<br>",
                      "%{yaxis.title.text}: %{y:.3f}<br>",
                      "%{xaxis.title.text}: %{x:.2f}<br>",
                      "Mean of Normalized Counts: ", up$BaseAvg,
                      "<extra></extra>"
                    )) %>%
          add_trace(x = down$`Log2(FC)` ,
                    y = -log10(down$`P-value`),
                    type = 'scatter',
                    mode = 'markers',
                    marker = list(size = 10, color = 'yellow', opacity = 0.5),
                    text = down$Gene,
                    color = down$group,
                    
                    hovertemplate = paste(
                      "<b>Gene Name: %{text}</b><br>",
                      "<b>ENSEMBL ID:<b>", down$ENSG, "<br>",
                      "%{yaxis.title.text}: %{y:.3f}<br>",
                      "%{xaxis.title.text}: %{x:.2f}<br>",
                      "Mean of Normalized Counts: ", down$BaseAvg,
                      "<extra></extra>"
                    )) %>%
          layout(xaxis = my_data$x,
                 yaxis = my_data$y,
                 shapes = shapes,
                 legend = list(orientation = 'h',
                               xanchor = 'center',
                               x = 0.5,
                               y = -0.2,
                               font = list(size = 16))) %>%
          config(toImageButtonOptions = my_data$conf) %>%
          # makes background black and font white-
          layout(
            font = list(color = "white"),
            paper_bgcolor='rgba(0,0,0,0)',
            plot_bgcolor='rgba(0,0,0,0)'
          )
      })
    
    # PCA plots ----
    mds_data <- reactiveValues(
      conf = list(format = 'svg', width = 800, height = 800)
    )
    
    output$RS_pcaNorm <- renderPlotly({
      # print("Norm matrix see")
      # print(head(processed.norm_data()))
      
      conditions <- get_conditions(processed_metadata())
      counts_norm <- processed.norm_data()
      counts_norm <- counts_norm %>% 
        mutate_all(as.numeric)
      comp <- process_counts(counts_norm)
      
      plot_PCA(comp_data = comp, metadata.1 = processed_metadata(), conditions.1 = conditions, mds_data.1 = mds_data)
    })
    
    # MDS Plots -----
    
    # output$RS_mdsRaw <- renderPlotly({
    #   
    #   metadata <- read.table(paste(project_path, input$RS_projects, 
    #                                input$experiment, 'METADATA', sep = '/'), 
    #                          sep = '\t', 
    #                          header = F)
    #   conditions <- get_conditions(metadata)
    #   
    #   counts_raw <- dataSet4()
    #   counts_raw_df <- plotMDS(counts_raw, plot = F)
    #   counts_raw_dist <- as.data.frame(counts_raw_df$distance.matrix.squared)  
    #   counts_raw_scaled <- as.data.frame(cmdscale(counts_raw_dist, eig = FALSE))
    #   counts_raw_scaled$Sample_ID <- rownames(counts_raw_scaled)
    #   
    #   counts_raw_scaled <- add_groups(counts_raw_scaled, metadata, conditions)
    #   
    #   x <- counts_raw_scaled$V1
    #   y <- counts_raw_scaled$V2
    #   
    #   plot_ly(data = counts_raw_scaled[,2:3], 
    #           x = counts_raw_scaled$V1,
    #           y = counts_raw_scaled$V2, 
    #           type = 'scatter', 
    #           mode = 'markers', 
    #           text = counts_raw_scaled$Sample_ID,
    #           color = counts_raw_scaled$Group,
    #           # colors = c('blue', 'gray40', 'red', 'green'),
    #           marker = list(size = 15),
    #           hovertemplate = paste(
    #             "<b>Sample Name: %{text}</b><br>",
    #             "<b>Group Name: ", counts_raw_scaled$Group, "<br>",
    #             "<b>PC1: %{x:.2f}<br>",
    #             "<b>PC2: %{y:.2f}<br>"
    #           )) %>% 
    #     layout(xaxis = list(title = list(text = paste('<b>Leading log2(FC) dim 1<b>'),
    #                                      color = 'black', 
    #                                      standoff = 20L),
    #                         titlefont = list(size = 20), 
    #                         tickfont = list(size = 18)),  
    #            yaxis = list(title = list(text = paste('<b>Leading log(FC) dim 2<b>'),
    #                                      color = 'black'), 
    #                         titlefont = list(size = 20), 
    #                         tickfont = list(size = 18)),
    #            legend = list(orientation = 'h', 
    #                          xanchor = 'center',
    #                          x = 0.5, 
    #                          y = -0.2, 
    #                          font = list(size = 16)), 
    #            height = 480) %>% 
    #     config(toImageButtonOptions = mds_data$conf)     
    # })
    
    output$RS_mdsNorm <- renderPlotly({
      
      
      conditions <- get_conditions(processed_metadata())
      
      counts_norm <- processed.norm_data()
      counts_norm <- counts_norm %>% 
        mutate_all(as.numeric)
      print("Step 1")
      print(head(counts_norm))
      
      counts_norm_df <- plotMDS(counts_norm, plot = F)
      
      counts_norm_dist <- as.data.frame(counts_norm_df$distance.matrix)  
      counts_norm_scaled <- as.data.frame(cmdscale(counts_norm_dist))
      counts_norm_scaled$Sample_ID <- rownames(counts_norm_scaled)
      
      # This step gives problems:
      print("Step 2")
      print(head(counts_norm_scaled))
      counts_norm_scaled <- add_groups(counts_norm_scaled, processed_metadata(), conditions)
      
      print("Step 3")
      print(head(counts_norm_scaled))
      x <- counts_norm_scaled$V1
      y <- counts_norm_scaled$V2
      
      plot_ly(data = counts_norm_scaled[,2:3], 
              x = counts_norm_scaled$V1,
              y = counts_norm_scaled$V2, 
              type = 'scatter', 
              mode = 'markers', 
              text = counts_norm_scaled$Sample_ID,
              color = counts_norm_scaled$Group,
              # colors = c('blue', 'gray40', 'red', 'green'),
              marker = list(size = 15),
              hovertemplate = paste(
                "<b>Sample Name: %{text}</b><br>",
                "<b>Group Name: ", counts_norm_scaled$Group, "<br>",
                "<b>PC1: %{x:.2f}<br>",
                "<b>PC2: %{y:.2f}<br>"
              )) %>% 
        layout(xaxis = list(title = list(text = paste('<b>Leading log2(FC) dim 1<b>'),
                                         color = 'black', 
                                         standoff = 20L),
                            titlefont = list(size = 20), 
                            tickfont = list(size = 18)),  
               yaxis = list(title = list(text = paste('<b>Leading log(FC) dim 2<b>'),
                                         color = 'black'), 
                            titlefont = list(size = 20), 
                            tickfont = list(size = 18)),
               legend = list(orientation = 'h', 
                             xanchor = 'center',
                             x = 0.5, 
                             y = -0.2, 
                             font = list(size = 16)), 
               height = 480) %>% 
        config(toImageButtonOptions = mds_data$conf) %>%
        layout(
          font = list(color = "white"),
          paper_bgcolor='rgba(0,0,0,0)',
          plot_bgcolor='rgba(0,0,0,0)'
        )
    })
    
    # DEG Tables ----
    
    table1_deg <- reactive({
      req(input$RS_contrast)
      tmp1 <- subset(processed_deg.tables(), processed_deg.tables()$`P-value` <= as.numeric(paste(input$RS_pval_deg)) & 
                       abs(processed_deg.tables()$`Log2(FC)`) >= as.numeric(paste(input$RS_logfc_deg)))
      
      colnames(tmp1)[1] <- 'ENSG'
      
      tmp1 <- tmp1 %>% dplyr::select(Gene, `Log2(FC)`, `P-value`, 'ENSG', BaseAvg, 
                                     everything())
      
      
      tmp1 <- datatable(tmp1, style = "bootstrap4",
                        extensions = c('FixedColumns', 'FixedHeader', 'Scroller'),
                        class = 'display nowrap', rownames = T)
    })
    
    output$RS_table1.1_deg <- DT::renderDT(
      table1_deg()
    )
    
    # Heatmaps ----
    
    output$RS_pheatmap1.1 <- renderPlotly({
      req(input$contrast)
      # metadata <- read.table(paste(project_path, input$RS_projects, 
      #                              input$experiment, 'METADATA', sep = '/'), 
      #                        sep = '\t', 
      #                        header = F)
      # print(head(processed_metadata()))
      # print(head(processed.norm_data()))
      conditions <- get_conditions(processed_metadata())
      counts_raw <- as.data.frame(processed.norm_data())
      counts_raw <- rownames_to_column(counts_raw, var = 'ENSG')
      diff <- table1_deg()$x$data
      filtered <- counts_raw[counts_raw$ENSG %in% diff$ENSG, ]
      
      filtered_tf <- setNames(data.frame(t(filtered[,-1])), filtered[,1])
      rownames(filtered) <- NULL
      filtered <- column_to_rownames(filtered, var = 'ENSG')
      
      
      filtered_tf$Sample_ID <- rownames(filtered_tf)
      filtered_tf <- add_groups(filtered_tf, processed_metadata(), conditions)
      filtered_tf <- filtered_tf %>% dplyr::select(Sample_ID, Group)
      
      heatmaply_conf <- reactiveValues(
        conf = list(format = 'svg', width = 800, height = 800)
      )
      
      if (max(diff$`P-value` > 0.05)) {} else {
        heatmaply(x = filtered, 
                  k_row = 2, 
                  k_col = 3, 
                  scale = 'row',
                  colors = hm_colors,
                  showticklabels = c(T,F), 
                  col_side_colors = filtered_tf, 
                  side_color_colorbar_len = 0.5,
                  plot_method = 'plotly',
                  scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                    low = 'cyan', 
                    mid = 'black', 
                    high = 'red',
                    midpoint = 0)) %>% layout(height = 580) %>% 
          config(toImageButtonOptions = heatmaply_conf$conf)   
      }
    })
    
    # Enrichment analysis ----
    # GO Term analysis ----
    
    output$RS_GO <- renderPlotly({
      geneDiff <- processed_deg.tables()
      colnames(geneDiff)[1] <- 'ENSG'
      universe <- geneDiff$ENSG
      geneDiff <- geneDiff[geneDiff$`P-value` <= 0.05 & geneDiff$`Log2(FC)` >= 0.58, ]
      sem <- qs::qread("APP_DATA/UCSC.homo.sapiens.sem.qs")
      GO <- enrichGO(gene = geneDiff$ENSG,
                     OrgDb = 'org.Hs.eg.db',
                     keyType = 'ENSEMBL',
                     ont = 'BP',
                     pAdjustMethod = 'BH',
                     pvalueCutoff = 0.05,
                     qvalueCutoff = 0.05,
                     readable = T,
                     universe = universe)
      GO <- pairwise_termsim(GO, method = 'Wang', semData = sem)
      ggplotly(barplot(GO, showCategory = 20)) 
      # ggplotly(barplot(GO.down, showCategory = 20)) 
    })
    
    # observe function close  
  })
}