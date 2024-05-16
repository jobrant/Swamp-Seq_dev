dim_red_server <- function(id, input, output, session) {
  ns <- session$ns
  
  # Placeholder reactive expressions to simulate the fetching of data
  processed_metadata <- reactive({
    # Replace this with the actual logic to fetch or process metadata
    # For example, reading from a file or a reactiveVal
    metadata <- data.frame(
      Sample_ID = c("Sample1", "Sample2", "Sample3"),
      Group = c("Control", "Treatment", "Control")
    )
    return(metadata)
  })
  
  processed_norm_data <- reactive({
    # Replace this with the actual logic to fetch or process normalized count data
    # For example, reading from a file or a reactiveVal
    norm_data <- matrix(rnorm(300), nrow = 3, ncol = 100)
    rownames(norm_data) <- c("Sample1", "Sample2", "Sample3")
    return(norm_data)
  })
  
  output$pcaPlot <- renderPlotly({
    req(processed_norm_data(), processed_metadata())
    
    # PCA Calculation
    norm_data <- processed_norm_data()
    metadata <- processed_metadata()
    
    pca_res <- prcomp(t(norm_data), scale. = TRUE)
    pca_data <- as.data.frame(pca_res$x)
    pca_data$Sample <- rownames(pca_data)
    pca_data <- merge(pca_data, metadata, by.x = "Sample", by.y = "Sample_ID")
    
    # Plotting PCA
    p <- plot_ly(
      data = pca_data,
      x = ~PC1,
      y = ~PC2,
      text = ~Sample,
      color = ~Group,  # Assuming 'Group' is a column in your metadata
      type = 'scatter',
      mode = 'markers'
    ) %>%
      layout(
        title = "PCA Plot",
        xaxis = list(title = "PC1"),
        yaxis = list(title = "PC2"),
        hovermode = "closest"
      )
    
    p  # Return the plotly plot
  })
  
  output$mdsPlot <- renderPlotly({
    req(processed_norm_data(), processed_metadata())
    
    # MDS Calculation
    norm_data <- processed_norm_data()
    metadata <- processed_metadata()
    
    dist_matrix <- dist(t(norm_data))
    mds_res <- cmdscale(dist_matrix)
    mds_data <- as.data.frame(mds_res)
    mds_data$Sample <- rownames(mds_data)
    mds_data <- merge(mds_data, metadata, by.x = "Sample", by.y = "Sample_ID")
    
    # Plotting MDS
    p <- plot_ly(
      data = mds_data,
      x = ~V1,
      y = ~V2,
      text = ~Sample,
      color = ~Group,  # Assuming 'Group' is a column in your metadata
      type = 'scatter',
      mode = 'markers'
    ) %>%
      layout(
        title = "MDS Plot",
        xaxis = list(title = "Dimension 1"),
        yaxis = list(title = "Dimension 2"),
        hovermode = "closest"
      )
    
    p  # Return the plotly plot
  })
}
