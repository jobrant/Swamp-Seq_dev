# Functions for plots
source('FUNCTIONS/helper_functions.R')
source('FUNCTIONS/global_options.R')

# Volcano plots
plot_volcano <- function(dataset) {
  
  codingDiff <- dataset()
  codingDiff <- process_diffs(codingDiff)
  sigFC <- subset(codingDiff, codingDiff$group == 'Significant&FoldChange')
  sig <- subset(codingDiff, codingDiff$group == 'Significant')
  FC <- subset(codingDiff, codingDiff$group == 'FoldChange')
  Not <- subset(codingDiff, codingDiff$group == 'NotSignificant')
  
  plot_ly(data = sigFC, 
          x = sigFC$`Log2(FC)` ,
          y = -log10(sigFC$`P-value`), 
          type = 'scatter', 
          mode = 'markers',
          height = 480,
          alpha = 0.3, 
          marker = list(size = 10, color = 'green', opacity = 0.5),
          text = sigFC$Gene, 
          color = sigFC$group, 
          
          hovertemplate = paste(
            "<b>Gene Name: %{text}</b><br>",
            "<b>ENSEMBL ID:<b>", sigFC$ENSG, "<br>",
            "%{yaxis.title.text}: %{y:.3f}<br>",
            "%{xaxis.title.text}: %{x:.2f}<br>",
            "Mean of Normalized Counts: ", sigFC$BaseAvg, 
            "<extra></extra>"
          )) %>% 
    add_trace(x = sig$`Log2(FC)` ,
              y = -log10(sig$`P-value`), 
              type = 'scatter', 
              mode = 'markers',
              marker = list(size = 10, color = 'red', opacity = 0.5),
              text = sig$Gene, 
              color = sig$group, 
              
              hovertemplate = paste(
                "<b>Gene Name: %{text}</b><br>",
                "<b>ENSEMBL ID:<b>", sig$ENSG, "<br>",
                "%{yaxis.title.text}: %{y:.3f}<br>",
                "%{xaxis.title.text}: %{x:.2f}<br>",
                "Mean of Normalized Counts: ", sig$BaseAvg, 
                "<extra></extra>"
              )) %>% 
    add_trace(x = FC$`Log2(FC)` ,
              y = -log10(FC$`P-value`), 
              type = 'scatter', 
              mode = 'markers',
              marker = list(size = 10, color = 'blue', opacity = 0.5),
              text = FC$Gene, 
              color = FC$group, 
              
              hovertemplate = paste(
                "<b>Gene Name: %{text}</b><br>",
                "<b>ENSEMBL ID:<b>", FC$ENSG, "<br>",
                "%{yaxis.title.text}: %{y:.3f}<br>",
                "%{xaxis.title.text}: %{x:.2f}<br>",
                "Mean of Normalized Counts: ", FC$BaseAvg, 
                "<extra></extra>"
              )) %>% 
    add_trace(x = Not$`Log2(FC)` ,
              y = -log10(Not$`P-value`), 
              type = 'scatter', 
              mode = 'markers',
              marker = list(size = 10, color = 'gray', opacity = 0.5),
              text = Not$Gene, 
              color = Not$group, 
              
              hovertemplate = paste(
                "<b>Gene Name: %{text}</b><br>",
                "<b>ENSEMBL ID:<b>", Not$ENSG, "<br>",
                "%{yaxis.title.text}: %{y:.3f}<br>",
                "%{xaxis.title.text}: %{x:.2f}<br>",
                "Mean of Normalized Counts: ", Not$BaseAvg, 
                "<extra></extra>"
              ))
}
  
# plot PCA

plot_PCA <- function(comp_data, metadata.1, conditions.1, mds_data.1) {
  
  ve <- get_ve(comp_data)
  comp_df <- data.frame(comp_data$x)
  comp_df$Sample_ID <- rownames(comp_df)
  comp_df <- add_groups(comp_df, metadata.1, conditions.1)
  x <- comp_df$PC1
  y <- comp_df$PC2
  name <- comp_df$Sample_ID
  
  plot_ly(comp_df, 
          x = x,
          y = y,
          type = 'scatter',
          mode = 'markers',
          height = 480,
          text = comp_df$Sample_ID,
          color = comp_df$Group,
          marker = list(size = 15),
          hovertemplate = paste(
            "<b>Sample Name: %{text}</b><br>", 
            "<b>Group: <b>", comp_df$Group, "<br>",
            "<b>PC1: %{x:.2f}<br>",
            "<b>PC2: %{y:.2f}<br>"
            # "<extra></extra>"
          )
  ) %>% 
    layout(xaxis = list(title = list(text = paste('<b>Principle Component 1 <b>', '(', 
                                                  format(ve[1] * 100, digits = 3), '%', ')', sep = ''),
                                     color = 'black', 
                                     standoff = 20L),
                        titlefont = list(size = 20), 
                        tickfont = list(size = 18)),  
           yaxis = list(title = list(text = paste('<b>Principle Component 2 <b>', '(', 
                                                  format(ve[2] * 100, digits = 3), '%', ')', sep = ''),
                                     color = 'black'), 
                        titlefont = list(size = 20), 
                        tickfont = list(size = 18)),
           legend = list(orientation = 'h', 
                         xanchor = 'center',
                         x = 0.5, 
                         y = -0.2, 
                         font = list(size = 16))) %>% 
    config(toImageButtonOptions = mds_data.1$conf) %>%
    # makes background black and font white-
    layout(
      font = list(color = "white"),
      paper_bgcolor='rgba(0,0,0,0)',
      plot_bgcolor='rgba(0,0,0,0)'
    )
}

# MDS plot

process_mds <- function(dataset, conditions.1, metadata.1){
  counts_raw <- dataset
  counts_raw_df <- plotMDS(counts_raw, plot = F)
  counts_raw_dist <- as.data.frame(counts_raw_df$distance.matrix)
  counts_raw_scaled <- as.data.frame(cmdscale(counts_raw_dist))
  counts_raw_scaled$Sample_ID <- rownames(counts_raw_scaled)

  counts_raw_scaled <- add_groups(counts_raw_scaled, metadata.1, conditions.1)
  return(counts_raw_scaled)
# 
#   x <- counts_raw_scaled$V1
#   y <- counts_raw_scaled$V2
}

plot_MDS <- function(counts_raw_scaled, mds_data.2){
  # counts_raw <- counts
  # counts_raw_df <- plotMDS(counts, plot = F)
  # counts_raw_dist <- as.data.frame(counts_raw_df$distance.matrix)
  # counts_raw_scaled <- as.data.frame(cmdscale(counts_raw_dist))
  # counts_raw_scaled$Sample_ID <- rownames(counts_raw_scaled)
  # 
  # counts_raw_scaled <- add_groups(counts_raw_scaled, metadata.2, conditions.2)
  # 
  # x <- counts_raw_scaled$V1
  # y <- counts_raw_scaled$V2
  # 
  counts_raw_scaled.filt <- counts_raw_scaled[,2:3]
  plot_ly(data = counts_raw_scaled.filt,
          # x = counts_raw_scaled$V1,
          # y = counts_raw_scaled$V2,
          type = 'scatter',
          mode = 'markers',
          text = counts_raw_scaled$Sample_ID,
          color = counts_raw_scaled$Group,
          # colors = c('blue', 'gray40', 'red', 'green'),
          marker = list(size = 15),
          hovertemplate = paste(
            "<b>Sample Name: %{text}</b><br>",
            "<b>Group Name: ", counts_raw_scaled$Group, "<br>",
            "<b>PC1: %{x:.2f}<br>",
            "<b>PC2: %{y:.2f}<br>"
          ))
    # layout(xaxis = list(title = list(text = paste('<b>Leading log2(FC) dim 1<b>'),
    #                                  color = 'black',
    #                                  standoff = 20L),
    #                     titlefont = list(size = 20),
    #                     tickfont = list(size = 18)),
    #        yaxis = list(title = list(text = paste('<b>Leading log(FC) dim 2<b>'),
    #                                  color = 'black'),
    #                     titlefont = list(size = 20),
    #                     tickfont = list(size = 18)),
    #        legend = list(orientation = 'h',
    #                      xanchor = 'center',
    #                      x = 0.5,
    #                      y = -0.2,
    #                      font = list(size = 16)),
    #        height = 480)
    # config(toImageButtonOptions = mds_data.2$conf)
}
# Plot for heatmaps