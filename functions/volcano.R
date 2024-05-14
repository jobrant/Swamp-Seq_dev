# Functions for plots
source('FUNCTIONS/helper_functions.R')
source('FUNCTIONS/global_options.R')



# define tabPanel---


# Define sidebar to select information-
sidebar_volcano <- sidebarPanel(
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
          height = 800,
          alpha = 0.3, 
          marker = list(size = 10, color = '#3fff0f', opacity = 0.5),
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
              marker = list(size = 10, color = 'yellow', opacity = 0.5),
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
              marker = list(size = 10, color = 'orange', opacity = 0.5),
              text = Not$Gene, 
              color = Not$group, 
              
              hovertemplate = paste(
                "<b>Gene Name: %{text}</b><br>",
                "<b>ENSEMBL ID:<b>", Not$ENSG, "<br>",
                "%{yaxis.title.text}: %{y:.3f}<br>",
                "%{xaxis.title.text}: %{x:.2f}<br>",
                "Mean of Normalized Counts: ", Not$BaseAvg, 
                "<extra></extra>"
              )) %>%
    layout(
      font = list(color = "white"),
      paper_bgcolor='rgba(0,0,0,0)',
      plot_bgcolor='rgba(0,0,0,0)'
    )
    
  
}