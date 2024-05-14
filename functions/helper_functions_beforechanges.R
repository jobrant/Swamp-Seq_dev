# HELPER FUNCTIONS

process_diffs <- function(diff) {
  colnames(diff)[1] <- 'ENSG'
  diff <- diff %>% dplyr::select(ENSG, Gene, `Log2(FC)`, `P-value`, BaseAvg)
  diff$BaseAvg <- as.numeric(format(round(diff$BaseAvg, 0)))
  diff['group'] <- 'NotSignificant'
  # 0.58 needs to be changed to 'RS_pval' and RS_logfc'
  diff$group[diff$`P-value` < 0.05 & abs(diff$`Log2(FC)`) < 0.58] <- 'Significant'
  diff$group[diff$`P-value` > 0.05 & abs(diff$`Log2(FC)`) > 0.58] <- 'FoldChange'
  diff$group[diff$`P-value` < 0.05 & abs(diff$`Log2(FC)`) > 0.58] <- 'Significant&FoldChange'
  return(diff)
}

get_lines <- function(diff) {
  shapes <- list(list(type = 'line', 
                      x0 = 0.58, 
                      x1 = 0.58, 
                      y0 = min(-log10(diff$`P-value`)) -2, 
                      y1 = max(-log10(diff$`P-value`)), 
                      line = list(dash = 'dot', width = 1)), 
                 list(type = 'line', 
                      x0 = -0.58, 
                      x1 = -0.58, 
                      y0 = min(-log10(diff$`P-value`)) -2, 
                      y1 = max(-log10(diff$`P-value`)), 
                      line = list(dash = 'dot', width = 1)), 
                 list(type = 'line', 
                      x0 = min(diff$`Log2(FC)`), 
                      x1 = max(diff$`Log2(FC)`), 
                      y0 = 1.3, 
                      y1 = 1.3,
                      line = list(dash = 'dot', width = 1)))
  return(shapes)
}


process_counts <- function(counts) {
  counts$name <- rownames(counts)
  counts <- counts %>% select(name, everything())
  counts_tf <- setNames(data.frame(t(counts[,-1])), counts[,1])
  comp <- prcomp(counts_tf[,2:ncol(counts_tf)], retx = T, scale. = T)
  return(comp)
}

get_ve <- function(comp) {
  ve <-comp$sdev^2 / sum(comp$sdev^2)
  return(ve[1:2])
}

get_experiments <- function(x) {
  paths <- list.files(path = x,
                      pattern = 'METADATA', 
                      full.names = T, 
                      recursive = T)
  dirs <- basename(dirname(paths))
  return(dirs)
}


get_conditions <- function(metadata) {
  conditions <- str_split(metadata[metadata$V1 == 'CONDITIONS', ][[2]], 
                          pattern = ',', 
                          simplify = T)
  return(conditions)
}


add_groups <- function(comp_df, metadata, conditions) {
  df_list <- list()
  sample_list <- list()
  for (i in seq_along(conditions)) {
    df_list[[i]] <- subset(metadata, metadata$V1 == paste(conditions[i]))
    sample_list[[i]] <- gsub('-', '.', str_split(df_list[[i]]$V2, 
                                                 pattern = ',',
                                                 simplify = T))
    comp_df$Group[comp_df$Sample_ID %in%
                       sample_list[[i]]] <- paste(df_list[[i]]$V1)
  
  }
  return(comp_df)
}
# blabla <- kalyanee
# project_path <- "C:/Users/kshirlekar/Desktop/R fun/Unfinished_App"
# choices = get_experiments(paste(project_path, "kalyanee", sep = '/'))
# 
# paths <- list.files(path = "C:/Users/kshirlekar/Desktop/R fun/Unfinished_App/kalyanee",
#                     pattern = 'METADATA',
#                     full.names = T,
#                     recursive = T)
# paths
# dirs <- basename(dirname(paths))
# 
# choices = get_experiments(paste(project_path, "kalyanee", sep = '/'))
# getSheetNames(paste(project_path, input$projects,
#                     input$experiment, paste(input$experiment, 'allgenediff.xlsx', sep = '-'), sep = '/'))
# 
# choices = getSheetNames(paste("C:/Users/kshirlekar/Desktop/R fun/Unfinished_App", "kalyanee", "Experiment1",
#                     paste("Novogene-Daphne-noout-062619", 'allgenediff.xlsx', sep = '-'), sep = '/'))
# choices1 = str_sub(choices, end = -13)
# pickerInput('contrast',
#             'Select Contrast',
#             choices = choices,
#             multiple = F,
# )