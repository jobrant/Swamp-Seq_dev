
source('FUNCTIONS/helper_functions.R')
source('FUNCTIONS/global_options.R')
source('FUNCTIONS/plotting_functions.R')
source('FUNCTIONS/homepage.R')
source('FUNCTIONS/dim_red.R')
source('FUNCTIONS/volcano.R')


z <- c("limma", "edgeR", "tidyverse", "openxlsx", 'pheatmap', 'data.table',
       'org.Hs.eg.db', 'Homo.sapiens', 'RColorBrewer', 'ggrepel', 'hues',
       'Biobase', 'stringr', 'ggfortify', 'qs', 'kableExtra', 'patchwork', 'openxlsx', 'knitr',
       'gtools')
aa <- lapply(z, require, character.only = T)

read.files <- function(file.list){
  x <- readDGE(files.list, columns = c(1,5))
  return(x)
}
