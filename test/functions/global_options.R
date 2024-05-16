# Global options

options(shiny.maxRequestSize = 1000*4096^2)

expTblOptions <- list(paging = T, 
                      dom = 'ft', 
                      scrollX = T, 
                      fixedColumns = T, 
                      scroller = T, 
                      scrollY = 365, 
                      rownames = F,
                      order = list(3, 'asc'), 
                      columnDefs = list(list(
                        className = 'dt-center', 
                        targets = '_all')))

cols = rep('black', 3)
hm_colors <- colorRampPalette(c("cyan", "black", "red"))(n = 50)

colors = paste0("color:",cols,";")
project_path <- '/var/www/html/reports'
templatedata <- read_tsv("template.tsv", col_names = F)

ax <- list(
  zeroline = T,
  showline = T,
  mirror = 'ticks',
  gridcolor = toRGB('gray90'), 
  gridwidth = .8, 
  zerolinecolor = toRGB('black'), 
  zerolinewidth = 1,
  linecolor = toRGB('black'), 
  linwidth = 2
)

x <- list(title = '<b>log2(fold change)<b>', color = 'black')
y <- list(title = '<b>-log10(adjusted p-value)<b>', color = 'black')
