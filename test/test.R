
project_path <- "C:/Users/kshirlekar/OneDrive - University of Florida/Work Stuff/R fun/Unfinished_App"
experiment <- 'Novogene-Daphne-noout-062619'
RS_projects <- 'kalyanee'
metadata <- read.table(paste(project_path, RS_projects, 
                             experiment, 'METADATA', sep = '/'), 
                       sep = '\t', 
                       header = F)
conditions <- get_conditions(metadata)

raw <- paste(project_path, RS_projects, 
             experiment, 'genes.rawmatrix.R.csv', sep = '/')
cat(file=stderr(), "dataSet4: ", raw, "\n")
dataset4 <- read.table(file = paste(raw), header = T, sep = '\t')

counts_norm_df <- plotMDS(dataset4, plot = F)
counts_norm_dist <- as.data.frame(counts_norm_df$distance.matrix)
counts_norm_scaled <- as.data.frame(cmdscale(counts_norm_dist))
counts_norm_scaled$Sample_ID <- rownames(counts_norm_scaled)

counts_norm_scaled <- add_groups(counts_norm_scaled, metadata, conditions)

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
        ))

counts_raw_scaled <- process_mds(data = dataset4, conditions.1 = conditions, metadata.1 = metadata)
