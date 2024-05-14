# HELPER FUNCTIONS

process_diffs <- function(diff, pval, logfc) {
  colnames(diff)[1] <- 'ENSG'
  diff <- diff %>% dplyr::select(ENSG, Gene, `Log2(FC)`, `P-value`, BaseAvg)
  diff$BaseAvg <- as.numeric(format(round(diff$BaseAvg, 0)))
  diff['group'] <- 'unchanged'
  diff$group[diff$`P-value` <= pval & diff$`Log2(FC)` > logfc] <- 'up-regulated'
  diff$group[diff$`P-value` <= pval & diff$`Log2(FC)` < (logfc * - 1)] <- 'down-regulated'
  return(diff)
}

get_lines <- function(diff, pval, logfc) {
  shapes <- list(list(type = 'line', 
                      x0 = logfc, 
                      x1 = logfc, 
                      y0 = min(-log10(diff$`P-value`)) -2, 
                      y1 = max(-log10(diff$`P-value`)), 
                      line = list(dash = 'dot', width = 1)), 
                 list(type = 'line', 
                      x0 = (logfc * - 1), 
                      x1 = (logfc * - 1), 
                      y0 = min(-log10(diff$`P-value`)) -2, 
                      y1 = max(-log10(diff$`P-value`)), 
                      line = list(dash = 'dot', width = 1)), 
                 list(type = 'line', 
                      x0 = min(diff$`Log2(FC)`), 
                      x1 = max(diff$`Log2(FC)`), 
                      y0 = -log10(pval), 
                      y1 = -log10(pval),
                      line = list(dash = 'dot', width = 1)))
  return(shapes)
}


process_counts <- function(counts) {
  counts$name <- rownames(counts)
  counts <- counts %>% dplyr::select(name, everything())
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
  conditions <- str_split(metadata[metadata[,1] == 'CONDITIONS', ][[2]], 
                          pattern = ',', 
                          simplify = T)
  return(conditions)
}


add_groups <- function(comp_df, metadata, conditions) {
  df_list <- list()
  sample_list <- list()
  for (i in seq_along(conditions)) {
    df_list[[i]] <- subset(metadata, metadata[,1] == paste(conditions[i]))
    sample_list[[i]] <- gsub('-', '.', str_split(df_list[[i]][,2], 
                                                 pattern = ',',
                                                 simplify = T))
    comp_df$Group[comp_df$Sample_ID %in%
                       sample_list[[i]]] <- paste(df_list[[i]][,1])
  
  }
  return(comp_df)
}


# # # # RNA Seq functions
# rsem_dir <- "C:/Users/kshirlekar/OneDrive - University of Florida/Work Stuff/R fun/Testing Lewks/Dashboard/Dashboards/Filestotest"
# ## Reads in all the tsv files into a edgeR DGE object
# files.list <- list.files(path = rsem_dir, pattern = 'results$', full.names = T)
# # Using EdgeR
# ## Read files into a DGEList object
# x <- readDGE(files.list, columns = c(1,5)) # gene name and expected count columns
# # Gets just the base sample name
# orig_names <- str_sub(string = basename(colnames(x)))
# rownames(x$samples) <- orig_names
# colnames(x$counts) <- orig_names
# dge.obj <- x
# 
# samplekeyinfo <- read.csv("C:/Users/kshirlekar/OneDrive - University of Florida/Work Stuff/R fun/Testing Lewks/Dashboard/Dashboards/Filestotest/Demo.csv")

# # # Clean and convert into factor variables- sample key info
# data1 <- x$counts

validate_datasets <- function(data1, data2){
  if (is.null(data1) || is.null(data2)) {
    return(NULL)
  }
  
  sample_names_data1 <- colnames(data1$counts)
  # print(sample_names_data1)
  
  if (all(endsWith(sample_names_data1, suffix = "results") == TRUE)){
    sample_names_data1 <- str_sub(sample_names_data1, end = -9)
    } else {
      sample_names_data1
      }
  sample_names_data2 <- data2$Sample_Name

  
  if (!identical(sample_names_data1, sample_names_data2)) {
    showModal(modalDialog(
      title = "Validation Error",
      "Sample names are not consistent between RSEM files and Sample Keys. Please upload again with matching sample names.",
      easyClose = TRUE
    ))

  } else {
    showModal(modalDialog(
      title = "Validation Success",
      "Sample names match. Raw DGE object created. You can proceed with the analysis",
      easyClose = TRUE
    ))
    
    # Merge the two datasets-
    colnames(data1$counts) <- sample_names_data1
    rownames(data1$samples) <- sample_names_data1
    data1$samples$Sample_Name <- sample_names_data1
    data1$samples <- left_join(data1$samples, data2, by = 'Sample_Name')
    print("Objects joined")
    
    # Add genes
    # dge.obj$samples[,c(6:9)] <- lapply((dge.obj$samples[,c(6:9)]), as.factor)
    
    # Add Gene ids
    require(Homo.sapiens)
    geneid <- rownames(data1)
    genes <- AnnotationDbi::select(Homo.sapiens,
                                   keys = geneid,
                                   columns = 'SYMBOL',
                                   keytype = 'ENSEMBL')
    genes <- genes[!duplicated(genes$ENSEMBL),]
    data1$genes <- genes
  }
  
  return(data1)
}



process_dge <- function(dge.obj){
  ## Gets just the base sample name
  # orig_names <- str_sub(string = basename(colnames(dge.obj)), end = -9)
  # colnames(dge.obj$counts) <- orig_names
  # rownames(dge.obj$samples) <- orig_names
  # 
  # # print("Orig names")
  # # print(orig_names)
  # # 
  # # print("Sample keys Info")
  # # print(samplekeyinfo$Sample_Name)
  # 
  # ## Add demographic information to DGEList
  # dge.obj$samples$Sample_Name <- orig_names
  # 
  # # print("DGE obj Sample names")
  # # print(dge.obj$samples$Sample_Name)
  # 
  # # dge.obj$samples$Sample_Name == samplekeyinfo$Sample_Name
  # 
  # # dge.obj$samples <- left_join(dge.obj$samples, samplekeyinfo, by = 'Sample_Name')
  # # # Let us join the samples in dge.obj and sample.keys
  # 
  # # if (all(dge.obj$samples$Sample_Name %in% samplekeyinfo$Sample_Name) && all(samplekeyinfo$Sample_Name %in% dge.obj$samples$Sample_Name)){
  # 
  #   if(all(dge.obj$samples$Sample_Name == samplekeyinfo$Sample_Name)){
  #   dge.obj$samples <- left_join(dge.obj$samples, samplekeyinfo, by = 'Sample_Name')
  #   print("objects joined")
  # } else {
  #   print("Sample_Names do not match")
  #   }
  # 
  # # Convert the columns into factors
  # dge.obj$samples[,c(6:9)] <- lapply((dge.obj$samples[,c(6:9)]), as.factor)
  # 
  # # Add Gene ids
  # require(Homo.sapiens)
  # geneid <- rownames(dge.obj)
  # genes <- AnnotationDbi::select(Homo.sapiens,
  #                                keys = geneid,
  #                                columns = 'SYMBOL',
  #                                keytype = 'ENSEMBL')
  # genes <- genes[!duplicated(genes$ENSEMBL),]
  # dge.obj$genes <- genes
  
  treatment <- as.factor(dge.obj$samples$Condition)
  
  # print(treatment)
# filter_and_norm <- function(x){
  ## Gets the counts per million (cpm) and the log CPM
  cpm <- cpm(dge.obj)
  print(head(cpm))
  lcpm <- cpm(dge.obj, log = T)
  
  L <- mean(dge.obj$samples$lib.size) * 1e-6
  M <- median(dge.obj$samples$lib.size) * 1e-6
  
  ## Filters genes based on expression level
  keep.exprs <- filterByExpr(dge.obj, group = treatment)
  dge.obj <- dge.obj[keep.exprs,, keep.lib.sizes = F] # keeps the well expressed genes, 
  # removes the read counts from the library sizes too
  
  ## Calculate Normalization Factors
  dge.obj <- calcNormFactors(dge.obj, method = 'TMM')
  
  # print(dge.obj$samples)
  # print(dge.obj$counts)
  # print(cpm(dge.obj, log = TRUE))
  
  return(dge.obj)
  
}
  
create.metadata <- function(samplekeyinfo) {
  emptydata <- templatedata
  emptydata[1,2] <- as.character(Sys.time())
  emptydata[2,2] <- "Dummy Experiment"
  conditions <- unique(samplekeyinfo$Condition)
  emptydata[4,2] <- as.character(paste0(conditions[1], ",", conditions[2])) # toString(unique(samplekeyinfo$Condition))
  emptydata[5,2] <- as.character(paste0(conditions[1], "^", conditions[2]))
  controls <- paste0((samplekeyinfo[grepl("Control$", samplekeyinfo$Sample_Name),1]), collapse = ',')
  emptydata[6,2] <- controls
  treatments <- paste0((samplekeyinfo[grepl("Treatment$", samplekeyinfo$Sample_Name),1]), collapse = ',')
  emptydata[7,2] <- treatments
  return(emptydata)
}

  # # M and L are mean and median of library sizes
  # lcpm.cutoff <- log2(10/M + 2/L)
  # 
  # nsamples <- ncol(dge.obj)
  # 
  # col <- brewer.pal(nsamples, "Paired")
  # par(mfrow=c(1,2))
  # 
  # plot(density(lcpm[,1]), col=col[1], lwd=2, ylim=c(0,0.26), las=2, main="", xlab="")
  # title(main="A. Raw data", xlab="Log-cpm")
  # abline(v=lcpm.cutoff, lty=3)
  # 
  # for (i in 2:nsamples){
  #   den <- density(lcpm[,i])
  #   lines(den$dge.obj, den$y, col=col[i], lwd=2)
  # }
  # legend("topright", samplenames, text.col=col, bty="n")
  # lcpm <- cpm(dge.obj, log=TRUE)
  # 
  # plot(density(lcpm[,1]), col=col[1], lwd=2, ylim=c(0,0.26), las=2, main="", xlab="")
  # title(main="B. Filtered data", xlab="Log-cpm")
  # abline(v=lcpm.cutoff, lty=3)
  # 
  # for (i in 2:nsamples){
  #   den <- density(lcpm[,i])
  #   lines(den$dge.obj, den$y, col=col[i], lwd=2)
  # }
  # legend("topright", samplenames, text.col=col, bty="n")
  # par(mfrow=c(1,1))
  
fit_a_model <- function(dge.obj){
  
  treatment <- as.factor(dge.obj$samples$Condition)
  
  ## Sets up the design matrix and contrasts
  design.1 <- model.matrix(~ 0 + treatment)
  rownames(design.1) <- dge.obj$samples$Sample_Name
  
  # Need to understand the meaning of this-
  colnames(design.1) <- gsub("treatment", "", colnames(design.1))
  
  # Make contrast table:
  contr.matrix <- makeContrasts(
    Treatment-Control,
    levels = colnames(design.1))
  
  # Voom:
  #par(mfrow=c(1,2))
  v <- voom(dge.obj, design.1, plot = F)
  
  # Fitting linear model in limma-
  
  vfit <- lmFit(v, design.1)
  
  # head(coef(vfit))
  
  # Estimate contrasts for each gene-
  vfit <- contrasts.fit(vfit, contrasts=contr.matrix)
  
  efit <- eBayes(vfit)
  
  # Plot the efit after smoothing of standard errors
  #plotSA(efit, main="Final model: Mean-variance trend")
  #par(mfrow=c(1,1))
  
  top.table <- topTable(efit, sort.by = "P", n = Inf)
  return(top.table)
}


format_change <- function(mydata){
  mydata <- mydata[-8]
  rownames(mydata) <- NULL
  colnames(mydata)[colnames(mydata) == "ENSEMBL"] <- '#ENSG'
  colnames(mydata)[colnames(mydata) == "SYMBOL"] <- 'Gene'
  colnames(mydata)[colnames(mydata) == "logFC"] <- 'Log2(FC)'
  colnames(mydata)[colnames(mydata) == "AveExpr"] <- 'BaseAvg'
  colnames(mydata)[colnames(mydata) == "P.Value"] <- 'P-value'
  mydata$Biotype <- "Unknown"
  return(mydata)
}
