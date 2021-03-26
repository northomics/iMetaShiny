# updatelog
# 
# 20180305: add function to plot a composition bar
# 20180305: add function from taxid to tax table
# 20180128: add a function to turn factor into colors
# 20180130: add a multiplot function in case it is not availabe in the 
# 20180130: add a handy function to plot the density plot, histogram in a plot, a lot of options available
# 20180115: add a mew function to plot the maxquant summary, check the the example for the required data format
# 20180110: revised version of matrix_missingvalue_filtering,  a new function name and 
# 20180109: revised/ a function to tidy proteingroups
# 20180109: revised/add a function to filter proteingroups by reverse, and concaminant
# 20180105: add a function from sthda to add grid on the scattter3d
# 20180104: add a function for changing color transparency
# 20171219: add a withCosoleRedirect function to direct the error messge to the browser
# 20171218: add a abundance support to compare to list: compare_two_vectorlist_with_value
# 20171216: specify package name to recode
# 20171212: add three functions, remove_allNA_rows, remove_allNA_columns, remove_1st_column


# special plot for maxquant result QC, to plot selected plot using data.frame with the follwing columns
# 1st column as sample names,
# 2nd column as values to plot
# columns after are meta information, usually the grouping information
# see example for the format
# this function has a newly desgined struction, to flexibly plot as required, instead of plotting all
# this is achieved by using a list structure to store the plot as required





# ggplot(data_frame) +
#   geom_point(aes_string(col_names[1], col_names[2],
#                         shape = shape_factor,
#                         color = shape_factor
#   ), 
#   size = 4,
#   alpha =  0.5
#   )







# since the colorbrewer only has limited number of colors, this function can generate defined number of colors based on the colorbrewer
ColorGenerator <- function(ncolors = 10, 
                           Rcolorbrewer_schemes =  "div", # this only works when Rcolorbrewer is selected, c("seq","qual","div")
                           RcolorBrewer_theme = "Spectral" # this only works when the right Rcolorbrewer and schemes are selected 
                           # For scheme "seq",  check the available options by: rownames(brewer.pal.info[brewer.pal.info$category == "seq",])
                           # for scheme "qual", check by: rownames(brewer.pal.info[brewer.pal.info$category == "qual",])
                           # for scheme "div", check by: rownames(brewer.pal.info[brewer.pal.info$category == "div",])
                           ){
  install.packages.auto("RColorBrewer")
  
  # get the max number of colors in the spectra
  max_color <- brewer.pal.info[which(rownames(brewer.pal.info) == RcolorBrewer_theme),1]
  
  # get the full series of the defined colorbrewer_colors
  
  my_colbrew_colors <- brewer.pal(max_color,RcolorBrewer_theme)
  # return the color generated
  colorRampPalette(my_colbrew_colors)(ncolors)
  
}





# data_frame, has one column as labeling, and two columns to be ploted as x and y, 
# additional  colums could be used as color and shap mapping
# df <- dplyr::select(mtcars, wt, mpg,  vs)
# df <- data.frame(label = row.names(df),df)
# #df$vs <- as.character(df$vs)
# data_frame <- df
# 
# 
ANCOM_rev <- function (OTUdat, sig = 0.05, multcorr = 3, tau = 0.02, theta = 0.1, 
                       repeated = FALSE) 
{
  num_col <- ncol(OTUdat)
  if (repeated == FALSE) {
    colnames(OTUdat)[num_col] <- "Group" # change the last group column name
    num_OTU <- ncol(OTUdat) - 1
    sub_drop <- data.frame(nm_drop = "N/A")
    sub_keep <- data.frame(nm_keep = "All subjects")
    colnames(sub_drop) <- "Subjects removed"
    colnames(sub_keep) <- "Subjects retained"
    n_summary <- paste0("No subjects entirely removed (not a repeated-measures design)")
  }
  else {
    colnames(OTUdat)[num_col - 1] <- "Group"
    colnames(OTUdat)[num_col] <- "ID"
    OTUdat$ID <- factor(OTUdat$ID)
    num_OTU <- ncol(OTUdat) - 2
    crossTab <- table(OTUdat$Group, OTUdat$ID) == 0
    id_drop <- apply(crossTab, 2, FUN = function(x) any(x))
    nm_drop <- names(which(id_drop))
    idx_drop <- OTUdat$ID %in% nm_drop
    OTUdat <- OTUdat[idx_drop == FALSE, ]
    if (nrow(OTUdat) == 0) {
      stop("Too many missing values in data, all subjects dropped")
    }
    OTUdat$ID <- droplevels(OTUdat$ID)
    num_dropped <- sum(id_drop)
    num_retain <- length(id_drop) - num_dropped
    sub_drop <- data.frame(nm_drop = paste(nm_drop, collapse = ", "))
    sub_keep <- data.frame(nm_keep = paste(levels(OTUdat$ID), 
                                           collapse = ", "))
    colnames(sub_drop) <- "Subjects removed"
    colnames(sub_keep) <- "Subjects retained"
    n_summary <- paste0("Analysis used ", num_retain, " subjects (", 
                        num_dropped, " were removed due to incomplete data)")
  }
  
  OTUdat$Group <- factor(OTUdat$Group) # force the group factor into 
  
  ##
  OTUdat <-  OTUdat[which(is.na(OTUdat$Group) == FALSE), ]
  # OTUdat <- data.frame(OTUdat[which(is.na(OTUdat$Group) == 
  #                                     FALSE), ], row.names = NULL)
  
  message("Doing ANCOM analysis...")
  W.detected <- ancom.detect(OTUdat, num_OTU, sig, multcorr, ncore = detectCores())
  message("ANCOM analysis is done...")
  
  W_stat <- W.detected
  if (num_OTU < 10) {
    detected <- colnames(OTUdat)[which(W.detected > num_OTU - 1)]
  }
  else {
    if (max(W.detected)/num_OTU >= theta) {
      c.start <- max(W.detected)/num_OTU
      cutoff <- c.start - c(0.05, 0.1, 0.15, 0.2, 0.25)
      prop_cut <- rep(0, length(cutoff))
      for (cut in 1:length(cutoff)) {
        prop_cut[cut] <- length(which(W.detected >= num_OTU * 
                                        cutoff[cut]))/length(W.detected)
      }
      del <- rep(0, length(cutoff) - 1)
      for (ii in 1:(length(cutoff) - 1)) {
        del[ii] <- abs(prop_cut[ii] - prop_cut[ii + 1])
      }
      if (del[1] < tau & del[2] < tau & del[3] < tau) {
        nu = cutoff[1]
      }
      else if (del[1] >= tau & del[2] < tau & del[3] < 
               tau) {
        nu = cutoff[2]
      }
      else if (del[2] >= tau & del[3] < tau & del[4] < 
               tau) {
        nu = cutoff[3]
      }
      else {
        nu = cutoff[4]
      }
      up_point <- min(W.detected[which(W.detected >= nu * 
                                         num_OTU)])
      W.detected[W.detected >= up_point] <- 99999
      W.detected[W.detected < up_point] <- 0
      W.detected[W.detected == 99999] <- 1
      detected <- colnames(OTUdat)[which(W.detected == 
                                           1)]
    }
    else {
      W.detected <- 0
      detected <- "No significant OTUs detected"
    }
  }
  results <- list(W = W_stat, detected = detected, dframe = OTUdat, 
                  repeated = repeated, n_summary = n_summary, sub_drop = sub_drop, 
                  sub_keep = sub_keep)
  class(results) <- "ancom"
  return(results)
}


plot_ancom_rev <- function (object, ncols = -1, select_plot_ids = NULL) 
{
  if (!(class(object) == "ancom")) {
    stop("'object' is not of class ancom")
  }
  repeated <- object$repeated
  Group <- OTU <- ID <- NULL
  dframe <- object$dframe
  if (repeated == FALSE) {
    colnames(dframe)[ncol(dframe)] <- "Group"
  }
  else {
    colnames(dframe)[ncol(dframe) - 1] <- "Group"
  }
  Sig_OTU <- object$detected
  if (Sig_OTU[1] == "No significant OTUs detected") {
    plot(1:5, 1:5, col = "white", xaxt = "n", yaxt = "n", 
         xlab = "", ylab = "", frame.plot = FALSE)
    text(3, 3, labels = "No significant OTUs detected")
  }
  else {
    if (repeated == FALSE) {
      W_check <- data.frame(colnames(dframe)[-ncol(dframe)], object$W, row.names = NULL)
      colnames(W_check) <- c("OTU_ID", "W")
    }
    else {
      W_check <- data.frame(colnames(dframe)[-c(ncol(dframe) - 1, ncol(dframe))], object$W, row.names = NULL)
      colnames(W_check) <- c("OTU_ID", "W")
    }
    
    ###
    if(is.null(select_plot_ids)){
      W_check <- W_check[which(W_check$OTU_ID %in% Sig_OTU), ]
    }else if(length(select_plot_ids)>0 && any(select_plot_ids %in% W_check$OTU_ID)){
      
      select_plot_ids <- select_plot_ids[select_plot_ids %in% W_check$OTU_ID]
      W_check <- W_check[which(W_check$OTU_ID %in% select_plot_ids), ]
      
    }else{
      W_check <- W_check[which(W_check$OTU_ID %in% Sig_OTU), ]
    }
    
    
    W_check <- W_check[order(-W_check$W), ]
    nplot <- nrow(W_check)
    
    t <-dframe[,colnames(dframe) %in% c(W_check$OTU_ID,"Group")] 
    pltDat <-reshape2::melt(t, id = "Group")
    colnames(pltDat) <- c("Group","OTU_name", "OTU" )

    
    pltDat$OTU <- log(pltDat$OTU + 1)
    pltDat$OTU_name <- factor(pltDat$OTU_name, Sig_OTU)
    if (ncols < 1) {
      ncols <- min(3, nplot)
    }
    gplot <- ggplot(pltDat, aes(x = factor(Group), y = OTU, fill = Group)) + 
      facet_wrap(~OTU_name, ncol = ncols, scales = "free_y") + 
      geom_boxplot()
    gplot + labs(x = "Group", y = "Log of Abundance") + 
      theme(panel.background = element_rect(fill = "white", 
                                            colour = "black"), panel.grid = element_blank(), 
            strip.text = element_text(size = rel(1.25)), 
            strip.background = element_rect(fill = "grey90", 
                                            color = "black"), axis.title = element_text(size = rel(1.25), 
                                                                                        color = "black"), axis.text = element_text(size = rel(1.05), 
                                                                                                                                   color = "black"), legend.position = "none", 
            legend.background = element_rect(colour = "black", 
                                             fill = "white"), legend.key = element_rect(fill = "white"), 
            legend.title = element_text(size = 15), legend.text = element_text(size = 12))
  }
}


ggplot_dual_axis = function(plot1, plot2, which.axis = "x") {
  
  # Update plot with transparent panel
  plot2 = plot2 + theme(panel.background = element_rect(fill = NA))
  
  library(grid)
  library(gtable)
  
  grid.newpage()
  
  # Increase right margin if which.axis == "y"
  if(which.axis == "y") plot1 = plot1 + theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
  
  # Extract gtable
  g1 = ggplot_gtable(ggplot_build(plot1))
  
  g2 = ggplot_gtable(ggplot_build(plot2))
  
  # Overlap the panel of the second plot on that of the first
  pp = c(subset(g1$layout, name == "panel", se = t:r))
  
  g = gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
  
  # Steal axis from second plot and modify
  axis.lab = ifelse(which.axis == "x", "axis-b", "axis-l")
  
  ia = which(g2$layout$name == axis.lab)
  
  ga = g2$grobs[[ia]]
  
  ax = ga$children[[2]]
  
  # Switch position of ticks and labels
  if(which.axis == "x") ax$heights = rev(ax$heights) else ax$widths = rev(ax$widths)
  
  ax$grobs = rev(ax$grobs)
  
  if(which.axis == "x") 
    
    ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm") else
      
      ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  
  # Modify existing row to be tall enough for axis
  if(which.axis == "x") g$heights[[2]] = g$heights[g2$layout[ia,]$t]
  
  # Add new row or column for axis label
  if(which.axis == "x") {
    
    g = gtable_add_grob(g, ax, 2, 4, 2, 4) 
    
    g = gtable_add_rows(g, g2$heights[1], 1)
    
    g = gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
    
  } else {
    
    g = gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    
    g = gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
    
    g = gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
    
  }
  
  # Draw it
  grid.draw(g)
  
}





# plto taxon barplot by level, from a taxon expresion matrix and taxon linage

taxon_barplot_by_level <-function(df_taxon_expression,
                                  df_taxon_lineage,
                                  level, 
                                  plot_NA = FALSE,
                                  use_percentage =  TRUE,
                                  draw_connection_line =  FALSE,
                                  BarWidth = 1,
                                  line_size = 1
                                  
){
  
  df_taxon_expression <- as.matrix(df_taxon_expression) # only matrix can have dupilicate ids
  df_taxon_lineage <- as.data.frame(df_taxon_lineage)
  
  if(!(level %in% colnames(df_taxon_lineage))){
    message(paste0(level, "does not exist in df_taxon_lineage, please check the input and try again "))
    stop()
  }
  
  # change the matrix row names as the corresponding taxon node name
  rownames(df_taxon_expression) <- df_taxon_lineage[[level]]
  
  
  if(!plot_NA){ 
    # remove rows with row names as NAs, which means that this 
    if(length(which(is.na(rownames(df_taxon_expression)))) > 0){
      df_taxon_expression <- df_taxon_expression[-which(is.na(rownames(df_taxon_expression))),]
    }
  }else{
    rownames(df_taxon_expression)[which(is.na(rownames(df_taxon_expression)))] <- "NA"
  }
  
  # combine rows with the same row names
  df_for_plot <- aggregate(df_taxon_expression, list(rownames(df_taxon_expression)), sum)
  
  # organize
  rownames(df_for_plot) <- df_for_plot[,1]
  df_for_plot <- df_for_plot[,-1]
  
  
  compositionbar_plot(df = df_for_plot, 
                      use_percentage = use_percentage, 
                      draw_connection_line = draw_connection_line,
                      legend_label = level,
                      BarWidth = BarWidth,
                      line_size = line_size
                      )
  
  ## example
  # otumat = matrix(sample(1:100, 100, replace = TRUE), nrow = 10, ncol = 10)
  # rownames(otumat) <- paste0("OTU", 1:nrow(otumat))
  # colnames(otumat) <- paste0("Sample", 1:ncol(otumat))
  # taxmat = matrix(sample(letters, 70, replace = TRUE), nrow = nrow(otumat), ncol = 7)
  # rownames(taxmat) <- rownames(otumat)
  # colnames(taxmat) <- c("Domain", "Phylum", "Class", "Order", "Family", "Genus", "Species")
  # 
  # taxon_barplot_by_level(df_taxon_expression = otumat,
  #                        df_taxon_lineage = taxmat,
  #                        plot_NA = TRUE,
  #                        level = "Family")
  
}


# plot a compostion bar from a matrix directly, with lines connected 
compositionbar_plot <- function(df, 
                                draw_connection_line = TRUE,
                                use_percentage = FALSE, 
                                legend_label = "Legend", 
                                BarWidth = 0.5, # 0~1， 0 is no width; 1 is full
                                line_size = 1 # only works when draw_connection_line is true
                                ){
  if(use_percentage){
    my_df <-table2lPercents_by_col(df)
  }else{
    my_df <- df
  }
  
  my_df_m <- reshape2::melt(t(my_df))
  colnames(my_df_m)[2] <- legend_label
  
  if(draw_connection_line){
    # data prepare
    y_start <- apply(my_df,2, cumsum)
    y_end <- y_start[,-1]
    y_end <- cbind(y_end, "new" = NA)
    colnames(y_end) <- colnames(y_start)
    
    my_df_y_start_m <-reshape2::melt(t(y_start))
    my_df_y_end_m <-reshape2::melt(t(y_end))
    
    # this is the data for plot
    my_df_forplot <- cbind.data.frame(my_df_m, "y_start" = my_df_y_start_m$value, "y_end" = my_df_y_end_m$value )
    my_df_forplot$x_start <- as.numeric(my_df_forplot$Var1) + 0.5*BarWidth
    my_df_forplot$x_end <- as.numeric(my_df_forplot$Var1)+ (1-0.5*BarWidth)
    
    
    # ploting
    compositionbar_plot <-ggplot(my_df_forplot, aes_string("Var1", "value", group=legend_label, fill = legend_label, colour = legend_label)) + 
      geom_bar(stat="identity", width = BarWidth,
               position = position_stack(reverse = TRUE)) +
      geom_segment(aes_string(x = "x_start", xend = "x_end", 
                              y = "y_start", yend = "y_end", 
                              color = legend_label),
                   size = line_size,
                   linetype = "solid"
      ) 
    
  }else{
    my_df_forplot <- my_df_m
    # ploting
    compositionbar_plot <-ggplot(my_df_forplot, aes_string("Var1", "value", group=legend_label, fill = legend_label, colour = legend_label)) + 
      geom_bar(stat="identity", width = BarWidth,
               position = position_stack(reverse = TRUE))
      
  }
  
  # prettier
  # compositionbar_plot <- ggplot2_prettier(compositionbar_plot,
  #                                         maintitle = "Composition bar",
  #                                         xlab = "Sample",
  #                                         ylab = "Composition",
  #                                         axis.text.angle.x = as.numeric(90),
  #                                         axis.text.angle.y = 0,
  #                                         vertical = FALSE)
  
  return(compositionbar_plot)
  ## example
  # my_df <- matrix(1:20, nrow = 4)
  # colnames(my_df) <- paste0("col_",LETTERS[1:5])
  # rownames(my_df) <- paste0("row_",c("a","f", "z", "k"))
  # compositionbar_plot(df = my_df, use_percentage = TRUE)
  # 
}





taxID_to_taxtable_gut <- function(ids, ranks_keep = c("phylum","class","order","family","genus","species")){
  # readin the preindexed database taxon information
  
  frequent_taxon_id_rank_lineage<- read.delim(".//data//taxon//taxon_id_rank_lineage_preindexed.tsv", header = TRUE, row.names = 1)
  frequent_taxon_id_rank_lineage$id <- rownames(frequent_taxon_id_rank_lineage)
  
  
  ranks_keep <- c("phylum","class","order","family","genus","species")
  
  index_colmn <- match(ranks_keep,colnames(frequent_taxon_id_rank_lineage))
  
  
  # get the taxon ranks
  
  index_row <- match(ids, frequent_taxon_id_rank_lineage$id)
  found_in_database <- frequent_taxon_id_rank_lineage[na.omit(index_row),index_colmn]
  return(found_in_database)
  
  ## example
  # ids <- sample(frequent_taxon_id_rank_lineage$id,10)
  # ta <- taxID_to_taxtable_gut(ids)
  
}











# avaliable colorschemes from Rcolor Brewer qulity colors
# "Accent"  "Dark2"   "Paired"  "Pastel1" "Pastel2" "Set1"    "Set2"    "Set3" 

factor2color <- function(factor_input, Rcolorbrewerscheme = "Accent"){
  
  install.packages.auto("RColorBrewer")
  
  colors <- as.factor(factor_input)
  
  max_color <- brewer.pal.info[which(rownames(brewer.pal.info) == Rcolorbrewerscheme),1]
  
  if(nlevels(colors) <= max_color){
    levels(colors) <- brewer.pal(nlevels(colors),Rcolorbrewerscheme)
  }else{

    levels(colors) <- rainbow(nlevels(colors))
  }

  return(as.vector(colors))
  
  # example:
  # f <- as.factor(sample(LETTERS[1:3], 10, replace = TRUE))
  # factor2color(f, "Accent")
}

PCA_wrapper_prcomp2 <- function(data_matrix, data_meta){
  
  suppressMessages(install.packages.auto(ggplot2))
  suppressMessages(install.packages.auto(ggfortify))# for autoplot
  
  data_matrix_t<-t(data_matrix)
  
  PCA_result <- prcomp(data_matrix_t)
  
  # scree plot
  pca_Scree <- PCA_Screeplot_2(prcomp_out = PCA_result)$Scree.plot
  
  if(missing(data_meta)){ # if there is no data_meta
    pca_component <- autoplot(PCA_result, label = TRUE )
    pca_component <- pca_component+
      labs(title = "PCA Clustering")+ 
      theme_bw()+
      theme(plot.title = element_text(hjust = 0.5))
    
    return(list(pca_result = PCA_result,
                pca_component_plot = pca_component,
                pca_scree_plot = pca_Scree
    ))
    
  }else{
    # reorder the meta (grouping information)
    data_meta <- data_meta[match(colnames(data_matrix), data_meta[,1]),]
    row.names(data_meta) <- data_meta[,1] # this is only for proper labeling
    colnames(data_meta) <- c("Sample.name", "grouping") 
    
    # check the order 
    if(any(colnames(data_matrix) !=  data_meta[,1])){stop("unmatched sample name/colun names")}
    
    pca_component <- autoplot(PCA_result, data = data_meta, colour = "grouping", label = TRUE)
    pca_component <- pca_component+labs(title = "PCA Clustering")+ theme_bw()+theme(plot.title = element_text(hjust = 0.5))
    
    
    pca_kmeans <- PCA_plot_with_ellipse_kmeans_2(prcomp_out = PCA_result, grouping = data_meta)
    
    pca_3d <- PCA_plot_3d_interactive_3(prcomp_out = PCA_result, grouping = data_meta)
    
    pca_confidence <- PCA_plot_with_confidence_2(prcomp_out = PCA_result, data_meta = data_meta)
    
    
    return(list(pca_result = PCA_result,
                pca_component_plot = pca_component,
                pca_component_plot_kmeans = pca_kmeans,
                pca_component_plot_3d_interactive = pca_3d,
                pca_confidence = pca_confidence,
                pca_scree_plot = pca_Scree
    ))
    
  }
  
}




# 
# advanced_scatterplot(data_frame = mtcars,
#                      x_index = 1,# column index/location
#                      y_index = 3,# column index/location
#                      
#                      point_shape_index = NULL, # column index/location
#                      point_shape_manual = c(15, 16, 17, 18),
#                      
#                      point_color_index = NULL, # column index/location 
#                      point_color_manual = rainbow(10), # if point_color_index is null, only use the first color, otherwise 
#                      point_color_type = "group", # c("graidnet", "group"), if group, will force to factor
#                      
#                      point_size_index = NULL,
#                      point_size_manual = 4, # will be overide by point_size_index
#                      point_alpha = 0.5,
#                      
#                      overlay_polygon = FALSE,
#                      polygon_type =  "convex", # or "ellipse"
#                      polygon_index = NULL, # draw polygon overlap, will choose 
#                      polygon_alpha = 0.5,
#                      ellipse_confidience = 0.95, # only works when ellipse is choose for polygon_type
#                      polygon_fill_color_manual = rainbow(10), # null means auto color, otherwise provide list of color values
#                      
#                      label_text_index = 8, # to do this section
#                      label_color_index = 8,
#                      label_color_manual = rainbow(10)[2:3],
#                      label_size_index =  NULL,
#                      label_size_manual  = 4,
#                      
#                      equal_xy = FALSE
# 
#                     )


advanced_scatterplot <- function(data_frame, # df, with at least three columns, 1 as labeling, 2 as x y mapping
                                 x_index = NULL,# column index/location
                                 y_index = NULL,# column index/location
                                 
                                 point_shape_index = NULL, # column index/location
                                 point_shape_manual = c(15, 16, 17, 18),
                                 
                                 point_color_index = NULL, # column index/location 
                                 point_color_manual = rainbow(10), # if point_color_index is null, only use the first color, otherwise 
                                 point_color_type = "group", # c("graidnet", "group"), if group, will force to factor
                                 
                                 point_size_index = NULL,
                                 point_size_manual = 4, # will be overide by point_size_index
                                 point_alpha = 0.5,
                                 
                                 overlay_polygon = FALSE,
                                 polygon_type =  "convex", # or "ellipse"
                                 polygon_index = NULL, # draw polygon overlap, will choose 
                                 polygon_alpha = 0.5,
                                 ellipse_confidience = 0.95, # only works when ellipse is choose for polygon_type
                                 polygon_fill_color_manual = rainbow(10), # null means auto color, otherwise provide list of color values
                                 
                                 label_text_index = NULL, # to do this section
                                 label_color_index = NULL,
                                 label_color_manual = rainbow(10),
                                 label_size_index =  NULL,
                                 label_size_manual  = 4,
                                 
                                 equal_xy = FALSE # if equal xy axix
                                 # to do: add more support to plot modeling curve, intensity etc.
){
  install.packages.auto(ggrepel)
  # get the colum name list, easy to index the location
  if(is.null(colnames(data_frame))){ # if there is no name, give a name
    colnames(data_frame) <- paste0("col_", 1:ncol(data_frame))
  }
  
  col_names <- colnames(data_frame) # get the names
  
  # check xy index 
  if(x_index >length(col_names)){
    stop("x_index is out of range" )
  }
  if(y_index >length(col_names)){
    stop("y_index is out of range" )
  }
  
  
  #___________________________
  # check point shape index
  
  if(is.null(point_shape_index)){ # if no shape mapping, there should be only 1 shape
    # in case there are more than one shapes given, better to have just one shape
    if(length(point_shape_manual) > 1){
      point_shape_manual <- point_shape_manual[1]
    }
    
  }else{
    if(point_shape_index > ncol(data_frame)){
      message("shape index is out of range")
      point_shape_index <- NULL
      if(length(point_shape_manual) > 1){
        point_shape_manual <- point_shape_manual[1]
      }
    }else{
      # in case this column is continuous numerics, force to factors, and this will overide color gradient settngs, 
      # if the color gradient set to the same column, and will only show as group
      data_frame[[point_shape_index]] <- as.factor(data_frame[[point_shape_index]])
    }
  }
  
  #_________________________________________
  # check point color index
  if(is.null(point_color_index)){ #if no color map, only one color
    if(length(point_color_manual) > 1){
      point_color_manual <- point_color_manual[1]
    }
  }else{ # if there is color mapping, either gradient or group (factor)
    if(point_color_index > ncol(data_frame)){
      message("color index is out of range")
      point_color_index <- NULL
      if(length(point_color_manual) > 1){
        point_color_manual <- point_color_manual[1]
      }
    }else{
      # in case this column is continuous numerics, force it to factor
      if(point_color_type == "group"){
        data_frame[[point_color_index]] <- as.factor(data_frame[[point_color_index]])
      }else if(point_color_type == "gradient" && point_color_index == point_shape_index){
        # if point shape and point color set to the same index, only use group mode
        point_color_type = "group"
      }
      
    }
  }
  #_________________________________________
  # check point size
  if(!is.null(point_size_index)){
    if( point_size_index > ncol(data_frame) |point_size_index < 0){
      message("point sizes index is out of range, set to null")
      point_size_index <- NULL
    }
    
  }
  
  #______________________________________________
  #check label text
  # check if shape_map set correctly
  if(!is.null(label_text_index)){
    if(label_text_index > ncol(data_frame)){
      message("label index is out of range, set to null")
      label_text_index <- NULL
    }
    
    #data_frame[[label_text_index]] <- as.character(data_frame[[label_text_index]])
  }
  
  
  #_________________________________________________
  # check label text color
  if(is.null(label_color_index)){
    if(length(label_color_manual) > 1){
      label_color_manual <- label_color_manual[1]
    }
  }else{
    if(label_color_index > ncol(data_frame) | label_color_index < 0){
      message("color index is out of range, set to null")
      label_color_index <- NULL
      if(length(label_color_manual) > 1){
        label_color_manual <- label_color_manual[1]
      }
    }
    # here force to factor, no need to map gradient to label text color
    data_frame[[label_color_index]] <- as.factor(data_frame[[label_color_index]])
  }
  
  
  # check label size
  if(!is.null(label_size_index) && label_size_index > ncol(data_frame)){
    message("label size index is out of range, set to null")
    label_size_index <- NULL
  }
  
  
  #_____________________________________________________________________________________________________
  # start plotting
  
  # maping data 
  this_plot <- ggplot(data = data_frame, aes_string(x = col_names[x_index], 
                                                    y = col_names[y_index]))
  
  # plot polygon layer first, behind the points
  if(overlay_polygon && !is.null(polygon_index) ){
    if( polygon_index < 0 || polygon_index > length(col_names)){
      message("polygon_index is out of range, cannot render polygon")
    }else{
      data_frame_for_polygon <- data_frame
      data_frame_for_polygon[[polygon_index]] <- as.factor(data_frame_for_polygon[[polygon_index]])
      
      switch(polygon_type,
             "convex" = {
               df_polygan_convex <- data_frame_for_polygon[,c(x_index,y_index,polygon_index)] # genearate a new df for polygon
               splitData <- split(df_polygan_convex, df_polygan_convex[[3]]) 
               
               # find the convex
               appliedData <- lapply(splitData, function(df){
                 df[chull(df), ]  # chull really is useful, even outside of contrived examples.
               })
               combinedData <- do.call(rbind, appliedData)
               # plot the polygon here
               this_plot <- this_plot + geom_polygon(data = combinedData,  # This is also a nice example of how to plot
                                                     aes_string(x = col_names[x_index], 
                                                                y = col_names[y_index],
                                                                fill = col_names[polygon_index]),  # two superimposed geoms
                                                     alpha = polygon_alpha) 
             },
             "ellipse" = {
               
               this_plot <- this_plot + stat_ellipse(data = data_frame_for_polygon, 
                                                     aes_string(fill = col_names[polygon_index]),
                                                     level = ellipse_confidience,
                                                     alpha = polygon_alpha, 
                                                     geom = "polygon")
             }
      )
      # manual coloring of the polygon
      if(!is.null(polygon_color_manual)){
        this_plot <- this_plot + scale_fill_manual(values = polygon_color_manual)
      }
    }
    
  }
  
  
  #________________________________________________________________________________
  #plot geom_points here
  #
  # plot the points here
  if(is.null(point_color_index)){ # without mapping color, that is single color
    
    if(is.null(point_shape_index)){ # without point shape mapping 
      if(is.null(point_size_index)){
        this_plot <- this_plot + geom_point(shape =  point_shape_manual, # use manual shapes
                                            size = point_size_manual, # use manul size
                                            color = point_color_manual, # use manual colors
                                            alpha = point_alpha) 
      }else{
        this_plot <- this_plot + geom_point(aes_string(size =col_names[point_size_index]),
                                            shape =  point_shape_manual,# use manual size
                                            color = point_color_manual, # use manual colors
                                            alpha = point_alpha) 
      }
      
    }else{ # with point shape mapping
      if(is.null(point_size_index)){
        this_plot <- this_plot + geom_point(aes_string(shape = col_names[point_shape_index]),
                                            size = point_size_manual, # use manul size
                                            color = point_color_manual, # use manual colors
                                            alpha = point_alpha) +
          scale_shape_manual(values = point_shape_manual)
      }else{
        this_plot <- this_plot + geom_point(aes_string(size = col_names[point_size_index],
                                                       shape = col_names[point_shape_index] ),
                                            color = point_color_manual, # use manual colors
                                            alpha = point_alpha) +
          scale_shape_manual(values = point_shape_manual)
      }
    }
    
  }else{  # if mapping color
    switch(point_color_type,
           "group" = {
             if(is.null(point_shape_index)){ # without point shape mapping 
               if(is.null(point_size_index)){
                 this_plot <- this_plot + geom_point(aes_string(color = col_names[point_color_index]),
                                                     shape =  point_shape_manual, # use manual shapes
                                                     size = point_size_manual, # use manul size
                                                     alpha = point_alpha) +
                   scale_color_manual(values = point_color_manual) # use manual colors as groups
               }else{
                 this_plot <- this_plot + geom_point(aes_string(color = col_names[point_color_index],
                                                                size =col_names[point_size_index]),
                                                     shape =  point_shape_manual,# use manul size
                                                     alpha = point_alpha) +
                   scale_color_manual(values = point_color_manual) # use manual colors as groups
               }
               
             }else{ # with point shape mapping
               if(is.null(point_size_index)){
                 this_plot <- this_plot + geom_point(aes_string(color = col_names[point_color_index],
                                                                shape = col_names[point_shape_index] ),
                                                     size = point_size_manual, # use manul size
                                                     alpha = point_alpha) +
                   scale_shape_manual(values = point_shape_manual) + # use manual shapes
                   scale_color_manual(values = point_color_manual) # use manual colors as groups
               }else{
                 this_plot <- this_plot + geom_point(aes_string(color = col_names[point_color_index],
                                                                size = col_names[point_size_index],
                                                                shape = col_names[point_shape_index] ),
                                                     alpha = point_alpha) +
                   scale_shape_manual(values = point_shape_manual)+ # use manual shapes
                   scale_color_manual(values = point_color_manual) # use manual colors as groups
               }
             }
           },
           "gradient" = {
             if(is.null(point_shape_index)){ # without point shape mapping 
               if(is.null(point_size_index)){
                 this_plot <- this_plot + geom_point(aes_string(color = col_names[point_color_index]),
                                                     shape =  point_shape_manual, # use manual shapes
                                                     size = point_size_manual, # use manul size
                                                     alpha = point_alpha) +
                   scale_color_gradientn(values = point_color_manual) # use manual colors as groups
               }else{
                 this_plot <- this_plot + geom_point(aes_string(color = col_names[point_color_index],
                                                                size =col_names[point_size_index]),
                                                     shape =  point_shape_manual,# use manul size
                                                     alpha = point_alpha) +
                   scale_color_gradientn(values = point_color_manual) # use manual colors as groups
               }
               
             }else{ # with point shape mapping
               if(is.null(point_size_index)){
                 this_plot <- this_plot + geom_point(aes_string(color = col_names[point_color_index],
                                                                shape = col_names[point_shape_index] ),
                                                     size = point_size_manual, # use manul size
                                                     alpha = point_alpha) +
                   scale_shape_manual(values = point_shape_manual) + # use manual shapes
                   scale_color_gradientn(values = point_color_manual) # use manual colors as groups
               }else{
                 this_plot <- this_plot + geom_point(aes_string(color = col_names[point_color_index],
                                                                size = col_names[point_size_index],
                                                                shape = col_names[point_shape_index] ),
                                                     alpha = point_alpha) +
                   scale_shape_manual(values = point_shape_manual)+ # use manual shapes
                   scale_color_gradientn(values = point_color_manual) # use manual colors as groups
               }
             }
           }
           
           
    )
  }
  
  
  # some other generic options: equal scaling of x and y
  if(equal_xy){
    this_plot <- this_plot + coord_equal()
  }
  
  # repel labeling
  if(!is.null(label_text_index)){
    
    if(is.null(label_color_index)){
      
      if(is.null(label_size_index)){
        this_plot <- this_plot +
          geom_text_repel(aes_string(col_names[x_index], 
                                     col_names[y_index],
                                     label = col_names[label_text_index]), 
                          color = label_color_manual,
                          size = label_size_manual
          )
      }else{
        this_plot <- this_plot +
          geom_text_repel(aes_string(col_names[x_index], 
                                     col_names[y_index],
                                     label = col_names[label_text_index],
                                     size = col_names[label_size_index]),
                          color = label_color_manual
          )
      }
      
      
    }else{
      if(is.null(label_size_index)){
        this_plot <- this_plot +
          geom_text_repel(aes_string(col_names[x_index], 
                                     col_names[y_index],
                                     color = col_names[label_color_index],
                                     label = col_names[label_text_index]), 
                          size = label_size_manual) + 
          scale_color_manual(values=label_color_manual) # manual color
      }else{
        this_plot <- this_plot +
          geom_text_repel(aes_string(col_names[x_index], 
                                     col_names[y_index],
                                     color = col_names[label_color_index],
                                     label = col_names[label_text_index],
                                     size = col_names[label_size_index]))+
          scale_color_manual(values=label_color_manual) # manual color
      }
    }
  }
  return(this_plot)
}


# test example using mtcars 
# # this is going to be depreciated

# this is a wrapper of scatter plot, with a lot of options 
scatterplot_ggrepel <- function(data_frame, # df, with at least three columns, 1 as labeling, 2 as x y mapping
                                x_index = NULL,# column index/location
                                y_index = NULL,# column index/location
                                
                                label_index = NULL,
                                label_color_index = NULL,
                                
                                point_shape_index = NULL, # column index/location
                                point_shape_manual = NULL,
                                point_color_index = NULL, # column index/location 
                                point_color_type = "group", # c("graidnet", "group")
                                
                                point_size = 4,
                                point_alpha = 0.5,
                                point_color = "black", # if point_color_index is null, only use the first color, otherwise 
                                
                                polygon_index = NULL, # if not null, will draw polygon overlap
                                polygon_color_manual = NULL, # null means auto color, otherwise provide list of color values
                                polygon_alpha = 0.5,
                                
                                equal_xy = FALSE
                                ){
  
  col_names <- colnames(data_frame)
  
  if(!is.null(label_index)){
    # check if shape_map set correctly
    if(label_index > ncol(data_frame)){
      message("lable index is not set correctly")
    }else{
      # in case this column is continuous numerics 
      #data_frame[[label_index]] <- as.character(data_frame[[label_index]])
      label_map <- colnames(data_frame)[label_index]
    }
  }else{
    label_map <- NULL
  }
  
  if(!is.null(label_color_index)){
    if(label_color_index > ncol(data_frame)){
      message("color index is not set correctly")
      label_color_map <- NULL
    }else{
      # in case this column is continuous numerics
      #data_frame[[label_color_index]] <- as.character(data_frame[[label_color_index]])
      label_color_map <- colnames(data_frame)[label_color_index]
    }
  }else{
    label_color_map <- NULL
  }
 
  if(!is.null(point_shape_index)){
    if(point_shape_index > ncol(data_frame)){
      message("shape index is not set correctly")
      point_shape_map <- NULL
      point_shape_single <- point_shape_manual
    }else{
      # in case this column is continuous numerics 
      #data_frame[[point_shape_index]] <- as.character(data_frame[[point_shape_index]])
      point_shape_map <- colnames(data_frame)[point_shape_index]
      point_shape_single <- NULL
    }
  }else{
    point_shape_map <- NULL
    point_shape_single <- point_shape_manual
  }
  
  if(!is.null(point_color_index)){
    if(point_color_index > ncol(data_frame)){
      message("color index is not set correctly")
      point_color_map <- NULL
    }else{
      # in case this column is continuous numerics
      if(point_color_type == "group"){
        data_frame[[point_color_index]] <- as.factor(data_frame[[point_color_index]])
      }
      point_color_map <- colnames(data_frame)[point_color_index]
    }
  }else{
    point_color_map <- NULL
  }
  
  
  # plotting
  # 
  this_plot <- ggplot(data = data_frame, aes_string(x = col_names[x_index], 
                                                    y = col_names[y_index]))
  # this is to add polygon layer behind the points
  if(!is.null(polygon_index)){
    if(polygon_index > ncol(data_frame)){
      message("polygon_index is not set correctly, out of boundary")
    }else{
      
      df_polygan <- data_frame[,c(x_index,y_index,polygon_index)] # genearate a new df for polygon
      df_polygan[[3]] <- as.factor(df_polygan[[3]])# force to be factors
      splitData <- split(df_polygan, df_polygan[[3]]) 

      # find the convex
      appliedData <- lapply(splitData, function(df){
        df[chull(df), ]  # chull really is useful, even outside of contrived examples.
      })
      combinedData <- do.call(rbind, appliedData)
      # plot the polygon here
      this_plot <- this_plot + geom_polygon(data = combinedData,  # This is also a nice example of how to plot
                                            aes_string(x = col_names[x_index], 
                                                       y = col_names[y_index],
                                                       fill = col_names[polygon_index]),  # two superimposed geoms
                                            alpha = polygon_alpha) 
      
    }
    # manual coloring of the polygon
    if(!is.null(polygon_color_manual)){
      this_plot <- this_plot + scale_fill_manual(values = polygon_color_manual)
    }
    
  }
  
  # plot the points here
  if(is.null(point_color_index)){
    this_plot <- this_plot + geom_point(aes_string(shape = point_shape_map),
                                        shape =  point_shape_single,
                                        size=point_size,
                                        color=point_color, # use manual colors
                                        alpha = point_alpha)
  }else{
    switch(point_color_type,
           "group" = {
             
             if(is.null(point_shape_single)){
               this_plot <- this_plot + geom_point(aes_string(shape = point_shape_map,
                                                              color = point_color_map),
                                                   #shape =  point_shape_single,
                                                   size=point_size,
                                                   alpha = point_alpha) +
                 scale_color_manual(values=point_color) # use manual colors as groups
             }else{
               this_plot <- this_plot + geom_point(aes_string(shape = point_shape_map,
                                                              color = point_color_map),
                                                   shape =  point_shape_single,
                                                   size=point_size,
                                                   alpha = point_alpha) +
                 scale_color_manual(values=point_color) # use manual colors as groups
             }
             
           },
           "gradient" = {
             this_plot <- this_plot + geom_point(aes_string(shape = point_shape_map,
                                                            color = point_color_map),
                                                 shape =  point_shape_single,
                                                 size=point_size,
                                                 alpha = point_alpha) +
               scale_color_gradientn(colours=point_color) # use manual colors gradient
           }
           
             
      )
  }
  
  # for manual point shape 
  if(!is.null(point_shape_manual)){
    #print(point_shape_manual)
    this_plot <- this_plot + scale_shape_manual(values=point_shape_manual)
  }
  

  # ggplot(df, aes(x=wt, y=mpg, group=cyl)) +
  #   geom_point(aes(color=cyl,shape =), size = 5, shape = 21)
  # 
  #   scale_shape_manual(values=16)


  # some other generic options: equal scaling of x and y
  if(equal_xy){
    this_plot <- this_plot + coord_equal()
  }


  # repel labeling
  if(!is.null(label_index)){
    this_plot <- this_plot +
      geom_text_repel(aes_string(col_names[x_index], 
                                 col_names[y_index],
                                 label = label_map,
                                 color = label_color_map
      )
      )
  }
  return(this_plot)
}




# this fucntion can be used to check not only the correlation, but the reproducibility
reproducibility_ggpairs <- function(data, columns = NULL){
  
  if(is.null(columns)){
    columns <- 1: ncol(data)
  }
  if (columns[1] > ncol(data) | columns[1] < 0 | columns[length(columns)] > ncol(data)){
    message("columm index is out of boundary")
    stop("columm index is out of boundary")
  }
  
  
  suppressMessages(require(ggplot2))
  suppressMessages(require(GGally))
  
  # self made functions to 
  my_fn <- function(data, mapping, ...){
    p <- ggplot(data = data, mapping = mapping) + 
      geom_point() + 
      geom_abline(intercept = 0, slope = 1,color="red") +
      #geom_smooth(method=loess, fill="red", color="red", ...) +
      geom_smooth(method=lm, fill="blue", color="blue", ...)
    p
    
  }
  
  
  
  plot <- ggpairs(data,columns = columns,lower = list(continuous = my_fn))
  
  return(plot)
  # example:
  # a <- rnorm(100)
  # b <- a+ rnorm(100)/10
  # c <- a+ rnorm(100)/2
  # d <- a+ abs(rnorm(100)/2)
  # df <- data.frame(a,b,c,d)
  # reproducibility_ggpairs(df)
  # 
  
}





# this is a wrapper of the mulitplot function, to return an object  
shiny_multiplot <- function(..., plotlist = NULL, cols=1, layout = NULL){
  
  plots <- c(list(...), plotlist)
  
  svg(tempfile(),onefile = TRUE)
  dev.control('enable') 
  
  multiplot(plotlist = plots, cols=cols, layout = layout)
  
  combined <- recordPlot()
  dev.off()
  return(combined)
  
}




# Multiple plot function
# this is only for simple square layout without layout, 
# but with complex layout with layout argument defined
# e.g.: layout <- matrix(c(1, 1, 2, 3, 4, 5), nrow = 2, byrow = TRUE)
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout = NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


plot_cum_density <- function(x, # x is a vector
                             show_hist_cum = FALSE,
                             show_hist = FALSE,
                             show_density_curve =  FALSE, 
                             show_density_curve_cum =  FALSE,
                             show_density_curve_cum_reversed =  FALSE,
                             xlab ="X Scale", 
                             ylab = "Frequency(#)", 
                             main = "" ){
  
  
  if(all(!show_hist_cum, # if all are false 
         !show_hist,
         !show_density_curve,
         !show_density_curve_cum,
         !show_density_curve_cum_reversed)){
    show_density_curve_cum =  TRUE
  }
  
  ## Calculate and plot the two histograms
  hcum <- h <- hist(x, plot=FALSE)
  hcum$counts <- cumsum(hcum$counts)
  
  svg(tempfile(),onefile = TRUE)
  dev.control('enable') 
  
  # plot the histograme
  plot(1,1, xlim = c(hcum$breaks[1], max(hcum$breaks)), ylim =  c(0,max(hcum$counts)),
       main= main, xlab = xlab, ylab = ylab, type =  "n")
  
  if(show_hist_cum){
    plot(hcum, add=T)
  }
  if(show_hist){
    plot(h, add=T, col="grey")
  }
  
  
  
  ## Plot the density and cumulative density
  
  
  d <- density(x)
  if(show_density_curve){
    lines(x = d$x, y = d$y * length(x) * diff(h$breaks)[1], lwd = 2)
  }
  if(show_density_curve_cum){
    lines(x = d$x, y = cumsum(d$y)/max(cumsum(d$y)) * length(x), lwd = 2)
  }
  
  if(show_density_curve_cum_reversed){
    lines(x = d$x, y = (1-cumsum(d$y)/max(cumsum(d$y))) * length(x), lwd = 2, col = "red")
  }
  
  
  p1 <- recordPlot()
  dev.off()
  return(p1)
  
  # test 
  # x <- sample(0:30, 200, replace=T, prob=15 - abs(15 - 0:30))
  # p <- plot_cum_density(x, main ="Frequency")
  # p
  
}




MQ_QC_plot<- function(data.frame, 
                      plot_type = c("scatter", "bar", "density", "histogram", "freqpoly", "box", "violin") ,
                      group = NULL, 
                      cutoff = 20, 
                      maintitle = "", 
                      xlabel = "",
                      vertical =  FALSE,
                      ...
                ){
  
  
  # in case some column names are not valid names (containign special symbol, like space, % etc)
  names(data.frame)[1] <- "names"
  names(data.frame)[2] <- "value"
  
  data.frame$plot_order_x <- 1: nrow(data.frame) # this column is going to be used as x axis
  
  # of there is no grouping information, give all rows the same group
  if(is.null(group)){
    group <- "All"
    data.frame$All = "All"
    
  }
  
  
  if(length(plot_type) == 0){
    stop
  }else{
    plot_out <- list()
  }
  
  if("scatter" %in% plot_type){
    scatter_plot <- ggplot(data.frame) + 
      # geom_rect(data=data.frame(xmin=-Inf, xmax=Inf, ymin= -Inf, ymax=cutoff), 
      #           aes(xmin=xmin, xmax=xmax, ymin=ymin,ymax=ymax), 
      #           fill="red", alpha=0.1) +
      # geom_rect(data=data.frame(xmin=-Inf, xmax=Inf, ymin= cutoff, ymax=Inf), 
      #           aes(xmin=xmin, xmax=xmax, ymin=ymin,ymax=ymax), 
      #           fill="lightblue", alpha=0.5) +
      annotate("rect", xmin=-Inf, xmax= Inf, ymin=0, ymax=cutoff, alpha=0.1, fill="red") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=cutoff, ymax=Inf, alpha=0.5, fill="lightblue") +
      geom_point(aes_string(x = "plot_order_x", y = "value", colour = group)) +
      geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
      scale_x_continuous(breaks = 1:nrow(data.frame),labels = data.frame[,1]) +
      labs(title = maintitle, x = "", y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    
      if(vertical){
        scatter_plot <- scatter_plot + coord_flip()
      }
    
    plot_out <- c(plot_out, list("scatter_plot" = scatter_plot))
  }
  if("bar" %in% plot_type){
    bar_plot <- ggplot(data.frame) +
      
      annotate("rect", xmin=-Inf, xmax= Inf, ymin=0, ymax=cutoff, alpha=0.1, fill="red") +
      annotate("rect", xmin=-Inf, xmax=Inf, ymin=cutoff, ymax=Inf, alpha=0.5, fill="lightblue") +
      geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
      geom_col(aes_string(x = "plot_order_x", y = "value", fill = group))+
      scale_x_continuous(breaks = 1:nrow(data.frame),labels = data.frame[,1]) +
      
      labs(title = maintitle, x = "",y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    
    if(vertical){
      bar_plot <- bar_plot + coord_flip()
    }
    
    plot_out <- c(plot_out, list("bar_plot" = bar_plot))
    
  }
  if("freqpoly" %in% plot_type){
    # distritibution
    freqpoly_plot <- ggplot(data.frame) +
      annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
      annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
      geom_freqpoly(aes_string("value",colour = group) )+
      geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)+
      labs(title = maintitle,  x = "",y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    if(vertical){
      freqpoly_plot <- freqpoly_plot + coord_flip()
    }
    
    plot_out <- c(plot_out, list("freqpoly_plot" = freqpoly_plot))
  }
  
  if("histogram" %in% plot_type){
    histogram_plot <- ggplot(data.frame) +
      annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
      annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
      geom_histogram(aes_string("value", colour = group, fill = group),position = "identity",alpha = 0.5)+
      geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)+
      labs(title = maintitle,  x = "",y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    if(vertical){
      histogram_plot <- histogram_plot + coord_flip()
    }
    plot_out <- c(plot_out, list("histogram_plot" = histogram_plot))
  }
  
  
  
  if("density" %in% plot_type){
    density_plot <-ggplot(data.frame) +
      annotate("rect", xmin=-Inf, xmax= cutoff, ymin=0, ymax=Inf, alpha=0.1, fill="red") +
      annotate("rect", xmin=cutoff, xmax=Inf, ymin=0, ymax=Inf, alpha=0.5, fill="lightblue") +
      geom_density(aes_string("value", colour = group, fill = group),position = "identity",alpha = 0.5) +
      geom_vline(xintercept = cutoff, linetype="dashed", color = "blue", size=1)+
      labs(title = maintitle,  x = "",y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    if(vertical){
      density_plot <- density_plot + coord_flip()
    }
    plot_out <- c(plot_out, list("density_plot" = density_plot))
  }
  
  
  if("violin" %in% plot_type){
    violin_plot <- ggplot(data.frame) +
      geom_violin(aes_string(x =group,  y = "value", colour = group, fill = group))+
      geom_jitter(aes_string(x =group,  y = "value",colour = group, fill = group),shape=21)  +
      geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
      labs(title = maintitle,  x = "",y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    if(vertical){
      violin_plot <- violin_plot + coord_flip()
    }
    plot_out <- c(plot_out, list("violin_plot" = violin_plot))
    
  }
  
  
  if("box" %in% plot_type){
    box_plot <- ggplot(data.frame) +
      geom_boxplot(aes_string(x =group,  y = "value", colour = group, fill = group))+
      geom_jitter(aes_string(x =group,  y = "value",colour = group, fill = group),shape=21)  +
      geom_hline(yintercept = cutoff, linetype="dashed", color = "blue", size=1) +
      labs(title = maintitle,  x = "",y = xlabel) + 
      theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid = element_blank())
    if(vertical){
      box_plot <- box_plot + coord_flip()
    }
    plot_out <- c(plot_out, list("box_plot" = box_plot))
    
  }
  
  
  return(plot_out) 
  
  # Example
  # my_test <- data.frame("samplename" = paste0("sample_", 1:20),
  #                       "msms_id" = c(abs(rnorm(10))*10+20, abs(rnorm(10))*10+30),
  #                       "treat_group" = c(paste0("group_", rep("A",10)), paste0("group_", rep("B",10)))
  # )
  # 
  # tt <-  MQ_QC_plot(my_test, plot_type = c("scatter","bar","density", "histogram", "freqpoly", "box", "violin"), cutoff = 35, group = "treat_group", maintitle = "MSMS ID Rate", xlabel = "MS/MS ID %")
  # now tt has all the required plot
  
  
}







# this function organize the readin data.frame of  summary.txt  
# then split it into three sections, one for rawfile summary, one for experiemnt summary, and one for total summary
# return a list




organize_summary.txt <- function(df_summary.txt){
  
  # take out the last line to organize
  last_line <- t(df_summary.txt[nrow(df_summary.txt),])
  last_line[last_line[,1] == ""] <- NA
  last_line[last_line[,1] == "0"] <- NA
  last_line <- last_line[-1,, drop = FALSE]
  last_line <- last_line[which(!is.na(last_line[,1])),, drop =  FALSE]
  colnames(last_line) <- ("values")
  
  # take out rows about raw files summary
  summary <- df_summary.txt[-nrow(df_summary.txt),] # remove the last line
  df_rawfiles <- remove_allNA_columns(summary[which(summary[,2] != ""),])
  
  # take out rows abotu experiment summary
  df_experiment <- summary[which(summary[,2] == ""),]
  df_experiment[df_experiment== ""] <- NA
  df_experiment <- remove_allNA_columns(df_experiment)
  
  
  return(list("summary_all" = last_line,
              "summary_rawfiles" = df_rawfiles,
              "summary_experiment" = df_experiment
              
  ))
  
}








# matrix_process is a wrapper of several function to do missvalue filtering, missing value imputation
# log transoformation, and scale
# default argument is do scaling only on column
# Value: a list, use $data_matrix_processed to get the processed value 

marix_process2 <- function(data_matrix, 
                           missingvalue_filtering = TRUE,
                           zero_as_missing = TRUE, # missing value is ususally marked as NA, if this switch is on, it will take convert 0 as NA first
                           Q = 0.75, 
                           log10_transform = FALSE,
                           log2_transform = FALSE,
                           Imputation = TRUE, 
                           Imputation_alpha = 0.9, # do not change unless you know it
                           scale_columnwise = TRUE, 
                           scale_rowwise = FALSE)
{
  # 
  result_list <- list() # this is a new way to record the temprary result
  
  if(missingvalue_filtering){
    if(zero_as_missing){
      data_matrix[data_matrix == 0]<-NaN # replace the 0 with NaN
    }
    
    # missing value filtering
    filter_result <- matrix_missingvalue_filtering(data_matrix, Q)
    
    # take over the result
    data_matrix <- filter_result$data_qualified 
    #my_list  <- 
    result_list <- c(result_list, list( data_qualified_for_missing_value = data_matrix,
                                        data_not_filtered_out_for_missing_value = filter_result$data_not.qualified,
                                        number_of_rows_qualified_by_missing_value = filter_result$number.qualified,
                                        number_of_rows_filtered_out_by_missing_value = filter_result$number.not.qualified
    ))
    
  }
  
  if(log10_transform){ # log transform
    data_matrix <- log10(data_matrix)
    result_list <- c(result_list, list(data_matrix_log10_tranformed = data_matrix))
  }
  
  if(log2_transform){ # log transform
    data_matrix <- log2(data_matrix)
    result_list <- c(result_list, list(data_matrix_log2_tranformed = data_matrix))
  }
  
  
  
  if(Imputation){  # missing value imputation
    data_matrix <- rrcovNA::impSeqRob(data_matrix, alpha = Imputation_alpha)$x
    result_list <- c(result_list, list(data_matrix_imputed = data_matrix))
  }
  
  if (scale_columnwise){ # do column scaling, keep in mind that the scale function in R is scaling by column
    data_matrix <- scale(data_matrix)
    result_list <- c(result_list, list(data_matrix_scale_columnwise = data_matrix))
  }
  
  if (scale_rowwise){ # scaling of each protein
    data_matrix <- t(scale(t(data_matrix)))
    result_list <- c(result_list, list(data_matrix_scale_rowwise = data_matrix))
  }
  
  #my_list  <- 
  result_list <- c(result_list, list(data_matrix_processed = data_matrix))
  
  return(result_list)
}



#________________________________________________________________________________________

#     matrix_missingvalue_filtering
#________________________________________________________________________________________

# ___Description___: 
# filter a matrix out clumn with more than NA/infinte preset(inf values could be generated from log transformation or dividing conversion), 
# threshold is the Q value of the valid values, rows with more valid values than threshold will be kept as qualified
# only do row-wise filtering, transpose first if do column-wise filtering

#__Usage__:
# matrix_missingvalue_filtering(data, Q =3)
# matrix_missingvalue_filtering(data, Q = 0.75)

# ___Arguments___:
# data: data matrix with missing values, NA/inf
# threshold: how many (percentage) non-missingvalues are required to be in the matrix
#             can be two types, one is the nnumber of the missing values,the other one is the so called Q value, which is the percentage(0 <= Q <= 1) to the number of columns
#             Q value == 1, require no missing values, 
#             Q value == 0, no filtering,
#             Q value will be converted to the number of missing value (1 < number < ncol(data))
#             will report an error if not setup in this range
#             in this function, celing is used to convert the percentage, if not expected, try floor/round etc

# ___Values___:
# a list
# qualified data matrix, not.qulified data.matrix, number of rows of quailfied/ not qulified. 
# 


matrix_missingvalue_filtering <- function(data.matrix, Q = 1){ #  value can only be from 0~1 or 1~ number of columns
  
  data.matrix[is.infinite(as.matrix(data.matrix))]<-NA # the is.infinite function does not work on data.frame, 
  # in case there are infinte values there
  
  if(Q < 0|Q > ncol(data.matrix) ){
    print ("Q value can only be from 0~1 or 1~ number of columns")
  }else{
    
    if(Q > 0 && Q <= 1){ # concet the q value to the real missing value number
      threshold <- ceiling(ncol(data.matrix)*Q)
    }else if(Q >1 && Q <=  ncol(data.matrix)) {
      threshold <- Q
    }
    
    data_qualified <- data.matrix[which(apply(data.matrix,1,function(x)(sum(is.na(x))) <= (ncol(data.matrix)-threshold))),]
    data_not.qualified <- data.matrix[which(apply(data.matrix,1,function(x)(sum(is.na(x))) > (ncol(data.matrix)-threshold))),]
    return(list(data_qualified = data_qualified, 
                data_not.qualified = data_not.qualified,
                number.qualified = nrow(data_qualified), 
                number.not.qualified = nrow(data_not.qualified)
    ))
    
  }
}



tidy_proteingroups_3 <- function(proteinGroups = NULL, # data.frame
                                 sample_names = NULL, # vector, with order determing the resulted column order
                                 remove_rows_marked_with = c("Only identified by site", "Reverse","Potential contaminant"),
                                 keep_cols_starts_with = "LFQ intensity ", # the columns to be selected starting with, use the maximum lengh (usually including the space)
                                 # Frequently used: "LFQ intensity ", "Intensity ", "Ratio H/L ", "Ratio H/L normalized "
                                 keep_data_type =  "intensity", # "intensity" or "ratio"
                                 filter_rows_with_all_missing_values = TRUE
                                 
){
  
  
  suppressMessages(install.packages.auto(dplyr)) # select command
  
  
  protein.ids_split <- strsplit(as.vector(proteinGroups$"Protein IDs"), ";") # this is a list of list of split names
  protein_primary_ids <- unlist(lapply(protein.ids_split, function(x) x[1])) # only keep the first one
  
  # rownames to be the protein ID
  proteinGroups_annoatation <- select(proteinGroups, c("Protein IDs", "Protein names","Gene names","Fasta headers"))
  rownames(proteinGroups_annoatation) <- proteinGroups$id
  proteinGroups_annoatation$first_major_protein <- protein_primary_ids
  
  # rename the rownames of the matrix to have the same id
  rownames(proteinGroups) <- proteinGroups$id 
  
  
  # removing rows marked with 
  if (length(remove_rows_marked_with) >=1 ){
    PG_row_filtering_plus <- PG_filter_rows_by_plus(proteinGroups, columnames = remove_rows_marked_with)
    proteinGroups_filtered <- PG_row_filtering_plus$proteinGroups_filtered
    proteinGroups_filtered_out_as_plus <- PG_row_filtering_plus$number_of_rows_filtered_out
  } else{
    proteinGroups_filtered_out_as_plus <- NULL
  }
  
  # keeping columns starts with
  if (length(keep_cols_starts_with) >= 1 ){
    #proteinGroups_filtered <- select(proteinGroups_filtered, starts_with(keep_cols_starts_with)) # select command in dplyr package
    
    selected_column_index <- match(paste0(keep_cols_starts_with,sample_names), colnames(proteinGroups_filtered))
    
    if(all(is.na(selected_column_index))){
      message("No column names matched in the data.frame provided")
      message(cat(paste0(keep_cols_starts_with,sample_names), sep = ";"))
      message(paste0(keep_cols_starts_with,sample_names, "\n"))
      stop("No column names matched in the data.frame provided")
    }
    
    if(any(is.na(selected_column_index))){
      message("Some samples do not exists in the data.frame provided")
      message(sample_names[which(is.na(selected_column_index))])
    }
    
    proteinGroups_filtered <- proteinGroups_filtered[,match(paste0(keep_cols_starts_with,sample_names), colnames(proteinGroups_filtered))]
    
  }
  
  
  # shorten column names, by removing the "starts with"
  colnames(proteinGroups_filtered)<-gsub(keep_cols_starts_with, "", colnames(proteinGroups_filtered))
  
  #remove rows with all 0, this is only for data value matrix
  if (filter_rows_with_all_missing_values){
    switch(keep_data_type,
           "intensity" = {index_all_zero <- apply(proteinGroups_filtered,1,function(x)all(x == 0))},
           "ratio" = {index_all_zero <- apply(proteinGroups_filtered,1,function(x)all(is.na(x)))}
    )
    #
    proteinGroups_filtered <- proteinGroups_filtered[-which(index_all_zero),]
    proteinGroups_filtered_out_all_zero <- length(which(index_all_zero))
    print(paste0("ProteinGroups with all zeros: ", proteinGroups_filtered_out_all_zero))
  }else{
    proteinGroups_filtered_out_all_zero <- NULL
  }
  
  
  # subsect the annotation file
  proteinGroups_annoatation <- proteinGroups_annoatation[match(rownames(proteinGroups_filtered),rownames(proteinGroups_annoatation)),]
  
  message("proteinGroups tidied, and a list is returned")
  message("use proteinGroups_filtered and proteinGroups_annoatation to take over the result")
  
  return(list(proteinGroups_filtered = proteinGroups_filtered,
              proteinGroups_annoatation = proteinGroups_annoatation,
              proteinGroups_filtered_out_as_plus =  proteinGroups_filtered_out_as_plus,
              proteinGroups_filtered_out_all_zero = proteinGroups_filtered_out_all_zero
  )) 
}



# this is a revised version for tidy proteinsGroups
# proteinGroups has to be readin with check.names = FALSE, otherwise not working

tidy_proteingroups_2 <- function(proteinGroups = NULL, 
                                 experimentDesgin = NULL,
                                 remove_rows_marked_with = c("Only identified by site", "Reverse","Potential contaminant"),
                                 keep_cols_starts_with = "LFQ intensity ", # the columns to be selected starting with, use the maximum lengh
                                 filter_all_zero = TRUE,
                                 keep_data_type =  "intensity" # "intensity" or "ratio"
){
  
  
  suppressMessages(install.packages.auto(dplyr)) # select command
  
  
  protein.ids_split <- strsplit(as.vector(proteinGroups$"Protein IDs"), ";") # this is a list of list of split names
  protein_primary_ids <- unlist(lapply(protein.ids_split, function(x) x[1])) # only keep the first one
  
  # rownames to be the protein ID
  proteinGroups_annoatation <- select(proteinGroups, c("Protein IDs", "Protein names","Gene names","Fasta headers"))
  rownames(proteinGroups_annoatation) <- proteinGroups$id
  proteinGroups_annoatation$first_major_protein <- protein_primary_ids
  
  # rename the rownames of the matrix to have the same id
  rownames(proteinGroups) <- proteinGroups$id 
  
  
  # removing rows marked with 
  if (length(remove_rows_marked_with) >=1 ){
    PG_row_filtering_plus <- PG_filter_rows_by_plus(proteinGroups, columnames = remove_rows_marked_with)
    proteinGroups_filtered <- PG_row_filtering_plus$proteinGroups_filtered
    proteinGroups_filtered_out_as_plus <- PG_row_filtering_plus$number_of_rows_filtered_out
  } else{
    proteinGroups_filtered_out_as_plus <- NULL
  }
  
  # keeping columns starts with
  if (length(keep_cols_starts_with) >= 1 ){
    proteinGroups_filtered <- select(proteinGroups_filtered, starts_with(keep_cols_starts_with)) # select command in dplyr package
  }
  
  # shorten column names, by removing the "starts with"
  colnames(proteinGroups_filtered)<-gsub(keep_cols_starts_with, "", colnames(proteinGroups_filtered))
  
  #remove rows with all 0, this is only for data value matrix
  if (filter_all_zero){
    switch(keep_data_type,
           "intensity" = {index_all_zero <- apply(proteinGroups_filtered,1,function(x)all(x == 0))},
           "ratio" = {index_all_zero <- apply(proteinGroups_filtered,1,function(x)all(is.na(x)))}
    )
    #
    proteinGroups_filtered <- proteinGroups_filtered[-which(index_all_zero),]
    proteinGroups_filtered_out_all_zero <- length(which(index_all_zero))
    print(paste0("ProteinGroups with all zeros: ", proteinGroups_filtered_out_all_zero))
  }else{
    proteinGroups_filtered_out_all_zero <- NULL
  }
  
  
  # subsect the annotation file
  proteinGroups_annoatation <- proteinGroups_annoatation[match(rownames(proteinGroups_filtered),rownames(proteinGroups_annoatation)),]
  
  
  # further select columns in Experiment desgin, if not all used in Experiment desgin(if experiment desgin provided)
  if(class(experimentDesgin) ==  "data.frame"){
    
    
    
    proteinGroups_filtered<- proteinGroups_filtered[,which(colnames(proteinGroups_filtered) %in% experimentDesgin[,1])]
    # reorder the columns according columns
    proteinGroups_filtered<-proteinGroups_filtered[,order(colnames(proteinGroups_filtered),decreasing=FALSE)]
    #df<-df[,order(as.numeric(colnames(df)),decreasing=FALSE)]
    
    # only keep the items which are overlapped in the protein.group file
    experimentDesgin <- experimentDesgin[match(colnames(proteinGroups_filtered),as.character(experimentDesgin[,1])),] 
    # check if the experiment is aligned properly
    alignment_check<-cbind(as.character(experimentDesgin[,1]),colnames(proteinGroups_filtered))
    
    
    
    return(list(proteinGroups_filtered = proteinGroups_filtered,
                proteinGroups_annoatation = proteinGroups_annoatation,
                proteinGroups_filtered_out_as_plus =  proteinGroups_filtered_out_as_plus,
                proteinGroups_filtered_out_all_zero = proteinGroups_filtered_out_all_zero,
                groups = experimentDesgin,
                alignment_check = alignment_check
    )) 
    
  }else{
    #write.table(proteinGroups_filtered,"Out_ProteinGroups_tidied_up.txt",sep="\t",row.names = TRUE,col.names = NA)  
    return(list(proteinGroups_filtered = proteinGroups_filtered,
                proteinGroups_annoatation = proteinGroups_annoatation,
                proteinGroups_filtered_out_as_plus =  proteinGroups_filtered_out_as_plus,
                proteinGroups_filtered_out_all_zero = proteinGroups_filtered_out_all_zero
    )) 
  }
  
}









# this function is a revised version filtering out the rows marked by "+", mostly with three columns c("Only.identified.by.site", "Reverse","Potential.contaminant"))


PG_filter_rows_by_plus<-function(proteinGroups, columnames = c("Only.identified.by.site", "Reverse","Potential.contaminant")){
  
  suppressMessages(install.packages.auto(dplyr))
  suppressMessages(install.packages.auto(lazyeval)) # this is very important for passing column names as parameters to the function
  # notice that the rownames are silently dropped druing filter, even there are row names, 
  # set the column names by using a temp column
  proteinGroups_temp <- proteinGroups
  proteinGroups_temp$temp.rownames <- rownames(proteinGroups_temp)
  
  for(filter_name in columnames)  {
    #print(paste("filtering by",filter_name))
    filter_criteria <- lazyeval::interp(quote(x != "+"), x = as.name(filter_name))
    # note:
    # though the underscored version dplyr verbs are deprecated, still, in some cases, only the underscored version works
    # use this try will try filter_ first, which is more likely to work, otherwise try filter, which in most cases not needed
    proteinGroups_filtered <- try(dplyr::filter_(proteinGroups_temp, filter_criteria)) 
    if(class(proteinGroups_filtered)  == "try-error"){
      proteinGroups_filtered <- try(dplyr::filter(proteinGroups_temp, filter_criteria)) 
    }
    number_filter_out <- nrow(proteinGroups_temp) - nrow(proteinGroups_filtered)
    proteinGroups_temp <- proteinGroups_filtered
    
  
    print(paste0(filter_name, ": ", number_filter_out))
    
  }
  # change back the row names
  rownames(proteinGroups_temp) <- proteinGroups_temp$temp.rowname
  proteinGroups_temp <- proteinGroups_temp[,-ncol(proteinGroups_temp)] # remove the temp column
  number_of_rows_filtered_out <- nrow(proteinGroups) -  nrow(proteinGroups_temp)
  
  return(list(
    proteinGroups_filtered = proteinGroups_temp,
    number_of_rows_filtered_out = number_of_rows_filtered_out
  )
  )  
}







# http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r
#' Add grids to a scatterplot3d
#' 
#' @description The goal of this function is to add grids on an existing
#'  plot created using the package scatterplot3d
#' @param x,y,z numeric vectors specifying the x, y, z coordinates of points.
#'  x can be a matrix or a data frame containing 3 columns corresponding to
#'  the x, y and z coordinates. In this case the arguments y and z are optional
#' @param grid specifies the facet(s) of the plot on which grids should be drawn.
#'  Possible values are the combination of "xy", "xz" or "yz".
#'  Example: grid = c("xy", "yz"). The default value is TRUE to add grids only on xy facet.
#' @param col.grid,lty.grid color and line type to be used for grids
#' @param lab a numerical vector of the form c(x, y, len).
#'  The values of x and y give the (approximate) number of tickmarks on the x and y axes.
#' @param lab.z the same as lab, but for z axis
#' @param scale.y of y axis related to x- and z axis
#' @param angle angle between x and y axis
#' @param "xlim, ylim, zlim" the x, y and z limits (min, max) of the plot.
#' 
#' @note
#' Users who want to extend an existing scatterplot3d graphic with the
#'  function addgrids3d, should consider to set the arguments scale.y, angle, ...,
#'  to the value used in scatterplot3d.
#' 
#' @author Alboukadel Kassambara \email{alboukadel.kassambara@@gmail.com}
#' @references http://www.sthda.com
#' 
#' @example
#' library(scatterplot3d)
#' data(iris)
#' scatterplot3d(iris[, 1:3], pch = 16, grid=T, box=F)
#' addgrids3d(iris[, 1:3], grid = c("xy", "xz", "yz"))
addgrids3d <- function(x, y=NULL, z=NULL, grid = TRUE,
                       col.grid = "grey", lty.grid = par("lty"),
                       lab = par("lab"), lab.z = mean(lab[1:2]),
                       scale.y = 1, angle = 40,
                       xlim=NULL, ylim=NULL, zlim=NULL){
  
  
  if(inherits(x, c("matrix", "data.frame"))){
    x <- as.data.frame(x)
    y <- unlist(x[,2])
    z <- unlist(x[,3])
    x <- unlist(x[,1])
  }
  
  p.lab <- par("lab")
  
  angle <- (angle%%360)/90
  yz.f <- scale.y * abs(if (angle < 1) angle else if (angle >3) angle - 4 else 2 - angle)
  yx.f <- scale.y * (if (angle < 2) 1 - angle else angle - 3)
  
  
  # x axis range
  x.range <- range(x[is.finite(x)], xlim)
  x.prty <- pretty(x.range, n = lab[1], min.n = max(1, min(0.5 *lab[1], p.lab[1])))
  x.scal <- round(diff(x.prty[1:2]), digits = 12)
  x <- x/x.scal
  x.range <- range(x.prty)/x.scal
  x.max <- ceiling(x.range[2])
  x.min <- floor(x.range[1])
  if (!is.null(xlim)) {
    x.max <- max(x.max, ceiling(xlim[2]/x.scal))
    x.min <- min(x.min, floor(xlim[1]/x.scal))
  }
  x.range <- range(x.min, x.max)
  
  # y axis range
  y.range <- range(y[is.finite(y)], ylim)
  y.prty <- pretty(y.range, n = lab[2], min.n = max(1, min(0.5 *lab[2], p.lab[2])))
  y.scal <- round(diff(y.prty[1:2]), digits = 12)
  y.add <- min(y.prty)
  y <- (y - y.add)/y.scal
  y.max <- (max(y.prty) - y.add)/y.scal
  if (!is.null(ylim))
    y.max <- max(y.max, ceiling((ylim[2] - y.add)/y.scal))
  
  # Z axis range
  z.range <- range(z[is.finite(z)], zlim)
  z.prty <- pretty(z.range, n = lab.z, min.n = max(1, min(0.5 *lab.z, p.lab[2])))
  z.scal <- round(diff(z.prty[1:2]), digits = 12)
  z <- z/z.scal
  z.range <- range(z.prty)/z.scal
  z.max <- ceiling(z.range[2])
  z.min <- floor(z.range[1])
  if (!is.null(zlim)) {
    z.max <- max(z.max, ceiling(zlim[2]/z.scal))
    z.min <- min(z.min, floor(zlim[1]/z.scal))
  }
  z.range <- range(z.min, z.max)
  
  # Add grid
  if ("xy" %in% grid || grid == TRUE) {
    i <- x.min:x.max
    segments(i, z.min, i + (yx.f * y.max), yz.f * y.max + 
               z.min, col = col.grid, lty = lty.grid)
    i <- 0:y.max
    segments(x.min + (i * yx.f), i * yz.f + z.min, x.max + 
               (i * yx.f), i * yz.f + z.min, col = col.grid, lty = lty.grid)
  }
  
  if ("xz" %in% grid) {
    i <- x.min:x.max
    segments(i + (yx.f * y.max), yz.f * y.max + z.min, 
             i + (yx.f * y.max), yz.f * y.max + z.max, 
             col = col.grid, lty = lty.grid)
    temp <- yx.f * y.max
    temp1 <- yz.f * y.max
    i <- z.min:z.max
    segments(x.min + temp,temp1 + i, 
             x.max + temp,temp1 + i , col = col.grid, lty = lty.grid)
    
  }
  
  if ("yz" %in% grid) {
    i <- 0:y.max
    segments(x.min + (i * yx.f), i * yz.f + z.min,  
             x.min + (i * yx.f) ,i * yz.f + z.max,  
             col = col.grid, lty = lty.grid)
    temp <- yx.f * y.max
    temp1 <- yz.f * y.max
    i <- z.min:z.max
    segments(x.min + temp,temp1 + i, 
             x.min, i , col = col.grid, lty = lty.grid)
  }
  
}


add.alpha <- function(col, alpha=1){
  if(missing(col))
    stop("Please provide a vector of colours.")
  apply(sapply(col, col2rgb)/255, 2, 
        function(x) 
          rgb(x[1], x[2], x[3], alpha=alpha))  
  # demo
  # color <-  c("#00B3FA","#00B3FA80")
  # add.alpha(color, 0.5)
}




withConsoleRedirect <- function(containerId, expr) {
  # Change type="output" to type="message" to catch stderr
  # (messages, warnings, and errors) instead of stdout.
  txt <- capture.output(results <- expr, type = "output")
  if (length(txt) > 0) {
    insertUI(paste0("#", containerId), where = "beforeEnd",
             ui = paste0(txt, "\n", collapse = "")
    )
  }
  results
}




# A, B is a vector, elemment is a also a vector
# A <-c("a,b,c", "d,e,f")
# B <-c("a,b,c,d", "d,e,f,g", "a,b,c,d,e")
# expression <- data.frame(protein = letters[1:7], abundance =  sample(1:100, 7))
## expression has to be two columns data.frame
# r <- compare_two_vectorlist_with_value(A, B, expression)
# 


compare_two_vectorlist_with_value <- function(A, B, expression, sep = ","){
  result_matrix_num <-  matrix(data = NA, ncol = length(A), nrow = length(B))
  rownames(result_matrix_num) <- names(B)
  colnames(result_matrix_num) <- names(A)
  
  result_matrix_ele <- result_matrix_expression <-result_matrix_num
  
  
  
  for(i in 1: length(A)){
    A_element<- strsplit(A[i], sep)[[1]]
    for(j in 1: length(B)){
      B_element<- strsplit(B[j], sep)[[1]]
      elements <- intersect(A_element, B_element)
      result_matrix_ele[j,i] <- toString(elements)
      result_matrix_expression[j,i] <- sum(expression[match(elements,expression[,1]),2])
      
      result_matrix_num[j,i] <- length(intersect(A_element, B_element))
    }
  }
  
  return(list(result_matrix_num = result_matrix_num,
              result_matrix_expression = result_matrix_expression,
              result_matrix_ele = result_matrix_ele
  )
  )
  
}





remove_allNA_rows <- function(df){
  
  if(any(apply(df,1,function(x)all(is.na(x))))){ # if there is any all NA rows
    df[-which(apply(df,1,function(x)all(is.na(x)))),]
  }else{
    df
  }
  
  # this function can deal with data.frame and data matrix
  # note that NA has to be NA, otherwise, precess the data first
}

remove_allNA_columns <- function(df){
  if(any(apply(df,2,function(x)all(is.na(x))))){ # if there is any all NA columns
    df[,-which(apply(df,2,function(x)all(is.na(x))))]
  }else{
    df
  }
  # this function can deal with data.frame and data matrix
  # note that NA has to be NA, otherwise, precess the data first
}




remove_1st_column <- function(df){
  if(dim(df)[2] >=2 && length(unique(df[,1])) == 1){
    #print(length(unique(df[,1])) == 1)
    df <- df[,-1, drop =  FALSE]
    df <- remove_1st_column(df)
    df
  }else{
    df
  }
  # this function deals with hirarchically structured data.frame, for better display using treemap like method 
  # if the first column only has one unique value, remove it
  # if the following one still has one unique value,do it again, untill the remaing first column 
  # example
  # d <- data.frame(a = "A", b = "B", c = LETTERS[1:5])
  # remove_1st_column(d)
}



range_standarize_1 <- function(x){
  (x-min(x))/(max(x)-min(x))
}

range_standarize_100 <- function(x){
  round(100*(x-min(x))/(max(x)-min(x)))
}



# subfunctions, as the function name says
combine_list_to_matrix <-function(vector_list){
  if(class(vector_list) == "list"){
    all <- unique(unlist(vector_list))
    match_list <- lapply(vector_list,function(x) all %in% x)
    presence_matrix <- matrix(unlist(match_list), ncol =length(match_list) , byrow = FALSE)
    colnames(presence_matrix) <- names(vector_list)
    rownames(presence_matrix) <-all
    presence_matrix[which(presence_matrix == FALSE)] <- 0
    presence_matrix[which(presence_matrix == TRUE)] <- 1
    return(presence_matrix)
    
  }else if (class(vector_list) == "character"){
    m <- matrix(1:length(vector_list))
    rownames(m) <- vector_list
    colnames(m) <- "inputlist"
    return(m)
  }
  
  # this function convert a vector list into a expression matrix, with presence as 1, and absence as 0
  
  #test_list <- list(A = letters[1:10], B =  letters[5:15])
  #combine_list_to_matrix(test_list)
}



recode_for_sankey <- function(df){
  install.packages.auto(car) # use the recode function
  nodes_names <- union(df[[1]], df[[2]])
  code <- 0:length(nodes_names)
  nodes <- data.frame(name = nodes_names)
  
  expression <- toString(paste0("'",nodes_names,"'"," = ",code,collapse=";"))
  df_recode<- as.data.frame(apply(df, 2, function(x) {x <- car::recode(x,expression)}))
  
  return(list(df_links = df_recode,
              df_nodes = nodes
  ))
  
  # selfmade function @ 20171130
  # general interaction network function/package accept data.frame data structure
  # with three columns, first two columns are node names with intereactiton direction
  # 3rd column is numeric value 
  # however, currently, sankeyNetwork {networkD3} only accepts Links with data.frame with node number/index , staring from 0 
  # and a Nodes argument with data.frame structure, with the node id and propertie
  # use ?sankeyNetwork for more help
  
  # this fucntion preprocess the edge data.frame and retures two data.frames in one variable, ready for plot
  # links data.frame format, the same as input data.frame, with node names converted to id/index (numeric)
  # the first col is the from/soruce column#
  # the second col is the to/target column
  # node data.frame format:
  # one column, name of node id 
  
  # example
  # df <- data.frame(from = sample(letters,10), to = sample(letters,10), value = sample(1:100,10))
  # df_s <- recode_for_sankey(df)
  
  
}


# this function can wrap long sentence into \n separated multilines, based on words
# this bult in function has the same function strwrap

wrap_sentence <- function(string, width) {
  words <- unlist(strsplit(string, " "))
  fullsentence <- ""
  checklen <- ""
  for(i in 1:length(words)) {
    checklen <- paste(checklen, words[i])
    if(nchar(checklen)>(width+1)) {
      fullsentence <- paste0(fullsentence, "\n")
      checklen <- ""
    }
    fullsentence <- paste(fullsentence, words[i])
  }
  fullsentence <- sub("^\\s", "", fullsentence)
  fullsentence <- gsub("\n ", "\n", fullsentence)
  return(fullsentence)
}




# A, B is a vector, elemment is a also a vector
# A <-c("a,b,c", "d,e,f")
# B <-c("a,b,c,d", "d,e,f,g", "a,b,c,d,e")
# r <- compare_two_vector_list(A, B)

compare_two_vector_list <- function(A, B, sep = ","){
  result_matrix_num <-  matrix(data = NA, ncol = length(A), nrow = length(B))
  rownames(result_matrix_num) <- names(B)
  colnames(result_matrix_num) <- names(A)
  
  result_matrix_ele <-result_matrix_num
  
  
  
  for(i in 1: length(A)){
    A_element<- strsplit(A[i], sep)[[1]]
    for(j in 1: length(B)){
      B_element<- strsplit(B[j], sep)[[1]]
      result_matrix_ele[j,i] <- toString(intersect(A_element, B_element))
      result_matrix_num[j,i] <- length(intersect(A_element, B_element))
    }
  }
  
  return(list(result_matrix_num = result_matrix_num,
              result_matrix_ele = result_matrix_ele
  )
  )
  
}





# turan a 2d data matrix into a column based percentage 

table2lPercents_by_col <- function (data_matrix, digits = 2){
  sums <- colSums(data_matrix)
  per <- t(apply(data_matrix, 1, function(x) x/sums))
  per <- round(100 * per, digits)
  per
  
}





ggsimpleline <- function(data_matrix, 
                         plot.by = "Row", 
                         x_cord = NULL, # has to be a numeric vector, with the same length with either row (for column plot) or column (for row plot)
                         index = c(1,2),
                         linetype = "solid",
                         linewidth = 1,
                         spline_smooth = FALSE,
                         # down is from ggplot2_prettier
                         maintitle = "",
                         xlab = "",
                         ylab = "",
                         axis.text.angle.x = 0,
                         axis.text.angle.y = 0,
                         vertical =  FALSE
){
  
  #install.packages.auto(reshape2)
  data<- as.matrix(data_matrix)
  
  
  if(plot.by == "Row"){
    df <- t(data[index, ,drop = FALSE])
    xlabels <- colnames(data)
  } else if(plot.by == "Column"){
    df <- data[,index,drop = FALSE]
    xlabels <- rownames(data)
  }
  
  if(!is.null(x_cord)){
    rownames(df) <- x_cord
  }
  
  m<- reshape2::melt(as.matrix(df))
  
  
  if(spline_smooth){
    # generate a new df for geom_line
    if(!is.null(x_cord)){
      
      df_smooth<- data.frame(apply(df, 2,function(x){spline(as.numeric(x_cord),x)$y}))
      m_smooth<- reshape2::melt(as.matrix(df_smooth))
      #m_smooth$Var1 <- spline(as.numeric(x_cord))$y 
      m_smooth$Var1 <- spline(as.numeric(x_cord),df[,1])$x
      
    }else{
      df_smooth<- data.frame(apply(df, 2,function(x){spline(x)$y}))
      m_smooth<- reshape2::melt(as.matrix(df_smooth))
      m_smooth$Var1 <- spline(1:nrow(df))$y # spline will generate 3 times point smoother by default ( change by n)
    }
    
    
    p <-   ggplot(m, aes(Var1, value, group=Var2, color = Var2)) + 
      geom_point()+
      geom_line(
        data=m_smooth,
        linetype = linetype,
        size = linewidth) +
      scale_colour_discrete(name = "")
    
    
  }else{
    p <- ggplot(m, aes(Var1, value, group=Var2, color = Var2)) + 
      geom_line(linetype = linetype,
                size = linewidth)  + 
      scale_colour_discrete(name = "")
    
  }
  
  p <- ggplot2_prettier(p,
                        maintitle = maintitle,
                        xlab = xlab,
                        ylab = ylab,
                        axis.text.angle.x = axis.text.angle.x,
                        axis.text.angle.y = axis.text.angle.y,
                        vertical =  FALSE
  )
  
  return(p)
  
}





ggplot2_prettier <- function(ggplot2_object,
                             maintitle = NULL,
                             xlab = NULL,
                             ylab = NULL,
                             axis.text.angle.x = 0,
                             axis.text.angle.y = 0,
                             vertical =  FALSE)
{
  
  p <- ggplot2_object 
  
  
  # title and labeling
  
  if(!is.null(maintitle)){
    if(maintitle != ""){
      p <- p + ggtitle(maintitle)
    }
  }
  
  if(!is.null(xlab)){
    if(xlab != ""){
      p <- p +xlab(xlab)
    }
    
  }
  
  if(!is.null(ylab)){
    if(ylab != ""){
      p <- p +ylab(ylab)
    }
   
  }

  p <- p+ theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = axis.text.angle.x, hjust = 1),
        axis.text.y = element_text(angle = axis.text.angle.y, hjust = 1),
        panel.grid = element_blank()) 
  
  
  if(vertical){
    p <- p+ coord_flip()
  }

  
  return(p)
  
}




#________________________________________________________________________________________
#     matrix_quick_heatmap
#________________________________________________________________________________________

# ___Description___: 
# 1: 
# 2: 

# ___Arguments___:
# data matrix
# factor (usually the grouping information) to be color grouped in either column  or row

#____Usage____;

# ___Values___:
# 
# 


matrix_quick_heatmap <- function(matrix, 
                                 scale = "row",
                                 col_groupcolor_factor = NULL , 
                                 row_groupcolor_factor = NULL, 
                                 Col_tree = TRUE){
  
  install.packages.auto(gplots)
  svg(tempfile(),onefile = TRUE)
  dev.control('enable')  
  
  if(is.null(row_groupcolor_factor) & is.null(col_groupcolor_factor)){
    # if no grouping information provided
    if(Col_tree){
      heatmap.2(matrix, col=bluered, trace = "none",
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = "") 
    }else{
      heatmap.2(matrix, col=bluered, trace = "none",
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                Colv  = FALSE,
                dendrogram = "row",
                key.title  = "",
                key.xlab = "",
                key.ylab = "") 
    }
    
    
    
  }else if((length(col_groupcolor_factor) > 0) & is.null(row_groupcolor_factor)){
    # if only colum grouping information provided
    
    col_groupcolor_factor <- col_groupcolor_factor[order(col_groupcolor_factor[,2]),]
    matrix<- matrix[,match(col_groupcolor_factor[,1],colnames(matrix))]
    
    color_labeling <- factor(col_groupcolor_factor[,2]) # removing unused factors
    levels(color_labeling) <- rainbow(length(levels(color_labeling))) # rename the 
    color_labeling <- as.vector(color_labeling)
    if(Col_tree){
      
      heatmap.2(matrix, 
                col=bluered, 
                ColSideColors = color_labeling,
                trace = "none", 
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = "") 
      
      
    }else{
      heatmap.2(matrix, 
                col=bluered, 
                ColSideColors = color_labeling,
                trace = "none", 
                scale = scale,
                Colv  = FALSE, 
                dendrogram = "row",
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = ""
      ) 
    }
    
  }else if((length(row_groupcolor_factor) > 0) & is.null(col_groupcolor_factor)){
    # if only row grouping information provided
    
    row_groupcolor_factor <- row_groupcolor_factor[match(rownames(matrix), row_groupcolor_factor[,1]),]
    
    row_groupcolor_factor <- row_groupcolor_factor[order(row_groupcolor_factor[,2]),]
    matrix<- matrix[match(row_groupcolor_factor[,1],rownames(matrix)),]
    
    color_labeling <- factor(row_groupcolor_factor) # removing unused factors
    levels(color_labeling) <- rainbow(length(levels(color_labeling))) # rename the 
    color_labeling <- as.vector(color_labeling)
    if(Col_tree){
      heatmap.2(matrix,
                col=bluered, 
                ColSideColors = color_labeling,
                trace = "none", 
                scale = scale,
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = "" ) 
      
    }else{
      heatmap.2(matrix,
                col=bluered, 
                ColSideColors = color_labeling,
                trace = "none", 
                scale = scale,
                Colv  = FALSE,
                dendrogram = "row",
                keysize = 1.5,
                density.info = "none",
                key.title  = "",
                key.xlab = "",
                key.ylab = ""
      ) 
    }
  }
  p1 <- recordPlot()
  dev.off()
  return(p1)
}

