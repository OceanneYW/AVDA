calcClusterHulls <- function(ql_bry_dend, nmds, num_clusters){
  
  #Cut tree into desired number of clusters based on k-means
  groups <- cutree(ql_bry_dend, k=num_clusters)

  groups.df <- data.frame(
    samples = names(groups),
    #need the clusters to be factor to plot the polygons properly with ggplot
    cluster = as.factor(groups))
  
  
  # Convert nmds into a dataframe
  nmds.df <- as.data.frame(nmds$points)
  
  # Create a "samples" column with rownames for inner_join 
  nmds.df$samples <- rownames(nmds.df)
  nmds.groups.df <- inner_join(nmds.df, groups.df, by="samples")
  
  # get the convex hulls
  hulls <- nmds.groups.df %>% group_by(cluster) %>% 
    mutate(hull = 1:n(), hull = as.numeric(factor(hull, chull(MDS1, MDS2)))) %>%
    arrange(cluster)%>%arrange(hull) %>% filter(!is.na(hull))
  
  return (hulls)
}

appendClustersToPlot <- function(ql_bry_dend, nmds, nmds_plot, num_clusters) {
  hulls <- calcClusterHulls(ql_bry_dend, nmds, num_clusters)
  nmds_plot <- nmds_plot +
 
    #alpha intensity of the fill
    #fill = cluster -> legend
    #Color = grey grey outline
    geom_polygon(data = hulls, alpha = 0.25, show.legend = F, size = 1,
                 aes(x = MDS1, y = MDS2, group = cluster, fill = cluster),
                 color = "grey")
  return (nmds_plot)
}
