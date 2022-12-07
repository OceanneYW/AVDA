#Plot dendrogram default "bray-curtis"
#Return cluster object for tree cutting and plot dendrogram

#TODO: implement binary (binary = T), passed in as arg
calcDendro <- function(df, algo = 'bray', binary = FALSE){
  d_overView <- vegdist(log1p(df), algo, binary = binary)
  hc <- hclust(d_overView, "average")
  return(hc)
}

plotDendro <- function(hc) {
  hcd <- as.dendrogram(hc)
  hcggd <- dendro_data(hcd, type = "rectangle")
  # plot dendrogram horizontally 
  gg <- ggdendrogram(hc, rotate = TRUE)
  
  #TODO: fix label overlap with the branches
  # p <- ggplot(segment(hcggd))+
  #   geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  #   geom_text(data = label(hcggd), 
  #             aes(x = x, y = y, label = label), vjust = -0.5, size = 3) +
  #   coord_flip() + 
  #   scale_y_reverse(expand = c(0.2, 0))

  return(gg)
}
