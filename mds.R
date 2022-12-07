calcMds <- function(df, algo, binary = F) {
  #reproducibility
  set.seed(55)
  nms <- metaMDS(log1p(df), algo, binary = binary, 2, 100, 1000)
  print(nms)
  return(nms)
}

plotMds <- function(p, nms, forPrint = F) {

  nmsPoints <- as.data.frame(nms$points)
  nmsPoints$samp <- row.names(nmsPoints)
  
  #assign to var for return
  p <- p +
    #Add labels "sample names" to the points when howering and printing
    geom_point(data = nmsPoints, aes(label = samp, x = MDS1, y = MDS2), size = 3) +
    geom_text_repel(data = nmsPoints, aes(x = MDS1, y = MDS2, label = samp), size = 3)+
    theme_bw()
  return(p)
}