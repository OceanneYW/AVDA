calcEnvfit <- function(nms, env, permutations = 999){
  fit <- envfit(nms, log1p(env), perm = permutations)
  return(fit)
}

plotEnvfit <- function(fit, nms){
  fit_df <- as.data.frame(fit$vectors$arrows*sqrt(fit$vectors$r))
  fit_df$species<-rownames(fit_df)
  fit_df$pvals <- fit$vectors$pvals
  fit_df$r2 <- fit$vectors$r
  
  # Filter statistically significant
  fit_df <- fit_df %>% filter(pvals <= 0.05)
  fit_df <-fit_df %>% filter(r2 >= 0.6)
  fit_df$label <- row.names(fit_df)
  
  nmsPoints <- as.data.frame(nms$points)
  nmsPoints$samp <- row.names(nmsPoints)
  
  #TODO: Standardized scaling for arrows?
  p <- ggplot(data = fit_df, aes(x = NMDS1, y = NMDS2))+
    geom_text_repel(aes(label=species))+
    geom_point(data = nmsPoints, aes(x=MDS1,y=MDS2))+
    #TODO: arrows different color with different genomotology
    geom_segment(data = fit_df, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, color = "Strength"), 
                 arrow = arrow())+
    scale_color_brewer(palette = "RdBu")+
    geom_text_repel(data = nmsPoints,aes(x=MDS1,y=MDS2,label=samp))
  return(p)
}

appendEnvfitToPlot <- function(fit, nmdsPlot){
  #nmsPoints <- as.data.frame(nms$points)
  p <- nmdsPlot + geom_segment(data = fit_df, aes(x=0, y=0, xend=NMDS1, yend=NMDS2, label=label), 
                               arrow = arrow())+
    geom_text_repel(aes(label=species))
  return(p)
}
