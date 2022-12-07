optimalClusters <- function(df, method = "wss"){
  set.seed(55)
  fviz_nbclust(df, kmeans, nstart = 10, method = method)
}

plotKMeans <- function(df, num_cluster = 2){
  fit <- kmeans(df, num_cluster)
  fviz_cluster(fit, df)
}
