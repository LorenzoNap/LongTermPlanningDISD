library(fpp)
library(lubridate)
library(plyr)
library(stlplus)
library(pander)
library(xtable)
library(grid)
library(gridExtra)
library(gtable)
library(fpp)
library(ggfortify)
library(cluster)
library(factoextra)


clusteringFunctions <- function(kvoli_rotte, dataFrame, pathFile){
  
  dataFrame <- scale(dataFrame)
  
  fviz_nbclust(dataFrame, kmeans, method = "wss") +
    geom_vline(xintercept = 3, linetype = 2)+
    labs(subtitle = "Elbow method")
  
  ggsave(paste(pathFile,"Elbow method",".png", sep=""))
  
  # Silhouette method
  fviz_nbclust(dataFrame, kmeans, method = "silhouette")+
    labs(subtitle = "Silhouette method")
  
  ggsave(paste(pathFile,"Silhouette method",".png", sep=""))
  
  # Gap statistic
  # nboot = 50 to keep the function speedy. 
  # recommended value: nboot= 500 for your analysis.
  # Use verbose = FALSE to hide computing progression.
  set.seed(123)
  fviz_nbclust(dataFrame, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
    labs(subtitle = "Gap statistic method")
  
  ggsave(paste(pathFile,"Gap statistic method",".png", sep=""))
  
}

printCluster <- function(kvoli_rotte, dataFrame, pathFile) {
  # Plot del cluster a partire dal kmeans
  #
  # Args:
  #   kvoli_rotte: risultato dell'esecuzione del kmeans
  #   dataFrame: data frame sul quale visualizzare i cluster
  
  # cluster <- factor(kvoli_rotte$cluster)
  # 
  # centroidi <- kvoli_rotte$centers
  # 
  # centroidi[,c("MTOW")]     <- 2*sd(dataFrame[,c("MTOW")])*centroidi[,c("MTOW")]         + mean(dataFrame$MTOW)*array(1, c(3,1))
  # centroidi[,c("distanza")] <- 2*sd(dataFrame[,c("distanza")])*centroidi[,c("distanza")] + mean(dataFrame[,c("distanza")])*array(1, c(3,1))
  # 
  # centroids <- data.frame(centroidi)
  # ggplot() +
  #   geom_point(data = as.data.frame(dataFrame),
  #              aes(x = distanza,
  #                  y = MTOW,
  #                  color = cluster),
  #              size = 3) +
  #   geom_point(data=centroids, aes(x=distanza,y=MTOW, color="Center")) +
  #   geom_point(data=centroids, aes(x=distanza,y=MTOW, color="Center"), size=52, alpha=.3, show.legend=F)
  fviz_cluster(kvoli_rotte, voli_rotte_clean, geom = c("point"))
  ggsave(paste(pathFile,"clustering",".png", sep=""))
  
}
