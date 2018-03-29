# ESTRAZIONE DATI DA DB JNB -----------------------------------------------
# Collegati al db di SQL 2016
library(RODBC)
source("./parametriConnessioniFile.R")
dbhandle <-odbcDriverConnect(connection=param.connessione.db)


a_flight   <- sqlQuery(dbhandle, "SELECT * FROM A_FLIGHT")
a_movement <- sqlQuery(dbhandle, "SELECT * FROM A_MOVEMENT")
library(dplyr)
dep_joined <- left_join(a_movement,a_flight,by=c("DEP_FLIGHT_ID"="ID"))
dep_joined <- select(dep_joined, DEP_FLIGHT_ID, AIRCRAFT_ID, AIRCRAFT_ID, AIRCRAFT_TYPE_ID, ROUTE_ID, FLIGHT_TYPE_ID, STO, QUALIFIER_ID, EXCEPTION_ID)
# Query associate (ricavate da datamining originario)
dep_joined <-rbind(
  dep_joined[which(dep_joined$FLIGHT_TYPE_ID==1 & is.na(dep_joined$EXCEPTION_ID) & dep_joined$QUALIFIER_ID==1),], #Regionali 
  dep_joined[which(dep_joined$FLIGHT_TYPE_ID==2 & is.na(dep_joined$EXCEPTION_ID) & dep_joined$QUALIFIER_ID==1),], #Internazionali
  dep_joined[which(dep_joined$FLIGHT_TYPE_ID==2 & is.na(dep_joined$EXCEPTION_ID) & dep_joined$QUALIFIER_ID==5),],
  dep_joined[which(dep_joined$FLIGHT_TYPE_ID==3 & is.na(dep_joined$EXCEPTION_ID) & dep_joined$QUALIFIER_ID==1),]) #Domestici
#
dep_joined <- left_join(dep_joined,as.data.frame(mappa_cluster),by=c("DEP_FLIGHT_ID"="ID"))
#
head(dep_joined)

library("cluster")
library(factoextra)

set.seed(123)
# Compute the gap statistic
gap_stat <- clusGap(voli_rotte_clean, FUN = kmeans, nstart = 25, 
                    K.max = 10, B = 10) 
fviz_gap_stat(gap_stat)


fviz_cluster(kvoli_rotte, voli_rotte_clean, geom = c("point"))

sil <- silhouette(kvoli_rotte$cluster, dist(voli_rotte_clean))
fviz_silhouette(sil)




source("./NuovaAnalisiDaCluster//time_series_analysis_from_clusters.r")
source("./AnalisiSerieStoriche//SerieStoricheFunctions.r")

pathFile <- "./NuovaAnalisiDaCluster//resultAnalysis//"


printCluster(kvoli_rotte, voli_rotte_clean, pathFile)


#questa funzione ci mette un infinita' di tempo
#clusteringFunctions(kvoli_rotte, voli_rotte_clean, pathFile)

#itera sul numero di cluster
for (i in seq(1,n_cluster)) {
  #per ogni cluster estrai i voli associati
  nam <- paste("A", i, sep = "")
  temp <- dep_joined %>% filter(cluster == i)###### %>% filter(QUALIFIER_ID == 1)
  # format delle date
  x <-format(temp, format="%Y-%m")
  #crea il data frame che contiene le frequenze delle date (Data, numero voli)
  df <- as.data.frame(table(x$STO))
  #crea la serie storica
  tserie <- ts(df[,-1], frequency=12, start=c(2003,4), end=c(2017,3))
  
  #crea la sub directory
  dir.create(file.path(pathFile, paste("Cluster", i, sep="")), showWarnings = FALSE)
  
  pathFileTemp <- paste(pathFile, paste("Cluster", i,"//", sep=""), sep="")
  
  tWindow <- 36
  #visualizza i plot della serie storica
  plotTimeseries(tserie,paste("#Voli Cluster ",i) ,pathFileTemp,TRUE)
  #cerca il miglior modello tra "Mean method","Naive method","Drift method", "Seasonal naive method"
  plotForecastTrainingSet(tserie, tWindow, paste("#Voli Cluster ",i) ,pathFileTemp, TRUE)
  #visualizza arima
  plotArimaModel(tserie, tWindow, paste("#Voli Cluster ",i) ,pathFileTemp, TRUE)
  #prendi il miglior modello
  bestModel <- evaluateBesModel(tserie, tWindow, paste("#Voli Cluster ",i),pathFileTemp, save = TRUE)
  #print del modello migliore
  print( paste("Il miglior modello in base alle misure di errore per", "Serie Storica Reg Arr","e'",bestModel ))
  
}


