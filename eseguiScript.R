#Modulo1 - Interpretazione db
source("InterpretazioneDB\\caricamento_serie_da_db.R", echo = TRUE)
#Modulo2 - Interpretazione db
source("AnalisiSerieStoriche\\AnalisiSerieStoriche.R", echo = TRUE)
#Modulo2.1 - Interpretazione db - Serie Covariate
source("AnalisiSerieStoriche\\AnalisiSerieStoricheCovariate.R", echo = TRUE)

#Modulo3 - Analisi DataBaseMining
source("AnalisiDatabaseDataMining\\main.R", echo = TRUE)
#Modulo3.1 - Analisi DataBaseMining/RegressioneLineare #non trovo il file
source("AnalisiDatabaseDataMining\\AnalisiRegressioneLineare\\regressione_lineare_aerei_vs_km_vs_flight_type.R")



#Modulo3.2 - Cluster analysis
source("ClusterAnalysis\\clustering_aerei_vs_km.R", echo = TRUE)
#Modulo3.2 - Analisi Serie Storiche
source("NuovaAnalisiDaCluster\\flight_secondo_cluster_lorenzo.R", echo = TRUE)
