#Modulo1 - Interpretazione db
source("InterpretazioneDB\\caricamento_serie_da_db.R", echo = TRUE)
#Modulo2 - Analisi DataBaseMining
source("AnalisiDatabaseDataMining\\main.R", echo = TRUE)
#crea le rotte 2
source("AnalisiDatabaseDataMining\\AnalisiRegressioneLineare\\voli_vs_rotte.R", echo = TRUE)
#clustering
source("ClusterAnalysis\\clustering_aerei_vs_km.R", echo = TRUE)
#analisi da clustering
source("NuovaAnalisiDaCluster\\time_series_analysis_from_clusters.R", echo = TRUE)
