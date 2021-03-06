# Questo file è il capofila di tutti gli altri presenti nella cartella.
# Lanciare questo per far partire il data mining!
#
# Questo file lancia l'estrazione delle rotte dal db e ne calcola le distanze
# - inoltre illustra con una mappa di tutte le destinazioni possibili!
# - ha come output "rotte.agg2"
source("AnalisiDatabaseDataMining\\rotte2.R", echo = TRUE)
# 
# Ottenute le rotte, si fa il join tre i voli e le rotte
# - rilascia in output voli_rotte2
source("AnalisiDatabaseDataMining\\voli_vs_rotte.R", echo = TRUE)
#
# Modello di regressione lineare
# - ha in input voli_rotte2
#source("AnalisiDatabaseDataMining\\regressione_lineare_aerei_vs_km_vs_flight_type.R")
# 
# Modello di clustering
# - ha in input voli_rotte2
#source("AnalisiDatabaseDataMining\\clustering_aerei_vs_km.R")
