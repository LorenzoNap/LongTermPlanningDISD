source("AnalisiSerieStoriche\\SerieStoricheFunctions.R", echo = TRUE)

print("#### Esecuzione iniziata ####")

n <- readline(prompt="Vuoi Salvare i file (y/n): ")

if( n == 'y' || n == 'yes'){
  saveFile = TRUE
} else if ( n == 'n' || n == 'no') {
  saveFile = FALSE
} else {
  saveFile = FALSE;
}

sprintf("#### Salvataggio File: %s ####", saveFile)



timeSeriesAnalysis <- function(tSerie, title, path, tWindow, saveFile){
  #plotTimeseries(tSerie,title ,path,saveFile)
  #plotForecastTrainingSet(tSerie, tWindow,title ,path, saveFile)
  #plotArimaModel(tSerie, tWindow, title ,path, saveFile)
  bestModel <- evaluateBesModel(tSerie, tWindow, title ,path, save = saveFile)
  print( paste("Il miglior modello in base alle misure di errore per", "Serie Storica Reg Arr","e'",bestModel ))
  #plot completo del best model
  plotBestModel(bestModel, tSerie, tWindow, title ,path, save = saveFile)
  
  
}

# arrivi regionali
timeSeriesAnalysis(reg.arr.db, "Regional Arrival Time Series","./AnalisiSerieStoriche//resultAnalysis//ArrReg//", 36, saveFile)
# partenze regionali
timeSeriesAnalysis(reg.dep.db, "Regional Departure Time Series","./AnalisiSerieStoriche//resultAnalysis//DepReg//", 36, saveFile)
#arrivi int
timeSeriesAnalysis(int.arr.db, "International Arrival Time Series","./AnalisiSerieStoriche//resultAnalysis//ArrInt//", 36, saveFile)
#partenze int
timeSeriesAnalysis(int.dep.db, "International Departure Time Serie","./AnalisiSerieStoriche//resultAnalysis//DepInt//", 36, saveFile)
#arrivi dom
timeSeriesAnalysis(dom.arr.db, "Domestic Arrival Time Series","./AnalisiSerieStoriche//resultAnalysis//ArrDom//", 36, saveFile)
#partenze dom
timeSeriesAnalysis(dom.dep.db, "Domestic Departure Time Series","./AnalisiSerieStoriche//resultAnalysis//DepDom//", 36, saveFile)