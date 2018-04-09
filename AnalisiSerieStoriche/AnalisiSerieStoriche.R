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

#arrivi regionali
timeSeriesAnalysis(reg.arr.db, "Serie Storica Reg Arr","./AnalisiSerieStoriche//resultAnalysis//ArrReg//", 36, saveFile)
#partenze regionali
timeSeriesAnalysis(reg.arr.db, "Serie Storica Dep Arr","./AnalisiSerieStoriche//resultAnalysis//DepReg//", 36, saveFile)
#arrivi int
timeSeriesAnalysis(reg.arr.db, "Serie Storica Int Arr","./AnalisiSerieStoriche//resultAnalysis//ArrInt//", 36, saveFile)
#partenze int
timeSeriesAnalysis(reg.arr.db, "Serie Storica Int Dep","./AnalisiSerieStoriche//resultAnalysis//DepInt//", 36, saveFile)
#arrivi dom
timeSeriesAnalysis(reg.arr.db, "Serie Storica Dom Arr","./AnalisiSerieStoriche//resultAnalysis//ArrDom//", 36)
#partenze dom
timeSeriesAnalysis(reg.arr.db, "Serie Storica Dom Dep","./AnalisiSerieStoriche//resultAnalysis//DepDom//", 36)