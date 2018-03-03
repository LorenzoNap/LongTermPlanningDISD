source("AnalisiSerieStoriche\\SerieStoricheFunctions.R", echo = TRUE)

#mie serie
reg.arr.db <- ts(reg.arr.aflight$CONTO, frequency=12, start=startSeriesDB)
reg.dep.db <- ts(reg.dep.aflight$CONTO, frequency=12, start=startSeriesDB)
int.arr.db <- ts(int.arr.aflight$CONTO, frequency=12, start=startSeriesDB)
int.dep.db <- ts(int.dep.aflight$CONTO, frequency=12, start=startSeriesDB)
dom.arr.db <- ts(dom.arr.aflight$CONTO, frequency=12, start=startSeriesDB)
dom.dep.db <- ts(dom.dep.aflight$CONTO, frequency=12, start=startSeriesDB)


timeSeriesAnalysis <- function(tSerie, title, path, tWindow){
  plotTimeseries(tSerie,title ,path,TRUE)
  plotForecastTrainingSet(tSerie, tWindow,title ,path, TRUE)
  plotArimaModel(tSerie, tWindow, title ,path, TRUE)
  bestModel <- evaluateBesModel(tSerie, tWindow, title ,path, save = TRUE)
  print( paste("Il miglior modello in base alle misure di errore per", "Serie Storica Reg Arr","e'",bestModel ))
}

#arrivi regionali
timeSeriesAnalysis(reg.arr.db, "Serie Storica Reg Arr","./AnalisiSerieStoriche//resultAnalysis//ArrReg//", 36)
#partenze regionali
timeSeriesAnalysis(reg.arr.db, "Serie Storica Dep Arr","./AnalisiSerieStoriche//resultAnalysis//DepReg//", 36)

#arrivi int
timeSeriesAnalysis(reg.arr.db, "Serie Storica Int Arr","./AnalisiSerieStoriche//resultAnalysis//ArrInt//", 36)
#partenze int
timeSeriesAnalysis(reg.arr.db, "Serie Storica Int Dep","./AnalisiSerieStoriche//resultAnalysis//DepInt//", 36)

#arrivi dom
timeSeriesAnalysis(reg.arr.db, "Serie Storica Dom Arr","./AnalisiSerieStoriche//resultAnalysis//ArrDom//", 36)
#partenze dom
timeSeriesAnalysis(reg.arr.db, "Serie Storica Dom Dep","./AnalisiSerieStoriche//resultAnalysis//DepDom//", 36)