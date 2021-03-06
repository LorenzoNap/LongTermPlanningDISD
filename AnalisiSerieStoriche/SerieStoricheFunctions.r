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

widthGraphs <- 10


plotTimeseries <- function(tserie, title, path, save = TRUE) {
  # Plot della serie storica passata in input
  #
  # Args:
  #   tserie: serie storica in formato ts
  #   tittle: titolo da visualizzare nel plot
  theme_set(theme_bw())
  df <- data.frame(Y=as.matrix(tserie), date=as.Date(as.yearmon(time(tserie))))
  
  ggplot(df, aes(x=date)) + 
    geom_line(aes(y=Y), color="blue") + 
    labs(title=title, 
         y="Number of Flights", x = "Time",
         subtitle=paste("Time series"),
         color=NULL) +  # title and caption
    scale_x_date(date_labels="%b %Y", date_breaks  ="4 month") +  # line color
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
          panel.grid.minor = element_blank())
  
  
  if(save){
    ggsave(paste(path,title,".jpg", sep=""), width = widthGraphs)
  }
  
  ggseasonplot(tserie, main=paste(sep="","Seasonal Plot ",title))
  
  if(save){
    ggsave(paste(path,"Boxplot ", title,".jpg", sep=""), width = widthGraphs)
  }
  
  autoplot(decompose(tserie), main= paste("Decomposition", title))
  
  if(save){
    ggsave(paste(path,"Decomposition ", title,".jpg", sep=""), width = widthGraphs)
  }
  
  dir.create(file.path(path, "CorrelationPlots//"), showWarnings = FALSE)
  
  
  #ACF e PACF
  require(cowplot)
  
  plot_grid(ggAcf(tserie, main=paste("ACF", title)), ggPacf(tserie, main=paste("PACF", title)))
  if(save){
    ggsave(paste(paste(path, "CorrelationPlots//", sep = ""),"1 ACF_PACF plot ", title,".jpg", sep=""), width = widthGraphs)
  }
  
  #detrend time series
  tserieDeTrend <- stl(tserie, s.window = 'periodic')$time.series[,2]
  tserieDeTrend <- tserie - tserieDeTrend
  autoplot(tserieDeTrend, main=paste("Detrend ", title), ylab = "Number of Flights", xlab="time")

  if(save){
    ggsave(paste(paste(path, "CorrelationPlots//", sep = ""),"2 Detrend ", title,".jpg", sep=""), width = widthGraphs)
  }  
  
  #PACF e ACF detrend
  plot_grid(ggAcf(tserieDeTrend, main=paste("ACF Detrend", title)), ggPacf(tserieDeTrend, main=paste("PACF Detrend", title)))
  if(save){
    ggsave(paste(paste(path, "CorrelationPlots//", sep = ""),"3 ACF_PACF Detrend plot ", title,".jpg", sep=""), width = widthGraphs)
  }
  
  #remove season
  tserieDeTrendDeSeason <- diff(tserieDeTrend, lG = 12)
  autoplot(tserieDeTrendDeSeason, main=paste("Detrend, Deseason ", title), ylab = "Number of Flights", xlab="time")
  if(save){
    ggsave(paste(paste(path, "CorrelationPlots//", sep = ""),"4 Detrend, DeSeason ", title,".jpg", sep=""), width = widthGraphs)
  }
  #ACF e PACF
  plot_grid(ggAcf(tserieDeTrendDeSeason, main=paste("ACF Detrend, DeSeason", title)), ggPacf(tserieDeTrendDeSeason, main=paste("PACF Detrend, DeSeason", title)))
  if(save){
    ggsave(paste(paste(path, "CorrelationPlots//", sep = ""),"5 ACF_PACF Detrend, DeSeason plot ", title,".jpg", sep=""), width = widthGraphs)
  }
  
  
}


plotForecastTrainingSet <- function(tserie, tWindow = 36, title, path, save = TRUE){
  # Training di diversi modelli di previsione a partire dal training set. Dopo il training
  # viene effettuato il plot delle previsioni di ogni modello a partire dalla fine del training set.
  #
  # Args:
  #   tserie: serie storica in formato ts
  #   tWindow: periodo delle previsioni (in mesi), di default e' 36 mesi
  #   tittle: titolo da visualizzare nel plot
  #   save: salva il grafico invece di mostrarlo

  # training set
  sr <- window(tserie, start=c(2003,4), end=c(2014,4))
  # test set
  ser = window(tserie, start=c(2014,5), end=c(2017,3))
  
  # variabili che servono per calcolarmi il minimo e il massimo dei modelli (per il grafico)
  
  meanMethod <- meanf(sr,h=tWindow)
  naiveMethod <- rwf(sr,h=tWindow)
  sNaiveMethod <- rwf(sr,drift=TRUE,h=tWindow)
  driftMethod <- snaive(sr,h=tWindow)
  arimaMod <- auto.arima(sr, stepwise=FALSE, approximation=FALSE)
  arimaMod.Fr <- forecast(arimaMod, h=tWindow)
 
  dfMeanMethod <- data.frame(Y=as.matrix(meanMethod$mean), date=as.Date(as.yearmon(time(meanMethod$mean))))
  dfsNaiveMethod <- data.frame(Y=as.matrix(sNaiveMethod$mean), date=as.Date(as.yearmon(time(sNaiveMethod$mean))))
  dfnaiveMethod <- data.frame(Y=as.matrix(naiveMethod$mean), date=as.Date(as.yearmon(time(naiveMethod$mean))))
  dfdriftMethod <- data.frame(Y=as.matrix(driftMethod$mean), date=as.Date(as.yearmon(time(driftMethod$mean))))
  dfArimaMod <- data.frame(Y=as.matrix(arimaMod.Fr$mea), date=as.Date(as.yearmon(time(arimaMod.Fr$mea))))
  
  
  dfSr <- data.frame(Y=as.matrix(sr), date=as.Date(as.yearmon(time(sr))))
  dfSer <- data.frame(Y=as.matrix(ser), date=as.Date(as.yearmon(time(ser))))
  
  
  ggplot() +
    geom_line(data=dfMeanMethod, aes(date, Y, color="Mean Method"), size = 0.5) +
    geom_line(data=dfsNaiveMethod, aes(date, Y,  color="Naive Method"), size = 0.5)+
    geom_line(data=dfnaiveMethod, aes(date, Y,  color="Drift Method"), size = 0.5)+
    geom_line(data=dfdriftMethod, aes(date, Y,  color="SeasonalMaive Method"), size = 0.5)+
    geom_line(data=dfArimaMod, aes(date, Y,  color="Arima Method"), size = 0.5)+
    geom_line(data=dfSr, aes(date, Y,  color="Test Set"), size = 0.5)+
    labs(title=title, 
         subtitle = "Prediction after Training Set",
         y="Number of Flights", x = "Time")+
    scale_x_date(date_labels="%b %Y", date_breaks  ="4 month") +  # line color
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
          panel.grid.minor = element_blank()) +
    scale_colour_manual(name="Methods", 
                        values=c("purple", "red", "green", "orange", "brown2", "blue"))

  if(save){
    ggsave(paste(path,"Predizioni Dopo Training Set-", title,".jpg", sep=""), width = widthGraphs)
  }
  
  ggplot() +
    geom_line(data=dfMeanMethod, aes(date, Y, color="Mean Method"), size = 0.5) +
    geom_line(data=dfsNaiveMethod, aes(date, Y,  color="Naive Method"), size = 0.5)+
    geom_line(data=dfnaiveMethod, aes(date, Y,  color="Drift Method"), size = 0.5)+
    geom_line(data=dfdriftMethod, aes(date, Y,  color="SeasonalMaive Method"), size = 0.5)+
    geom_line(data=dfArimaMod, aes(date, Y,  color="Arima Method"), size = 0.5)+
    geom_line(data=dfSer, aes(date, Y,  color="Test Set"), size = 0.5)+
    labs(title=title, 
         subtitle = "Predictions Comparison Test Set",
         y="Number of Flights", x = "Time")+
    scale_x_date(date_labels="%b %Y", date_breaks  ="4 month") +  # line color
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
          panel.grid.minor = element_blank())+
    scale_colour_manual(name="Methods",
                        values=c("purple", "red", "green", "orange", "brown2","blue"))
  
  if(save){
    ggsave(paste(path,"Confronto Predizioni su Test Set-", title,".jpg", sep=""), width = widthGraphs)
  }
  
  
  #plot predizioni serie completa
  meanMethod <- meanf(tserie,h=tWindow)
  naiveMethod <- rwf(tserie,h=tWindow)
  sNaiveMethod <- rwf(tserie,drift=TRUE,h=tWindow)
  driftMethod <- snaive(tserie,h=tWindow)
  arimaMod <- auto.arima(tserie, stepwise=FALSE, approximation=FALSE)
  arimaMod.Fr <- forecast(arimaMod, h=tWindow)
  
  dfMeanMethod <- data.frame(Y=as.matrix(meanMethod$mean), date=as.Date(as.yearmon(time(meanMethod$mean))))
  dfsNaiveMethod <- data.frame(Y=as.matrix(sNaiveMethod$mean), date=as.Date(as.yearmon(time(sNaiveMethod$mean))))
  dfnaiveMethod <- data.frame(Y=as.matrix(naiveMethod$mean), date=as.Date(as.yearmon(time(naiveMethod$mean))))
  dfdriftMethod <- data.frame(Y=as.matrix(driftMethod$mean), date=as.Date(as.yearmon(time(driftMethod$mean))))
  dfArimaMod <- data.frame(Y=as.matrix(arimaMod.Fr$mea), date=as.Date(as.yearmon(time(arimaMod.Fr$mea))))
  
  
  dfSerie <- data.frame(Y=as.matrix(tserie), date=as.Date(as.yearmon(time(tserie))))
  
  ggplot() +
    geom_line(data=dfMeanMethod, aes(date, Y, color="Mean Method"), size = 0.5) +
    geom_line(data=dfsNaiveMethod, aes(date, Y,  color="Naive Method"), size = 0.5)+
    geom_line(data=dfnaiveMethod, aes(date, Y,  color="Drift Method"), size = 0.5)+
    geom_line(data=dfdriftMethod, aes(date, Y,  color="SeasonalMaive Method"), size = 0.5)+
    geom_line(data=dfArimaMod, aes(date, Y,  color="Arima Method"), size = 0.5)+
    geom_line(data=dfSerie, aes(date, Y,  color="Test Set"), size = 0.5)+
    labs(title=title, 
         subtitle = "Predictions Comparison Time Series",
         y="Number of Flights", x = "Time")+
    scale_x_date(date_labels="%b %Y", date_breaks  ="4 month") +  # line color
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
          panel.grid.minor = element_blank())+
    geom_vline(xintercept = as.numeric(as.Date("2017-03-01")),color="red" ,size = 0.5,linetype=4)+
    scale_colour_manual(name="Methods",
                        values=c("purple", "red", "green", "orange", "brown2","blue"))
  
  if(save){
    ggsave(paste(path,"Confronto Predizioni su serie completa-", title,".jpg",sep=""), width = widthGraphs)
  }
  
}


plotArimaModel <- function(tserie, tWindow, title, path, save = TRUE){
  # Training del modello arima. Dopo il training viene effettuato il plot
  # delle previsioni di ogni modello a partire dalla fine del training set.
  #
  # Args:
  #   tserie: serie storica in formato ts
  #   tWindow: periodo delle previsioni (in mesi), di default e' 36 mesi
  #   tittle: titolo da visualizzare nel plot
  #   save: salva il grafico invece di mostrarlo
  
  trainData <- window(tserie, start=c(2003,4), end=c(2014,4))
  testData <- window(tserie, start=c(2014,5), end=c(2017,3))
  
  arimaMod <- auto.arima(trainData, stepwise=FALSE, approximation=FALSE)
  arimaMod.Fr <- forecast(arimaMod, h=tWindow)
  
  dfArimaMod <- data.frame(Y=as.matrix(arimaMod.Fr$mea), date=as.Date(as.yearmon(time(arimaMod.Fr$mea))))
  
  dfTrainData <- data.frame(Y=as.matrix(trainData), date=as.Date(as.yearmon(time(trainData))))
  dftestData <- data.frame(Y=as.matrix(testData), date=as.Date(as.yearmon(time(testData))))
  
  dfAllSeries <- data.frame(Y=as.matrix(tserie), date=as.Date(as.yearmon(time(tserie))))
  
  ggplot() +
    geom_line(data=dfArimaMod, aes(date, Y,  color="Arima Method"), size = 0.5)+
    geom_line(data=dfAllSeries, aes(date, Y,  color="Test Set"), size = 0.5)+
    labs(title=paste("Arima Predictions Comparison Training Set",title), 
         subtitle = arimaMod.Fr$method,
         y="Number of Flights", x = "Time")+
    scale_x_date(date_labels="%b %Y", date_breaks  ="4 month") +  # line color
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
          panel.grid.minor = element_blank())+
    scale_colour_manual(name="Methods",
                        values=c("red", "blue"))
  
  
  if(save){
    ggsave(paste(path, arimaMod.Fr$method , title,".jpg", sep=""), width = widthGraphs)
  }
  
 
  ggplot() +
    geom_line(data=dfArimaMod, aes(date, Y,  color="Arima Method"), size = 0.5)+
    geom_line(data=dftestData, aes(date, Y,  color="Test Set"), size = 0.5)+
    labs(title=paste("Arima Predictions Comparison Test Set",title), 
         subtitle = arimaMod.Fr$method,
         y="Number of Flights", x = "Time")+
    scale_x_date(date_labels="%b %Y", date_breaks  ="4 month") +  # line color
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
          panel.grid.minor = element_blank())+
    scale_colour_manual(name="Methods",
                        values=c("red", "blue"))
  
  if(save){
    ggsave(paste(path, "Confronto Arima su Test Set",title,".jpg", sep=""), width = widthGraphs)
  }
  
  #serie completa
  dfSerie <- data.frame(Y=as.matrix(tserie), date=as.Date(as.yearmon(time(tserie))))
  arimaMod <- auto.arima(tserie, stepwise=FALSE, approximation=FALSE)
  arimaMod.Fr <- forecast(arimaMod, h=tWindow)
  
  dfArimaMod <- data.frame(Y=as.matrix(arimaMod.Fr$mea), date=as.Date(as.yearmon(time(arimaMod.Fr$mea))))
  
  ggplot() +
    geom_line(data=dfArimaMod, aes(date, Y,  color="Arima Method"), size = 0.5)+
    geom_line(data=dfSerie, aes(date, Y,  color="Time Series"), size = 0.5)+
    labs(title=paste("Arima Predictions Comparison Time Series",title), 
         subtitle = arimaMod.Fr$method,
         y="Number of Flights", x = "Time")+
    scale_x_date(date_labels="%b %Y", date_breaks  ="4 month") +  # line color
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
          panel.grid.minor = element_blank())+
    geom_vline(xintercept = as.numeric(as.Date("2017-03-01")),color="red" ,size = 0.5,linetype=4)+
    scale_colour_manual(name="Methods",
                        values=c("red", "blue"))
  
  if(save){
    ggsave(paste(path, "Confronto Arima su Serie Completa",title,".jpg", sep=""),width = widthGraphs)
  }
  
}



evaluateBesModel <- function(tserie, tWindow, title,path, save = TRUE){
  # Calcola il modello migliore in base alle misure di accuracy dei modelli.
  # Dopo il calcolo viene visualizzata la tabella con tutte le misure di errore
  # 
  #
  # Args:
  #   tserie: serie storica in formato ts
  #   tWindow: periodo delle previsioni (in mesi), di default e' 36 mesi
  #   title: titolo da visulizzare nella tabella delle misure di errore
  #   save: salva il grafico invece di mostrarlo
  #
  # Returns:
  #   Il modello migliore in base alle metriche "ME", "RMSE", "MAE", "MPE", "MAPE"
 
  sr <- window(tserie, start=c(2003,4), end=c(2014,4))
  ser <- window(tserie, start=c(2014,5), end=c(2017,3))
  
  meanMethod <- meanf(sr,h=tWindow)
  naiveMethod <- rwf(sr,h=tWindow)
  sNaiveMethod <- rwf(sr,drift=TRUE,h=tWindow)
  driftMethod <- snaive(sr,h=tWindow)
  arimaMod <- auto.arima(sr, stepwise=FALSE, approximation=FALSE)
  arimaMod.Fr <- forecast(arimaMod, h=tWindow)
  
  a1 = accuracy(meanMethod, ser)
  a2 = accuracy(naiveMethod, ser)
  a3 = accuracy(sNaiveMethod, ser)
  a4 = accuracy(driftMethod, ser)
  a5 = accuracy(arimaMod.Fr, ser)
  
  keeps <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  a1 <- a1[2,keeps, drop = FALSE]
  a2 <- a2[2,keeps, drop = FALSE]
  a3 <- a3[2,keeps, drop = FALSE]
  a4 <- a4[2,keeps, drop = FALSE]
  a5 <- a5[2,keeps, drop = FALSE]
  
  temp.table <- as.data.frame(cbind(a1[1, ], a2[1, ], a3[1, ], a4[1, ], a5[1, ]))
  
  
  colnames(temp.table) <-
    c("Average Method",
      "Naive Method",
      "SNaive Method",
      "Drift method",
      "Arima")
  
  is.num <- sapply(temp.table, is.numeric)
  temp.table[is.num] <- lapply(temp.table[is.num], round, 10)
  
  t1 <- tableGrob(temp.table)
  titleTable <- textGrob(paste("Errori Test Set-", title), gp=gpar(fontsize=25))
  
  padding <- unit(5,"mm")
  
  table <- gtable_add_rows(
    t1, 
    heights = grobHeight(titleTable) + padding,
    pos = 0)
  table <- gtable_add_grob(
    table, 
    titleTable, 
    1, 1, 1, ncol(table))
  
  
  if(save){
    ggsave(file=paste(path,"Errori Test Set-", title,".jpg", sep=""), grid.draw(table),width = widthGraphs)
  }else{
    grid.newpage()
    grid.draw(table)
  }
  
  
  dfEvalColumn <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  dfEvalRow <- c(names(temp.table)[which.min(apply(temp.table[1,],MARGIN=2,min))],
                 names(temp.table)[which.min(apply(temp.table[2,],MARGIN=2,min))],
                 names(temp.table)[which.min(apply(temp.table[3,],MARGIN=2,min))],
                 names(temp.table)[which.min(apply(temp.table[4,],MARGIN=2,min))],
                 names(temp.table)[which.min(apply(temp.table[5,],MARGIN=2,min))])
  dfEval <- data.frame(dfEvalColumn, dfEvalRow)
  
  tableEval <-  table(dfEval$dfEvalRow)
  
  return (names(tableEval)[which.max(tableEval)])
  
}

plotBestModel<- function(model, sr, tWindow, title,path, save = TRUE){
  if(model == 'Arima'){
    arimaMod <- auto.arima(sr, stepwise=FALSE, approximation=FALSE)
    arimaMod.Fr <- forecast(arimaMod, h=tWindow)
<<<<<<< HEAD
    if(save){
      jpeg(paste(path,"Best Model con Arima - ", title,".jpg", sep=""), width = 800)
    }
    plot(arimaMod.Fr, main=paste('Arima', title), ylab="Number of flights", xlab= 'Time')
    if(save){
      dev.off()
=======
    autoplot(arimaMod.Fr, main=paste('Arima', title), ylab="Number of flights", xlab= 'Time')
    if(save){
      ggsave(file=paste(path,"Best Model con Arima - ", title,".jpg", sep=""))
>>>>>>> devLorenzo
    }
  }
  if(model == 'Drift method'){
    driftMethod <- snaive(sr,h=tWindow)
    driftMethod.Fr <- forecast(driftMethod, h=tWindow)
<<<<<<< HEAD
    if(save){
      jpeg(paste(path,"Best Model con Drift method - ", title,".jpg", sep=""), width = 800)
    }
    plot(driftMethod.Fr, main=paste('Drift method', title), ylab="Number of flights", xlab= 'Time')
    if(save){
      dev.off()
=======
    autoplot(driftMethod.Fr, main=paste('Drift method', title), ylab="Number of flights", xlab= 'Time')
    if(save){
      ggsave(file=paste(path,"Best Model con Drift method - ", title,".jpg", sep=""))
>>>>>>> devLorenzo
    }
  }
  if(model == 'SNaive Method'){
    sNaiveMethod <- rwf(sr,drift=TRUE,h=tWindow)
    sNaiveMethod.Fr <- forecast(sNaiveMethod, h=tWindow)
<<<<<<< HEAD
    if(save){
      jpeg(paste(path,"Best Model con SNaive Method - ", title,".jpg", sep=""), width = 800)
    }
    plot(sNaiveMethod.Fr, main=paste('SNaive Method', title), ylab="Number of flights", xlab= 'Time')
    if(save){
      dev.off()
=======
    autoplot(sNaiveMethod.Fr, main=paste('SNaive Method', title), ylab="Number of flights", xlab= 'Time')
    if(save){
      ggsave(file=paste(path,"Best Model con SNaive Method - ", title,".jpg", sep=""))
>>>>>>> devLorenzo
    }
  }
  if(model == 'Naive Method'){
    naiveMethod <- rwf(sr,h=tWindow)
    naiveMethod.Fr <- forecast(naiveMethod, h=tWindow)
<<<<<<< HEAD
    if(save){
      jpeg(paste(path,"Best Model con Naive Method - ", title,".jpg", sep=""), width = 800)
    }
    plot(naiveMethod.Fr, main=paste('Naive Method', title), ylab="Number of flights", xlab= 'Time')
    if(save){
      dev.off()
      }
=======
    autoplot(naiveMethod.Fr, main=paste('Naive Method', title), ylab="Number of flights", xlab= 'Time')
    if(save){
      ggsave(file=paste(path,"Best Model con Naive Method - ", title,".jpg", sep=""))
    }
>>>>>>> devLorenzo
  }
  if(model == 'Average Method'){
    meanMethod <- meanf(sr,h=tWindow)
    meanMethod.Fr <- forecast(meanMethod, h=tWindow)
<<<<<<< HEAD
    if(save){
      jpeg(paste(path,"Best Model con Average Method - ", title,".jpg", sep=""), width = 800)
    }
    plot(meanMethod.Fr, main=paste('Average Method', title), ylab="Number of flights", xlab= 'Time')
    if(save){
      dev.off()
=======
    autoplot(meanMethod.Fr, main=paste('Average Method', title), ylab="Number of flights", xlab= 'Time')
    if(save){
      ggsave(file=paste(path,"Best Model con Average Method - ", title,".jpg", sep=""))
>>>>>>> devLorenzo
    }
  }
}
