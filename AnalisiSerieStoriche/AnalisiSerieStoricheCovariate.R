source("./parametriConnessioniFile.R")


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

path <- "./AnalisiSerieStoriche//resultAnalysisCov//"

#Serie storiche coviariate
library(readxl)
library(fpp)
library(corrplot)
library(grid)
library(gridExtra)
library(gtable)
library(forecast)
library(ggplot2)
library(scales)

theme_set(theme_bw())

#leggo file excel delle serie
Statistiche2003 <- read_excel(param.file.seriePetrolioEGdp.csv, 
                              col_types = c("date", "numeric", "numeric"))

Statistiche2003$Date <- as.Date(Statistiche2003$Date)


ggplot(Statistiche2003, aes(x=Date)) + 
  geom_line(aes(y=Oil)) + 
  labs(title="Prezzo Petrolio", 
       y="Oil $") +  # title and caption
  scale_x_date(date_labels="%b %Y", date_breaks  ="4 month")+  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())

if(saveFile){
  ggsave(paste(path,"PrezzoPetrolioChart",".jpg", sep=""))
}

ggplot(Statistiche2003, aes(x=Date)) + 
  geom_line(aes(y=Oil)) + 
  labs(title="Crescita GDP", 
       y="GDP") +  # title and caption
  scale_x_date(date_labels="%b %Y", date_breaks  ="4 month")+  # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())

if(saveFile){
  ggsave(paste(path,"CresciatGDPChart",".jpg", sep=""))
}



#Merge voli inter arr e partenze
int.dep.aflight$tot <-int.arr.aflight$CONTO + int.dep.aflight$CONTO
Statistiche2003$voliTOT <- int.dep.aflight$tot

Statistiche2003$rescaleGDP <- rescale(Statistiche2003$GDP)
Statistiche2003$rescaleOil <- rescale(Statistiche2003$Oil)
Statistiche2003$rescaleVoli <- rescale(Statistiche2003$voliTOT)

ggplot(Statistiche2003, aes(x=Date)) + 
  geom_line(aes(y=rescaleVoli, color="Voli")) + 
  geom_line(aes(y=rescaleOil, color="Oil price")) + 
  labs(title="Voli vs Oil Price", 
       subtitle="Comparazione Prezzo petrolio e voli totali",
       color=NULL) +  # title and caption
  scale_x_date(date_labels="%b %Y", date_breaks  ="4 month") +  # change to monthly ticks and labels
  scale_colour_manual(name="Serie",
                      values=c("#00ba38", "#f8766d")) +  # line color
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank())

if(saveFile){
  ggsave(paste(path,"VoliVSOilPrice",".jpg", sep=""))
}


ggplot(Statistiche2003, aes(x=Date)) + 
  geom_line(aes(y=rescaleVoli, color="Voli")) + 
  geom_line(aes(y=rescaleGDP, color="GPD")) + 
  labs(title="Voli vs GDP", 
       subtitle="Comparazione GDP e voli totali",
       color=NULL) +  # title and caption
  scale_x_date(date_labels="%b %Y", date_breaks  ="4 month") +  # change to monthly ticks and labels
  scale_colour_manual(name="Serie",
                      values=c("#00ba38", "#f8766d")) +  # line color
  theme(axis.text.x = element_text(angle = 90, vjust=0.5, size = 8),  # rotate x axis text
        panel.grid.minor = element_blank())

if(saveFile){
  ggsave(paste(path,"VoliVSGDP",".jpg", sep=""))
}



#Correlazione
my_data <- Statistiche2003[, c(2,3,4)]
res <- cor(my_data)
res <- round(res, 2)
res1 <- cor.mtest(my_data, conf.level = .95)

if(saveFile){
  jpeg(paste(path,"PlotCorrelazione",".jpg", sep=""))
}

corrplot(res, order = "hclust", type = "upper",
         tl.col = "black", tl.srt = 45)

if(saveFile){
  dev.off()
}

if(saveFile){
  jpeg(paste(path,"PlotCorrelazioneNumber",".jpg", sep=""))
}

corrplot(res, method="number")

if(saveFile){
  dev.off()
}

#creazione serie storiche
voliTS <- ts(Statistiche2003$voliTOT, frequency = 12, start=c(2003,4))
gdpTS <- ts(Statistiche2003$GDP, frequency=12, start=c(2003,4))
popTS <- ts(Statistiche2003$Oil, frequency=12, start=c(2003,4))

#Costruzione modello arima senza regressori
tWindow <- 120
arimaMod <- auto.arima(voliTS, stepwise=FALSE, approximation=FALSE)
arimaMod.Fr <- forecast(arimaMod, h = tWindow)
autoplot(arimaMod.Fr, main  = "Predizioni Arima")

if(saveFile){
  ggsave(paste(path,"ArimaSenzaRegressori",".jpg", sep=""))
}

dfArimaMod <- data.frame(Y=as.matrix(arimaMod.Fr$mea), date=as.Date(as.yearmon(time(arimaMod.Fr$mea))))

#arima con regressori
xreg <- cbind(gdpTS, popTS )
fit <- auto.arima(voliTS, xreg=xreg, stepwise=FALSE, approximation=FALSE)
arimaMod.Fr <- forecast(fit, xreg=xreg, h= tWindow)
autoplot(arimaMod.Fr, main = "Predizioni Arima Regressori")

if(saveFile){
  ggsave(paste(path,"ArimaConRegressori",".jpg", sep=""))
}

dfArimaCov <- data.frame(Y=as.matrix(arimaMod.Fr$mea), date=as.Date(as.yearmon(time(arimaMod.Fr$mea))))



#arima normale
dfSerie <- data.frame(Y=as.matrix(voliTS), date=as.Date(as.yearmon(time(voliTS))))


ggplot() +
  geom_line(data=dfArimaMod, aes(date, Y,  color="Arima Method"), size = 0.5)+
  geom_line(data=dfArimaCov, aes(date, Y,  color="Arima Cov"), size = 0.5)+
  geom_line(data=dfSerie, aes(date, Y,  color="Serie Completa"), size = 1.0)+
  labs(title=paste("Confronto Arima,Arima reg su Serie Completa"),
       y="Numero voli", x = "Data")+
  geom_vline(xintercept = as.numeric(as.Date("2017-03-01")),color="black" ,size = 0.5,linetype=4)+
  scale_colour_manual(name="Serie",
                      values=c("red", "green", "blue"))

if(saveFile){
  ggsave(paste(path,"ConfrontoArima",".jpg", sep=""))
}

