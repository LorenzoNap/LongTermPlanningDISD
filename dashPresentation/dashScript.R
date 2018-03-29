library(RODBC)
dbhandle <-odbcDriverConnect(connection=param.connessione.db)

# Scarica le serie storiche a partire dalla stessa data di cui alle serie precedenti.
reg.arr.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
                           FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.ARR_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON ARR_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=1
                           --and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")
reg.dep.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
                           FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.DEP_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON DEP_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=1
                           --and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")
int.arr.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
                           FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.ARR_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON ARR_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=2
                           --and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")
int.dep.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
                           FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.DEP_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON DEP_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=2
                           --and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")
dom.arr.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
                           FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.ARR_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON ARR_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=3
                           --and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")
dom.dep.aflight<- sqlQuery(dbhandle, "SELECT count(*) as CONTO, DATEPART(YEAR, STO) as ANNO, DATEPART(MONTH, STO) as MESE
                           FROM [AMS516].[dbo].[A_FLIGHT] inner join S_ROUTE on A_FLIGHT.ROUTE_ID=S_ROUTE.ID
                           WHERE A_FLIGHT.ID in
                           (SELECT A_MOVEMENT.DEP_FLIGHT_ID
                           FROM [AMS516].[dbo].[A_MOVEMENT] 
                           INNER JOIN A_FLIGHT ON DEP_FLIGHT_ID=A_FLIGHT.ID)
                           and S_ROUTE.FLIGHT_TYPE_ID=3
                           --and A_FLIGHT.STO between '20120401' and '20171231'
                           and A_FLIGHT.QUALIFIER_ID=1
                           and A_FLIGHT.EXCEPTION_ID is null
                           GROUP BY DATEPART(YEAR, STO), DATEPART(MONTH, STO)
                           order by ANNO, MESE;")

# trasforma i dati in time series
startSeriesDB <- c(2003,4)
reg.arr.db <- ts(reg.arr.aflight$CONTO, frequency=12, start=startSeriesDB)
reg.dep.db <- ts(reg.dep.aflight$CONTO, frequency=12, start=startSeriesDB)
int.arr.db <- ts(int.arr.aflight$CONTO, frequency=12, start=startSeriesDB)
int.dep.db <- ts(int.dep.aflight$CONTO, frequency=12, start=startSeriesDB)
dom.arr.db <- ts(dom.arr.aflight$CONTO, frequency=12, start=startSeriesDB)
dom.dep.db <- ts(dom.dep.aflight$CONTO, frequency=12, start=startSeriesDB)


createMap <- function(){
  rotte <- sqlQuery(dbhandle, 
                    "SELECT S_ROUTE.ID
                    ,S_ROUTE.HOME_AIRPORT_ID
                    ,PAR.LATTITUDE as LAT1
                    ,PAR.LONGITUDE as LON1
                    ,S_ROUTE.CODE
                    ,S_ROUTE.FLIGHT_TYPE_ID
                    ,S_ROUTE.FLIGHT_NATURE_ID
                    ,S_ROUTE.PORT_AIRPORT_ID
                    ,ARR.LATTITUDE as LAT2
                    ,ARR.LONGITUDE as LON2
                    ,S_ROUTE.DESCRIPTION1 
                    ,S_ROUTE.FLYING_TIME
                    FROM [AMS516].[dbo].[S_ROUTE] 
                    join S_AIRPORT as ARR  on ARR.ID=S_ROUTE.PORT_AIRPORT_ID
                    join S_AIRPORT as PAR on PAR.ID=S_ROUTE.HOME_AIRPORT_ID")
  
  library(tidyr)
  library(dplyr) # per convertire i gradi gps a decimali
  rotte <- mutate(rotte, 
                  LAT1c=LAT1 %>%
                    sub(':', 'd', .) %>%
                    sub(':', '\'', .) %>%
                    sub(':', '" ', .) %>%
                    char2dms %>%
                    as.numeric,
                  LON1c=LON1 %>%
                    sub(':', 'd', .) %>%
                    sub(':', '\'', .) %>%
                    sub(':', '" ', .) %>%
                    char2dms %>%
                    as.numeric)
  
  rotte.null      <- rotte[grep("0:0:0", rotte$LAT2),]
  rotte.na        <- rotte[which(is.na(rotte$LAT2)),]
  rotte.not.null  <- rotte[-grep("0:0:0", rotte$LAT2),]
  rotte.not.null  <- setdiff(rotte.not.null,rotte.na) # fa la differenza dei due dataset in argomento
  rotte.not.null <- mutate(rotte.not.null, 
                           LAT2c=LAT2 %>%
                             sub(':', 'd', .) %>%
                             sub(':', '\'', .) %>%
                             sub(':', '" ', .) %>%
                             char2dms %>%
                             as.numeric,
                           LON2c=LON2 %>%
                             sub(':', 'd', .) %>%
                             sub(':', '\'', .) %>%
                             sub(':', '" ', .) %>%
                             char2dms %>%
                             as.numeric)       
  
  library(ggmap) # carico ggmap per chiedere a Google le coordinate mancanti dei luoghi (http://stat405.had.co.nz/ggmap.pdf)
  # temp1          <- geocode(paste(rotte.null$DESCRIPTION1, "airport", sep = " "))#occhio che lat e lon sono invertiti
  # temp2          <- geocode(paste(rotte.null$CODE, "airport", sep = " ")) # faccio la stessa cosa cercando i CODE
  # temp <- temp1
  # temp[which(is.na(temp1$lon)),]<-temp2[which(is.na(temp1$lon)),] # sostituisci agli na di temp1 i valori di temp2
  
  #temp <- read.csv("filename.csv")
  
  #colnames(temp) <- c("LON2c", "LAT2c")
  #write.csv(temp, file = "downloaded.csv")
  library(readr)
  downloaded <- read_csv("downloaded.csv", 
                         col_types = cols(X1 = col_skip()))
  
  pippo <- cbind(rotte.null, downloaded[,c(2,1)])        # aggiunto al db iniziale le due colonne con i nuovi geo points
  rotte.null2 <- pippo[which(is.na(pippo$LAT2c)),] # valori ancora non trovati che rimandiamo al passo successivo!
  rotte.null <- setdiff(pippo, rotte.null2)        # db dei valori trovati
  
  rotte.na <- rbind(rotte.na, subset(rotte.null2, select=-c(LAT2c,LON2c)))
  rm(pippo, rotte.null2) #Eliminiamo quello che non serve più...
  # Da ultimo vediamo se riusciamo a recuperare le coordinate per il gruppo delle NA e dei 50 che non sono stati reperiti al punto precedente.
  
  aeroporti <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat", header = TRUE, stringsAsFactors=FALSE)
  
  temp0 <- as.data.frame(cbind(aeroporti[,5], as.numeric(aeroporti[,7]), as.numeric(aeroporti[,8])))
  colnames(temp0) <-  c("CODE", "LAT2c", "LON2c")
  
  rotte.na <- merge(x = rotte.na, y = temp0, by = "CODE", all.x = TRUE)
  
  rotte.agg <- rbind(rotte.not.null, rotte.null, rotte.na) # , rotte.null.trovate
  
  rotte.agg$LAT1c <- as.numeric(rotte.agg$LAT1c)
  rotte.agg$LON1c <- as.numeric(rotte.agg$LON1c)
  rotte.agg$LAT2c <- as.numeric(rotte.agg$LAT2c)
  rotte.agg$LON2c <- as.numeric(rotte.agg$LON2c) # DB SISTEMATO!
  
  rm(aeroporti, rotte.na, rotte.not.null, rotte.null, temp0)
  
  
  rotte.agg2 <- rotte.agg[-which(is.na(rotte.agg$LAT2c)),] #45 ################
  
  deg2rad <- function(deg) {(deg * pi) / (180)} # convertire i gradi per funzioni trigonometriche usate sotto
  
  
  distanza <- numeric(nrow(rotte.agg2))
  
  for(i in 1:nrow(rotte.agg2) ){
    deltaL=deg2rad(rotte.agg2$LON2c[i]-rotte.agg2$LON1c[i])
    fi1=deg2rad(rotte.agg2$LAT1c[i])
    fi2=deg2rad(rotte.agg2$LAT2c[i])
    cos(deltaL)
    A=(cos(fi2)*sin(deltaL))**2
    B=(cos(fi1)*sin(fi2)-sin(fi1)*cos(fi2)*cos(deltaL))**2
    C=sin(fi1)*sin(fi2)+cos(fi1)*cos(fi2)*cos(deltaL)
    DeltaSigma=atan2(sqrt(A+B),C)
    Raggio=6371
    lunghezza=Raggio*DeltaSigma
    print(lunghezza)
    distanza[i]<-lunghezza
  } # restituisce distanza in km ... formula da https://en.wikipedia.org/wiki/Great-circle_distance#Computational_formulas
  
  rotte.agg2 <- cbind(rotte.agg2,distanza)
  
  rm(lunghezza, distanza)
  
  
  prova <- rotte.agg2
  prova[which(rotte.agg2$LAT2c>0 & rotte.agg2$FLIGHT_TYPE_ID==3), c("LAT2c")] <- c(-27.86083)  
  prova[which(rotte.agg2$LAT2c>0 & rotte.agg2$FLIGHT_TYPE_ID==3), c("LON2c")] <- c(28.24611) 
  # Per i valori dei domestici troppo a est:
  prova[which(rotte.agg2$LON2c>33 & rotte.agg2$FLIGHT_TYPE_ID==3), c("LAT2c")] <- c(-27.86083) 
  prova[which(rotte.agg2$LON2c>33 & rotte.agg2$FLIGHT_TYPE_ID==3), c("LON2c")] <- c(28.24611) 
  # Per i valori dei domestici troppo a ovest:
  prova[which(rotte.agg2$LON2c<16 & rotte.agg2$FLIGHT_TYPE_ID==3), c("LAT2c")] <- c(-27.86083)
  prova[which(rotte.agg2$LON2c<16 & rotte.agg2$FLIGHT_TYPE_ID==3), c("LON2c")] <- c(28.24611)
  #
  prova[which(rotte.agg2$LAT2c>0 & rotte.agg2$FLIGHT_TYPE_ID==3), c("distanza")] <- c(NA)# Abbiamo assegnato 
  prova[which(rotte.agg2$LON2c>33 & rotte.agg2$FLIGHT_TYPE_ID==3), c("distanza")] <- c(NA)
  prova[which(rotte.agg2$LON2c<16 & rotte.agg2$FLIGHT_TYPE_ID==3), c("distanza")] <- c(NA)
  rotte.agg2 <- prova 
  rm(prova)
  # Voli regionali: elimino quei voli il cui recupero di coordinate non è andato a buon fine.
  prova <- rotte.agg2
  # Per i valori dei regionali troppo a ovest:
  prova[which(rotte.agg2$LON2c<16 & rotte.agg2$FLIGHT_TYPE_ID==1), c("LAT2c")] <- c(-27.86083)
  prova[which(rotte.agg2$LON2c<16 & rotte.agg2$FLIGHT_TYPE_ID==1), c("LON2c")] <- c(28.24611)
  prova[which(rotte.agg2$LON2c<16 & rotte.agg2$FLIGHT_TYPE_ID==1), c("distanza")] <- c(NA)
  # Per i valori dei regionali troppo a est:
  prova[which(rotte.agg2$LON2c>33 & rotte.agg2$FLIGHT_TYPE_ID==1), c("LAT2c")] <- c(-27.86083)
  prova[which(rotte.agg2$LON2c>33 & rotte.agg2$FLIGHT_TYPE_ID==1), c("LON2c")] <- c(28.24611)
  prova[which(rotte.agg2$LON2c>33 & rotte.agg2$FLIGHT_TYPE_ID==1), c("distanza")] <- c(NA)
  rotte.agg2 <- prova 
  rm(prova)
  
  rotte.agg2[grep(4, rotte.agg2$FLIGHT_TYPE_ID), ]$distanza <- c(NA)
  # 
  rotte.agg2[grep(7, rotte.agg2$FLIGHT_TYPE_ID), ]$distanza <- c(NA)
  
  
  rotte.agg3 <- rotte.agg2[!(rotte.agg2$DESCRIPTION1=="UNKNOWN INTERNATIONAL" 
                             || rotte.agg2$DESCRIPTION1=="UNKNOWN REGIONAL" 
                             || rotte.agg2$DESCRIPTION1=="UNKNOWN DOMESTIC"),]
  
  # map_world <- map_data("world")
  # g <- ggplot() +
  #   geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
  #   geom_point(data = rotte.agg3, aes(x = LON2c, y = LAT2c), color = 'red')
  
  
  
  return (rotte.agg3);
}


gen_array <- function(forecast_obj){
  
  actuals <- forecast_obj$x
  lower <- forecast_obj$lower[,2]
  upper <- forecast_obj$upper[,2]
  point_forecast <- forecast_obj$mean
  
  cbind(actuals, lower, upper, point_forecast)
}



chooseTimeSeries <- function(tipoVolo, tipologia){
  if(tipoVolo == 'Domestici'){
    # prendi la tipologia
    if(tipologia == 'arr'){
      return (dom.arr.db)
    }
    else if(tipologia == 'partenze'){
      return (dom.dep.db)
    }
    else { #tutti
      return (ts( dom.arr.aflight$CONTO + dom.dep.aflight$CONTO, frequency=12, start=startSeriesDB))
    }
    
  } else if (tipoVolo == 'Regionali'){

    if(tipologia == 'arr'){
      return (reg.arr.db)
    }
    else if(tipologia == 'partenze'){
      return (reg.dep.db)
    }
    else { #tutti
      return (ts( reg.arr.aflight$CONTO + reg.dep.aflight$CONTO, frequency=12, start=startSeriesDB))
    }
    
  } else if (tipoVolo == 'Internazionali'){
    if(tipologia == 'arr'){
      return (int.arr.db)
    }
    else if(tipologia == 'partenze'){
      return (int.dep.db)
    }
    else { #tutti
      return (ts(int.arr.aflight$CONTO + int.dep.aflight$CONTO, frequency=12, start=startSeriesDB))
    }
  }
}


createDataSetClustering <- function(){
  
  voli_rotte2 <- read_csv("./dashPresentation/voli_rotte2.csv.csv", 
                          col_types = cols(X1 = col_skip()))
  
  average <- data.frame(cbind(
    c(mean(voli_rotte2[grep("1", voli_rotte2$FLIGHT_TYPE_ID.x),]$MTOW), 
      mean(voli_rotte2[grep("2", voli_rotte2$FLIGHT_TYPE_ID.x),]$MTOW),
      mean(voli_rotte2[grep("3", voli_rotte2$FLIGHT_TYPE_ID.x),]$MTOW)),
    c(mean(na.omit((voli_rotte2[grep("1", voli_rotte2$FLIGHT_TYPE_ID.x),]$distanza))),
      mean(na.omit((voli_rotte2[grep("2", voli_rotte2$FLIGHT_TYPE_ID.x),]$distanza))),
      mean(na.omit((voli_rotte2[grep("3", voli_rotte2$FLIGHT_TYPE_ID.x),]$distanza))))),
    row.names = c("Regional","International","Domestic"))
  colnames(average) <- c("MTOW","Distance")
  
  voli_rotte_clean <- na.omit(voli_rotte2[ , c("MTOW","distanza")])
  voli_rotte_clean_sd <- voli_rotte_clean[!duplicated(voli_rotte_clean), ]
  voli_rotte_clean_sd$MTOW <- (voli_rotte_clean_sd$MTOW - mean(voli_rotte_clean_sd$MTOW))/(2*sd(voli_rotte_clean_sd$MTOW))
  voli_rotte_clean_sd$distanza <- (voli_rotte_clean_sd$distanza - mean(voli_rotte_clean_sd$distanza))/(2*sd(voli_rotte_clean_sd$distanza))
  
}

doClustering <- function(nClust, dataSet) {
 
  
  
  km <- kmeans(dataSet, centers = nClust, nstart = 100)
  
  return (km)
  
}



