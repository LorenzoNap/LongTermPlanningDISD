library(leaflet)
library(geosphere)
library(sp)

lat_ny <- 40.73
lng_ny <- -73.9
lat_del <- 40.73
lng_del <- 77.21
lng_ca <- 40.73
lat_ca <- 39.16414

inter1 <- gcIntermediate(c(lng_ny, lat_ny), c(lng_del, lat_del), n=200, addStartEnd=TRUE, sp = TRUE, breakAtDateLine = TRUE)
lines(inter1)

inter2 <- gcIntermediate(c(lng_ca, lat_ca), c(lng_del, lat_del), n=10, addStartEnd=TRUE, sp = TRUE, breakAtDateLine = TRUE)
lines(inter2)

inters <- c(inter1,inter2)

ll0 <- lapply( inters , function(x) `@`(x , "lines") )
ll1 <- lapply( unlist( ll0 ) , function(y) `@`(y,"Lines") )
Sl <- SpatialLines( list( Lines( unlist( ll1 ) , ID = 1 ) ) )

leaflet(Sl)  %>% addTiles() %>% addPolylines(color="red", weight=2, dashArray = "8,6")
