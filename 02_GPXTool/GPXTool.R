install.packages("dplyr");install.packages("xlsx");install.packages("rgdal")
install.packages("exifr");install.packages("devtools");install.packages("stringr")
install.packages("rJava");install.packages("plotKML");install.packages("checkmate")
install.packages("spatial")
library(exifr)
library (dplyr)
library(devtools)
library(stringr)
library(xlsx)
library(rJava)
library (rgdal)
library(plotKML)
library(spatial)
library(sf)
library(raster)
library(sp)
library(purrr)
library(rgeos)
library(lubridate)

# Select the gpx path (use readclipboard() to get the right path in windows).
# Use the gpx.folder as a working directory (just temporary)
g.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\gpx\\20130925_26"
# tables output folder
t.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\tablas"
# geo folder to make joins
geo <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\geo\\PPEE"
# shp output folder
shp.out <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\shp\\output"

epsg1 <- CRS("+init=epsg:4324")
epsg2 <- CRS("+init=epsg:25829")
epsg2

# list files with .gpx extension
files <- list.files(g.folder, pattern="*.gpx", full.names=T)

#leer archivos
gpx1 <- readGPX(files[1],metadata=F,bounds=F, waypoints=T,tracks = T, routes= F)
gpx2 <- readGPX(files[2],metadata=F,bounds=F, waypoints=T,tracks = T, routes= F)

#GENERACION DE TABLAS WP INDICENCIAS, AVES Y COLISIONES
wp1 <- data.frame(gpx1['waypoints'])
wp2 <- data.frame(gpx2['waypoints'])

#seleccionar incidencias
wp1i <- filter(wp1,waypoints.name=='I')
#seleccionar avifauna. metodo sencillo seleccionando filas con cod==5
wp1a <- wp1[nchar(wp1$waypoints.name)==5,]
#seleccionar colisiones
wp1c <- filter(wp1,waypoints.name=='C')
#any other code
wp1o <- filter(wp1, waypoints.name!='I' & waypoints.name !='C' & nchar(wp1$waypoints.name)!=5)

#exportar tablas a csv
file_name_wp1i <- paste(t.folder,"\\","incidencias_vinculada.csv",sep="")
file_name_wp1a <- paste(t.folder,"\\","avifauna_vinculada.csv",sep="")
file_name_wp1c <- paste(t.folder,"\\","colisiones_vinculada.csv",sep="")
file_name_wp1o <- paste(t.folder,"\\","otros_vinculada.csv",sep="")

write.csv2(wp1i, file = file_name_wp1i,row.names=FALSE, na="")
write.csv2(wp1a, file = file_name_wp1a,row.names=FALSE, na="")
write.csv2(wp1c, file = file_name_wp1c,row.names=FALSE, na="")
write.csv2(wp1o, file = file_name_wp1o,row.names=FALSE, na="")
#_________________________________________________________________________
#por hacer:
#asignacion de especies por código
#reproyeccion de coordenadas
#asignacion automatica de fotos en caso de incidencia
#_________________________________________________________________________

#TRACK POINTS ANALYSIS
#read trackpoints and create unique data.frame
tp1 <- gpx1['tracks']
#create empty df
t <- data.frame(lon=double(),
                lat=double(),
                ele=double(),
                time=character(),
                track=integer())
#create unique data.frame with track number
for (i in 1:length(combine(tp1))){
    df <- data.frame(combine(tp1)[i])
    colnames(df) <- c("lon","lat","ele", "time")
    track <- mutate(df,track=i)
    t <- rbind(t,track)
}
#add ID
t$ID <- seq.int(nrow(t))
t

#function to add coords conversion to df
UTM29 = function(data,
                    src.proj = epsg.gpx,
                    dst.proj = epsg.new) {
    require(sp)
    as.data.frame(
        spTransform(
            SpatialPointsDataFrame(
                coords = data.frame(x = t$lon,
                                    y = t$lat),
                data = data.frame(ID =  t$ID,
                                  track = t$track,
                                  lon = t$lon,
                                  lat = t$lat,
                                  ele = t$ele,
                                  time = t$time),
                proj4string = src.proj), dst.proj))

}
t2 <- UTM29(data=t)
#transform time
#replace T and z by blank in gps time
t2$time <- gsub("T", ' ',t2$time)
t2$time <- gsub("Z", '',t2$time)
t2

#make spatial data frame
coords <- data.frame(x=t2$x, y = t2$y)
t2.sp <- SpatialPointsDataFrame(coords=coords, data=t2,proj4string = epsg2 )

## read shp of ppee
PPEE <- readOGR(geo, 'PPEE')
# create buffer 25m by id
PPEE25m <- gBuffer(PPEE, byid= T, width = 25, quadsegs=10)

#INTERSECT points with buffer 25m
i <- intersect(t2.sp,PPEE25m)

#merge all
r <- merge(t2.sp, i, by="ID", all.x=TRUE)

#transform time
#replace T and z by blank in gps time
r$time <- gsub("T", ' ',r$time)
r$time <- gsub("Z", '',r$time)
names(r)

#identify min and max hour within each 25m buffer
minT <- data.frame(r) %>%
    group_by(Aero) %>%
    summarise(minTime=min(time))
data.frame(minT)
maxT <- data.frame(r) %>%
    group_by(Aero) %>%
    summarise(maxTime=max(time))
data.frame(maxT)

#merge dataframes of min and max within 25m buffer
minmax <- merge(minT, maxT, by="Aero", all.x=T)
#calculate time betweeen min and max in seconds
minmax <- mutate(minmax,
                 tiempo_s = as.numeric(difftime(as_datetime(minmax$maxTime),
                                 as_datetime(minmax$minTime),
                                 units="secs")))
dim(minmax)[1]
length(minmax)

#extract values in range minTime and maxTime and join all in SpatialLines
Llist <- list()
for (i in 1:dim(minmax)[1]){
    tp <- subset(t2, time>=minmax$minTime[i] & time<=minmax$maxTime[i])
    l <- cbind(tp$x, tp$y)
    Sl <- Line(l)
    S <- Lines(list(Sl1), ID=minmax$Aero[i])
    Llist[[i]] <- S
}

#create shp with proyeccion
Sl <- SpatialLines(list(S), proj4string = epsg2)
dim(Sl)

#pasar de spatiallines a spatiallinesdataframe que contenga los datos y crs
Slx <- SpatialLinesDataFrame(sl=Sl, data=minmax, match.ID = F)
plot(Slx)

#save shp
writeOGR(obj=Slx, dsn=shp.out, layer="Prospeccion", driver="ESRI Shapefile", overwrite_layer = T)
