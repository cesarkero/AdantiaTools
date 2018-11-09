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

# Select the gpx path (use readclipboard() to get the right path in windows). 
# Use the gpx.folder as a working directory (just temporary)
g.folder <- "Z:\\De sastre\\CAC\\github\\R_gpx_table\\gpx"
t.folder <- "Z:\\De sastre\\CAC\\github\\R_gpx_table\\tablas"
geo <- "Z:\\De sastre\\CAC\\github\\R_gpx_table\\geo"
epsg.gpx <- 4324
epsg.new <- 25829

# Listar archivos solo .gpx
files <- list.files(g.folder, pattern="*.gpx", full.names=T)
files

#leer archivos
gpx1 <- readGPX(files[1],metadata=F,bounds=F, waypoints=T,tracks = T, routes= F)
gpx2 <- readGPX(files[2],metadata=F,bounds=F, waypoints=T,tracks = T, routes= F)
gpx3 <- readGPX(files[3],metadata=F,bounds=F, waypoints=T,tracks = T, routes= F)

#GENERACION DE TABLAS WP INDICENCIAS, AVES Y COLISIONES
#leer waypoints y pasar a data.frame y cambiar nombre de columnas
cnames <- c('x','y','cod','desc','time','type')

wp1 <- data.frame(gpx1['waypoints'])
colnames(wp1) <- cnames

wp2 <- data.frame(gpx2['waypoints'])
colnames(wp2) <- cnames

wp3 <- data.frame(gpx3['waypoints'])
colnames(wp3) <- cnames

#revisar la lectura de estos gpx pues, en al menos los wp, unos tienen 5 cols y otros 6
#se podría sortear de la siguiente forma
#si hay 5 columnas añadir una ultima columna 'desc' al final 
#si hay 6 columnas mover la columna 4 al final


#seleccionar incidencias
wp3i <- filter(wp3,cod=='I')
#seleccionar avifauna. metodo sencillo seleccionando filas con cod==5
wp3a <- wp3[nchar(wp3$cod)==5,]
#seleccionar colisiones
wp3c <- filter(wp3,cod=='C')
wp3c

#exportar tablas a csv
file_name_wp3i <- paste(t.folder,"\\","incidencias_vinculada.csv",sep="")
file_name_wp3a <- paste(t.folder,"\\","avifauna_vinculada.csv",sep="")
file_name_wp3c <- paste(t.folder,"\\","colisiones_vinculada.csv",sep="")

write.csv2(wp3i, file = file_name_wp3i,row.names=FALSE, na="")
write.csv2(wp3a, file = file_name_wp3a,row.names=FALSE, na="")
write.csv2(wp3a, file = file_name_wp3c,row.names=FALSE, na="")

#JOIN DE ATRIBUTOS
##leer archivo kml y generar geo
PPEE <- readOGR(geo, PPEE)
PPEE <- getKMLcoordinates(kmlfile=kmlPPEE, ignoreAltitude=T)
#make polygon
p1 = Polygon(PPEE)
#make Polygon class
p2 = Polygons(list(p1), ID = "drivetime")
#make spatial polygons class
p3= SpatialPolygons(list(p2),proj4string=CRS("+init=epsg:4326"))


wp3a
crs=epsg.gpx
wp3a_geo <- st_as_sf(wp3a, coords=c("x","y"), crs=epsg.gpx)
st_crs(wp3a_geo)
plot(wp3a_geo)
