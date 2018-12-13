library(exifr);library (dplyr);library(devtools);library(stringr)
library(xlsx);library(rJava);library (rgdal);library(plotKML)
library(spatial);library(sf);library(raster);library(sp)
library(purrr);library(rgeos);library(lubridate);library(SDraw)

#PARAMETROS
# Select the gpx path (use readclipboard() to get the right path in windows).

# Use the gpx.folder as a working directory (just temporary)
# g.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\gpx"
g.folder <- "Z:\\Proxectos\\448_Seguementos_PPEE_zonas_4b_5\\3_Seguimento"

# shp output folder
# shp.out <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\shp\\output"
shp.out <- "Z:\\Proxectos\\448_Seguementos_PPEE_zonas_4b_5\\AdantiaTools\\Output"

# tables output folder
t.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\tablas"
# geo folder to make joins
geo <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\geo\\PPEE"

#set epsg gpx y epsg destino
epsg1 <- CRS("+init=epsg:4326")
epsg2 <- CRS("+init=epsg:25829")
#procesar wp
wp <- F
#SHP TOTAL
TOTAL <- T
#reread FOLDER
reread <- F



#------------------------------------------------------------------
#TRACK PROCESS

# list files with .gpx extension
files <- list.files(g.folder, pattern="*.gpx", full.names=T, recursive = T)
# gpx <- readGPX(files[1],metadata=F,bounds=F, waypoints=F,tracks = T, routes= F)
# tp1 <- gpx['tracks']
# t <- data.frame(lon=double(),
#                 lat=double(),
#                 ele=double(),
#                 time=character(),
#                 track=integer()
# )
# #create unique data.frame with track number
# for (i in 1:length(combine(tp1))){
#     df <- data.frame(combine(tp1)[i])
#     if (length(colnames(df)) != 4){
#         df <- df[,1:4]
#     }
#     colnames(df) <- c("lon","lat","ele", "time")
#     track <- mutate(df,track=i)
#     t <- rbind(t,track)
# }
# tail(t)

#TRACK POINTS ANALYSIS
#lista vacia para archivos procesados
procesados <- list()

#treat warnings as errors
options(warn=2)


for (f in 1:length(files)){
    #guardar nombre de archivos procesados
    procesados <- c(procesados,files[f])

    #read gpx file
    gpx <- readGPX(files[8],metadata=F,bounds=F, waypoints=F,tracks = T, routes= F)
    #base filename
    gpxName <- tools::file_path_sans_ext(basename(files[f]))
    gpxName
    #read trackpoints and create unique data.frame
    tp1 <- gpx['tracks']
    tp1
    #create empty df
    t <- data.frame(lon=double(),
                    lat=double(),
                    ele=double(),
                    time=character(),
                    track=integer()
                    )


    #create unique data.frame with track number
    for (i in 1:length(combine(tp1))){
        df <- data.frame(combine(tp1)[i])
        if (length(colnames(df)) != 4){
            df <- df[,1:4]
        } else {
            df <- df
        }
        colnames(df) <- c("lon","lat","ele", "time")
        track <- mutate(df,track=i)
        t <- rbind(t,track)
    }
    combine(tp1)[2]
    data.frame(combine(tp1)[2])

    df <- data.frame(combine(tp1)[2])

    #add ID
    t$ID <- seq.int(nrow(t))

    #function to add coords conversion to df
    UTM29 = function(data,
                     src.proj = epsg1,
                     dst.proj = epsg2) {
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
    #apply function to add new coord to data
    t2 <- UTM29(data=t)

    #transform time
    #replace T and z by blank in gps time
    t2$time <- gsub("T", ' ',t2$time)
    t2$time <- gsub("Z", '',t2$time)

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

    #identify min and max hour within each 25m buffer
    minT <- data.frame(r) %>%
        group_by(Cod_aero) %>%
        summarise(minTime=min(time))
    maxT <- data.frame(r) %>%
        group_by(Cod_aero) %>%
        summarise(maxTime=max(time))

    #merge dataframes of min and max within 25m buffer
    minmax <- merge(minT, maxT, by="Cod_aero", all.x=T)
    #calculate time betweeen min and max in seconds
    minmax <- mutate(minmax,
                     tiempo_s = as.numeric(difftime(as_datetime(minmax$maxTime),
                                                    as_datetime(minmax$minTime),
                                                    units="secs")))

    #add tecnico
    str(minmax)
    minmax <- mutate(minmax, tecnico=str_sub(gpxName,-3,-1))

    #extract values in range minTime and maxTime and join all in SpatialLines
    L <- list()
    for (i in 1:dim(minmax)[1]){
        tp <- subset(t2, time>=minmax$minTime[i] & time<=minmax$maxTime[i])
        l <- cbind(tp$x, tp$y)
        Sl <- Line(l)
        S <- Lines(list(Sl), ID=minmax$Cod_aero[i])
        L[[i]] <- S
    }

    #create shp with proyeccion
    Sl <- SpatialLines(L, proj4string = epsg2)

    #pasar de spatiallines a spatiallinesdataframe que contenga los datos y crs
    Slx <- SpatialLinesDataFrame(sl=Sl, data=minmax, match.ID = F)

    #calculate length of track with function
    Slx$len <- lineLength(Slx, byid=T)

    #filter len>10 and len<1000
    Slx <- Slx[Slx$len>10 & Slx$len<1000,]

    #Error in x@lines[[1]] : subscript out of bounds
    #para descartar gpx de otras zonas que rompan el codigo filtrar por Slx es 0
    if (length(Slx)==0){
        print (paste("No se guarda el archivo",gpxName))
    } else{
        #join other fiels like cod_aero and parque
        campos <- c("Cod_aero","Aero","Cod_parque","minTime","maxTime","tiempo_s","len","tecnico")
        Slx2 <-merge(Slx, PPEE, by="Cod_aero", all.x=T)[,campos]
        #save shp
        writeOGR(obj=Slx2, dsn=shp.out, layer=gpxName, driver="ESRI Shapefile", overwrite_layer = T)
    }
}

procesados

#TOTAL
if (TOTAL == T){
    #merge all files
    #leer nombre de archivos shp sin extension para meter en readOGR
    shps <- tools::file_path_sans_ext(basename(dir(shp.out, '*shp')))
    shp0 <- readOGR(shp.out, shps[1])[0,]
    for (i in 1:length(shps)){
        shpX <- readOGR(shp.out, shps[i])
        shp0 <- rbind(shp0,shpX)
        print(shp0)
    }
    #save shp TOTAL
    writeOGR(obj=shp0, dsn=shp.out,
             layer="TOTAL",
             driver="ESRI Shapefile",
             overwrite_layer = T)
} else {
    print ('TOTAL.shp no generado')
}

#NOTAS
# sería mejor crear un spatiallines dentro de la función donde se fueran juntando los shp creados
# para ello hay que crear un spatiallines identico al final pero vacio donde se vayan haciendo los merge







#---------------------------------------------------------------------------
#PROCESAR WAYPOINTS
if (wp == T) {
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
} else {
    print ('WP no extraídos')
}

#_________________________________________________________________________
#por hacer:
#asignacion de especies por codigo
#reproyeccion de coordenadas
#asignacion automatica de fotos en caso de incidencia
#_________________________________________________________________________
