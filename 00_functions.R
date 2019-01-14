library(base)
if("digest" %in% rownames(installed.packages()) == FALSE) {install.packages("digest")}
library(digest)
if("tidyselect" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyselect")}
library(tidyselect)
if("tibble" %in% rownames(installed.packages()) == FALSE) {install.packages("tibble")}
library(tibble)
if("rlang" %in% rownames(installed.packages()) == FALSE) {install.packages("rlang")}
library(rlang)
if("zoo" %in% rownames(installed.packages()) == FALSE) {install.packages("zoo")}
library(zoo)
if("FNN" %in% rownames(installed.packages()) == FALSE) {install.packages("FNN")}
library(FNN)
if("xts" %in% rownames(installed.packages()) == FALSE) {install.packages("xts")}
library(xts)
if("XML" %in% rownames(installed.packages()) == FALSE) {install.packages("XML")}
library(XML)
if("classInt" %in% rownames(installed.packages()) == FALSE) {install.packages("classInt")}
library(classInt)
if("scales" %in% rownames(installed.packages()) == FALSE) {install.packages("scales")}
library(scales)
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
library(dplyr)
if("devtools" %in% rownames(installed.packages()) == FALSE) {install.packages("devtools")}
library(devtools)
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
library(stringr)
if("rJava" %in% rownames(installed.packages()) == FALSE) {install.packages("rJava")}
library(rJava)
if("xlsx" %in% rownames(installed.packages()) == FALSE) {install.packages("xlsx")}
library(xlsx)
if("rgdal" %in% rownames(installed.packages()) == FALSE) {install.packages("rgdal")}
library(rgdal)
if("plotKML" %in% rownames(installed.packages()) == FALSE) {install.packages("plotKML")}
library(plotKML)
if("spatial" %in% rownames(installed.packages()) == FALSE) {install.packages("spatial")}
library(spatial)
if("sf" %in% rownames(installed.packages()) == FALSE) {install.packages("sf")}
library(sf)
if("raster" %in% rownames(installed.packages()) == FALSE) {install.packages("raster")}
library(raster)
if("purrr" %in% rownames(installed.packages()) == FALSE) {install.packages("purrr")}
library(purrr)
if("rgeos" %in% rownames(installed.packages()) == FALSE) {install.packages("rgeos")}
library(rgeos)
if("lubridate" %in% rownames(installed.packages()) == FALSE) {install.packages("lubridate")}
library(lubridate)
if("SDraw" %in% rownames(installed.packages()) == FALSE) {install.packages("SDraw")}
library(SDraw)
if("rlist" %in% rownames(installed.packages()) == FALSE) {install.packages("rlist")}
library(rlist)
if("sp" %in% rownames(installed.packages()) == FALSE) {install.packages("sp")}
library(sp)
if("exifr" %in% rownames(installed.packages()) == FALSE) {install.packages("exifr")}
library(exifr)
if("stringr" %in% rownames(installed.packages()) == FALSE) {install.packages("stringr")}
library(stringr)
if("tinytex" %in% rownames(installed.packages()) == FALSE) {install.packages("tinytex")}
library(tinytex)
if("parallel" %in% rownames(installed.packages()) == FALSE) {install.packages("parallel")}
library(parallel)
if("rbenchmark" %in% rownames(installed.packages()) == FALSE) {install.packages("rbenchmark")}
library(rbenchmark)
if("snow" %in% rownames(installed.packages()) == FALSE) {install.packages("snow")}
library(snow)
if("MASS" %in% rownames(installed.packages()) == FALSE) {install.packages("MASS")}
library(MASS)


#___________________________________________________________________________________________
#CopyFilesExtructure
#Funcion para copiar todod los arhivos de una lista y mantener estructura de carpetas desde disco
CopyFilesExtructure <- function (files,newdir) {
    split_path <- function(path) {
        setdiff(strsplit(path,"/|\\\\")[[1]], "")
    } 
    
    for (f in files){
        newpath <- newdir
        splitedPath <- split_path(f)[-1] #eliminar elemento disco
        splitedPath <- splitedPath[1:length(splitedPath)-1] #eliminar elemento archivo
        for (p in splitedPath){
            newnewpath <- file.path(newpath, p)
            dir.create(newnewpath, showWarnings = F)
            newpath <- newnewpath
        }
        file.copy(f, newpath, overwrite = T, recursive = T, copy.mode = TRUE)
    }
}


#-----------------------------------------------------------------------------
#TrackToDF
#Read trackspoints from a gpx file (class string - filepath) and returns a
#data.frame with lon, lat, ele and time
TrackToDF <- function (gpx){
    tracks <- gpx['tracks'] #read trackpoints and create unique data.frame

    #create empty df
    t <- data.frame(lon=numeric(),
                    lat=numeric(),
                    ele=character(),
                    time=character(),
                    track=numeric(),
                    stringsAsFactors=FALSE
    )

    #create unique data.frame with track number
    iteracion <- 1
    for (i1 in 1:length(combine(tracks))){
        for (i2 in 1:length(combine(combine(tracks)[i1]))){
            df <- data.frame(combine(tracks)[[i1]][i2])
            if (nrow(df)<2){ #eliminar df con puntos insuficientes
                #no sucede nada porque es una secuencia nula
            } else {
                if (length(colnames(df)) != 4){
                    df <- df[,1:4]
                } else {
                    df <- df
                }
                colnames(df) <- c("lon","lat","ele", "time") #adjust number of columns
                track <- mutate(df,track=iteracion)
                t <- rbind(t,track)
                iteracion <- iteracion+1
            }
            iteracion <- iteracion+1
        }
    }
    
    #add ID to unique df
    t <- mutate(t,ID=rownames(t))
    #replace T and z by blank in gps time
    t$time <- gsub("T", ' ',t$time)
    t$time <- gsub("Z", '',t$time)

    #output
    return (t)
}

#--------------------------------------------------------------------------------

#DFtoPoints
#get a data.frame with lon lat and creates an spatial object converting CRS
DFToPoints01 <- function(df,
                       epsg1 = CRS("+init=epsg:4326"),
                       epsg2 = CRS("+init=epsg:25829")){

    #create spatial objec from coords
    coordinates(df) <- c("lon","lat")
    proj4string(df) <- epsg1
    sdf <- spTransform(df,epsg2) #change CRS
    sdf$x <- sdf@coords[,1] #add coords to table
    sdf$y <- sdf@coords[,2] #add coords to table

    #output
    return (sdf)
}

#-------------------------------------------------------------------------
#PPEElines
#intersecta los track points con PPEE, selecciona minimo y maximo dentro del radio
#y crea una linea a partir de los puntos entre ese rango de minimo y maximo
#usa el campo Cod_aero como atributo de union
PPEElines <- function(points, PPEE25m, gpxName, filepath,
                      epsg = CRS("+init=epsg:25829")){
    i <- intersect(points,PPEE25m) #intersect
    r <- merge(points, i, by="ID", all.x=TRUE) #merge
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

    #ADD VARIABLES
    minmax <- mutate(minmax, tecnico = str_sub(gpxName,-3,-1)) #add tecnico
    minmax <- mutate(minmax, filepath) #add filepath

    #extract values in range minTime and maxTime and join all in SpatialLines
    L <- list()
    for (i in 1:dim(minmax)[1]){
        tp <- subset(as.data.frame(t2), time>=minmax$minTime[i] & time<=minmax$maxTime[i])
        l <- cbind(tp$x, tp$y)
        Sl <- Line(l)
        S <- Lines(list(Sl), ID=minmax$Cod_aero[i])
        L[[i]] <- S
    }

    #create shp with proyeccion
    Sl <- SpatialLines(L, proj4string = epsg)
    #pasar de spatiallines a spatiallinesdataframe que contenga los datos y crs
    Slx <- SpatialLinesDataFrame(sl=Sl, data=minmax, match.ID = F)
    #calculate length of track with function
    Slx$len <- lineLength(Slx, byid=T)

    #output
    return (Slx)
}

#---------------------------------------------------------------------
#EmptySLDF
#create an empty dataframe from a list of var names . epsg 25829 by default
EmptySLDF <- function (AtributosPPEE=c("Cod_aero","Aero","Cod_Parque","minTime",
                             "maxTime","tiempo_s","len","tecnico","filepath"),
                       epsg = CRS("+init=epsg:25829")){
    
    s = SpatialLinesDataFrame(
        sl=SpatialLines(
            LinesList=list(
                Lines(
                    list(
                        Line(
                            coords=matrix(c(0,1,0,1),2,2)
                        )
                    ),ID=1)
            )
        ),
        data=data.frame(matrix(ncol=length(AtributosPPEE), nrow=1)))[-1,]
    crs(s) <- epsg #set epsg
    names(s) <- AtributosPPEE
    return(s)
}

#----------------------------------------------------------------------------
#GPXTool
GPXTool <- function (filepath, 
                     PPEE25m, 
                     AtributosPPEE = c("Cod_aero","Aero","Cod_parque","minTime","maxTime","tiempo_s","len","tecnico","filepath")) {
   
    gpxName <- tools::file_path_sans_ext(basename(filepath)) #base filename
    filepath <- filepath #filepath
    
    if (class(try(readGPX(filepath),silent=T)) == "try-error"){
        
        #si no funciona asi habra que hacer un rbind a partir de una tabla predefinida
        tabla <- data.frame(filepath = filepath, estado="archivo corrupto") #add no en PPEE
        return (list(NA,tabla))
        
    } else {
        
        gpx <- readGPX(filepath,metadata=F,bounds=F, waypoints=F,tracks = T, routes= F) #read gpx file
        
        #RPOCESO con 3 funciones principales a prueba TrackToDF, DFToPoints01 y TrackToDF
        if (class(try(TrackToDF(gpx),silent=T)) == "try-error"){
            
            tabla <- data.frame(filepath=filepath,estado="error 01 en DFToPoints01") #add no en PPEE
            return (list(NA,tabla))
            
        } else {
            
            t <- TrackToDF(gpx) #gpx track to dataframe
            
            if (nrow(t)<2){
                
                tabla <- data.frame(filepath=filepath,estado="DF vacío tras TrackToDF") #conjunto vacio
                return (list(NA,tabla))
                
            } else {
                
                if (class(try(DFToPoints01(t),silent=T)) == "try-error"){
                    
                    tabla <- data.frame(filepath=filepath,estado="error 02 en DFToPoints01") #add no en PPEE
                    return (list(NA,tabla))
                    
                } else {
                    
                    t2 <- DFToPoints01(t) #df to points from epsg 4326 to epsg 25829
                    
                    if (class(try(PPEElines(t2, PPEE25m, gpxName, filepath), silent=T)) == "try-error"){
                        
                        tabla <- data.frame(filepath = filepath,estado="error 03 en PPEElines - interseccion") #add no en PPEE
                        return (list(NA,tabla))
                        
                    } else {
                        
                        Slx <- PPEElines(t2, PPEE25m, gpxName, filepath) #track in 25m buffer
                        
                        #FILTROOOOOOOS
                        Slx <- subset(Slx, Slx$len>50) #filter len>50
                        Slx <- subset(Slx, Slx$len/Slx$tiempo_s<=2) #filter len_t less than 2
                        Slx <- subset(Slx, Slx$tiempo_s<=1800) #eliminar tiempo superior a 30 min
                        Slx <- Slx[!(is.na(Slx$Cod_aero) | Slx$Cod_aero==""), ] #eliminar tracks con Aero no asignado
                        
                        if (length(Slx)==0){
                            
                            tabla <- data.frame(filepath=filepath,estado="gpx no en PPEE") #add no en PPEE
                            return (list(NA,tabla))
                            
                        } else {
                            #merge Slx2 with the shp
                            crs(PPEE25m) <- crs(Slx)
                            Slx2 <-merge(Slx, PPEE25m, by="Cod_aero", all.x=T) [,AtributosPPEE] #merge with PPEE info
                            tabla <- data.frame(filepath=filepath,estado="procesado") #add procesado
                            return (list(Slx2,tabla))
                            
                        }
                    }
                }
            }
        }
    }
}




#______________________________________________________________________________
#---------------------------------------------------------------------------
# #PROCESAR WAYPOINTS
# if (wp == T) {
#     #GENERACION DE TABLAS WP INDICENCIAS, AVES Y COLISIONES
#     wp1 <- data.frame(gpx1['waypoints'])
#     wp2 <- data.frame(gpx2['waypoints'])
#
#     #seleccionar incidencias
#     wp1i <- filter(wp1,waypoints.name=='I')
#     #seleccionar avifauna. metodo sencillo seleccionando filas con cod==5
#     wp1a <- wp1[nchar(wp1$waypoints.name)==5,]
#     #seleccionar colisiones
#     wp1c <- filter(wp1,waypoints.name=='C')
#     #any other code
#     wp1o <- filter(wp1, waypoints.name!='I' & waypoints.name !='C' & nchar(wp1$waypoints.name)!=5)
#
#     #exportar tablas a csv
#     file_name_wp1i <- paste(t.folder,"\\","incidencias_vinculada.csv",sep="")
#     file_name_wp1a <- paste(t.folder,"\\","avifauna_vinculada.csv",sep="")
#     file_name_wp1c <- paste(t.folder,"\\","colisiones_vinculada.csv",sep="")
#     file_name_wp1o <- paste(t.folder,"\\","otros_vinculada.csv",sep="")
#
#     write.csv2(wp1i, file = file_name_wp1i,row.names=FALSE, na="")
#     write.csv2(wp1a, file = file_name_wp1a,row.names=FALSE, na="")
#     write.csv2(wp1c, file = file_name_wp1c,row.names=FALSE, na="")
#     write.csv2(wp1o, file = file_name_wp1o,row.names=FALSE, na="")
# } else {
#     print ('WP no extraidos')
# }

#_________________________________________________________________________
#por hacer:
#asignacion de especies por codigo
#reproyeccion de coordenadas
#asignacion automatica de fotos en caso de incidencia
#_________________________________________________________________________

##_______________________________________________________________________________
#_______________________________________________________________________________
#FOTOTOOLS FUNCTIONS

#CreateCounter creates a counter adding lef 0 until desired length
CreateCounter <- function(n,l=3){
    counter <- c(1:n) # Create a counter
    counter <- str_replace_all(str_pad(sapply(counter,toString),l,"left")," ","0")
    return (counter)
}

#------------------------------------------------------------------------------------
#CoordsToDF
#funcion para transformar el tibble de lectura gpx en una tabla de nombre de archivo y coordenadas
#files data.frame procedente de read_exif. Definir GPSPosition y filepath
PhotoCoodsToDF <- function (files, GPSPosition = "GPSPosition", ID = "newfilename") {
    coords <- strsplit(files[,GPSPosition], " ") #splits unique text of coords into y,x list
    XYdf <- data.frame(newfilepath = numeric(), # creates empty dataframe
                       lon = numeric(),
                       lat = numeric())

    for (i in 1:length(files[[1]])){
        x <- coords[[i]][2]
        numX <- sub('\\..*','',x) #mantiene todo lo que hay delante de un punto
        decX <- substr(sub('...\\.*','',x),1,5) #mantiene todo lo que hay detras de un punto
        corX <- paste0(numX,'.',decX)
        y <- coords[[i]][1]
        numY <- sub('\\..*','',y) #mantiene todo lo que hay delante de un punto
        decY <- substr(sub('...\\.*','',y),1,5) #mantiene todo lo que hay detras de un punto
        corY <- paste0(numY,'.',decY)
        XYdf[i,] <- c(files[,ID][i], corX, corY) #add values to df
    }
    return (XYdf)
}




