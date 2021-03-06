library(exifr)
library(dplyr)
library(devtools)
library(stringr)
library(xlsx)
library(rJava)
library(rgdal)
library(plotKML)
library(spatial)
library(sf)
library(raster)
library(sp)
library(purrr)
library(rgeos)
library(lubridate)
library(SDraw)
library(rlist)

#PARAMETROS
# g.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\gpx" #folder with gpx files
g.folder <- "Z:\\Proxectos\\448_Seguementos_PPEE_zonas_4b_5\\3_Seguimento" #folder with gpx files

# shp.out <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\output" # shp output folder
# shp.out <- "Z:\\Proxectos\\448_Seguementos_PPEE_zonas_4b_5\\AdantiaTools\\Output"
shp.out <- "C:\\Users\\cac\\Dropbox\\Adantia_gpx"

t.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\tablas" # tables output folder
geo <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\geo\\PPEE" #shp for joins PPEE
epsg1 <- CRS("+init=epsg:4326") #epsg gps
epsg2 <- CRS("+init=epsg:25829") #epsg destino
wp <- F #procesar waypoints
TOTAL <- T #SHP TOTAL
reread <- T#reread FOLDER
TABLA_PROCESADOS <- T #tabla de archivos procesados

#------------------------------------------------------------------
#TRACK PROCESS

# list files with .gpx extension and check if reread is T
if (reread == T){
    files <- list.files(g.folder, pattern="*.gpx", full.names=T, recursive = T)
}

#copy files to new folder
file.copy(files,shp.out, overwrite = F)
files[1]

#TRACK POINTS ANALYSIS
procesados <- list() #lista vac�a para archivos procesados filepaht
procesado <- list() #lista vac�a para archivos procesados

options(warn=2) #treat warnings as errors

#FALLO al leer el gpx SI EL GPX ESTA CORRUPTO
for (f in 1:length(files)){
    # f <- 1770
    #check if gpx is not corrupted
    GPXok <- try(readGPX(files[f]), silent=T)

    if (class(GPXok) == "try-error"){
        procesados <- c(procesados,files[f]) #guardar nombre de archivos procesados
        procesado <- c(procesado,'corrupto') #add no en PPEE

    } else {
        gpx <- readGPX(files[f],metadata=F,bounds=F, waypoints=F,tracks = T, routes= F) #read gpx file
        gpxName <- tools::file_path_sans_ext(basename(files[f])) #base filename
        tp1 <- gpx['tracks'] #read trackpoints and create unique data.frame

        #condition if file gpx has just 1 point
        if (dim(tp1[[1]][[1]][[1]])[1] == 1) {
            procesados <- c(procesados,files[f]) #guardar nombre de archivos procesados
            procesado <- c(procesado,'error') #add no en PPEE
        } else if (is.null(tp1$tracks)){
            procesados <- c(procesados,files[f]) #guardar nombre de archivos procesados
            procesado <- c(procesado,'track null') #add no en PPEE
        } else { #PROCEEES

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
            for (i1 in 1:length(combine(tp1))){
                for (i2 in 1:length(combine(combine(tp1)[i1]))){
                    df <- data.frame(combine(tp1)[[i1]][i2])
                    str(df)
                    if (length(colnames(df)) != 4){
                        df <- df[,1:4]
                    } else {
                        df <- df
                    }
                    colnames(df) <- c("lon","lat","ele", "time") #ajustar numero de columnas
                    track <- mutate(df,track=iteracion)
                    t <- rbind(t,track)
                    iteracion <- iteracion+1
                }
            }

            #add ID to unique df
            t <- mutate(t,ID=rownames(t))

            #replace T and z by blank in gps time
            t$time <- gsub("T", ' ',t$time)
            t$time <- gsub("Z", '',t$time)



            #create spatial objec from coords
            coordinates(t) <- c("lon","lat")
            proj4string(t)<-epsg1
            t2 <- spTransform(t,epsg2)

            #add coords to table
            t2$x <- t2@coords[,1]
            t2$y <- t2@coords[,2]
            t2

            #GET MIN AND MAX HOUR WITHIN AERO BUFFER
            PPEE <- readOGR(geo, 'PPEE') # read shp of ppee
            PPEE25m <- gBuffer(PPEE, byid= T, width = 25, quadsegs=10) # create buffer 25m by id

            #check if intersect works and continue #BUSCAR ALTERNATIVA
            INTok <- try(intersect(t2,PPEE25m), silent=T)
            class(INTok) == "try-error"
            if (class(INTok) == "try-error"){
                procesados <- c(procesados,files[f]) #guardar nombre de archivos procesados
                procesado <- c(procesado,'error intersect') #add no en PPEE
            } else {
                i <- intersect(t2,PPEE25m) #intersect
                r <- merge(t2, i, by="ID", all.x=TRUE) #merge
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
                minmax <- mutate(minmax, filepath = files[f]) #add filepath

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
                Sl <- SpatialLines(L, proj4string = epsg2)

                #pasar de spatiallines a spatiallinesdataframe que contenga los datos y crs
                Slx <- SpatialLinesDataFrame(sl=Sl, data=minmax, match.ID = F)

                #calculate length of track with function
                Slx$len <- lineLength(Slx, byid=T)

                #FILTROOOOOOOS
                Slx <- subset(Slx, Slx$len>50) #filter len>50
                Slx <- subset(Slx, Slx$len/Slx$tiempo_s<=2) #filter len_t less than 2
                Slx <- Slx <- subset(Slx, Slx$tiempo_s<=1800) #eliminar tiempo superior a 30 min

                #para descartar gpx de otras zonas que rompan el codigo filtrar por Slx es 0
                if (length(Slx)==0){
                    print (paste("No se guarda el archivo",gpxName))
                    procesado <- c(procesado,'no en PPEE') #add no en PPEE
                } else{
                    #join other fiels like cod_aero and parque
                    campos <- c("Cod_aero",
                                "Aero",
                                "Cod_parque",
                                "minTime",
                                "maxTime",
                                "tiempo_s",
                                "len",
                                "tecnico",
                                "filepath")

                    Slx2 <-merge(Slx, PPEE, by="Cod_aero", all.x=T)[,campos]

                    #save shp
                    writeOGR(obj=Slx2,
                             dsn=shp.out,
                             layer=gpxName,
                             driver="ESRI Shapefile",
                             overwrite_layer = T)
                    procesado <- c(procesado,'procesado') #add procesado
                }
                procesados <- c(procesados,files[f]) #guardar nombre de archivos procesados
            }
        }
    }
}

if (TABLA_PROCESADOS ==T) {
    #combinar dos listas en un data.frame
    tabla <- do.call(rbind, Map(data.frame, filepath=procesados, procesado=procesado))
    #exportar tablas a csv
    file_name <- paste(shp.out,"\\","TABLA_PROCESADOS.csv",sep="")
    write.csv2(tabla, file = file_name,row.names=F, na="")
}

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
# ser�a mejor crear un spatiallines dentro de la funci�n donde se fueran juntando los shp creados
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
    print ('WP no extra�dos')
}

#_________________________________________________________________________
#por hacer:
#asignacion de especies por codigo
#reproyeccion de coordenadas
#asignacion automatica de fotos en caso de incidencia
#_________________________________________________________________________
