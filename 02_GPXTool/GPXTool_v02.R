source("C:/GitHub/AdantiaTools/02_GPXTool/funciones.R")

#PARAMETROS
#FILES
g.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\gpx" #folder with gpx files
# g.folder <- "Z:\\Proxectos\\448_Seguementos_PPEE_zonas_4b_5\\3_Seguimento" #folder with gpx files
shp.out <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\output" # shp output folder
# shp.out <- "Z:\\Proxectos\\448_Seguementos_PPEE_zonas_4b_5\\AdantiaTools\\Output"

#TFOLDER
t.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\tablas" # tables output folder
geo <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\geo\\PPEE" #shp for joins PPEE

#TOTAL
#estos son los atributos a seleccionar, ojo deben coincir en nombre a los de PPEE
shpnames <- c("Cod_aero","Aero","Cod_parque","minTime","maxTime","tiempo_s","len","tecnico","filepath")
shp0 <- EmptySLDF(shpnames) #creates empty SLDF from names

#TOTALES TABLA
tabla <- data.frame(matrix(ncol=2, nrow=0)); colnames(tabla) <- c("filepath","estado")

#PPEE
PPEE <- readOGR(geo, 'PPEE') # read shp of ppee
PPEE25m <- gBuffer(PPEE, byid= T, width = 25, quadsegs=10) # create buffer 25m by id

TOTAL <- T #SHP TOTAL
INDIVIDUAL <- T #SHP individual
reread <- T #reread FOLDER
TABLA_PROCESADOS <- T #tabla de archivos procesados

#------------------------------------------------------------------
#TRACK PROCESS

# list files with .gpx extension and check if reread is T
if (reread == T){
    files <- list.files(g.folder, pattern="*.gpx", full.names=T, recursive = T)
}
# #COPY GPX FILES TO
# gpx.copy <- "C:\\Users\\cac\\Dropbox\\Adantia_gpx"
# file.copy(files,gpx.copy, overwrite = F)
# #ojo, al copiar los archivos gpx de un directorio a otro se pierden de alrededor de 2500
# #habria que comprobar por que. es posible que sea por nombres repetidos

#ANALYSIS
for (f in 1:length(files)){
    f <- 11
    gpxName <- tools::file_path_sans_ext(basename(files[f])) #base filename
    filepath <- files[f] #filepath

    #3 funciones principales a prueba TrackToDF, DFToPoints01 y TrackToDF
    if (class(try(TrackToDF(files[f]),silent=T)) != "try-error"){
        t <- TrackToDF(files[f]) #gpx track to dataframe

        if (class(try(DFToPoints01(t)),silent=T)) != "try-error"){
            t2 <- DFToPoints01(t) #df to points from epsg 4326 to epsg 25829

            if (class(try(PPEElines(t2,PPEE25m,Cod_aero,gpxName,filepath,epsg2)),silent=T)) != "try-error"){
                Slx <- PPEElines(t2,PPEE25m,Cod_aero,gpxName,filepath,epsg2) #track in 25m buffer
                #FILTROOOOOOOS
                Slx <- subset(Slx, Slx$len>50) #filter len>50
                Slx <- subset(Slx, Slx$len/Slx$tiempo_s<=2) #filter len_t less than 2
                Slx <- subset(Slx, Slx$tiempo_s<=1800) #eliminar tiempo superior a 30 min
                length(Slx) != 0

                if (length(Slx)!=0){
                    crs(PPEE) <- crs(Slx)
                    Slx2 <-merge(Slx, PPEE, by="Cod_aero", all.x=T) [,shpnames]#merge with PPEE info

                    #salvar archivos individuales
                    if (INDIVIDUAL == T){
                        writeOGR(obj=Slx2, dsn=shp.out, layer=gpxName, driver="ESRI Shapefile",
                                 overwrite_layer = T) #save shp
                    }

                    #merge Slx2 with the shp
                    shp0 <- rbind(shp0,Slx2)
                    tabla <- rbind(tabla,data.frame(filepath=filepath,estado="procesado")) #add procesado

                    } else {
                        tabla <- rbind(tabla,data.frame(filepath=filepath,estado="gpx no en PPEE")) #add no en PPEE
                    }
                } else {
                tabla <- rbind(tabla,data.frame(filepath=filepath,estado="error en PPEElines")) #add no en PPEE
            }
        } else {
            tabla <- rbind(tabla,data.frame(filepath=filepath,estado="error en DFToPoints01")) #add no en PPEE
        }
    } else {
        tabla <- rbind(tabla,data.frame(filepath=filepath,estado="error en TrackToDF")) #add no en PPEE
    }
}

# save shp TOTAL
writeOGR(obj=shp0, dsn=shp.out,
         layer="TOTAL",
         driver="ESRI Shapefile",
         overwrite_layer = T)

#save status table
colnames(tabla) <- c("filepath", "estado")
file_name <- paste(shp.out,"\\","TABLA_PROCESADOS.csv",sep="")
write.csv2(tabla, file = file_name,row.names=F, na="")

#NOTAS
# sería mejor crear un spatiallines dentro de la función donde se fueran juntando los shp creados
# para ello hay que crear un spatiallines identico al final pero vacio donde se vayan haciendo los merge
