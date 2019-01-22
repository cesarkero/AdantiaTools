source("C:/GitHub/AdantiaTools/00_functions.R")

#PARAMETROS
#FILES
# g.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\gpx" #folder with gpx files
g.folder <- "Z:\\Proxectos\\448_Seguementos_PPEE_zonas_4b_5\\3_Seguimento" #folder with gpx files
# shp.out <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\output" # shp output folder
shp.out <- "Z:\\Proxectos\\448_Seguementos_PPEE_zonas_4b_5\\AdantiaTools\\Output_02"

#TFOLDER
t.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\tablas" # tables output folder
geo <- "C:\\GitHub\\AdantiaTools\\000_GEO\\PPEE" #shp for joins PPEE

#TOTAL
#estos son los atributos a seleccionar, ojo deben coincir en nombre a los de PPEE
shpnames <- c("Cod_aero","Aero","Cod_parque","minTime","maxTime","tiempo_s","len","tecnico","filepath")
shp0 <- EmptySLDF(shpnames) #creates empty SLDF from names

#TOTALES TABLA
tabla <- data.frame(matrix(ncol=2, nrow=0)); colnames(tabla) <- c("filepath","estado")

#PPEE
PPEE <- readOGR(geo, 'PPEE') # read shp of ppee
PPEE25m <- gBuffer(PPEE, byid= T, width = 25, quadsegs=10) # create buffer 25m by id

INDIVIDUAL <- F #guarda cada SHP individual
reread <- T #reread FOLDER

#------------------------------------------------------------------
#TRACK PROCESS

# list files with .gpx extension and check if reread is T
if (reread == T){
    files <- list.files(g.folder, pattern="*.gpx", full.names=T, recursive = T)
}

# #COPY GPX FILE structure TO
# gpx.copy <- "C:/Users/cac/Dropbox/Adantia_gpx"
# CopyFilesExtructure(files,gpx.copy) # copiar archivos filtrados manteniendo estrucura de carpetas
# files2 <- list.files(gpx.copy, pattern="*.gpx", full.names=T, recursive = T) #leer copia

#ANALYSIS
for (f in 1:length(files)){
    gpxName <- tools::file_path_sans_ext(basename(files[f])) #base filename
    filepath <- files[f] #filepath
    print (paste("procesando ",gpxName,' - Archivo ', f, " de ", length(files))) #mensaje
    
    if (class(try(readGPX(filepath),silent=T)) == "try-error"){
        tabla <- rbind(tabla,data.frame(filepath=filepath,estado="archivo corrupto")) #add no en PPEE
    } else {
        gpx <- readGPX(filepath,metadata=F,bounds=F, waypoints=F,tracks = T, routes= F) #read gpx file
        #RPOCESO con 3 funciones principales a prueba TrackToDF, DFToPoints01 y TrackToDF
        if (class(try(TrackToDF(gpx),silent=T)) == "try-error"){
            tabla <- rbind(tabla,data.frame(filepath=filepath,estado="error 01 en DFToPoints01")) #add no en PPEE
        } else {
            t <- TrackToDF(gpx) #gpx track to dataframe
            if (nrow(t)<2){
                tabla <- rbind(tabla,data.frame(filepath=filepath,estado="DF vacío tras TrackToDF")) #conjunto vacio
            } else {
                if (class(try(DFToPoints01(t),silent=T)) == "try-error"){
                    tabla <- rbind(tabla,data.frame(filepath=filepath,estado="error 02 en DFToPoints01")) #add no en PPEE
                } else {
                    t2 <- DFToPoints01(t) #df to points from epsg 4326 to epsg 25829
                    t2
                    if (class(try(PPEElines(t2,PPEE25m,Cod_aero,gpxName,filepath),silent=T)) == "try-error"){
                        tabla <- rbind(tabla,data.frame(filepath=filepath,estado="error 03 en PPEElines - interseccion")) #add no en PPEE
                    } else {
                        Slx <- PPEElines(t2,PPEE25m,Cod_aero,gpxName,filepath) #track in 25m buffer
                        #FILTROOOOOOOS
                        Slx <- subset(Slx, Slx$len>50) #filter len>50
                        Slx <- subset(Slx, Slx$len/Slx$tiempo_s<=2) #filter len_t less than 2
                        Slx <- subset(Slx, Slx$tiempo_s<=1800) #eliminar tiempo superior a 30 min
                        Slx <- Slx[!(is.na(Slx$Cod_aero) | Slx$Cod_aero==""), ] #eliminar tracks con Aero no asignado
                        Slx
                        if (length(Slx)==0){
                            tabla <- rbind(tabla,data.frame(filepath=filepath,estado="gpx no en PPEE")) #add no en PPEE
                        } else {
                            #salvar archivos individuales
                            if (INDIVIDUAL == T){
                                writeOGR(obj=Slx2, dsn=shp.out, layer=gpxName, driver="ESRI Shapefile",
                                         overwrite_layer = T) #save shp
                            }
                            #merge Slx2 with the shp
                            crs(PPEE) <- crs(Slx)
                            Slx2 <-merge(Slx, PPEE, by="Cod_aero", all.x=T) [,shpnames]#merge with PPEE info
                            shp0 <- rbind(shp0,Slx2)
                            tabla <- rbind(tabla,data.frame(filepath=filepath,estado="procesado")) #add procesado
                        }
                    }
                }
            }
        }
    }
}    

# save shp TOTAL
writeOGR(obj=shp0, dsn=shp.out,
         layer="TOTAL",
         driver="ESRI Shapefile",
         overwrite_layer = T)

#save status table
colnames(tabla) <- c("filepath", "estado")
file_name_proceso <- paste(shp.out, "\\", "archivos_procesados.csv", sep="")
write.csv2(tabla, file = file_name_proceso, row.names=F, na="")

#save results table
file_name_results <- paste(shp.out,"\\","resultados.csv",sep="")
write.csv2(data.frame(shp0), file= file_name_results, row.names=F, na="")
