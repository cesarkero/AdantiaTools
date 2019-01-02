source("C:/GitHub/AdantiaTools/02_GPXTool/01_functions/TrackToDF.R")

#PARAMETROS
#FILES
g.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\gpx" #folder with gpx files
# g.folder <- "Z:\\Proxectos\\448_Seguementos_PPEE_zonas_4b_5\\3_Seguimento" #folder with gpx files
shp.out <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\output" # shp output folder
# shp.out <- "Z:\\Proxectos\\448_Seguementos_PPEE_zonas_4b_5\\AdantiaTools\\Output"
#TOTAL FILE - basado en una estructura ya creada...
shp.total <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\geo\\TOTAL"
shp0 <- readOGR(shp.total, "TOTAL")[0,] #sustituir esto por la ccreacion de un shp vacio
#TOTAL TABLE
tabla <- data.frame(filepath = as.character(), estado = as.character() )
#PPEE
PPEE <- readOGR(geo, 'PPEE') # read shp of ppee
PPEE25m <- gBuffer(PPEE, byid= T, width = 25, quadsegs=10) # create buffer 25m by id
#TFOLDER
t.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\tablas" # tables output folder
geo <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\geo\\PPEE" #shp for joins PPEE
wp <- F #procesar waypoints
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
    gpxName <- tools::file_path_sans_ext(basename(files[f])) #base filename
    filepath <- files[f] #filepath

    t <- TrackToDF(files[f]) #gpx track to dataframe
    t2 <- DFToPoints01(t) #df to points from epsg 4326 to epsg 25829
    Slx <- PPEElines(t2,PPEE25m,Cod_aero,gpxName,filepath,epsg2) #track in 25m buffer

    if (length(Slx)==0){
        tabla <- rbind(tabla,c(filepath,"gpx no en PPEEE")) #add no en PPEE
    } else {
        #FILTROOOOOOOS
        Slx <- subset(Slx, Slx$len>50) #filter len>50
        Slx <- subset(Slx, Slx$len/Slx$tiempo_s<=2) #filter len_t less than 2
        Slx <- Slx <- subset(Slx, Slx$tiempo_s<=1800) #eliminar tiempo superior a 30 min

        #join other fiels like cod_aero and parque
        campos <- c("Cod_aero", "Aero","Cod_parque","minTime","maxTime","tiempo_s","len",
                    "tecnico","filepath")

        Slx2 <-merge(Slx, PPEE, by="Cod_aero", all.x=T)[,campos]

        #salvar archivos individuales
        if (INDIVIDUAL == T){
            #save shp
            writeOGR(obj=Slx2,
                     dsn=shp.out,
                     layer=gpxName,
                     driver="ESRI Shapefile",
                     overwrite_layer = T)
        }

        #merge shapefile with the shp0
        crs(shp0)=crs(Slx2)
        shp0 <- rbind(shp0,Slx2)
        tabla <- rbind(tabla,c(filepath,"procesado")) #add procesado
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


