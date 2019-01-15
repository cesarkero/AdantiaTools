source("C:/GitHub/AdantiaTools/00_functions.R")

#PARAMETROS
#FILES
g.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\gpx" #folder with gpx files
shp.out <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\output" # shp output folder
AtributosPPEE <- c("Cod_aero","Aero","Cod_parque","minTime","maxTime","tiempo_s","len","tecnico","filepath")
shp0 <- EmptySLDF(AtributosPPEE) #creates empty SLDF from names

#TOTALES TABLA
tabla <- data.frame(matrix(ncol=2, nrow=0)); colnames(tabla) <- c("filepath","estado")
tabla
#PPEE
geo <- "C:\\GitHub\\AdantiaTools\\000_GEO\\PPEE" #shp for joins PPEE
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
files <- files[1:25]

#------------------------------------------
#Proceso lapply
l1 <-  lapply(files,
              GPXTool,
              PPEEbuffer = PPEE25m)

#------------------------------------------------------
#proceso de parallel para windows NO FUNCIONA TODAVIA
no_cores <- detectCores() - 1 # Calculate the number of cores
cl <- makeCluster(no_cores, type="SOCK") # Initiate cluster

#export objects and functions
clusterExport(cl=cl, objects())
clusterExport(cl=cl, as.list(ls()),
              envir=environment())
l2 <- parLapply(cl,
                files,
                GPXTool,
                PPEEbuffer = PPEE25m)
stopCluster(cl)

#-----------------------------------------------
#merge all valid shp in the list
for (i in 1:length(l1)) {
    if (!is.na(l1[i][[1]][[1]])){
        shp0 <- rbind(shp0,l1[i][[1]][[1]])
    }
}
# save shp TOTAL
writeOGR(obj=shp0, dsn=shp.out,
         layer="TOTAL",
         driver="ESRI Shapefile",
         overwrite_layer = T)

#save results table
file_name_results <- paste(shp.out,"\\","resultados.csv",sep="")
write.csv2(data.frame(shp0), file= file_name_results, row.names=F, na="")

#save status table
#merge all valid shp in the list
for (i in 1:length(l1)) {
    tabla <- rbind(tabla,l1[i][[1]][[2]])
}
colnames(tabla) <- c("filepath", "estado")
file_name_proceso <- paste(shp.out, "\\", "archivos_procesados.csv", sep="")
write.csv2(tabla, file = file_name_proceso, row.names=F, na="")


#-----------------------------------------------------------------------
#benchmark -- NO FUNCIONA PORQUE AUN NO ESTA PARLAPPLY LISTO
benchmark("lapply" = {l1 <-  lapply(files,
                                    GPXTool,
                                    PPEEbuffer = PPEE25m)},
          "parLapply" = {
              #proceso de parallel para windows NO FUNCIONA TODAVIA
              no_cores <- detectCores() - 1 # Calculate the number of cores
              cl <- makeCluster(no_cores, type="SOCK") # Initiate cluster
              
              #export objects and functions
              clusterExport(cl=cl, objects())
              clusterExport(cl=cl, as.list(ls()),
                            envir=environment())
              l2 <- parLapply(cl,
                              files,
                              GPXTool,
                              PPEEbuffer = PPEE25m)
              stopCluster(cl)
          },
          replications = 1,
          columns = c("test", "replications", "elapsed")
)
