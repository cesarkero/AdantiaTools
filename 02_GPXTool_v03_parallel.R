source("C:/GitHub/AdantiaTools/00_functions.R")

#PARAMETROS
#FILES
g.folder <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\gpx" #folder with gpx files
shp.out <- "C:\\GitHub\\AdantiaTools\\02_GPXTool\\output" # shp output folder
AtributosPPEE <- c("Cod_aero","Aero","Cod_parque","minTime","maxTime","tiempo_s","len","tecnico","filepath")
shp0 <- EmptySLDF(AtributosPPEE) #creates empty SLDF from names

#TOTALES TABLA
tabla <- data.frame(matrix(ncol=2, nrow=0)); colnames(tabla) <- c("filepath","estado")

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

#PARALLEL PROCESS
prueba <- GPXTool(files[3], PPEE25m)
prueba
lapply(files,GPXTool(files, PPEE25m))

no_cores <- detectCores() - 1 # Calculate the number of cores
cl <- makeCluster(no_cores, type="SOCK") # Initiate cluster

#funcion, a partir de un numero crea una lista de 3 
a <- function(x) c(x, x^2, x^3)

benchmark("lapply" = {lapply(1:10000000,a)},
          "parLapply" = {parLapply(cl, 1:10000000, a)},
          replications = 5,
          columns = c("test", "replications", "elapsed")
)

stopCluster(cl)


prueba <- GPXTool(files[11], PPEE25m)
prueba[[1]]   
prueba[[2]]
data.frame(prueba[[1]])

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
