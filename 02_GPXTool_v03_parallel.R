#read functions depending on OS
ifelse(Sys.info()[[1]] == "Windows",
       source("C:/GitHub/AdantiaTools/00_functions.R"),
       source("/data/home/cesarkero/Dropbox/Azure/00_functions.R"))

#PARAMETROS
g.folder <- choose.dir(default = "C:\\GitHub\\AdantiaTools\\02_GPXTool\\gpx",
           caption = "Select folder with gpx files:")
output.folder <- choose.dir(default = "C:\\GitHub\\AdantiaTools\\02_GPXTool\\output",
                            caption = "Select output folder:")
PPEE.folder <- choose.dir(default = "C:\\GitHub\\AdantiaTools\\000_GEO\\PPEE",
                         caption = "Select folder with PPEE.shp (points):")

AtributosPPEE <- c("Cod_aero","Aero","Cod_parque","minTime","maxTime","tiempo_s",
                   "len","tecnico","filepath")

files <- list.files(g.folder, pattern="*.gpx", full.names=T, recursive = T) #creates a list of files
PPEE <- readOGR(PPEE.folder, 'PPEE')
PPEE25m <- gBuffer(PPEE, byid= T, width = 25, quadsegs=10) # create buffer 25m by id

#-------------------------------------------------------------------------------
# PBLAPPLY
l1 <- pblapply(files, GPXTool, PPEEbuffer = PPEE25m, AtributosPPEE = AtributosPPEE) #PROCESS FILES WITH PBLAPPLY

#-------------------------------------------------------------------------------
# PARALLEL
no_cores <- detectCores() - 1 # Calculate the number of cores
cl <- makeCluster(no_cores, type="SOCK") # Initiate cluster

#export objects and functions
clusterExport(cl=cl, objects())
clusterExport(cl=cl, as.list(ls()),
              envir=environment())
l1 <- parLapply(cl,
                files,
                GPXTool,
                PPEEbuffer = PPEE25m,
                AtributosPPEE = AtributosPPEE)
stopCluster(cl)


#-------------------------------------------------------------------------------
# OUTPUT
# MERGE files
l1.shp <- pblapply(l1, '[[', 1) #lista de elementos en posicion 1 de cada sublista
l1.shp <- l1.shp[!is.na(l1.shp)] #remove NA shp
shp0 <- do.call(rbind, l1.shp) #rbind of all shp in the list
l1.tabla <- pblapply(l1, '[[', 2) #merge tables
tabla0 <- do.call(rbind, l1.tabla) #merge all tables

# EXPORT SHP
writeOGR(obj=shp0, dsn=output.folder, layer="TOTAL",driver="ESRI Shapefile",
         overwrite_layer = T)
write.csv2(data.frame(shp0),
           file= paste(output.folder,"\\","resultados.csv",sep=""),
           row.names=F, na="")
# EXPORT TABLE
write.csv2(tabla0,
           file = paste(output.folder, "\\", "archivos_procesados.csv", sep=""),
           row.names=F, na="")

#-----------------------------------------------------------------------
# #benchmark de GPXTool
# benchmark("lapply" = {l1 <- pblapply(files, GPXTool, PPEEbuffer = PPEE25m, AtributosPPEE = AtributosPPEE)},
#           "parLapply" = {
#               no_cores <- detectCores() - 1 # Calculate the number of cores
#               cl <- makeCluster(no_cores, type="SOCK") # Initiate cluster
#               
#               #export objects and functions
#               clusterExport(cl=cl, objects())
#               clusterExport(cl=cl, as.list(ls()),
#                             envir=environment())
#               l1 <- parLapply(cl,
#                               files,
#                               GPXTool,
#                               PPEEbuffer = PPEE25m,
#                               AtributosPPEE = AtributosPPEE)
#               stopCluster(cl)
#           },
#           replications = 5,
#           columns = c("test", "replications", "elapsed")
# )