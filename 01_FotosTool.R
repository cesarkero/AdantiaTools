source("C:/GitHub/AdantiaTools/funciones.R")

#set parameters
PhotoFolder <- "C:\\GitHub\\AdantiaTools\\01_FotosTool\\Sources" #photo folder
ShpToJoin <- "Z:\\Proxectos\\448_Seguementos_PPEE_zonas_4b_5\\5_GIS\\SHP_PPEE\\PPEE.shp" #shp to join data
exiftags <- c("FileName", "CreateDate", "GPSPosition")
Tec1 <- "dri"
Tec2 <- "mal"

FotosTool <- function (PhotoFolder, ShpToJoin, exiftags, Tec1, Tec2){
    files <- list.files(PhotoFolder, pattern="*.jpg|.JPG", full.names=T) # Read just .jpg files with exifr
    p.files <- read_exif(files, tags=exiftags) # Select fields of interest
    p.files <- data.frame(p.files)

    # newfile name from "AAAA:MM:DD HH:mm:ss" to "AAAAMMDD_HHmmss_001.jpg"
    a <- str_replace(str_replace_all(p.files$CreateDate,":","")," ","_")
    counter <- CreateCounter(nrow(p.files),3) #create counter
    filename <- paste0(a,"_",Tec1,"_",Tec2,"_",counter,".jpg") # create filename
    p.files <- mutate(p.files, newfilename = filename) #add new column with filename
    file.rename(files,paste(PhotoFolder,"\\",p.files$newfilename,sep="")) # Rename files with the newfilename

    # separar x e y en columnas y unificar los decimales 5
    p.coords <- PhotoCoodsToDF(p.files)

    # Add collums from exif data:
    p.files <-
        p.files %>%
        mutate(lon = as.numeric(p.coords$lon))%>%
        mutate(lat = as.numeric(p.coords$lat))%>%
        mutate(Ano = str_sub(p.files$CreateDate,1,4))%>%
        mutate(Mes = str_sub(p.files$CreateDate,6,7))%>%
        mutate(Dia = str_sub(p.files$CreateDate,9,10))%>%
        mutate(Hora = str_sub(p.files$CreateDate,12,13))%>%
        mutate(Min = str_sub(p.files$CreateDate,15,16))%>%
        mutate(Seg = str_sub(p.files$CreateDate,18,19))

    # addd a new collum with the previous newfilepath to the table photos_sel
    p.files <- mutate(p.files, newfilepath = paste0(PhotoFolder,"\\",newfilename))

    #modify colnames and save file
    file_name <- paste(PhotoFolder,"\\","fotos_vinculada.csv",sep="")
    colnames(p.files) <- c("SourceFile","FileName","CreateDate",
                           "GPSPosition","newfilename","lon",
                           "lat","Ano","Mes","Dia","Hora","Min",
                           "Seg","filepath")

    fotosSHP <- DFToPoints01(p.files[!is.na(p.files$lon) | !is.na(p.files$lat),] ) #shp filterer by noNA lon lat

    #save shp
    dir.create(file.path(PhotoFolder, "shp"), showWarnings = F)
    writeOGR(obj = fotosSHP, dsn = file.path(PhotoFolder, "shp"),
             layer="fotos",
             driver="ESRI Shapefile",
             overwrite_layer = T)

    #save csv
    write.csv2(p.files, file = file_name,row.names=FALSE, na="")
}

