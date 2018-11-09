# FotosTool

#set parameters
p.folder <- "C:\\GitHub\\AdantiaTools\\01_FotosTool\\Sources"
PPEE <- "Z:\\Proxectos\\448_Seguementos_PPEE_zonas_4b_5\\5_GIS\\SHP_PPEE\\PPEE.shp"
tec1 <- "cac"
tec2 <- "lbn"
Tablanueva <- FALSE


# Read just .jpg files with exifr
files <- list.files(p.folder, pattern="*.jpg|.JPG", full.names=T)
# Select fields of interest
p.files <- read_exif(files, tags=c("FileName", "CreateDate", "GPSPosition"))

# CREATE NEW VARIABLES: newfilename
# Creates a chain from field 'CreateDate'
# The aim is to change "AAAA:MM:DD HH:mm:ss" to "AAAAMMDD_HHmmss_001.jpg"
a <- str_replace(str_replace_all(p.files$CreateDate,":","")," ","_")

# Create a counter to add to photo file --> 001 ...999 type. Length of the number of files.
counter <- c(1:nrow(p.files))
counter <- str_replace_all(str_pad(sapply(counter,toString),3,"left")," ","0")
b <- paste0(a,"_",tec1,"_",tec2,"_",counter,".jpg")

# add a new collum with the previous newfilename to the table photos_sel
p.files <- mutate(p.files,newfilename = b)

# Rename files with the newfilename (in the photo folder and not into the wd)
file.rename(files,paste(p.folder,"\\",p.files$newfilename,sep=""))

# CREATE A TABLE WITH THE COLUMNS OF INTEREST
#create list of X_coord (ojo seleccion distinta de caracteres en hemisferio norte por el - en coord x)
coords <- strsplit(p.files$GPSPosition, " ")
X_coord_list <- c()
Y_coord_list <- c()
for (i in coords){
        X_coord_list <-  c(X_coord_list, str_sub(i[2],9,start=1))
}
for (i in coords){
        Y_coord_list <-  c(Y_coord_list, str_sub(i[1],8,start=1))
}

# Add collums from exif data:
p.files <-
    p.files %>%
    mutate(X_coord = X_coord_list)%>%
    mutate(Y_coord = Y_coord_list)%>%
    mutate(Ano = str_sub(p.files$CreateDate,1,4))%>%
    mutate(Mes = str_sub(p.files$CreateDate,6,7))%>%
    mutate(Dia = str_sub(p.files$CreateDate,9,10))%>%
    mutate(Hora = str_sub(p.files$CreateDate,12,13))%>%
    mutate(Min = str_sub(p.files$CreateDate,15,16))%>%
    mutate(Seg = str_sub(p.files$CreateDate,18,19))
# addd a new collum with the previous newfilepath to the table photos_sel
p.files <- mutate(p.files, newfilepath = paste0(p.folder,"\\",newfilename))

#modify colnames and save file
file_name <- paste(p.folder,"\\","fotos_vinculada.csv",sep="")
colnames(p.files) <- c("SourceFile","FileName","CreateDate",
                       "GPSPosition","newfilename","X_coord",
                       "Y_coord","Ano","Mes","Dia","Hora","Min",
                       "Seg","filepath")
write.csv2(p.files, file = file_name,row.names=FALSE, na="")

# ACTUALIZACI?N --> CREAR COORDENADAS UTM, Y CREAR TABLA COMO LA VIEJA
# x <- list()
# for (i in strsplit(p.files$GPSPosition, " ")){
#   print (str_sub(i[1],8,star=1))
#
# }
# x
# summary(str_sub(p.files$CreateDate,1,4))
# p.files["X_coord"]

#________________________________________________________
#Convertir coordenadas en utm
# p.files["X_coord"]
# coordinates(xy) <- c("X_coord", "Y_coord")
#
#
#
# if (isTRUE(Tablanueva)) {
#   # Export table to csv and save it into the photo folder
#   file_name <- paste(p.folder,"\\","fotos_vinculada.csv",sep="")
#   write.csv2(p.files, file = file_name,row.names=FALSE, na="")
# } else{
#
# }


