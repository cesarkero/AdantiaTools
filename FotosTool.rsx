##Fotos=group
##Directorio=folder

#Load libraries
library(dplyr); library(devtools); library(stringr); library(xlsx); library(rJava); library(exifr)

p.folder <- Directorio

# Read just .jpg files with exifr
files <- list.files(p.folder, pattern="*.jpg", full.names=T)

# Select fields of interest
p.files <- read_exif(files, tags=c("FileName", "CreateDate", "GPSPosition"))

# CREATE NEW VARIABLES: newfilename
# Creates a chain from field 'CreateDate'
# The aim is to change "AAAA:MM:DD HH:mm:ss" to "AAAAMMDD_HHmmss_001.jpg"
a <- str_replace(str_replace_all(p.files$CreateDate,":","")," ","_")

# Create a counter to add to photo file --> 001 ...999 type. Length of the number of files.
counter <- c(1:nrow(p.files))
counter <- str_replace_all(str_pad(sapply(counter,toString),3,"left")," ","0")
b <- paste0(a,"_",counter,".jpg")

# add a new collum with the previous newfilename to the table photos_sel
p.files <- mutate(p.files,newfilename = b)

# Rename files with the newfilename (in the photo folder and not into the wd)
file.rename(files,paste(p.folder,"\\",p.files$newfilename,sep=""))

# CREATE A TABLE WITH THE COLUMNS OF INTEREST
# Add collums from exif data:
p.files <- 
    p.files %>%
    mutate(X_coord = str_sub(p.files$GPSPosition,-17))%>%
    mutate(Y_coord = str_sub(p.files$GPSPosition, 16,start = 1))%>%
    mutate(Ano = str_sub(p.files$CreateDate,1,4))%>%
    mutate(Mes = str_sub(p.files$CreateDate,6,7))%>%
    mutate(Dia = str_sub(p.files$CreateDate,9,10))%>%
    mutate(Hora = str_sub(p.files$CreateDate,12,13))%>%
    mutate(Min = str_sub(p.files$CreateDate,15,16))%>%
    mutate(Seg = str_sub(p.files$CreateDate,18,19))

# Export table to csv and save it into the photo folder
file_name <- paste(p.folder,"\\","fotos_vinculada.csv",sep="")
write.csv2(p.files, file = file_name,row.names=FALSE, na="")
