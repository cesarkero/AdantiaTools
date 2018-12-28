#TrackToDF
#Reed trackspoints from a gpx file (class string - filepath) and returns a
#data.frame with lon, lat, ele and time
TrackToDF <- function (gpxpath){
    gpx <- readGPX(gpxpath,metadata=F,bounds=F, waypoints=F,tracks = T, routes= F) #read gpx file
    tp <- gpx['tracks'] #read trackpoints and create unique data.frame

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
            if (length(colnames(df)) != 4){
                df <- df[,1:4]
            } else {
                df <- df
            }
            colnames(df) <- c("lon","lat","ele", "time") #adjust number of columns
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

    #output
    return (t)
}
#___________________________________________________________________

#DFtoPoints
#get a data.frame with lon lat and creates an spatial object converting CRS
DFToPoints01 <- function(df,
                       epsg1 = CRS("+init=epsg:4326"),
                       epsg2 = CRS("+init=epsg:25829")){
    #create spatial objec from coords
    coordinates(df) <- c("lon","lat")
    proj4string(df) <- epsg1
    sdf <- spTransform(df,epsg2) #change CRS
    sdf$x <- t2@coords[,1] #add coords to table
    sdf$y <- t2@coords[,2] #add coords to table

    #output
    return (sdf)
}


