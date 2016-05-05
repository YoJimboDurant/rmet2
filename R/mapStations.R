mapStations <- function(csvFile){
  #require(plotKML)
  #require(sp)
  dataSites <- read.csv(csvFile)
  dataSites$size <- 4
  sp::coordinates(dataSites) <- c("surf_Longitude", "surf_Latitude")
  sp::proj4string(dataSites) <- sp::CRS("+init=epsg:4326")
  tempFile <- paste0(tempfile(),".kml")
  plotKML::plotKML(dataSites, balloon=TRUE, 
                   size = as.name("size"),
                   colour = as.name("state"),
                   points_names = paste(dataSites$surf_USAF, 
                                                   dataSites$surf_WBAN, 
                                                   dataSites$surf_CALL),
                   file.name=tempFile, open.kml = FALSE)


  system(paste("open ", tempFile))
  
  
} 


