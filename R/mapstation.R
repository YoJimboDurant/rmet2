#' @title Map station around a point of interest
#' 
#' @description
#' \code{mapstation} Creates a basic map with the stations around a site of interest.
#' The map shows the site of interest in red and the stations in blue popups. Popups
#' can be clicked for additional information about each particular station. Station
#' information is obtained from \url{http://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.txt})
#' 
#' @param site a character string that can be geocoded by the Google Maps API.
#' For example, "30341", "4770 Buford Hwy. Atlanta GA 30341", and "CDC".
#' @param farthest a numeric value in meters that determine the farthest station
#' to be mapped from the \code{site} If no value is specified a defalt value
#' of 25 kilometers is used.
#' @param engine a character string to specify the mapping application to be used:
#' \code{"leaflet"} or \code{"kml"}.

#' @export
mapstation <- function(site,farthest=25,engine="leaflet", labels = "popup") {
  if("surfhist" %in% ls (envir = rmetData)){
    stations <- get("surfhist", envir = rmetData)
  }
  
  # Remove stations without longitude/latitude
  stations <- stations[!is.na(stations$LAT) & !is.na(stations$LON),]
  
  # Remove stations with erroneous longitude/latitude
  stations <- stations[stations$LON>-360,]
  
  # Get coord of point of interest
  coord <- ggmap::geocode(site)
  
  #Calculate distance between site and stations
  stations$dist <- as.vector(geosphere::distm(as.matrix(coord),
                                              as.matrix(stations[, c("LON", "LAT")])))
  
  # Filter stations to only those where the distance is less than "farthest"
  stations <- stations[stations$dist<farthest*1000,]
  
  # Create spatial dataframe for mapping
  pts <- cbind(stations$LON,stations$LAT)
  sdf <- sp::SpatialPointsDataFrame(pts,stations)
  
  # Create labels for mapping
  dist <- paste(format(sdf$dist/1000,digits = 3),"km from your site")
  if(labels == "popup") {sdf$label <- paste(sdf$STATION_NAME,
                     dist,
                     paste("USAF =",sdf$USAF,"|","WBAN =",sdf$WBAN),
                     sep="<br>")
  }else{sdf$label <- paste(sdf$STATION_NAME,
                           dist,
                           paste("USAF =",sdf$USAF,"|","WBAN =",sdf$WBAN),
                           sep="\n")
  }
  
  # Plot stations in Leaflet
  if(engine=="leaflet" & labels == "text"){
    map <- leaflet::leaflet(sdf) %>%
      addTiles() %>%
      addMarkers(label=~label,
                          labelOptions = labelOptions(
                            noHide = 'T')) %>%
      addCircleMarkers(coord$lon,coord$lat,fillColor = "red",color="red")
  }
  
  if(engine=="leaflet" & labels == "popup"){
    map <- leaflet::leaflet(sdf) %>%
      addTiles() %>%
      addMarkers(label=~label, labelOptions = labelOptions(clickable = TRUE)) %>%
      addCircleMarkers(coord$lon,coord$lat,fillColor = "red",color="red")
  }
  
  
  # PLot stations in KML (Google Earth)
  if(engine=="kml") {
    sdf$size <- 5
    sdf$colour <- "red"
    proj4string(sdf) = CRS("+init=epsg:4326")
    map <- plotKML::plotKML(sdf,
                            balloon=TRUE,
                            size=as.name("size"),
                            colour=as.name("colour"))
  }
  
  map
  
}