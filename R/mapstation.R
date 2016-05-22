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
#' @param farthest a numeric value in meters that determine the farthest station to be mapped from the \code{site} If no value is specified a defalt value of 25,000 meters is used.

#' @export
mapstation <- function(site,farthest=25000) {
  
  if("surfhist" %in% ls (envir = rmetData)){
    stations <- get("surfhist", envir = rmetData)
  }
  #stations <- readLines("http://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.txt")
  # stations <- read.csv("input.csv")

  # Remove stations without longitude/latitude
  stations <- stations[!is.na(stations$LAT),]
  stations <- na.omit(stations)
  
  # Remove stations with erroneous longitude/latitude
  stations <- stations[stations$LON>-360,]
  
  #summary(as.matrix(stations[,c("LON","LAT")]))
  
  # Get coord of point of interest
  coord <- ggmap::geocode(site)
  
  
  #as.matrix(stations[,c("LON","LAT")])
  
  #Calculate distance between site and stations
  aaa <- geosphere::distm(as.matrix(coord),as.matrix(stations[,c("LON","LAT")]))
  stations$dist <- as.vector(aaa)

  # Filter stations to only those where the distance is less than "farthest"
  stations <- stations[stations$dist<farthest,]
  
  pts <- cbind(stations$LON,stations$LAT)
  bbb <- sp::SpatialPointsDataFrame(pts,stations)

  map <- leaflet::leaflet(bbb) %>% addTiles() %>% 
    addMarkers(popup=~htmltools::htmlEscape(paste0(STATION_NAME,"\n USAF:",USAF,"\n WBAN:",WBAN))) %>% 
    addCircleMarkers(coord$lon,coord$lat,fillColor = "red",color="red")
  map
}