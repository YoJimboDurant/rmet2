#' @title mesoData
#' 
#' @description
#' \code{mesoData} 
#' 
#' @export


mesoData<- function(rmetObj, API_KEY, type=c("surf","ua")){
  stopifnot(type %in% c("surf", "ua"))
  stopifnot(is(rmetObj) == "rmet")
  if(rmetObj$surf_Call==""| is.na(rmetObj$surf_Call) | is.null(rmetObj$surf_Call)) stop("empty or missing surf_Call")
  
#   library(RCurl)
#   library(jsonlite)

  if("surf" %in% type)
    station <- rmetObj$surf_Call
    x <- getURL(paste0("http://api.mesowest.net/v2/stations/metadata?&token=",myAPI, "&complete=1&sensorvars=1", "&stids=", station))
    x <- fromJSON(x)       
    if(x$SUMMARY$RESPONSE_MESSAGE !="OK") stop("Error retrieving data from Meso West")
    rmetObj$surf_Latitude  <- as.numeric(x$STATION$LATITUDE[[1]])
    rmetObj$surf_Longitude <- as.numeric(x$STATION$LONGITUDE[[1]])
    rmetObj$surf_Elevation <- as.numeric(x$STATION$ELEVATION)[[1]] * 0.3048

    #get station anemometer height
    noaaSite <- "http://www.nws.noaa.gov/ops2/Surface/documents/windtower.xls"
    
  return(rmetObj)
  
}

