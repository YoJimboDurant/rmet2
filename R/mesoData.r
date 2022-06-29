#' @title mesoData
#' 
#' @description
#' 
#' This function will query the mesowest API and extract location and elevation (in meters) 
#' of surface station using the  International Civil Aviation Organization (ICAO) 
#' code. If a rmet class object is passed as well, it will append the information to the rmet list and return it.
#' 
#' \code{mesoData} 
#' 
#' @param station a 4 letter ICAO character string.
#' For example, "KATL" for Altanta.
#' @param rmetObj is an rmet class list created by createMetProject (optional). 

#' @export


mesoData<- function(station = NULL, rmetObj = NULL, API_KEY, type=c("surf")){
  stopifnot(type %in% c("surf"))
  stopifnot(is(rmetObj) == "rmet" | is.null(rmetObj))
 if(!is.null(rmetObj)) stopifnot(is(rmetObj == "rmet"))
  
#   library(RCurl)
#   library(jsonlite)

  if("surf" %in% type)
    if(!is.null(rmetObj)) station <- rmetObj$surf_Call
    #https://api.synopticdata.com/v2/stations/metadata?stids=KLCH&&complete=1&sensorvars=1&token=57f54928e49d4687858b06695f4068c6
    x <- paste0("https://api.synopticdata.com/v2/stations/metadata?", "&stids=", station, "&complete=1&sensorvars=1", "&token=",API_KEY)
    x <- jsonlite::fromJSON(x)       
    if(x$SUMMARY$RESPONSE_MESSAGE !="OK") stop("Error retrieving data from Meso West")
    rmetObj$surf_Latitude  <- as.numeric(x$STATION$LATITUDE[[1]])
    rmetObj$surf_Longitude <- as.numeric(x$STATION$LONGITUDE[[1]])
    rmetObj$surf_Elevation <- as.numeric(x$STATION$ELEVATION)[[1]] * 0.3048


  return(rmetObj)
  
}

