#' @title mesoData
#' 
#' @description
#' \code{mesoData} 
#' 
#' @export


mesoData<- function(rmetObj, mesoSite = "http://mesowest.utah.edu/cgi-bin/droman/station_total_text.cgi?stn="){
  stopifnot(is(rmetObj) == "rmet")
  if(rmetObj$surf_Call==""| is.na(rmetObj$surf_Call) | is.null(rmetObj$surf_Call)) stop("empty or missing surf_Call")
  
  mesoSite <- paste0(mesoSite, rmetObj$surf_Call)
  readLines(mesoSite)
}