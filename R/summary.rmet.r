#' @title summary.rmet
#' 
#' @description
#' \code{summary.rmet} 
#' 
#' @export


summary.rmet <- function(rmetObj){
  
  stopifnot(is(rmetObj)=="rmet")
  rmetSummary <- list()
  station_ID <- paste0(rmetObj$surf_USAF, rmetObj$surf_WBAN)
  loc_years <- locYears(rmetObj)
  yearDir <- paste(rmetObj$project_Dir, loc_years, sep="/")
  td3505_downloaded <- paste0(yearDir,"/","S",station_ID,
                              "_",loc_years, ".ISH")
  rmetSummary$td3505_downladed <- data.frame(td305_file = td3505_downloaded, downloaded = file.exists(td3505_downloaded))
  
  
  fsl_downloaded <- paste0(yearDir, "/", rmetObj$ua_WMO, ".FSL")
  
  rmetSummary$fsl_downloaded <- data.frame(fsl_file = fsl_downloaded, 
                                           downloaded = file.exists(fsl_downloaded))
  class(rmetSummary) <- "rmetSum"
  return(rmetSummary)
}