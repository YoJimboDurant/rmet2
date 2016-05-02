#' @title downloadFSL
#' 
#' @description
#' \code{downloadFSL} querries raob database at
#' \url{http://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi}. 
#' It then places the resulting text files in subdirectories by year as .FSL files. A comment.
#' 
#' @export


downloadFSL <- function(rmetObj){
  
  loc_years <- names(nyc$td3505_noaa)
  require(RCurl)
  require(stringr)
# Internet options for Windows

# stopif
#stopifnot(!is.null(WMO) & !is.null(WBAN))

if (R.version$os !="linux-gnu"){
  setInternet2(use = NA)
  setInternet2(use = FALSE)
  setInternet2(use = NA)
}

  # check is directories exist and if not, create them
  

localFiles <- paste(rmetObj$project_Dir, "/", loc_years,"/", rmetObj$ua_WMO,".FSL", sep="")
WMO <- rmetObj$ua_WMO
WBAN <- rmetObj$ua_WBAN
if(!all(sapply(localFiles, file.exists))){
# this is how to do it:
if(!is.null(WMO)){
  for (i in 1:length(loc_years)){
    
    d1 <- as.numeric(paste(loc_years[i], "010000", sep=""))
    d2 <- as.numeric(
      paste0(
        format(rmetObj$start_Date, "%Y%m%d", tz="UTC"),
        "00")
    )
    
    bdate <- ifelse(d1<d2, d2, d1)

    e1 <- as.numeric(paste(as.numeric(loc_years[i])+1, "010100", sep=""))
    e2 <- as.numeric(
      paste0(
        format(rmetObj$end_Date, "%Y%m%d", tz="UTC"),
        "00")
    )
    
    e2 <- e2 + 0000000100
    
    
    edate <- ifelse(e1<e2, e1, e2)
    
      qForm <- postForm("http://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi",
                    shour = "All Times",
                    ltype = "All Levels",
                    wunits ="Knots",
                    bdate = bdate,
                    edate = edate,
                    access ="WMO Station Identifier",
                    view="NO",
                    osort="Station Series Sort",
                    StationIDs = WMO,
                    oformat = "FSL format (ASCII text)",
                    SUBMIT = "Continue Data Access")
  
      tempFile <- paste("http://www.esrl.noaa.gov/raobs/", str_extract_all(qForm, "temp..*tmp"), sep="")
  
      download.file(tempFile, localFiles[i])
  }

# }else{
#   
#   if(!is.null(WBAN)){
#     for (i in 1:length(loc_years)){
#       qForm <- postForm("http://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi",
#                         shour = "All Times",
#                         ltype = "All Levels",
#                         wunits ="Knots",
#                         bdate = paste(loc_years[i], "010100", sep=""),
#                         edate = paste(loc_years[i]+1, "010100", sep=""),
#                         access ="WBAN Station Identifier",
#                         view="NO",
#                         osort="Station Series Sort",
#                         StationIDs = WBAN,
#                         oformat = "FSL format (ASCII text)",
#                         SUBMIT = "Continue Data Access")
#       
#       tempFile <- paste("http://www.esrl.noaa.gov/raobs/", str_extract_all(qForm, "temp..*tmp"), sep="")
#       
#       download.file(tempFile, paste(rmetObj$project_Dir, "/", loc_years[i],"/", WMO, startYear, stopYear,".FSL", sep=""))
#     }
# 
#   }
} 
}


  return(NULL)
}
