# Check TD6405 database and select files for download
#
# Author: JTD
###############################################################################
checkTD6405 <- function(start_Date, end_Date,  surf_Call){
  noaaSite <- getOption("rmet.noaa.site")
  noaaSite <- paste0(noaaSite, "asos-onemin/")
  startYear <- as.numeric(format(start_Date, "%Y", tz="UTC"))
  endYear <- as.numeric(format(end_Date, "%Y", tz="UTC"))
  stopifnot(endYear>startYear)
  
  months <- format(seq.Date(as.Date(start_Date), as.Date(end_Date), "month"),"%m")
  years <- format(seq.Date(as.Date(start_Date), as.Date(end_Date), "month"),"%Y")  
  fileName <- sapply(seq_along(years), function(i){
    paste0(
      noaaSite, "6405-", years[i], "/", "64050", surf_Call,  years[i], months[i],
      ".dat")
  })
  
  checkTD6405 <- sapply(fileName, RCurl::url.exists)
  if(any(!checkTD6405)){
    warning(paste("Missing file on NOAA site:\n", fileName[!checkTD6405], "\n"))
  }
  
  fileName <- fileName[checkTD6405]
  
  fileNames <- plyr::llply(unique(years), function(x){
    grep(x, fileName, value=TRUE)
  })
  
  names(fileNames) <- as.character(unique(years))
  fileNames
}


#http://www1.ncdc.noaa.gov/pub/data/asos-onemin/6405-2014/64050KNYC201408.dat
