# Check TD6405 database and select files for download
#
# Author: JTD
###############################################################################
checkTD6405 <- function(start_Date, end_Date,  surf_Call){
  noaaSite <- getOption("rmet.noaa.1min")
  
  if(grepl("ncdc[.]noaa[.]gov/pub/data/", noaaSite)){
  noaaSite <- getOption("rmet.noaa.site1min")
  noaaSite <- paste0(noaaSite, "asos-onemin/")

  xtz <- lubridate::tz(start_Date)
  startYear <- as.numeric(format(start_Date, "%Y", tz=xtz))
  endYear <- as.numeric(format(end_Date, "%Y", tz=xtz))
  stopifnot(endYear>=startYear)
  
  
  
  locDates <- seq.Date(as.Date(start_Date, tz=xtz), as.Date(end_Date, tz=xtz), by = "month")
  months <- format(locDates, "%m",tz=xtz)
  years <- format(locDates, "%Y", tz=xtz)  
  fileName <- sapply(seq_along(years), function(i){
    paste0(
      noaaSite, "6405-", years[i], "/", "64050", surf_Call,  years[i], months[i],
      ".dat")
  })
  
  
  
  
} else{
  if(grepl("ncei[.]noaa[.]gov/data/automated-surface-observing-system-one-minute-pg1/access/", 
           noaaSite)){
    
    
    
    xtz <- lubridate::tz(start_Date)
    startYear <- as.numeric(format(start_Date, "%Y", tz=xtz))
    endYear <- as.numeric(format(end_Date, "%Y", tz=xtz))
    stopifnot(endYear>=startYear)
    
    locDates <- seq.Date(as.Date(start_Date, tz=xtz), as.Date(end_Date, tz=xtz), by = "month")
    years <- format(locDates, "%Y", tz=xtz) 
    
    
    
    td6405x <-
      "https://www.ncei.noaa.gov/data/automated-surface-observing-system-one-minute-pg1/access/YYYY/MM/asos-1min-pg1-XXXX-YYYYMM.dat"
    
    
    fileName <- sapply(locDates, function(x){
      y <- gsub("YYYY", lubridate::year(x), td6405x)
      y <- gsub("MM", stringr::str_pad(lubridate::month(x),2, pad = "0"), y)
      y <- gsub("XXXX", surf_Call, y)
      y
    })
    
    
  }
}
  
  fileName <- unique(fileName)
  checkTD6405 <- sapply(fileName, httr::http_error)
  if(any(!checkTD6405)){
    warning(paste("Missing file on NOAA site:\n", fileName[checkTD6405], "\n"))
  }
  
  
  fileName <- fileName[!checkTD6405]
  
  fileNames <- plyr::llply(unique(years), function(x){
    grep(x, fileName, value=TRUE)
  })
  
  names(fileNames) <- as.character(unique(years))
  fileNames
}


#http://www1.ncdc.noaa.gov/pub/data/asos-onemin/6405-2014/64050KNYC201408.dat
