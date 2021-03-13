# Check TD3505 database and select files for download
#
# Author: JTD
###############################################################################
checkTD3505 <- function(start_Date, end_Date, surf_USAF, surf_WBAN){
  noaaSite <- getOption("rmet.noaa.site")
  startYear <- as.numeric(format(start_Date, "%Y", tz="UTC"))
  endYear <- as.numeric(format(end_Date, "%Y", tz="UTC"))
  stopifnot(endYear >= startYear)
  
  years <- seq(startYear, endYear, by=1) # we actually need this to be local since minute level is local...
  loc_years <- seq(as.numeric(format(start_Date, "%Y")), as.numeric(format(end_Date, "%Y")))

    fileName <- sapply(seq_along(years), function(i){
      paste0(
        noaaSite, "noaa/", years[i], "/", paste(surf_USAF, surf_WBAN, years[i], sep="-"), 
        ".gz")
    })

    checkTD3505 <- NULL
    for(i in 1:length(fileName))function(x){
      
      checkTD3505[x] <- httr::http_error(x)
    }
      
    if(any(checkTD3505)){
      stop(paste("Missing file on NOAA site:\n", fileName[!checkTD3505]))
    }
    
    
    #we need to detect if date is not UTC and adjust the files.
    surfaceDZ <-   
        as.numeric(lubridate::ymd_hm(format(start_Date, "%Y-%m-%d %H:%M" )) -
                 as.numeric(start_Date)) / 3600
    
                                        #if surfaceDZ <0 we are west of prime meridian time
  if(surfaceDZ < 0){
        fileNames <- plyr::llply(unique(loc_years), function(x){
          grep(paste0(x,"|",x+1), fileName, value=TRUE)
    })
      
 }       
                                        #if surfaceDZ >0 we are east of prime meridian time
  if(surfaceDZ > 0){

          fileNames <- plyr::llply(unique(loc_years), function(x){
              grep(paste0(x,"|",x-1), fileName, value=TRUE)
          })

      }
                                        #finally timezone is UTC

  if(surfaceDZ == 0){
                 fileNames <- plyr::llply(unique(loc_years), function(x){
          grep(paste0(x), fileName, value=TRUE)
      })
 
    }
    
    
    names(fileNames) <- as.character(loc_years)
    fileNames
}


# start_Date = lubridate::mdy_hm("01/01/2013 00:00", tz="UTC") 
# end_Date = lubridate::mdy_hm("03/31/2016 23:00", tz="UTC")
# surf_UTC = -5
# surf_WBAN = 94728
# surf_USAF = 725053
# 
# checkTD3505(start_Date, end_Date, surf_USAF, surf_WBAN)
