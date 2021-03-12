#' @title downloadFSL
#' 
#' @description
#' \code{downloadFSL} querries raob database at
#' \url{http://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi}. 
#' It then places the resulting text files in subdirectories by year as .FSL files. 
#' 
#' @export


downloadFSL <- function(rmetObj, ...){
  
  loc_years <- names(rmetObj$td3505_noaa)
  # check is directories exist and if not, create them
  
  
  localFiles <- paste(rmetObj$project_Dir, "/", loc_years,"/", rmetObj$ua_WMO,".FSL", sep="")
  WMO <- stringr::str_pad(rmetObj$ua_WMO,5, pad="0")
  WBAN <- rmetObj$ua_WBAN
  
      
      if(!is.null(WMO)){
        for (i in 1:length(loc_years)){
          
          d1 <- as.numeric(paste(loc_years[i], "010000", sep=""))
          d2 <- as.numeric(
            paste0(
              format(rmetObj$start_Date - 24 * 60 * 60, "%Y%m%d", tz="UTC"),
              "00")
          )
          
          bdate <- ifelse(d1<d2, d2, d1)
          
          e1 <- as.numeric(paste(as.numeric(loc_years[i])+1, "010100", sep=""))
          e2 <- as.numeric(
            paste0(
              format(rmetObj$end_Date + 24 * 60 * 60, "%Y%m%d", tz="UTC"),
              "00")
          )
          
          e2 <- e2 + 0000000100
          
          
          edate <- ifelse(e1<e2, e1, e2)
          
          
          
          xlink <- paste0("https://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi?",
                          "shour=All+Times&ltype=All%2B+Levels&wunits=Knots&bdate=",
                          bdate,"&edate=", edate,
                          "&access=WMO+Station+Identifier&view=NO&StationIDs=",WMO,
                          "&osort=Station+Series+Sort&oformat=FSL+format+%28ASCII+text%29")
          
          qForm <- httr::GET(xlink)
          xdata <- httr::content(qForm, encoding="UTF-8", as = "text")
          xdata <- gsub("\\n$", "", xdata)
          writeLines(xdata, localFiles[i])
        }
      
    
  }else{
    
    if(is.null(WMO)){
      for (i in 1:length(loc_years)){
        
        d1 <- as.numeric(paste(loc_years[i], "010000", sep=""))
        d2 <- as.numeric(
          paste0(
            format(rmetObj$start_Date - 24 * 60 * 60, "%Y%m%d", tz="UTC"),
            "00")
        )
        
        bdate <- ifelse(d1<d2, d2, d1)
        
        e1 <- as.numeric(paste(as.numeric(loc_years[i])+1, "010100", sep=""))
        e2 <- as.numeric(
          paste0(
            format(rmetObj$end_Date + 24 * 60 * 60, "%Y%m%d", tz="UTC"),
            "00")
        )
        
        e2 <- e2 + 0000000100
        
        
        edate <- ifelse(e1<e2, e1, e2)
        
        
        
        xlink <- paste0("https://www.esrl.noaa.gov/raobs/intl/GetRaobs.cgi?",
                        "shour=All+Times&ltype=All%2B+Levels&wunits=Knots&bdate=",
                        bdate,"&edate=", edate,
                        "&access=WBAN+Station+Identifier&view=NO&StationIDs=",WBAN,
                        "&osort=Station+Series+Sort&oformat=FSL+format+%28ASCII+text%29")
        
        qForm <- httr::GET(xlink)
        
        xdata <- httr::content(qForm, encoding="UTF-8")
        xdata <- gsub("\\n$", "", xdata)
        writeLines(xdata, localFiles[i])
      }
    }
  } 
  
  print("All files downloaded to:")
  print(localFiles)
  return(NULL)
}
