#' @title downloadTD6401
#' 
#' @description
#' \code{downloadTD6401} 
#' 
#' The function is designed to perform basic checks to make sure that 
#' the data are valid (e.g. WBAN and USAF id's match \url{http://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.txt})
#' 
#' @export


downloadTD6401 <- function (rmetObj, check=TRUE, ...) {
  loc_years <- names(rmetObj$td6401_noaa) 
  if(check){
    
    print("Checking if files have been downloaded")
    print(rmetObj$td6401_noaa)
    
    locExist <- lapply(seq_along(loc_years), function(i){
      locFiles <- gsub (
        paste0(getOption("rmet.noaa.site"), "asos-fivemin/6401-"),
                        "", rmetObj$td6401_noaa[[i]])
      locFiles <- paste0(rmetObj$project_Dir, "/",
                         locFiles)
      print(locFiles)
      
      locExist <- file.exists(locFiles)
      locExist <- !locExist
    })
    
    loc_years <- loc_years[unlist(lapply(locExist, all))]
    
 
  
    lapply(seq_along(loc_years), function(i) {
      destDir <- paste(rmetObj$project_Dir, loc_years[[i]], sep="/")
      
        lapply(seq_along(rmetObj$td6401_noaa[[i]]), function(j){
          if(locExist[[i]][[j]]){
          destFile <- paste(destDir, gsub("^.*/", "", 
                                          rmetObj$td6401_noaa[[i]][[j]]), sep="/")
          download.file(rmetObj$td6401[[i]][[j]], destFile)
          }
        })
        
    })
  }else{
    loc_years <- names(rmetObj$td6401_noaa)
    
    lapply(seq_along(loc_years), function(i) {
      destDir <- paste(rmetObj$project_Dir, loc_years[[i]], sep="/")
      
      lapply(seq_along(rmetObj$td6401_noaa[[i]]), function(j){
        destFile <- paste(destDir, gsub("^.*/", "", 
                                        rmetObj$td6401_noaa[[i]][[j]]), sep="/")
        download.file(rmetObj$td6401[[i]][[j]], destFile)
        
      })
      
    })
    
    
  }
  
        
  return(NULL)
}

