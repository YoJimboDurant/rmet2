#' @title downloadTD6405
#' 
#' @description
#' \code{downloadTD6405} Boo!
#' 
#' The function is designed to perform basic checks to make sure that 
#' the data are valid (e.g. WBAN and USAF id's match \url{http://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.txt})
#' 
#' @export


downloadTD6405 <- function (rmetObj, ...) {
  
  print("Checking if files have been downloaded")
  print(rmetObj$td6405_noaa)
  
  loc_years <- names(rmetObj$td6405_noaa)
  
    lapply(seq_along(loc_years), function(i) {
      destDir <- paste(rmetObj$project_Dir, loc_years[[i]], sep="/")
      
        lapply(seq_along(rmetObj$td6405[[i]]), function(j){
          destFile <- paste(destDir, gsub("^.*/", "", 
                                          rmetObj$td6405[[i]][[j]]), sep="/")
          download.file(rmetObj$td6405[[i]][[j]], destFile)
          
        })
        
    })
          
        
  return(NULL)
}

