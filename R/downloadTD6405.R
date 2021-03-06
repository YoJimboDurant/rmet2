#' @title downloadTD6405
#' 
#' @description
#' \code{downloadTD6405} Boo!
#' 
#' The function is designed to perform basic checks to make sure that 
#' the data are valid (e.g. WBAN and USAF id's match \url{http://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.txt})
#' 
#' @param rmetObj is valid rmetObject created by createMetProject
#' @export


downloadTD6405 <- function (rmetObj, check=TRUE, ...) {
  loc_years <- names(rmetObj$td6405_noaa)
  if(check){
    
    print("Checking if files have been downloaded")
    print(rmetObj$td6405_noaa)
    
    locExist <- lapply(seq_along(loc_years), function(i){
      locFiles <- gsub (
        paste0(getOption("rmet.noaa.site"), "asos-onemin/6405-"),
                        "", rmetObj$td6405_noaa[[i]])
      locFiles <- paste0(rmetObj$project_Dir, "/",
                         locFiles)
      print(locFiles)
      
      locExist <- file.exists(locFiles)
      
      locExist <- !locExist
    })
    
    loc_years <- loc_years[unlist(lapply(locExist, all))]
   
    lapply(seq_along(loc_years), function(i) {
      destDir <- paste(rmetObj$project_Dir, loc_years[[i]], sep="/")
      
      lapply(seq_along(rmetObj$td6405[[i]]), function(j){
        if(locExist[[i]][[j]]){
          destFile <- paste(destDir, gsub("^.*/", "", 
                                          rmetObj$td6405[[i]][[j]]), sep="/")
          download.file(rmetObj$td6405[[i]][[j]], destFile)
        }
      })
      
    })
    
  }else{
 
  
  loc_years <- names(rmetObj$td6405_noaa)
  
    lapply(seq_along(loc_years), function(i) {
      destDir <- paste(rmetObj$project_Dir, loc_years[[i]], sep="/")
      
        lapply(seq_along(rmetObj$td6405[[i]]), function(j){
          destFile <- paste(destDir, gsub("^.*/", "", 
                                          rmetObj$td6405[[i]][[j]]), sep="/")
          download.file(rmetObj$td6405[[i]][[j]], destFile)
          
        })
        
    })
  }
  
        
  return(NULL)
}

