writeAerminute <- function(rmetObj){
  stopifnot(class(rmetObj) =="rmet")
  print("Writing AERMINUTE inpute files:\n")
  
  loc_years <- names(rmetObj$td6405_noaa)
  
  lapply(seq_along(loc_years), function(i){
    oneMinFiles <- paste(destDir, gsub("^.*/", "", 
                                       rmetObj$td6405[[i]], sep="/"))
    
    
  })

}