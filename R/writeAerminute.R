writeAerminute <- function(rmetObj){
  stopifnot(class(rmetObj) =="rmet")
  print("Writing AERMINUTE inpute files:\n")
  
  loc_years <- names(rmetObj$td6405_noaa)
  
  aerminInputFiles <- lapply(seq_along(loc_years), function(i){
    
    destDir <- paste(rmetObj$project_Dir, loc_years[[i]], sep="/")
    oneMinFiles <- paste(destDir, gsub("^.*/", "", 
                                       rmetObj$td6405_noaa[[i]]), sep="/")
    
    stopifnot(all(file.exists(oneMinFiles)))
    
    
    aerminInp <- paste(aerminTemplate, collapse="\n")
    aerminInp <- gsub("!minFiles!", paste(oneMinFiles, collapse = "\n"), aerminInp)
    
    if(!is.null(rmetObj$td6401_noaa[[i]])){
      fiveMinFiles <- paste(destDir, gsub("^.*/", "", 
                                       rmetObj$td6401_noaa[[i]]), sep="/")
    
      stopifnot(all(file.exists(fiveMinFiles)))
    
    
      aerminInp <- paste(aerminTemplate, collapse="\n")
      aerminInp <- gsub("!minFiles!", paste(fiveMinFiles, collapse = "\n"), 
                        aerminInp)
    }
    
    aerminInp
    
  })

}