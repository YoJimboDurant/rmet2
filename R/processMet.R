#' @title processMet
#' 
#' @description
#' \code{processMet} Boo!
#' 
#' 
#' @export

processMet <- function(rmetObj, processor = c("aerminute", "aersurface", "aermet")){
  
  stopifnot(processor %in% c("aerminute", "aersurface", "aermet"))
  
  if("aerminute" %in% processor) {
    sapply(seq_along(rmetObj$inputFiles$aerminute), function(i) {
      stopifnot(!is.null(rmetObj$inputFiles$aerminute[[i]]))
      system(getOption("aerminute"), input=rmetObj$inputFiles$aerminute[[i]])
      moveFiles <- c("aerminute.log",
                   "bad_records.dat",
                   "good_records.dat",
                   "check_records.dat",
                   "bad_records_5.dat",
                   "good_records_5.dat",
                   "calm_variable_records.dat"
      )
    tmp <- sapply(moveFiles, function(x) file.rename(x, paste(rmetObj$project_Dir,
                                                              locYears(rmetObj)[[i]], x, 
                                                              sep="/")))
    return(NULL)
  }
  )
  }
  
  if("aersurface" %in% processor){
    
    system(getOption("aersurface"), 
           input=readLines(rmetObj$inputFiles$aersurface[grepl("aersurface.inp", 
                                                            rmetObj$inputFiles$aersurface)]))
  }
  
  
  
}