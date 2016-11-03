#' @title processMet
#' 
#' @description
#' \code{processMet} Boo!
#' 
#' 
#' @export

processMet <- function(rmetObj, processor = c("aerminute", "aersurface", "aermet1",
                                              "aermet2", "aermet3")){
  
  stopifnot(processor %in% c("aerminute", "aersurface", "aermet1", 
                             "aermet2", "aermet3"))
  
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
    
  }
  )
  }
  
  if("aersurface" %in% processor){
    
    system(getOption("aersurface"), 
           input=readLines(rmetObj$inputFiles$aersurface[grepl("aersurface.inp", 
                                                            rmetObj$inputFiles$aersurface)]))
    rmetObj$output$aersurface <- readLines(paste(rmetObj$project_Dir, "aersurface/aersurface.out", sep="/"))
  }
  
  if("aermet1" %in% processor){
  
  lapply(rmetObj$inputText$aermet$s1, function(x) {
    write(x, file="AERMET.INP")
    system(getOption("aermet"))
  })
  }
  
  
  if("aermet2" %in% processor){
    
  lapply(rmetObj$inputText$aermet$s2, function(x) {
    write(x, file="AERMET.INP")
    system(getOption("aermet"))
  })
  
  }
  
  if("aermet3" %in% processor){
    
  lapply(seq_along(rmetObj$inputText$aermet$s3), function(i) {
    write(c(rmetObj$inputText$aermet$s3[[i]], rmetObj$output$aersurface), file="AERMET.INP")
    system(getOption("aermet"))
  })
  
  }
  
  
  return(rmetObj)
}