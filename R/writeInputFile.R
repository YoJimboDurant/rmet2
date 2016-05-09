#' @title writeInputFile
#' 
#' @description
#' \code{writeInputFile} 
#' 
#' @export


writeInputFile.rmet <- function(rmetObj, type="aerminute"){
stopifnot(is.rmet(rmetObj))
stopifnot(class(type) == "character")

loc_years <- locYears(rmetObj)

if("aerminute" %in% type){
  inpFiles <- lapply(seq_along(loc_years), function(i){
    inpFile <- paste0(rmetObj$project_Dir,"/", loc_years[[i]], "/AM_", loc_years[[i]],".inp")
    write(rmetObj$inputText$aerminute[[i]], file= inpFile)
    path.expand(inpFile)
  })
} 

rmetObj$inputFiles$aerminute <- prepareThePath(paste(inpFiles))
return(rmetObj)
}