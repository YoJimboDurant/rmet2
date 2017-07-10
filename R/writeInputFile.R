#' @title writeInputFile
#' 
#' @description
#' \code{writeInputFile} 
#' 
#' @export


writeInputFile.rmet <- function(rmetObj, type=c("aerminute", "aersurface"), dewinter = FALSE){
stopifnot(is.rmet(rmetObj))
stopifnot(class(type) == "character")
stopifnot(type %in% c("aerminute", "aersurface"))
loc_years <- locYears(rmetObj)

if("aerminute" %in% type){
  inpFiles <- lapply(seq_along(loc_years), function(i){
    inpFile <- paste0(rmetObj$project_Dir,"/", loc_years[[i]], "/AM_", loc_years[[i]],".inp")
    write(rmetObj$inputText$aerminute[[i]], file= inpFile)
    path.expand(inpFile)
  })
  rmetObj$inputFiles$aerminute <- prepareThePath(paste(inpFiles))
  
}

if("aersurface" %in% type){
    inpFile <- paste(rmetObj$project_Dir, "aersurface/aersurface.inp", sep="/")
    write(rmetObj$inputText$aersurface$surface, file= inpFile)
    inpFile <- path.expand(inpFile)
    rmetObj$inputFiles$aersurface$surface <- inpFile
    
    if(!is.null(rmetObj$onsite_Latitude) & !is.null(rmetObj$onsite_Latitude)){
    inpFileOS <- paste(rmetObj$project_Dir, "aersurface/aersurface_onsite.inp", sep="/")
    write(rmetObj$inputText$aersurface$onsite, file= inpFileOS)
    inpFile <- path.expand(inpFile)
    rmetObj$inputFiles$aersurface$onsite <- inpFileOS
    }else{
      rmetObj$inputFiles$aersurface$onsite <- NULL
    }
    
   
    if(dewinter){
      inpFile <- paste(rmetObj$project_Dir, "aersurface/aersurface_dewinter.inp", sep="/")
      newObj <- rmetObj
      newObj$aersurface$surfaceSeason$as_Winter_NS <- paste(rmetObj$aersurface$surfaceSeason$as_Winter_WS, rmetObj$aersurface$surfaceSeason$as_Winter_NS)
      newObj$aersurface$surfaceSeason$as_Winter_WS <- NULL
      newObj$aersurface$surfaceChar$as_Snow <- "N"
      newObj <- createInput(newObj, "aersurface")
      newObj$inputText$aersurface <- gsub("aersurface.out", "aersurface_dewinter.out", newObj$inputText$aersurface)
      write(newObj$inputText$aersurface, file= inpFile)
      inpFile <- path.expand(inpFile)
      rmetObj$inputFiles$aersurface$surface[2] <- inpFile
    }
  }


return(rmetObj)

}