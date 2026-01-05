#' makeFinal
#' 
#' @description
#' \code{makeFinal} reads the surface file and profile output of aermet
#'  across multiple years and creates a multi-year surface files. This function
#'  works by scanning the project directory year files for .sfc and .pfl file
#'  output and appending them together into single files and writing them at the
#'  rmetObj project directory. 
#'
#' @param rmetObj {An object of class \code{rmet}.}
#'  
#' @param outFilePrefix is the name of the profile and surface file. For
#' instance, \dQuote{KFTY} will result in \dQuote{KFTY.sfc} and \dQuote{KFTY.pfl}.   
#' @export
#' 
makeFinal.rmet <- function(rmetObj, outfilePrefix="outFile"){
  stopifnot(is(rmetObj) == "rmet")
  loc_years <- locYears(rmetObj)
  years <- paste(rmetObj$project_Dir, loc_years, sep="/")
  sdExist <- sapply(as.character(years), FUN=file.exists )
  if(!all(sdExist)) stop(paste("\nSubdirectory missing for:", years[!sdExist], sep=" "))
  
  
  # read surface file
  sFile <- paste(years, paste0("AM_",loc_years,".SFC"), sep="/")
  sFile1 <- readLines(sFile[1])
  surface_file_1 <- plyr::llply(sFile[2:length(sFile)], function(lx){
    sfcLines <- readLines(lx)
    sfcLines[2:length(sfcLines)]
  })
  
  surfOut <- paste0(outfilePrefix, ".sfc")
  surfOut <- paste(rmetObj$project_Dir, surfOut, sep="/")
  write(sFile1, file=surfOut)
  plyr::l_ply(surface_file_1, function(lx) write(lx, surfOut, append=TRUE))
  
  # read profile file
  pFile <- paste(years, paste0("AM_",loc_years,".PFL"), sep="/")
  pFile1 <- readLines(pFile[1])
  profile_file_1 <- plyr::llply(pFile[2:length(pFile)], function(lx){
    readLines(lx, skip=1)
  })
  
  pflOut <- paste0(outfilePrefix, ".pfl")
  pflOut <- paste(rmetObj$project_Dir, pflOut, sep="/")
  write(pFile1, file=pflOut)
  plyr::l_ply(profile_file_1, function(lx) write(lx, pflOut, append=TRUE))
  
}
