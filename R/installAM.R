#' @title installAM
#' 
#' @description
#' \code{installAM} Function will download and unzip files from links at \url{http://www3.epa.gov/scram001/metobsdata_procaccprogs.htm}
#' to a C: drive location. Sets a pathvariable for location of executables. The function also will create "options" for the 
#' locations of aermet, aerminute, and aersurface executables. If these exist, subsequent functions will utilize them
#' in deference to locally unzipped programs.
#' @param rootDir us the directory where you want aermet, aerminute, and aersurface unpacked
#' @param aermetExists controls if you want to download the binaries again, or if you only want to set options
#' The first value controls downloading AERMET, the second AERMINUTE, and the third AERSURFACE.  
#' @examples 
#' installAM()
#' @export


installAM <- function(rootDir="C:/", aermetExists=rep(FALSE,3)){


  if(!grepl("/$", rootDir)) rootDir <- paste0(rootDir, "/")

  #aerminutedownload
  aerminuteLink <- "https://gaftp.epa.gov/Air/aqmg/SCRAM/models/met/aerminute/aerminute_15272.zip"
  aerminuteVersion <- stringr::str_extract(aerminuteLink, "[0-9]{5}")
  programTree <- list(
    aermetEx = paste0(rootDir,"aermet_exe"),
    aerminEx = paste0(rootDir,"aerminute_",aerminuteVersion),
    aerSurfaceEx= paste0(rootDir,"aersurface_exe")
  )
  
  
    
  if(!aermetExists[1]){
    print("downloading aermet")
    downloader::download("https://gaftp.epa.gov/Air/aqmg/SCRAM/models/met/aermet/aermet_exe.zip", "aermet_exe.zip", mode = "wb")
    unzip("aermet_exe.zip", exdir=programTree$aermetEx)
  }
  
  
  if(!aermetExists[2]){
    print("downloading aerminute")
    downloader::download(aerminuteLink, stringr::str_extract(aerminuteLink, "aerminute_.*[.]zip"), mode = "wb")
    unzip(stringr::str_extract(aerminuteLink, "aerminute_.*[.]zip"), exdir=programTree$aerminEx)
  }
  
  
  if(!aermetExists[3]){
    print("downloading aersurface")
    downloader::download("https://gaftp.epa.gov/Air/aqmg/SCRAM/models/related/aersurface/aersurface_exe-64.zip", "aersurface_exe.zip", mode = "wb")
    unzip("aersurface_exe.zip", exdir=programTree$aerSurfaceEx)
  }
  
  # check option status for location of aermet, aersurface and aerminute
  aermet <- getOption("aermet")
  aerminute <- getOption("aerminute")
  aersurface <- getOption("aersurface")  
  
  options("aermet" = list.files(path = programTree$aermetEx, pattern="aermet.*[.]exe", full.names = TRUE) )
  options("aerminute" = list.files(path = programTree$aerminEx, pattern="aerminute.*[.]exe", full.names = TRUE) )
  options("aersurface" = list.files(path = programTree$aerSurfaceEx, pattern="aersurface.*[.]exe", full.names = TRUE) )
  return(NULL)
}
