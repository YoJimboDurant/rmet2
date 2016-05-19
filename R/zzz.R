.onLoad <- function(libname, pkgname) {
  op <- options()
  op.rmet <- list(
    rmet.noaa.site = "http://www1.ncdc.noaa.gov/pub/data/",
    rmet.noaa.surfhist = "http://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.txt",
    rmet.install.args = "",
    rmet.name = "rmet",
    rmet.desc.author = '"James Durant <hzd3@cdc.gov> [aut, cre]"',
    rmet.desc.license = "MIT",
    rmet.desc.suggests = NULL,
    rmet.desc = list(),
    rmet.aermet = "aermet",
    rmet.aerminute = "aerminute",
    rmet.aersurface = "aersurface"
  )
  toset <- !(names(op.rmet) %in% names(op))
  if(any(toset)) options(op.rmet[toset])
  assign("rmetData", new.env(hash = TRUE), envir = .GlobalEnv) 
  
  # Read surface station history file and parse each variable
  surfhist <- readLines("http://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.txt")
  surfhist <- surfhist[23:length(surfhist)]
  surfhist <- sapply(surfhist,substring,
                     c(1,8,14,44,49,52,58,66,75,83,92),
                     c(7,13,43,48,51,57,65,74,82,91,99),
                     USE.NAMES = FALSE)
  surfhist <- t(surfhist)
  surfhist <- as.data.frame(surfhist,stringsAsFactors = FALSE)
  names(surfhist) <- c("USAF","WBAN","STATION_NAME","CTRY","ST",
                       "CALL","LAT","LON","ELEV","BEGIN","END")
  
  # Change variable types
  surfhist$LAT <- as.numeric(surfhist$LAT)
  surfhist$LON <- as.numeric(surfhist$LON)
  
  assign("surfhist", surfhist, envir = rmetData)
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to rmet! \n
                        current rmet options are set to:\n")
  
  print(sapply(grep("rmet", names(options()), value=TRUE), getOption))
  if("surf_Hist" %in% ls (envir = rmetData)){
    print(paste("Surface File History File Loaded from", getOption("rmet.noaa.surfhist")))
  }else{
    print("Error loading surface file history see ?rmet.options")
    } 
}