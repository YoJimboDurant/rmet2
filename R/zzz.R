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
  
  
  rmet2:::readisd()
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to rmet! \n
                        current rmet options are set to:\n")
  
  print(sapply(grep("rmet", names(options()), value=TRUE), getOption))
  rmet2:::readisd()
  # if("surf_Hist" %in% ls (envir = rmetData)){
  #   print(paste("Surface File History File Loaded from", getOption("rmet.noaa.surfhist")))
  # }else{
  #   print("Error loading surface file history see ?rmet.options")
  #   } 
}