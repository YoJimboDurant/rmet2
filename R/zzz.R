.onLoad <- function(libname, pkgname) {
  op <- options()
  op.rmet <- list(
    rmet.noaa.site = "https://noaa-isd-pds.s3.amazonaws.com/data/",
    rmet.noaa.1min = "https://www.ncei.noaa.gov/data/automated-surface-observing-system-one-minute-pg1/access/",
    rmet.noaa.5min = "https://www.ncei.noaa.gov/data/automated-surface-observing-system-five-minute/access/",
    rmet.noaa.surfhist = "https://noaa-isd-pds.s3.amazonaws.com/isd-history.txt",
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
  
  
  try(rmet2:::readisd())
  
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to rmet! \n
                        current rmet options are set to:\n")
  
  print(sapply(grep("rmet", names(options()), value=TRUE), getOption))
  try(rmet2:::readisd("https://noaa-isd-pds.s3.amazonaws.com/isd-history.txt"))
  # if("surf_Hist" %in% ls (envir = rmetData)){
  #   print(paste("Surface File History File Loaded from", getOption("rmet.noaa.surfhist")))
  # }else{
  #   print("Error loading surface file history see ?rmet.options")
  #   } 
}