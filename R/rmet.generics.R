# rmet.generics

##' @export
rmet <- function(x){
  class(x) <- "rmet"
  return(x)
}



##' @export
locYears <- function(rmetObj) {
  stopifnot(is(rmetObj) =="rmet")
  names(rmetObj$td6405_noaa)
}

##' @export
is.rmet <- function(rmetObj, full.test=FALSE){
  output <- is(rmetObj) == "rmet"
  
  #full.test
  if(full.test & output ==TRUE){
    
    output <- all(c("project_Name", "project_Dir", "start_Date", "end_Date", "surf_WBAN", 
                    "surf_USAF", "surf_Call", "surf_UTC", "td3505_noaa", "td6405_noaa", 
                    "td6401_noaa", "ua_WMO", "ua_UTC", "ifg", "inputText", "inputFiles") %in% 
                    names(rmetObj))
  }
  
  return(output)
}

##' @export
##' @method print rmet
print.rmet <- function(rmetObj){
  stopifnot(is.rmet(rmetObj, full.test=TRUE))
  print(data.frame(rmetObj[c("project_Name", "project_Dir", "start_Date", "end_Date", "surf_WBAN", 
                             "surf_USAF", "surf_Call", "surf_UTC", "ua_WMO", "ua_UTC", "ifg")]))
  
}



##' @export
##' @method print rmetSum
print.rmetSum <- function(rmetSum){
  stopifnot(class(rmetSum) == "rmetSum")
  sapply(rmetSum, function(x){
  pander::pander(x)
  })
}

##' @export
prepareThePath <- function(x) {
  x <- path.expand(x)
  x <- paste0("\"", x, "\"")
  x
}

writeAerminute.rmet <- function(rmetObj){
  stopifnot(class(rmetObj) =="rmet")
  print("Writing AERMINUTE inpute files:\n")
  
  loc_years <- names(rmetObj$td6405_noaa)
  
  aerminInputFiles <- lapply(seq_along(loc_years), function(i){
    aerminInp <- paste(aerminTemplate, collapse="\n")
    destDir <- paste(rmetObj$project_Dir, loc_years[[i]], sep="/")
    
    
    oneMinFiles <- paste(destDir, gsub("^.*/", "", 
                                       rmetObj$td6405_noaa[[i]]), sep="/")
    stopifnot(all(file.exists(oneMinFiles)))
    
    startYear <- format(rmetObj$start_Date, "%Y")
    endYear <- format(rmetObj$end_Date, "%Y")
    startMonth <- ifelse(startYear == loc_years[[i]], 
                         format(rmetObj$start_Date, "%m"), "01")
    endMonth <-ifelse(endYear == loc_years[[i]], 
                      format(rmetObj$end_Date, "%m"), "12")
    
    ifgFlag <- rmetObj$ifg
    
    aerminInp <- gsub("!ifg!", ifgFlag, aerminInp)
    
    
    aerminInp <- gsub("!start_Date!", paste(startMonth, endMonth), aerminInp)
    aerminInp <- gsub("!end_Date!", paste(startMonth, endMonth), aerminInp)
    
    
    aerminInp <- gsub("!minFiles!", paste(oneMinFiles, collapse = "\n"), aerminInp)
    
    if(!is.null(rmetObj$td6401_noaa[[i]])){
      fiveMinFiles <- paste(destDir, gsub("^.*/", "", 
                                          rmetObj$td6401_noaa[[i]]), sep="/")
      
      stopifnot(all(file.exists(fiveMinFiles)))
      aerminInp <- gsub("!min5Files!", paste(fiveMinFiles, collapse = "\n"), 
                        aerminInp)
      
      hourFile <- paste0("AM_", "1min_", loc_years[[i]], ".dat")
      summFile <- paste0("AM_", "1min_", loc_years[[i]], "_summ.dat")
      compFile <- paste0("AM_", "1min_", loc_years[[i]], "_comp.dat")
      
      aerminInp <- gsub("!hourfile_AM!", hourFile, aerminInp)
      aerminInp <- gsub("!summfile_AM!", summFile, aerminInp)
      aerminInp <- gsub("!compfile_AM!", compFile, aerminInp)
      
    }
    
    aerminInp
  })
  
  names(aerminInputFiles) <- loc_years
  rmetObj$amInpFiles <- aerminInputFiles
  
  return(rmetObj)
  
}

##' @export
createInput <- function(object,...)
  UseMethod("createInput")

##' @export
writeInputFile <- function(object,...)
  UseMethod("writeInputFile")

##' @export
convertLat <- function(lat){
  stopifnot(is.numeric(lat))
  if (lat<0){
    lat <- paste(gsub("-", "", lat), "S", sep="")
  }else{
    lat <- paste(lat, "N", sep="")
  }
  lat
  }


##' @export
convertLong <- function(long){
  stopifnot(is.numeric(long))
  long <- round(long, 3)
  if (long<0){
    long <- paste(gsub("-", "", long), "W", sep="")
  }
  long
}  
  

# import isd
readisd <- function(con="http://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.txt") {
  surfhist <- readLines(con)
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
  surfhist$ELEV <- as.numeric(surfhist$ELEV)
  surfhist$BEGIN <- lubridate::ymd(surfhist$BEGIN)
  surfhist$END <- lubridate::ymd(surfhist$END)
  assign("rmetData", new.env(hash = TRUE), envir = .GlobalEnv) 
  assign("surfhist", surfhist, envir = rmetData)
}

  