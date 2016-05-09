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

  