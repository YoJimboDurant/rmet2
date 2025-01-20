#' @title downloadIGRA
#' 
#' @description
#' \code{downloadIGRA} 
#' 
#' The function takes Integrated Global Radiosode Archieve (igra) zip file and extracts the zipfile and
#' trims the file to match the start date and end date of the 
#' study period. It adds the local file locations to the rmetObj and returns it.
#' @param rmetObj is valid rmetObject created by createMetProject
#' @param igraLoc is the https location of the igra files. By default location is https://www.ncei.noaa.gov/data/integrated-global-radiosonde-archive/access/data-por/ 
#' @export
#' 
downloadIGRA <- function(rmetObj, igraLoc = "https://www.ncei.noaa.gov/data/integrated-global-radiosonde-archive/access/data-por/"){
  if(is.null(rmetObj$ua_IGRA_zip)) {
    stop("Missing IGRA zip file argument.")
  }
  
  igraFileLocal <- paste(rmetObj$project_Dir, "IGRA", rmetObj$ua_IGRA_zip, sep = "/")
  igraFileRemote <- paste(igraLoc, rmetObj$ua_IGRA_zip, sep = "")
  

  if(!file.exists(paste(rmetObj$project_Dir, "IGRA", rmetObj$ua_IGRA_zip, sep = "/"))){
    warning("Missing IGRA file, downloading IGRA file")
    
    response <-   response <- tryCatch({
      httr::HEAD(igraFileRemote)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(response)) {
      stop(paste("Invalid url location for IGRA:\n", igraFileRemote))  # Invalid URL or no connection
    }
    
    if (httr::status_code(response) != 200) {
      stop(paste("Status error:\n", igraFileRemote, "\n", httr::status_code(response)))
    } else {
      dir.create(paste(rmetObj$project_Dir, "IGRA", sep = "/"), showWarnings = FALSE)
      download.file(igraFileRemote, igraFileLocal, method = "curl")
    }
  }
  
  # get startDate and EndDate in UTC:
  startUTC <- format(
    lubridate::with_tz(rmetObj$start_Date, "UTC"), "%Y %m %d"
  )
  endUTC <- format(
    lubridate::with_tz(rmetObj$end_Date + 60 * 60 * 24 , "UTC"),
    "%Y %m %d"
  )
  
  wmoName <- paste("#", 
                   stringr::str_extract(rmetObj$ua_IGRA_zip, 
                                        "^[A-Za-z0-9]+"),
                   sep = ""
  )
  
  uaTxtFile <- paste(
    gsub("#", "", wmoName),
    "-data.txt",
    sep = ""
  )
  
  
  startCut <- paste(wmoName, startUTC)
  endCut <- paste(wmoName, endUTC)
  
  tempFile <- paste(tempdir(), uaTxtFile, sep = "\\")
  
  unzip(igraFileLocal, exdir = tempDir)  
  
  xLines <- readr::read_lines(tempFile)
  
  xmin <- min(grep(startCut, xLines))

  xmax <- min(grep(endCut, xLines))
  
  ua_data <- xLines[xmin:xmax]
  
  print(paste("Extracting", signif(100* length(ua_data)/length(xLines),4), "percent of upper air data."))

  ua_years <- rmet2:::locYears(rmetObj)

  exFiles <- lapply(ua_years, function(x){
    exFile <- paste(
      rmetObj$project_Dir, "/", x, "/",
      paste("igra", x, sep = "_"),
      ".txt",
      sep = ""
      )
    
    minLine <- min(grep( paste (wmoName, x), ua_data))
    maxLine <- min(grep( paste (wmoName, as.numeric(x) + 1), ua_data)) - 1
    
    ua_year_data <- ua_data[minLine:maxLine]
    
    writeLines(ua_year_data, exFile)
    return (exFile)
    
  })
  
  rmetObj$ua_IGRA_ext <- exFiles
  file.remove(tempFile)
  
  return(rmetObj)
  }
