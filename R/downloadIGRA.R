#' @title downloadIGRA
#'
#' @description
#' Downloads, extracts, and trims Integrated Global Radiosonde Archive (IGRA)
#' upper-air data to match the rmet project date range. Extracted yearly
#' files are written to the project directory and tracked in project state.
#'
#' @param rmetObj A valid \code{rmet} object created by \code{createMetProject()}
#' @param igraLoc Base URL for IGRA data files
#'
#' @return Updated \code{rmet} object with IGRA files and state updated
#'
#' @export
downloadIGRA <- function(
    rmetObj,
    igraLoc = "https://www.ncei.noaa.gov/data/integrated-global-radiosonde-archive/access/data-por/"
) {
  
  stopifnot(is.rmet(rmetObj))
  
  if (is.null(rmetObj$ua_IGRA_zip)) {
    stop("Missing IGRA zip file argument (ua_IGRA_zip).")
  }
  
  ## ---- initialize state node -------------------------------------------
  if (is.null(rmetObj$state$data$igra)) {
    rmetObj$state$data$igra <- list()
  }
  
  downloaded <- FALSE
  
  ## ---- file paths ------------------------------------------------------
  igraDir <- file.path(rmetObj$project_Dir, "IGRA")
  igraFileLocal  <- file.path(igraDir, rmetObj$ua_IGRA_zip)
  igraFileRemote <- paste0(igraLoc, rmetObj$ua_IGRA_zip)
  
  ## ---- download if needed ----------------------------------------------
  if (!file.exists(igraFileLocal)) {
    
    warning("Missing IGRA file; downloading from NOAA.")
    
    response <- tryCatch(
      httr::HEAD(igraFileRemote),
      error = function(e) NULL
    )
    
    if (is.null(response)) {
      rmetObj$state$data$igra$error <- "Invalid URL or connection failure"
      stop("Invalid IGRA URL or connection error:\n", igraFileRemote)
    }
    
    if (httr::status_code(response) != 200) {
      rmetObj$state$data$igra$error <- paste(
        "HTTP status", httr::status_code(response)
      )
      stop("IGRA download failed with HTTP status ",
           httr::status_code(response))
    }
    
    dir.create(igraDir, showWarnings = FALSE)
    download.file(igraFileRemote, igraFileLocal, method = "curl")
    downloaded <- TRUE
  }
  
  ## ---- determine extraction window ------------------------------------
  startUTC <- format(
    lubridate::with_tz(rmetObj$start_Date, "UTC"),
    "%Y %m %d"
  )
  
  endUTC <- format(
    lubridate::with_tz(rmetObj$end_Date + 86400, "UTC"),
    "%Y %m %d"
  )
  
  wmoName <- paste0(
    "#",
    stringr::str_extract(rmetObj$ua_IGRA_zip, "^[A-Za-z0-9]+")
  )
  
  uaTxtFile <- paste0(gsub("#", "", wmoName), "-data.txt")
  
  startCut <- paste(wmoName, startUTC)
  endCut   <- paste(wmoName, endUTC)
  
  ## ---- extract ---------------------------------------------------------
  tempDir  <- tempdir()
  tempFile <- file.path(tempDir, uaTxtFile)
  
  unzip(igraFileLocal, exdir = tempDir)
  
  xLines <- readr::read_lines(tempFile)
  
  xmin <- min(grep(startCut, xLines))
  xmax <- min(grep(endCut,   xLines))
  
  ua_data <- xLines[xmin:xmax]
  
  message(
    "Extracting ",
    signif(100 * length(ua_data) / length(xLines), 4),
    "% of upper-air data."
  )
  
  ## ---- split by year ---------------------------------------------------
  ua_years <- locYears(rmetObj)
  
  exFiles <- lapply(ua_years, function(yy) {
    
    exFile <- file.path(
      rmetObj$project_Dir,
      yy,
      paste0("igra_", yy, ".txt")
    )
    
    minLine <- min(grep(paste(wmoName, yy), ua_data))
    
    if (yy == ua_years[[length(ua_years)]]) {
      maxLine <- length(ua_data) - 1
    } else {
      maxLine <- min(
        grep(paste(wmoName, as.numeric(yy) + 1), ua_data)
      ) - 1
    }
    
    ua_year_data <- ua_data[minLine:maxLine]
    writeLines(ua_year_data, exFile)
    
    exFile
  })
  
  ## ---- cleanup ---------------------------------------------------------
  file.remove(tempFile)
  
  ## ---- update rmet object ----------------------------------------------
  rmetObj$ua_IGRA_ext <- exFiles
  
  ## ---- update state ----------------------------------------------------
  rmetObj$state$data$igra$done       <- TRUE
  rmetObj$state$data$igra$years      <- ua_years
  rmetObj$state$data$igra$files      <- exFiles
  rmetObj$state$data$igra$zip        <- basename(igraFileLocal)
  rmetObj$state$data$igra$downloaded <- downloaded
  rmetObj$state$data$igra$timestamp  <- Sys.time()
  
  return(rmetObj)
}

