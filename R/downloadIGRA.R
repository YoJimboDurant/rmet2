#' @title downloadIGRA
#'
#' @description
#' Downloads, extracts, and trims Integrated Global Radiosonde Archive (IGRA)
#' upper-air data to match the rmet project date range.
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
  
  ## ---- init state -----------------------------------------------------
  rmetObj$state$data$igra <- rmetObj$state$data$igra %||% list()
  downloaded <- FALSE
  
  ## ---- paths ----------------------------------------------------------
  igraDir <- file.path(rmetObj$project_Dir, "IGRA")
  igraFileLocal  <- file.path(igraDir, rmetObj$ua_IGRA_zip)
  igraFileRemote <- paste0(igraLoc, rmetObj$ua_IGRA_zip)
  
  ## ---- download -------------------------------------------------------
  if (!file.exists(igraFileLocal)) {
    
    message("Downloading IGRA archive...")
    
    response <- tryCatch(httr::HEAD(igraFileRemote), error = function(e) NULL)
    if (is.null(response) || httr::status_code(response) != 200) {
      stop("IGRA download failed: ", igraFileRemote)
    }
    
    dir.create(igraDir, showWarnings = FALSE)
    utils::download.file(igraFileRemote, igraFileLocal, method = "libcurl")
    downloaded <- TRUE
  }
  
  ## ---- short-circuit if already done -----------------------------------
  if (isTRUE(rmetObj$state$data$igra$done)) {
    
    existing <- rmetObj$state$data$igra$files
    
    if (!is.null(existing) && all(file.exists(existing))) {
      message("IGRA data already downloaded and extracted — skipping.")
      return(rmetObj)
    }
    
    message("IGRA state marked done, but files missing — reprocessing.")
  }
  
  
  ## ---- extraction window ---------------------------------------------
  startUTC <- format(lubridate::with_tz(rmetObj$start_Date, "UTC"), "%Y %m %d")
  endUTC   <- format(lubridate::with_tz(rmetObj$end_Date + 86400, "UTC"), "%Y %m %d")
  
  wmo <- stringr::str_extract(rmetObj$ua_IGRA_zip, "^[A-Za-z0-9]+")
  wmoTag <- paste0("#", wmo)
  uaTxtFile <- paste0(wmo, "-data.txt")
  
  ## ---- unzip ----------------------------------------------------------
  tmp <- tempdir()
  unzip(igraFileLocal, exdir = tmp)
  txtPath <- file.path(tmp, uaTxtFile)
  
  if (!file.exists(txtPath)) {
    stop("Expected IGRA text file not found after unzip: ", uaTxtFile)
  }
  
  lines <- readLines(txtPath, warn = FALSE)
  
  ## ---- slice safely ---------------------------------------------------
  startHits <- grep(paste(wmoTag, startUTC), lines)
  endHits   <- grep(paste(wmoTag, endUTC),   lines)
  
  if (length(startHits) == 0 || length(endHits) == 0) {
    cat("\nIGRA EXTRACTION FAILED\n")
    cat("Station:", wmo, "\n")
    cat("Start tag:", startUTC, "\n")
    cat("End tag  :", endUTC, "\n")
    cat("File preview:\n")
    print(head(lines, 10))
    stop("IGRA date range not found in archive.")
  }
  
  ua_data <- lines[min(startHits):min(endHits)]
  
  .validate_igra_extract(ua_data, wmoTag)
  
  ## ---- split by year --------------------------------------------------
  years <- locYears(rmetObj)
  
  exFiles <- vapply(years, function(yy) {
    
    outFile <- file.path(
      rmetObj$project_Dir,
      yy,
      paste0("igra_", yy, ".txt")
    )
    
    yrHits <- grep(paste(wmoTag, yy), ua_data)
    if (length(yrHits) == 0) return(NA_character_)
    
    endIdx <- if (yy == tail(years, 1)) {
      length(ua_data)
    } else {
      min(grep(paste(wmoTag, as.numeric(yy) + 1), ua_data)) - 1
    }
    
    writeLines(ua_data[min(yrHits):endIdx], outFile)
    outFile
    
  }, character(1))
  
  exFiles <- exFiles[!is.na(exFiles)]
  
  ## ---- cleanup --------------------------------------------------------
  file.remove(txtPath)
  
  ## ---- update object --------------------------------------------------
  rmetObj$ua_IGRA_ext <- exFiles
  
  rmetObj$state$data$igra <- list(
    done       = TRUE,
    years      = years,
    files      = exFiles,
    zip        = basename(igraFileLocal),
    downloaded = downloaded,
    timestamp  = Sys.time()
  )
  
  return(rmetObj)
}

#' Validate extracted IGRA data block
#'
#' @noRd
.validate_igra_extract <- function(lines, wmoTag, min_lines = 100) {
  
  if (length(lines) < min_lines) {
    stop("IGRA extract too small (", length(lines), " lines)")
  }
  
  if (!any(grepl(wmoTag, lines, fixed = TRUE))) {
    stop("IGRA extract missing station tag: ", wmoTag)
  }
  
  if (any(grepl("<html>|404 Not Found", lines, ignore.case = TRUE))) {
    stop("IGRA extract appears to contain HTML or error output")
  }
  
  TRUE
}

