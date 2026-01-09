#' @title downloadTD3505
#'
#' @description
#' Downloads and prepares TD3505 (ISHD) surface meteorological data.
#' Handles UTC year overlap correctly and trims data to project date range.
#'
#' @param rmetObj A valid \code{rmet} object
#' @param check Logical; validate existing outputs before reprocessing
#' @param ... Unused
#'
#' @export
downloadTD3505 <- function(rmetObj, check = TRUE, ...) {
  
  stopifnot(is.rmet(rmetObj))
  
  message("Processing TD3505 (ISHD hourly) data...")
  
  years     <- names(rmetObj$td3505_noaa)
  failures  <- list()
  
  for (i in seq_along(years)) {
    
    yr       <- years[[i]]
    src_urls <- rmetObj$td3505_noaa[[i]]
    yearDir  <- file.path(rmetObj$project_Dir, yr)
    
    if (!dir.exists(yearDir)) dir.create(yearDir, recursive = FALSE)
    
    destFile <- .td3505_destfile(rmetObj, yr)
    
    if (check && file.exists(destFile)) {
      if (.validate_td3505_file(destFile, rmetObj$surf_Call)) {
        next
      }
    }
    
    message("Building TD3505 file for year ", yr)
    
    out <- tryCatch(
      .build_td3505_year(
        urls      = src_urls,
        year      = yr,
        startDate = rmetObj$start_Date,
        endDate   = rmetObj$end_Date,
        utcOffset = rmetObj$surf_UTC
      ),
      error = function(e) e
    )
    
    if (inherits(out, "error")) {
      failures[[yr]] <- out$message
      next
    }
    
    writeLines(out, destFile)
  }
  
  ## ---- report failures ------------------------------------------------
  if (length(failures) > 0) {
    cat("\nTD3505 processing failures:\n")
    for (y in names(failures)) {
      cat("❌", y, "→", failures[[y]], "\n")
    }
    stop("TD3505 processing failed for one or more years.")
  }
  
  ## ---- update state ---------------------------------------------------
  if (is.null(rmetObj$state$data$td3505)) {
    rmetObj$state$data$td3505 <- list()
  }
  
  rmetObj$state$data$td3505$done      <- TRUE
  rmetObj$state$data$td3505$years     <- years
  rmetObj$state$data$td3505$timestamp <- Sys.time()
  
  return(rmetObj)
}

#' Build TD3505 yearly data with UTC trimming
#'
#' @noRd
.build_td3505_year <- function(urls, year, startDate, endDate, utcOffset) {
  
  lines <- character()
  
  for (u in urls) {
    message("  reading ", basename(u))
    lines <- c(lines, readLines(gzcon(url(u)), warn = FALSE))
  }
  
  if (length(lines) == 0) stop("No data read from source files")
  
  ts <- as.numeric(substr(lines, 16, 27))
  
  startUTC <- as.numeric(format(
    lubridate::with_tz(startDate, "UTC"), "%Y%m%d%H%M"
  ))
  
  endUTC <- as.numeric(format(
    lubridate::with_tz(endDate + 86400, "UTC"), "%Y%m%d%H%M"
  ))
  
  keep <- ts >= startUTC & ts <= endUTC
  
  out <- unique(lines[keep])
  
  if (length(out) == 0) stop("No TD3505 records remain after trimming")
  
  out
}

#' TD3505 destination file name
#'
#' @noRd
.td3505_destfile <- function(rmetObj, year) {
  
  station_id <- substr(
    basename(rmetObj$td3505_noaa[[year]][[1]]),
    1, 12
  )
  
  file.path(
    rmetObj$project_Dir,
    year,
    paste0("S", station_id, "_", year, ".ISH")
  )
}

#' Validate TD3505 ISHD file
#'
#' @noRd
.validate_td3505_file <- function(file, call, min_bytes = 1000) {
  
  fail <- function(msg) {
    cat("\nTD3505 VALIDATION FAILED\n")
    cat("File   :", file, "\n")
    cat("Reason :", msg, "\n")
    if (file.exists(file)) {
      cat("Preview:\n")
      print(readLines(file, n = 5))
    }
    FALSE
  }
  
  if (!file.exists(file)) return(FALSE)
  
  sz <- file.size(file)
  if (is.na(sz) || sz < min_bytes)
    return(fail(paste("File too small:", sz)))
  
  hdr <- try(readLines(file, n = 10), silent = TRUE)
  if (inherits(hdr, "try-error") || length(hdr) == 0)
    return(fail("Unreadable or empty"))
  
  if (!any(grepl(call, hdr, fixed = TRUE)))
    return(fail(paste("Station call sign not found:", call)))
  
  TRUE
}




