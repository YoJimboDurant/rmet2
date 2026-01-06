#' @title downloadTD6405
#'
#' @description
#' Downloads NOAA TD6405 ASOS 1-minute surface data and validates file integrity.
#'
#' @param rmetObj A valid rmet object
#' @param check Logical; if TRUE (default) only missing/invalid files are downloaded
#' @param ... Unused
#'
#' @return Updated rmet object
#' @export
downloadTD6405 <- function(rmetObj, check = TRUE, ...) {
  
  stopifnot(is.rmet(rmetObj))
  
  loc_years <- names(rmetObj$td6405_noaa)
  failures  <- list()
  
  for (yy in loc_years) {
    
    destDir <- file.path(rmetObj$project_Dir, yy)
    urls    <- rmetObj$td6405_noaa[[yy]]
    
    for (u in urls) {
      
      fname    <- basename(u)
      destFile <- file.path(destDir, fname)
      
      need_dl <- TRUE
      
      if (check && file.exists(destFile)) {
        need_dl <- !.validate_td6405_file(
          destFile,
          call = rmetObj$surf_Call,
          wban = rmetObj$surf_WBAN
        )
      }
      
      if (need_dl) {
        message("Downloading TD6405: ", fname)
        
        tryCatch(
          {
            utils::download.file(u, destFile, quiet = FALSE)
            
            ok <- .validate_td6405_file(
              destFile,
              call = rmetObj$surf_Call,
              wban = rmetObj$surf_WBAN
            )
            
            if (!ok) {
              failures[[length(failures) + 1]] <- destFile
            }
            
          },
          error = function(e) {
            cat("\nTD6405 DOWNLOAD ERROR\n")
            cat("URL :", u, "\n")
            cat("File:", destFile, "\n")
            cat("Error:", conditionMessage(e), "\n\n")
            failures[[length(failures) + 1]] <- destFile
          }
        )
      }
    }
  }
  
  # ---- state handling -------------------------------------------------
  if (length(failures) > 0) {
    
    rmetObj$state$data$td6405 <- list(
      done      = FALSE,
      failed    = failures,
      timestamp = Sys.time()
    )
    
    stop(
      "TD6405 download failed for ",
      length(failures),
      " file(s). See diagnostics above."
    )
  }
  
  rmetObj$state$data$td6405 <- list(
    done      = TRUE,
    years     = loc_years,
    timestamp = Sys.time()
  )
  
  rmetObj
}

#' Validate a TD6405 ASOS 1-minute data file
#'
#' Internal integrity check used by downloadTD6405().
#'
#' @param file Path to downloaded TD6405 file
#' @param call Station call sign (e.g., "KBMG")
#' @param wban Optional WBAN for extra checking
#' @param show_lines Number of lines to print on failure (default 5)
#'
#' @return Logical; TRUE if file passes basic integrity checks
#'
#' @noRd
.validate_td6405_file <- function(
    file,
    call,
    wban = NULL,
    show_lines = 5
) {
  
  fail <- function(reason, hdr = NULL) {
    cat("\nTD6405 VALIDATION FAILED\n")
    cat("File   :", file, "\n")
    cat("Reason :", reason, "\n")
    
    if (!is.null(hdr)) {
      cat("\nFile preview:\n")
      print(utils::head(hdr, show_lines))
    }
    
    cat("\n")
    return(FALSE)
  }
  
  # ---- existence ------------------------------------------------------
  if (!file.exists(file)) {
    return(fail("File does not exist"))
  }
  
  # ---- size -----------------------------------------------------------
  sz <- file.size(file)
  if (is.na(sz) || sz < 1000) {
    return(fail(paste("File size too small:", sz, "bytes")))
  }
  
  # ---- readability ----------------------------------------------------
  hdr <- try(readLines(file, n = 20), silent = TRUE)
  if (inherits(hdr, "try-error") || length(hdr) == 0) {
    return(fail("File could not be read or is empty"))
  }
  
  # ---- obvious HTML / error pages ------------------------------------
  if (any(grepl("<html>|<!DOCTYPE|404 Not Found",
                hdr, ignore.case = TRUE))) {
    return(fail("File appears to be an HTML or error page", hdr))
  }
  
  # ---- station identifier sanity -------------------------------------
  if (!any(grepl(call, hdr, fixed = TRUE))) {
    return(fail(paste("Station call sign not found:", call), hdr))
  }
  
  if (!is.null(wban)) {
    if (!any(grepl(wban, hdr, fixed = TRUE))) {
      return(fail(paste("WBAN not found:", wban), hdr))
    }
  }
  
  # ---- passed ---------------------------------------------------------
  TRUE
}


