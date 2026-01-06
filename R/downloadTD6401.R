#' Download and validate TD6401 ASOS 5-minute data
#'
#' @description
#' \code{downloadTD6401()} downloads NOAA TD6401 (ASOS 5-minute) surface
#' meteorological data files required by AERMINUTE.
#'
#' The function verifies file integrity using lightweight checks
#' (existence, minimum file size, readability, station identifiers)
#' and re-downloads files that are missing or fail validation.
#'
#' Validation failures are reported to the console with a short file
#' preview to aid debugging. If any file fails validation after download,
#' the function stops with an error.
#'
#' Successful execution updates the project \code{state} to record
#' completion time and covered years.
#'
#' @param rmetObj An object of class \code{"rmet"} created by
#'   \code{createMetProject()}.
#'
#' @param check Logical; if \code{TRUE} (default), existing files are
#'   validated and only re-downloaded if they fail integrity checks.
#'   If \code{FALSE}, all files are downloaded unconditionally.
#'
#' @param ... Reserved for future options.
#'
#' @return An updated \code{"rmet"} object with TD6401 files downloaded
#'   and \code{state$data$td6401} updated.
#'
#' @details
#' TD6401 files are not fully QA'd by NOAA and may contain malformed
#' records. The validation performed here is intentionally conservative
#' and does not attempt to enforce strict formatting rules; detailed
#' cleaning is deferred to AERMINUTE.
#'
#' @seealso
#' \code{\link{downloadTD6405}}, \code{\link{downloadTD3505}},
#' \code{\link{downloadIGRA}}
#'
#' @export
downloadTD6401 <- function(rmetObj, check = TRUE, ...) {
  
  stopifnot(is.rmet(rmetObj))
  
  loc_years <- names(rmetObj$td6401_noaa)
  failures  <- list()
  
  message("Processing TD6401 (5-minute) data...")
  
  for (i in seq_along(loc_years)) {
    
    yr      <- loc_years[[i]]
    destDir <- file.path(rmetObj$project_Dir, yr)
    urls    <- rmetObj$td6401_noaa[[i]]
    
    for (j in seq_along(urls)) {
      
      url      <- urls[[j]]
      filename <- basename(url)
      destFile <- file.path(destDir, filename)
      
      needs_download <- TRUE
      
      if (check && file.exists(destFile)) {
        needs_download <- !.validate_td6401_file(destFile, 
                                                 call = rmetObj$surf_Call)
      }
      
      if (needs_download) {
        message("Downloading TD6401: ", filename)
        
        tryCatch(
          {
            utils::download.file(
              url,
              destFile,
              method = "libcurl",
              quiet  = FALSE
            )
          },
          error = function(e) {
            failures[[destFile]] <<- paste("Download error:", e$message)
            return(NULL)
          }
        )
      }
      
      ok <- .validate_td6401_file(
        destFile,
        call = rmetObj$surf_Call
      )
    }
  }
  
  ## ---- report failures ------------------------------------------------
  if (length(failures) > 0) {
    message("\nTD6401 integrity check failures:")
    for (f in names(failures)) {
      cat("❌", f, "→", failures[[f]], "\n")
      if (file.exists(f)) {
        cat("---- File preview ----\n")
        print(readLines(f, n = 5))
        cat("----------------------\n")
      }
    }
    stop("One or more TD6401 files failed validation.")
  }
  
  ## ---- update state ---------------------------------------------------
  if (is.null(rmetObj$state$data$td6401)) {
    rmetObj$state$data$td6401 <- list()
  }
  
  rmetObj$state$data$td6401$done      <- TRUE
  rmetObj$state$data$td6401$years     <- loc_years
  rmetObj$state$data$td6401$timestamp <- Sys.time()
  
  return(rmetObj)
}

#' Validate a TD6401 ASOS 5-minute data file
#'
#' Internal integrity check used by downloadTD6405().
#'
#' @param file Path to downloaded TD6401 file
#' @param call Station call sign (e.g., "KBMG")
#' @param wban Optional WBAN for extra checking
#' @param show_lines Number of lines to print on failure (default 5)
#'
#' @return Logical; TRUE if file passes basic integrity checks
#'
#' @noRd
.validate_td6401_file <- function(
    file,
    call,
    wban = NULL,
    show_lines = 5
) {
  
  fail <- function(reason, hdr = NULL) {
    cat("\nTD6401 VALIDATION FAILED\n")
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
  if (is.na(sz) || sz < 500) {
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