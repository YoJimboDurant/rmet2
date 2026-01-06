# -------------------------------------------------------------------------
# rmet.generics.R
# Core S3 generics, methods, and internal helpers for rmet objects
# -------------------------------------------------------------------------

# =========================
# Internal constructor
# =========================

#' Internal constructor for rmet objects
#'
#' @noRd
rmet <- function(x) {
  structure(x, class = "rmet")
}

# =========================
# Internal utilities
# =========================

#' Internal test for rmet objects
#'
#' @noRd
is.rmet <- function(x, full.test = FALSE) {
  if (!inherits(x, "rmet")) return(FALSE)
  
  if (isTRUE(full.test)) {
    required <- c(
      "project_Name", "project_Dir",
      "start_Date", "end_Date",
      "surf_WBAN", "surf_USAF", "surf_Call",
      "inputFiles", "inputText"
    )
    return(all(required %in% names(x)))
  }
  
  TRUE
}

#' Internal helper to expand and quote paths for executables
#'
#' @noRd
prepareThePath <- function(x) {
  x <- path.expand(x)
  paste0("\"", x, "\"")
}

#' Convert numeric latitude to AERMET format
#'
#' @noRd
convertLat <- function(lat) {
  stopifnot(is.numeric(lat))
  if (lat < 0) {
    paste0(abs(lat), "S")
  } else {
    paste0(lat, "N")
  }
}

#' Convert numeric longitude to AERMET format
#'
#' @noRd
convertLong <- function(long) {
  stopifnot(is.numeric(long))
  long <- round(long, 3)
  if (long < 0) {
    paste0(abs(long), "W")
  } else {
    paste0(long, "E")
  }
}

#' Read and cache NOAA ISD station history
#'
#' @noRd
readisd <- function(con = getOption("rmet.noaa.surfhist")) {
  
  surfhist <- readLines(con)
  surfhist <- surfhist[23:length(surfhist)]
  
  surfhist <- sapply(
    surfhist,
    substring,
    c(1, 8, 14, 44, 49, 52, 58, 66, 75, 83, 92),
    c(7, 13, 43, 48, 51, 57, 65, 74, 82, 91, 99),
    USE.NAMES = FALSE
  )
  
  surfhist <- as.data.frame(t(surfhist), stringsAsFactors = FALSE)
  
  names(surfhist) <- c(
    "USAF", "WBAN", "STATION_NAME", "CTRY", "ST",
    "CALL", "LAT", "LON", "ELEV", "BEGIN", "END"
  )
  
  surfhist$LAT   <- as.numeric(surfhist$LAT)
  surfhist$LON   <- as.numeric(surfhist$LON)
  surfhist$ELEV  <- as.numeric(surfhist$ELEV)
  surfhist$BEGIN <- lubridate::ymd(surfhist$BEGIN)
  surfhist$END   <- lubridate::ymd(surfhist$END)
  
  rmetData <- new.env(hash = TRUE)
  assign("surfhist", surfhist, envir = rmetData)
  assign("rmetData", rmetData, envir = .GlobalEnv)
  
  invisible(surfhist)
}

# =========================
# User-facing helpers
# =========================

#' List years covered by an rmet project
#'
#' @param rmetObj An \code{rmet} object
#'
#' @return Character vector of years
#'
#' @export
locYears <- function(rmetObj) {
  stopifnot(is.rmet(rmetObj))
  names(rmetObj$td6405_noaa)
}

# =========================
# S3 generics
# =========================

#' Create meteorological model input files
#'
#' @param object An object
#' @param ... Additional arguments
#'
#' @export
createInput <- function(object, ...) {
  UseMethod("createInput")
}

#' Write model input files to disk
#'
#' @param object An object
#' @param ... Additional arguments
#'
#' @export
writeInputFile <- function(object, ...) {
  UseMethod("writeInputFile")
}

# =========================
# S3 methods
# =========================

#' Print an rmet object
#'
#' @param x An \code{rmet} object
#' @param ... Unused
#'
#' @export
print.rmet <- function(x, ...) {
  stopifnot(is.rmet(x, full.test = TRUE))
  
  base_fields <- c(
    "project_Name", "project_Dir",
    "start_Date", "end_Date",
    "surf_WBAN", "surf_USAF",
    "surf_Call", "surf_UTC"
  )
  
  ua_fields <- if (!is.null(x$ua_WMO)) {
    c("ua_WMO", "ua_UTC", "ifg")
  } else {
    c("ua_IGRA_zip", "ua_UTC", "ua_elevation", "ifg")
  }
  
  print(as.data.frame(x[c(base_fields, ua_fields)]))
  invisible(x)
}

#' @export
summary.rmet <- function(object, ...) {
  
  st <- object$state
  
  cat("rmet project:", object$project_Name, "\n")
  cat("Directory:", object$project_Dir, "\n")
  cat("Period:", 
      format(object$start_Date), "to", format(object$end_Date), "\n\n")
  
  cat("Data:\n")
  cat("  TD3505   :", if (st$data$td3505$done) "✓" else "✗", "\n")
  cat("  TD6405   :", if (st$data$td6405$done) "✓" else "✗", "\n")
  cat("  TD6401   :", if (st$data$td6401$done) "✓" else "✗", "\n")
  cat("  IGRA     :", if (st$data$igra$done)   "✓" else "✗", "\n\n")
  
  cat("Processing:\n")
  cat("  AERMINUTE:", if (st$processing$aerminute$done) "✓" else "✗", "\n")
  cat("  AERSURFACE:", if (st$processing$aersurface$done) "✓" else "✗", "\n")
  cat("  AERMET:\n")
  cat("    Stage 1:", if (st$processing$aermet$stage1$done) "✓" else "✗", "\n")
  cat("    Stage 2:", if (st$processing$aermet$stage2$done) "✓" else "✗", "\n")
  cat("    Stage 3:", if (st$processing$aermet$stage3$done) "✓" else "✗", "\n")
}

