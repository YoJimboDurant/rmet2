#' Create or load an rmet meteorological project
#'
#' @description
#' \code{createMetProject()} creates, loads, or recreates an \code{rmet} project
#' used to prepare meteorological data for AERMET, AERMINUTE, and AERSURFACE.
#'
#' The function defines project metadata, station information, land-use inputs,
#' and temporal coverage, and saves the resulting project object as an \code{.rds}
#' file in \code{project_Dir}.
#'
#' If a project already exists, behavior is controlled by
#' \code{clobber_project}:
#' \itemize{
#'   \item \code{FALSE} (default): load and return the existing project
#'   \item \code{TRUE}: delete the existing project object and recreate it from
#'   scratch, preserving downloaded data files
#' }
#'
#' @param project_Name Character string giving the project name. This value is
#'   used to name the saved \code{.rds} project file.
#'
#' @param project_Dir Character string specifying the directory where the project
#'   will reside. Subdirectories for each year, AERSURFACE, and preprocessed data
#'   will be created if they do not already exist.
#'
#' @param start_Date,end_Date POSIXct date-time objects specifying the start and
#'   end of the meteorological period to be processed. Times should be supplied
#'   in local standard time with an explicit time zone.
#'
#' @param surf_WBAN,surf_USAF Character or numeric identifiers for the surface
#'   meteorological station. These must correspond to valid NOAA ISD station
#'   identifiers.
#'
#' @param surf_Call Character string giving the surface station call sign
#'   (e.g., \code{"KBMG"}).
#'
#' @param surf_Latitude,surf_Longitude Numeric latitude and longitude of the
#'   surface station in decimal degrees (NAD83/WGS84).
#'
#' @param surf_Elevation Numeric elevation of the surface station in meters.
#'
#' @param surf_AnenometerHeight Numeric height of the anemometer above ground
#'   level in meters.
#'
#' @param surf_UTC Numeric UTC offset for the surface station (e.g., \code{-5}
#'   for Eastern Standard Time).
#'
#' @param ifg Character string specifying the AERMET IFG control option
#'   (e.g., \code{"Y 05 25 2007"}).
#'
#' @param ua_IGRA_zip Character string giving the filename of the IGRA upper-air
#'   data archive to be used.
#'
#' @param ua_IGRA_ext Optional character string specifying the extracted IGRA
#'   data filename. If \code{NULL}, the default naming convention is used.
#'
#' @param ua_Latitude,ua_Longitude Numeric latitude and longitude of the upper-air
#'   station in decimal degrees.
#'
#' @param ua_elevation Numeric elevation of the upper-air station in meters.
#'
#' @param ua_UTC Numeric UTC offset for the upper-air station.
#'
#' @param ua_WMO,ua_WBAN Deprecated identifiers for legacy FSL upper-air data.
#'   Retained for backward compatibility but no longer used.
#'
#' @param lc_File,imp_File,cnpy_File Character strings giving file paths to
#'   land-cover, impervious surface, and canopy raster datasets used by
#'   AERSURFACE.
#'
#' @param lc_Type,imp_Type,cnpy_Type Character strings identifying the dataset
#'   types (e.g., \code{"NLCD2019"}, \code{"MPRV2019"}, \code{"CNPY2019"}).
#'
#' @param as_Snow Logical or character flag indicating whether continuous snow
#'   cover is assumed for AERSURFACE.
#'
#' @param as_Arid Logical or character flag indicating whether the site is arid
#'   for AERSURFACE processing.
#'
#' @param as_Moisture Character code specifying surface moisture conditions
#'   (e.g., \code{"A"} for average).
#'
#' @param as_Airport Logical or character flag indicating whether the site is an
#'   airport.
#'
#' @param as_Winter_NS,as_Winter_WS,as_Spring,as_Summer,as_Autumn Character strings
#'   specifying month groupings for seasonal land-surface characterization.
#'
#' @param as_nsector Deprecated numeric value specifying the number of sectors
#'   for AERSURFACE. Retained for backward compatibility.
#'
#' @param as_radius Numeric radius (in kilometers) used for AERSURFACE sector
#'   characterization.
#'
#' @param nws_sector,os_sector Data frames defining wind-direction sector
#'   boundaries for NWS and onsite sector configurations.
#'
#' @param onsite_Latitude,onsite_Longitude Optional numeric coordinates for an
#'   onsite meteorological station.
#'
#' @param onsite_Fstring Optional character string defining onsite data format
#'   for AERMET.
#'
#' @param clobber_project Logical flag controlling behavior when a project already
#'   exists. If \code{FALSE} (default), the existing project is loaded. If
#'   \code{TRUE}, the existing project object is deleted and recreated, while
#'   preserving downloaded data files.
#'
#' @return An object of class \code{"rmet"} containing project metadata,
#'   configuration settings, processing inputs/outputs, and project state.
#' @examples
#' \dontrun{
#' ## Create a new rmet project for Bloomington, Indiana (KBMG)
#'
#' rootDir <- "C:/rmet2/IN/KBMG"
#'
#' KBMG <- createMetProject(
#'   project_Name = "BMG_AP",
#'   project_Dir  = rootDir,
#'
#'   start_Date = lubridate::mdy_hm("01/01/2019 00:00", tz = "Etc/GMT+5"),
#'   end_Date   = lubridate::mdy_hm("01/31/2023 23:59", tz = "Etc/GMT+5"),
#'
#'   surf_WBAN   = "03893",
#'   surf_USAF   = "724375",
#'   surf_Call   = "KBMG",
#'   surf_Latitude  = 39.14306,
#'   surf_Longitude = -86.61667,
#'   surf_Elevation = 257,
#'   surf_AnenometerHeight = 10,
#'   surf_UTC = -5,
#'
#'   ifg = "Y 05 25 2007",
#'
#'   ua_IGRA_zip = "USM00072426-data.txt.zip",
#'   ua_Latitude = 39.4214,
#'   ua_Longitude = -83.8217,
#'   ua_elevation = 323,
#'   ua_UTC = -5,
#'
#'   lc_File   = "path/to/land_cover.tif",
#'   lc_Type   = "NLCD2019",
#'   imp_File  = "path/to/impervious.tif",
#'   imp_Type  = "MPRV2019",
#'   cnpy_File = "path/to/canopy.tif",
#'   cnpy_Type = "CNPY2019",
#'
#'   as_Snow     = "N",
#'   as_Arid     = "N",
#'   as_Moisture = "A",
#'   as_Airport  = "Y",
#'
#'   as_Winter_NS = "3",
#'   as_Winter_WS = "12 1 2",
#'   as_Spring    = "4 5",
#'   as_Summer    = "6 7 8",
#'   as_Autumn    = "9 10",
#'
#'   clobber_project = FALSE
#' )
#' }

#' @export

createMetProject <- function(
    project_Name,
    project_Dir,
    start_Date,
    end_Date,
    
    surf_WBAN,
    surf_USAF,
    surf_Call,
    surf_Latitude,
    surf_Longitude,
    surf_Elevation,
    surf_AnenometerHeight,
    surf_UTC,
    
    ifg,
    
    ua_WMO = NULL,
    ua_WBAN = NULL,
    ua_IGRA_zip,
    ua_IGRA_ext = NULL,
    ua_elevation,
    ua_UTC,
    ua_Latitude,
    ua_Longitude,
    
    lc_File,
    lc_Type,
    imp_File = NULL,
    imp_Type = NULL,
    cnpy_File = NULL,
    cnpy_Type = NULL,
    
    as_Snow,
    as_Arid,
    as_Moisture,
    as_Airport,
    as_Winter_NS,
    as_Winter_WS,
    as_Spring,
    as_Summer,
    as_Autumn,
    
    as_nsector = NULL,
    as_radius = 1,
    
    nws_sector = data.frame(
      index = 1:12,
      start = seq(0, 330, 30),
      end   = seq(30, 360, 30),
      ap    = rep("AP", 12)
    ),
    
    os_sector = data.frame(
      index = 1:12,
      start = seq(0, 330, 30),
      end   = seq(30, 360, 30),
      ap    = rep("NONAP", 12)
    ),
    
    onsite_Latitude = NULL,
    onsite_Longitude = NULL,
    onsite_Fstring = NULL,
    
    clobber_project = FALSE
) {
  
  ## ---- basic warnings / deprecations ----------------------------------
  if (!is.null(as_nsector)) {
    warning("as_nsector is deprecated and ignored unless AERSURFACE < 13016")
  }
  
  if (any(!is.null(c(ua_WMO, ua_WBAN)))) {
    warning("FSL upper-air data no longer available. WBAN/WMO retained for compatibility.")
  }
  
  ## ---- project file path ----------------------------------------------
  rmetFile <- file.path(
    project_Dir,
    paste0(make.names(project_Name), ".rds")
  )
  
  ## ---- existing project logic -----------------------------------------
  if (file.exists(rmetFile)) {
    
    if (!clobber_project) {
      message("Loading existing rmet project: ", rmetFile)
      return(readRDS(rmetFile))
    } else {
      warning(
        "Clobbering existing rmet project object (downloaded data preserved): ",
        rmetFile
      )
      unlink(rmetFile)
      # fall through to recreate
    }
  }
  
  ## ---- directory setup ------------------------------------------------
  if (!file.exists(project_Dir)) {
    dir.create(project_Dir, recursive = FALSE)
  }
  
  years <- seq(
    as.integer(format(start_Date, "%Y")),
    as.integer(format(end_Date, "%Y"))
  )
  
  for (yy in years) {
    yy_dir <- file.path(project_Dir, yy)
    if (!file.exists(yy_dir)) dir.create(yy_dir, recursive = FALSE)
  }
  
  dir.create(file.path(project_Dir, "aersurface"), showWarnings = FALSE)
  dir.create(file.path(project_Dir, "preprocessed_data"), showWarnings = FALSE)
  
  ## ---- date checks ----------------------------------------------------
  stopifnot(inherits(start_Date, "POSIXct"))
  stopifnot(inherits(end_Date,   "POSIXct"))
  if (start_Date >= end_Date) stop("start_Date must be before end_Date")
  
  start_DateUTC <- as.numeric(format(start_Date, "%Y%m%d", tz = "UTC"))
  end_DateUTC   <- as.numeric(format(end_Date,   "%Y%m%d", tz = "UTC"))
  
  ## ---- surface station validation -------------------------------------
  surf_WBAN <- sprintf("%05d", as.numeric(surf_WBAN))
  
  surf_Hist <- get("surfhist", envir = rmetData)
  
  stopifnot(any(
    trimws(surf_Hist$USAF) == surf_USAF &
      trimws(surf_Hist$WBAN) == surf_WBAN
  ))
  
  TD3505df <- dplyr::filter(
    surf_Hist,
    trimws(USAF) == surf_USAF,
    trimws(WBAN) == surf_WBAN
  )
  
  if (nrow(TD3505df) > 1) {
    stop("More than one surface station matched USAF/WBAN")
  }
  
  if (as.numeric(TD3505df$BEGIN) > start_DateUTC)
    stop("start_Date precedes station BEGIN date")
  
  if (as.numeric(as.POSIXct(TD3505df$END, tz = "UTC")) < end_Date)
    stop("end_Date exceeds station END date")
  
  ## ---- NOAA file checks -----------------------------------------------
  td3505_noaa <- rmet2:::checkTD3505(start_Date, end_Date, surf_USAF, surf_WBAN)
  td6405_noaa <- try(rmet2:::checkTD6405(start_Date, end_Date, surf_Call))
  td6401_noaa <- try(rmet2:::checkTD6401(start_Date, end_Date, surf_Call))
  
  ## ---- initialize project object --------------------------------------
  aermetProject <- list(
    
    project_Name = project_Name,
    project_Dir  = project_Dir,
    start_Date   = start_Date,
    end_Date     = end_Date,
    
    surf_WBAN    = surf_WBAN,
    surf_USAF    = surf_USAF,
    surf_Call    = surf_Call,
    surf_UTC     = surf_UTC,
    surf_Latitude  = surf_Latitude,
    surf_Longitude = surf_Longitude,
    surf_Elevation = surf_Elevation,
    surf_AnenometerHeight = surf_AnenometerHeight,
    
    td3505_noaa = td3505_noaa,
    td6405_noaa = td6405_noaa,
    td6401_noaa = td6401_noaa,
    
    ua_WMO        = ua_WMO,
    ua_WBAN       = ua_WBAN,
    ua_IGRA_zip   = ua_IGRA_zip,
    ua_IGRA_ext   = ua_IGRA_ext,
    ua_elevation  = ua_elevation,
    ua_UTC        = ua_UTC,
    ua_Latitude   = ua_Latitude,
    ua_Longitude  = ua_Longitude,
    
    onsite_Latitude  = onsite_Latitude,
    onsite_Longitude = onsite_Longitude,
    onsite_Fstring   = onsite_Fstring,
    
    ifg = ifg,
    
    aersurface = list(
      inputFiles = list(
        lc_File   = lc_File,
        lc_Type   = lc_Type,
        imp_File  = imp_File,
        imp_Type  = imp_Type,
        cnpy_File = cnpy_File,
        cnpy_Type = cnpy_Type
      ),
      surfaceChar = list(
        as_Snow     = as_Snow,
        as_Arid     = as_Arid,
        as_Moisture = as_Moisture,
        as_Airport  = as_Airport,
        as_nsector  = as_nsector,
        as_radius   = as_radius,
        nws_sector  = nws_sector,
        os_sector   = os_sector
      ),
      surfaceSeason = list(
        as_Winter_NS = as_Winter_NS,
        as_Winter_WS = as_Winter_WS,
        as_Spring    = as_Spring,
        as_Summer    = as_Summer,
        as_Autumn    = as_Autumn
      )
    ),
    
    inputText = list(
      aerminute = NULL,
      aersurface = list(surface = NULL, onsite = NULL),
      aermet = NULL
    ),
    
    inputFiles = list(
      aerminute = NULL,
      aersurface = list(surface = NULL, onsite = NULL),
      aermet = list(s1 = NULL, s2 = NULL, s3 = NULL)
    ),
    
    output = list(
      aerminute = NULL,
      aersurface = NULL,
      aermet = list(s1 = NULL, s2 = NULL, s3 = NULL)
    )
  )
  
  ## ---- initialize state -----------------------------------------------
  aermetProject$state <- list(
    project = list(created = TRUE),
    data = list(),
    processing = list(),
    qa = list()
  )
  
  class(aermetProject) <- "rmet"
  
  ## ---- save project ---------------------------------------------------
  saveRDS(aermetProject, rmetFile)
  
  return(aermetProject)
}