#' @title createMetProject
#' 
#' @description
#' \code{createMetProject} Creates basic aermet project, specifying 
#' the directory, surface station (surf), onsite station (on), upper air (ua) station,
#' landuse data, start date and ending date.
#' 
#' The function is designed to perform basic checks to make sure that 
#' the data are valid (e.g. WBAN and USAF id's match \url{http://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.txt})
#' 
#' @export

createMetProject <- function(project_Name,
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
                             ifg_Call,
                             ua_WMO,
                             ua_UTC,
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
                             as_Autumn
                             
                             ){
  
  #check if project directory exists
  if(!file.exists(project_Dir)){
    dir.create(project_Dir, recursive = TRUE)
  }
  
  
  
  
  loc_years <- seq(as.numeric(format(start_Date, "%Y")), as.numeric(format(end_Date, "%Y")))
  lapply(seq_along(loc_years), function(i){
    theDir <-paste(project_Dir, loc_years[i], sep="/")
    if(!file.exists(theDir)){
      dir.create(theDir, recursive = TRUE)
    }
  })
  
 # R packages that use S3 method
  
  #create preprocessed_data directory
  prepocessed_data_dir <- paste(project_Dir, "preprocessed_data", sep="/")
  if(!file.exists(prepocessed_data_dir)){
    dir.create(prepocessed_data_dir, recursive = TRUE)
  }
  
  
  #check date/time object for start/end time
  stopifnot(all(c("POSIXct", "POSIXt") %in% class(start_Date)))
  stopifnot(all(c("POSIXct", "POSIXt") %in% class(end_Date)))
  
  #convert TZ to UTC since we are using TD3505 data for 
  #and it is UTC.
  
  start_DateUTC <- format(start_Date, "%Y%m%d", tz="UTC")
  end_DateUTC <- format(end_Date, "%Y%m%d", tz="UTC")
  
  # fix WBAN to 5 digits
  surf_WBAN <- sprintf("%05d",
           as.numeric(surf_WBAN))
  
  
  #check surface station exists and is within correct dates.
    stopifnot(any(grepl(paste0("^", surf_USAF, " +", surf_WBAN), get("surf_Hist", envir=rmetData))))
    
    TD3505Line <- grep(paste0("^", surf_USAF, " +", surf_WBAN), get("surf_Hist", envir=rmetData), value=TRUE)
    #check dates
    if(length(TD3505Line) > 1) stop(paste(" Identified more than 1 surface station:\n", paste(TD3505Line, collapse = "\n")))
    TD3505df <- read.fwf(textConnection(TD3505Line), widths = c(6,-1,5, -1,
                                                                29, -1, 2, -3,
                                                                2, -1, 4, -2, 7,
                                                                -1, 7, -1, 8, -1, 8, 
                                                                -1, 8))
    names(TD3505df) <- c("USAF", "WBAN", "STATION NAME", "CTRY", "ST",  
                         "CALL",  "LAT", "LON",  "ELEV(M)",  "BEGIN", "END")
    TD3505df$BEGIN <- lubridate::ymd(TD3505df$BEGIN)
    TD3505df$END <- lubridate::ymd(TD3505df$END)
    
    if(as.numeric(start_DateUTC) > as.numeric(end_DateUTC)) 
      stop("Surface dates data error: \n start_Date before end_Date!")
    
    if(as.numeric(TD3505df$BEGIN) > as.numeric(start_Date)) 
      stop(paste("NOAA TD3505 data error: \n start_Date before BEGIN for station (", TD3505df$BEGIN, ") UTC"))
    if(as.numeric(as.POSIXct(TD3505df$END, tz="UTC")) < as.numeric(end_Date)) 
      stop(paste("NOAA TD3505 data error: \n end_Date after END for station (", TD3505df$END, ") UTC"))
  
  #Here we identify which files we need to download data from noaa database
    td3505_noaa <- rmet2:::checkTD3505(start_Date, end_Date, surf_USAF, surf_WBAN)
    td6405_noaa <- rmet2:::checkTD6405(start_Date, end_Date, surf_Call)
    td6401_noaa <- rmet2:::checkTD6401(start_Date, end_Date, surf_Call)
  
    
  #inputFiles
    inputFiles <-list(aerminute=NULL, aersurface = NULL, aermet= NULL)
  
  aermetProject <- list(project_Name = project_Name,
                        project_Dir = project_Dir, 
                        start_Date = start_Date, 
                        end_Date = end_Date,
                        surf_WBAN = surf_WBAN,
                        surf_USAF = surf_USAF,
                        surf_Call = surf_Call,
                        surf_UTC = surf_UTC,
                        td3505_noaa = td3505_noaa,
                        td6405_noaa = td6405_noaa,
                        td6401_noaa = td6401_noaa,
                        ua_WMO = ua_WMO,
                        ua_UTC = ua_UTC,
                        ifg = ifg,
                        amInp = NULL,
                        inputFiles =inputFiles)
  
  class(aermetProject) <- "rmet"
  return(aermetProject)
}