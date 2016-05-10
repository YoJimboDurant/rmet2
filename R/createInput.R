#' @title createInput
#' 
#' @description
#' \code{createInput} 
#' 
#' @export



createInput.rmet <- function(rmetObj, type=c("aerminute", "aersurface", "aermet1")){
 # type <- match.arg(type)
  stopifnot(class(rmetObj) =="rmet")
  stopifnot(all(type %in% c("aerminute", "aersurface", "aermet1")))
  
  if("aerminute" %in% type){
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
          
          
          aerminInp <- gsub("!start_Date!", paste(startMonth, loc_years[[i]]), aerminInp)
          aerminInp <- gsub("!end_Date!", paste(endMonth, loc_years[[i]]), aerminInp)
          
          oneMinFiles <- path.expand(oneMinFiles)
          oneMinFiles <- paste0("\"", oneMinFiles, "\"")
          aerminInp <- gsub("!minFiles!", paste(oneMinFiles, collapse = "\n"), aerminInp)
          
          if(!is.null(rmetObj$td6401_noaa[[i]])){
            fiveMinFiles <- paste(destDir, gsub("^.*/", "", 
                                             rmetObj$td6401_noaa[[i]]), sep="/")
            
            fiveMinFiles <- path.expand(fiveMinFiles)
            stopifnot(all(file.exists(fiveMinFiles)))
            
            fiveMinFiles <- paste0("\"", fiveMinFiles, "\"")
            aerminInp <- gsub("!min5Files!", paste(path.expand(fiveMinFiles), collapse = "\n"), 
                              aerminInp)
          }  
            hourFile <- paste0(destDir, "/AM_", "1min_", loc_years[[i]], ".dat")
            summFile <- paste0(destDir, "/AM_", "1min_", loc_years[[i]], "_summ.dat")
            compFile <- paste0(destDir, "/AM_", "1min_", loc_years[[i]], "_comp.dat")
            
            aerminInp <- gsub("!hourfile_AM!", prepareThePath(hourFile), aerminInp)
            aerminInp <- gsub("!summfile_AM!", prepareThePath(summFile), aerminInp)
            aerminInp <- gsub("!compfile_AM!", prepareThePath(compFile), aerminInp)
            
          
          
          if(!is.null(rmetObj$td3505_noaa[[i]])){
            destDir <- paste(rmetObj$project_Dir, loc_years[[i]], sep="/")
            hourFile <- paste0(destDir, "/S", rmetObj$surf_USAF, 
                               rmetObj$surf_WBAN,
                                "_",loc_years[[i]], ".ISH")
            hourFile <-prepareThePath(hourFile)
            aerminInp <- gsub("!surfFile!", hourFile, aerminInp)
          }else{
            aerminInp <- gsub("!surfFile!", "", aerminInp)
          }
          
          aerminInp
        })
        
      names(aerminInputFiles) <- loc_years
      rmetObj$inputText$aerminute <- aerminInputFiles
      
      rmetObj$outputFiles$aerminute <- sapply(seq_along(loc_years), function(i){
        destDir <- paste(rmetObj$project_Dir, loc_years[[i]], sep="/")
        paste0(destDir, "/AM_", "1min_", loc_years[[i]], ".dat")
      }
        )
  }
  
  if("aersurface" %in% type){
  
  
    stopifnot(file.exists(rmetObj$aersurface$inputFiles$lc_File))
    as_inpFile <- list(
      lcFile = prepareThePath(path.expand(rmetObj$aersurface$inputFiles$lc_File)),
      outFile = prepareThePath(paste(path.expand(rmetObj$project_Dir), "aersurface.out", sep="/")),
      latLon = "LATLON",
      lat = rmetObj$surf_Latitude,
      long = rmetObj$surf_Longitude,
      proj = "NAD83",
      radius = rmetObj$aersurface$surfaceChar$as_radius,
      sector = "Y",
      nSector = rmetObj$aersurface$surfaceChar$as_nsector,
      timePeriod = "M",
      asSnow=rmetObj$aersurface$surfaceChar$as_Snow,
      specify = "Y",
      asWinterNS = rmetObj$aersurface$surfaceSeason$as_Winter_NS,
      asWinterWS = rmetObj$aersurface$surfaceSeason$as_Winter_WS,
      asSpring = rmetObj$aersurface$surfaceSeason$as_Spring,
      asSummer = rmetObj$aersurface$surfaceSeason$as_Summer,
      asAutum = rmetObj$aersurface$surfaceSeason$as_Autumn,
      airport = rmetObj$aersurface$surfaceChar$as_Airport,
      arid = rmetObj$aersurface$surfaceChar$as_Arid,
      asMoisture =rmetObj$aersurface$surfaceChar$as_Moisture
    )
    
    if(as_inpFile$asSnow == "N") {
      if(grepl("[[:digit:]]",as_inpFile$asWinterWS)){
        stop(paste("AERSURFACE Option No Snow but Winter With Snow months in input file", as_inpFile$asWinterWS))
      }
      
      
      
      as_inpFile$asWinterWS <- NULL
    }
    
    if(as_inpFile$asSnow == "Y") {
      if(!grepl("[[:digit:]]",as_inpFile$asWinterWS)){
        stop(paste("AERSURFACE Option Yes Snow but there are no Winter With Snow months in input file", as_inpFile$asWinterWS))
      }
      
      if(as_inpFile$arid == "Y"){
        stop(paste("AERSURFACE incompatable options - Snow = Y and arid =", as_inpFile$arid))
      }
      
      as_inpFile$arid <- NULL
    }
    
    rmetObj$inputText$aersurface <- paste(as_inpFile, collapse="\n")
    
  }
  
  if("aermet1" %in% type){
    stopifnot(is(rmetObj)=="rmet")
    loc_years <- locYears(rmetObj)
    fslFiles <- paste(path.expand(rmetObj$project_Dir), "/", loc_years,"/", rmetObj$ua_WMO,".FSL", sep="")
    uaexoutFiles <- paste(path.expand(rmetObj$project_Dir), loc_years, "UAEXOUT.DAT", sep="/")
    uaqaoutFiles <- paste(path.expand(rmetObj$project_Dir), loc_years, "UAQAOUT.DAT", sep="/")
    stationID <- paste0(rmetObj$surf_USAF, rmetObj$surf_WBAN)
    ishFiles <- paste0(path.expand(rmetObj$project_Dir), "/", loc_years,"/","S",stationID,"_",loc_years, ".ISH")
    ishqaoutFiles <- paste(path.expand(rmetObj$project_Dir), loc_years, "SFQAOUT.DAT", sep="/")
    destDir <- paste(path.expand(rmetObj$project_Dir), loc_years, sep="/")
    sxDates <- paste0(loc_years,"/1/1")
    exDates <- paste0(loc_years,"/12/31")
    
    sxDates[[1]] <- paste(loc_years[[1]], 
                          as.numeric(format(rmetObj$start_Date, "%m")),
                          as.numeric(format(rmetObj$start_Date, "%d")), sep="/")
    
    exDates[[length(exDates)]] <- paste(loc_years[[length(loc_years)]], 
                                        as.numeric(format(rmetObj$end_Date, "%m")),
                                        as.numeric(format(rmetObj$end_Date, "%d")), sep="/")
    
    xdates <- paste(sxDates,exDates, sep=" TO ")
    
    S1 <- lapply(seq_along(loc_years), function(i) {list(
      JOB=list(
        "JOB",
        "**",
        paste0("  REPORT     ", prepareThePath(paste(destDir[[i]], "S1.RPT", sep="/"))),
        paste0("  MESSAGES   ", prepareThePath(paste(destDir[[i]], "S1.MSG", sep="/")))),
      UPPERAIR=list(
        "UPPERAIR",
        paste("**          Upper air data for WMO:",
              rmetObj$ua_WMO ,"FSL format"),
        paste0("  DATA      ", prepareThePath(fslFiles[[i]]), " FSL"),
        paste0("  EXTRACT   ", prepareThePath(uaexoutFiles[[i]])),
        "  AUDIT     UATT UAWS UALR",
        paste0("  XDATES    ", xdates[[i]]),
        paste0("  LOCATION  ", rmetObj$ua_WMO,"  ",convertLat(rmetObj$ua_Latitude),"   ",
               convertLong(rmetObj$ua_Longitude),"   ",-(rmetObj$ua_UTC)),
        paste0("  QAOUT     ", prepareThePath(uaqaoutFiles[[i]])),
        "  MODIFY"),
      SURFACE = list(
        "SURFACE",
        paste("**          Surface air data for WBAN:",
              rmetObj$surf_WBAN ,"ISHD format"),
        paste0("  EXTRACT       ", prepareThePath(ishFiles[[i]])),
        "  AUDIT          SLVP PRES CLHT TSKC PWTH ASKY HZVS RHUM",
        "  RANGE TMPD  -300  <=  450  999",
        paste0("  XDATES        ", xdates[[i]]),
        paste("  LOCATION     ", rmetObj$surf_WBAN, convertLat(rmetObj$surf_Latitude), 
              convertLong(rmetObj$surf_Longitude), -(rmetObj$surf_UTC), 
              rmetObj$surf_Elevation),
        paste0("  QAOUT        ", prepareThePath(ishqaoutFiles[[i]])),
        "  NO_MISSING           ASKY TSKC"
      )
    )
      
      
    })
    names(S1) <- loc_years
    S1 <- lapply(S1, function(lx){
      paste(lapply(lx, function(x) paste(x, collapse="\n")), collapse="\n")
    })
    
    rmetObj$inputText$aermet$s1 <- S1
    
    
  }
  

  
return(rmetObj)

}




# 
# writeS1 <- function(rmetObj){
#   
#             
#   
#   
#   cat(paste("  DATA      ",fslFile,"  FSL\n", sep=""), file=inpFile, append=TRUE)
#   cat("  EXTRACT   UAEXOUT.DAT\n", file=inpFile, append=TRUE)
#   cat(c("  AUDIT     ", auditUA,"\n"), file=inpFile, append=TRUE)
#   cat(paste("  XDATES    ",xdates,"\n", sep=""), file=inpFile, append=TRUE)
#   cat(paste("  LOCATION  ", ua.WBAN,"  ",ua.lat,"   ", ua.long,"   ",tzCor_ua, "\n", sep=""),file=inpFile, append=TRUE)
#   cat("  QAOUT     UAQAOUT.DAT\n", file=inpFile, append=TRUE)
#   cat("  MODIFY\n", file=inpFile, append=TRUE)
#   
#             )
#             
# 
# 
# 
#         )
# 
#     year, stationData, tzCor_surf, tzCor_ua, NO_MISSING=c("ASKY", "TSKC"),
#                     startMonth=1, startDay=1, endMonth=12, endDay=31,
#                     auditSurf = c("SLVP", "PRES", "CLHT", 
#                                  "TSKC", "PWTH", "ASKY",
#                                  "HZVS", "DPTP", 
#                                  "RHUM"),
#                     minTMPD = -300,  maxTMPD= 450,
#                     auditUA = c("UATT", "UAWS","UALR"),
#                     correct=TRUE){
#   
#   
#   
#   inpFile <- paste("./", year, "/S1.INP", sep="")
#   
#   #job
#   cat("JOB\n", file=inpFile)
#   cat("**\n", file=inpFile, append=TRUE)
#   cat("  REPORT     S1.RPT\n", file=inpFile, append=TRUE)
#   cat("  MESSAGES   S1.MSG\n", file=inpFile, append=TRUE)
#   
#   # upperair
#   myfiles <- dir(paste("./", year, sep=""))
#   fslFile <- grep("[.]FSL", myfiles, value=TRUE)
#   if(length(fslFile) !=1) stop(paste("\nMISSING or DUPLICATE FSL file in year", year))
#   
#   if(correct){
#     #add 24 hours to end date to cover tz overlaps
#     theDate <- lubridate::ymd_h(paste(year,endMonth,endDay, 23)) + tzCor_surf * 60 * 60
#     endYear <- format(theDate, "%Y")
#     endMonth <- format(theDate, "%m")
#     endDay <- format(theDate, "%d")
#   
#     xdates <- paste0(year,"/",startMonth,"/", startDay," TO ", endYear,"/",endMonth,"/",endDay)
#     
#   }else{
#   xdates <- paste0(year,"/",startMonth,"/", startDay," TO ", year,"/",endMonth,"/",endDay)
#   }
#   
#   ua.WBAN <- stationData$ua.WBAN[stationData$year==year]
#   
#   ua.lat <- stationData$ua.lat[stationData$year==year]
#   ua.long <- stationData$ua.long[stationData$year==year]
#   
#   cat("UPPERAIR\n", file=inpFile, append=TRUE)
#   cat(paste("**          Upper air data for WBAN:",
#             ua.WBAN ,"FSL format\n"),
#       file=inpFile, append=TRUE)
#   
#   
#   cat(paste("  DATA      ",fslFile,"  FSL\n", sep=""), file=inpFile, append=TRUE)
#   cat("  EXTRACT   UAEXOUT.DAT\n", file=inpFile, append=TRUE)
#   cat(c("  AUDIT     ", auditUA,"\n"), file=inpFile, append=TRUE)
#   cat(paste("  XDATES    ",xdates,"\n", sep=""), file=inpFile, append=TRUE)
#   cat(paste("  LOCATION  ", ua.WBAN,"  ",ua.lat,"   ", ua.long,"   ",tzCor_ua, "\n", sep=""),file=inpFile, append=TRUE)
#   cat("  QAOUT     UAQAOUT.DAT\n", file=inpFile, append=TRUE)
#   cat("  MODIFY\n", file=inpFile, append=TRUE)
#   #surface
#   ishFile <- grep("[.]ISH", myfiles, value=TRUE)
#   if(length(ishFile) != 1) stop(paste("\nMISSING or DUPLICATE ISH file in year", year))
#   
#   
#   surf.WBAN <- sprintf("%05d", as.numeric(stationData$surf.wban[stationData$year==year]))
#   surf.lat <- stationData$surf.lat[stationData$year==year]
#   surf.long <- stationData$surf.long[stationData$year==year]
#   surf.elev <- stationData$surf.elev[stationData$year==year]
#   
#   cat("SURFACE\n", file=inpFile, append=TRUE)
#   cat(paste("**          Surface air data for WBAN:",
#             surf.WBAN ,"ISHD Format\n"),
#       file=inpFile, append=TRUE)
#   cat(paste("  DATA      ",ishFile,"  ISHD\n", sep=""), file=inpFile, append=TRUE)
#   cat("  EXTRACT   SFEXOUT.DAT\n", file=inpFile, append=TRUE)
#   cat(c("  AUDIT     ", auditSurf,"\n"), file=inpFile, append=TRUE)
#   cat(c("  RANGE TMPD ", minTMPD, " <=  ", maxTMPD, " 999\n"), file=inpFile, append=TRUE)
#   cat(paste("  XDATES    ",xdates,"\n", sep=""), file=inpFile, append=TRUE)
#   cat(paste("  LOCATION  ", surf.WBAN,"  ",surf.lat," ", surf.long," ",tzCor_surf, "  ", surf.elev, "\n", sep=""),file=inpFile, append=TRUE)
#   cat("  QAOUT     SFQAOUT.DAT\n", file=inpFile, append=TRUE)
#   if(!is.null(NO_MISSING)) cat(c("  NO_MISSING   ",NO_MISSING), file=inpFile, append=TRUE)
#   
# }
