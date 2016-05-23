#' @title createInput
#' 
#' @description
#' \code{createInput} 
#' 
#' @export



createInput.rmet <- function(rmetObj, type=c("aerminute", "aersurface", "aermet1",
                                             "aermet2", "aermet3"), ustar=TRUE){
 # type <- match.arg(type)
  stopifnot(class(rmetObj) =="rmet")
  stopifnot(all(type %in% c("aerminute", "aersurface", "aermet1", "aermet2", "aermet3")))
  missAerminute <- all(c(sapply(rmetObj$td6401_noaa, function(x) length(x)==0),
                       sapply(rmetObj$td6405_noaa, function(x) length(x)==0)))
  if(missAerminute){
    warning("No TD6401 Files and TD6405 Files - no aerminute inputs created!")
    type <- type[type !="aerminute"]
  }
  
  loc_years <- locYears(rmetObj)
  
  xtz <- lubridate::tz(rmetObj$start_Date)
  
  fslFiles <- paste(path.expand(rmetObj$project_Dir), "/", loc_years,"/", rmetObj$ua_WMO,".FSL", sep="")
  uaexoutFiles <- paste(path.expand(rmetObj$project_Dir), loc_years, "UAEXOUT.DAT", sep="/")
  uaqaoutFiles <- paste(path.expand(rmetObj$project_Dir), loc_years, "UAQAOUT.DAT", sep="/")
  stationID <- paste0(rmetObj$surf_USAF, rmetObj$surf_WBAN)
  ishFiles <- paste0(path.expand(rmetObj$project_Dir), "/", loc_years,"/","S",stationID,"_",loc_years, ".ISH")
  ishqaoutFiles <- paste(path.expand(rmetObj$project_Dir), loc_years, "SFQAOUT.DAT", sep="/")
  destDir <- paste(path.expand(rmetObj$project_Dir), loc_years, sep="/")
  sxDates2 <- sxDates <- paste0(loc_years,"/1/1")

  exDates <- paste0(as.numeric(loc_years)+1,"/1/1")
  exDates2 <- paste0(as.numeric(loc_years),"/12/31")
  
  sxDates[[1]] <- paste(format(rmetObj$start_Date, "%Y", tz="UTC"), 
                        as.numeric(format(rmetObj$start_Date, "%m", tz="UTC")),
                        as.numeric(format(rmetObj$start_Date, "%d", tz="UTC")), sep="/")
  
  sxDates2[[1]] <- paste(format(rmetObj$start_Date, "%Y", tz=xtz), 
                        as.numeric(format(rmetObj$start_Date, "%m", tz=xtz)),
                        as.numeric(format(rmetObj$start_Date, "%d", tz=xtz)), sep="/")
  
  exDates[[length(exDates)]] <- paste(as.numeric(format(rmetObj$end_Date, "%Y", tz="UTC")), 
                                      as.numeric(format(rmetObj$end_Date, "%m", tz="UTC")),
                                      as.numeric(format(rmetObj$end_Date, "%d", tz="UTC")), sep="/")
  
  exDates2[[length(exDates2)]] <- paste(as.numeric(format(rmetObj$end_Date, "%Y", tz=xtz)), 
                                        as.numeric(format(rmetObj$end_Date, "%m", tz=xtz)),
                                        as.numeric(format(rmetObj$end_Date, "%d", tz=xtz)), sep="/")
  
  
  xdates <- paste(sxDates,exDates, sep=" TO ")
  xdates2 <- paste(sxDates, exDates2, sep=" TO ")
  
  if("aerminute" %in% type){
        print("Writing AERMINUTE inpute files:\n")
        
        
        
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
            hourFile <- prepareThePath(paste0(destDir, "/AM_", "1MIN_", loc_years[[i]], ".DAT"))
            summFile <- prepareThePath(paste0(destDir, "/AM_", "1MIN_", loc_years[[i]], "_summ.DAT"))
            compFile <- prepareThePath(paste0(destDir, "/AM_", "1MIN_", loc_years[[i]], "_comp.DAT"))
            
            aerminInp <- gsub("!hourfile_AM!", hourFile, aerminInp)
            aerminInp <- gsub("!summfile_AM!", summFile, aerminInp)
            aerminInp <- gsub("!compfile_AM!", compFile, aerminInp)
            
          
          
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
        paste0(destDir, "/AM_", "1MIN_", loc_years[[i]], ".DAT")
      }
        )
  }
  
  if("aersurface" %in% type){
  
  
    stopifnot(file.exists(rmetObj$aersurface$inputFiles$lc_File))
    as_inpFile <- list(
      lcFile = prepareThePath(path.expand(rmetObj$aersurface$inputFiles$lc_File)),
      outFile = prepareThePath(paste(path.expand(rmetObj$project_Dir), "aersurface", "aersurface.out", sep="/")),
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
      if(!is.null(as_inpFile$asWinterWS)){
        stop(paste("AERSURFACE Option No Snow but Winter With Snow months in input file", as_inpFile$asWinterWS))
      }
      
      
      
      as_inpFile$asWinterWS <- NULL
    }
    
    if(as_inpFile$asSnow == "Y") {
      if(is.null(as_inpFile$asWinterWS)){
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
        paste0("  DATA       ", prepareThePath(ishFiles[[i]])," ISHD"),
        paste0("  EXTRACT    ", prepareThePath(paste(destDir[[i]],"SFEXOUT.DAT",sep="/"))),
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
  
  
  if("aermet2" %in% type){
    S2 <- lapply(seq_along(loc_years), function(i) {
      
      
      
        list(
        JOB=list(
          "JOB",
          "**",
          paste0("  REPORT     ", prepareThePath(paste(destDir[[i]], "S2.RPT", sep="/"))),
          paste0("  MESSAGES   ", prepareThePath(paste(destDir[[i]], "S2.MSG", sep="/")))),
        UPPERAIR=list(
          "UPPERAIR",
          "**",
          paste0("  QAOUT      ", prepareThePath(paste(destDir[[i]],"UAQAOUT.DAT",sep="/")))
        ),
        SURFACE = list(
          "SURFACE",
          "**",
           paste0("  QAOUT        ", prepareThePath(ishqaoutFiles[[i]])),
          if(!missAerminute){
           paste0("  ASOS1MIN        ", prepareThePath(paste0(destDir[[i]], "/AM_", "1MIN_", 
                                                              loc_years[[i]], ".DAT")))
          }else{"** MISSING AERMINUTE FILES"}
          
        ),
        MERGE = list(
          "MERGE",
          "**",
          paste0("  OUTPUT        ", prepareThePath(paste0(destDir[[i]], "/AMS2_ISHD.MRG"))),
          paste0("  XDATES        ", xdates2[[i]])
          
        )
      )
    })
    
    S2 <- lapply(S2, function(lx){
      paste(lapply(lx, function(x) paste(x, collapse="\n")), collapse="\n")
    })
    
    rmetObj$inputText$aermet$s2 <- S2
    
  }
  
  if("aermet3" %in% type){
    S3 <- lapply(seq_along(loc_years), function(i) {
      
      
      
      list(
        JOB=list(
          "JOB",
          "**",
          paste0("  REPORT     ", prepareThePath(paste(destDir[[i]], "S3.RPT", sep="/"))),
          paste0("  MESSAGES   ", prepareThePath(paste(destDir[[i]], "S3.MSG", sep="/")))),
        METPREP=list(
          "METPREP",
          "**",
          paste0("  XDATES        ", xdates2[[i]]),
          paste0("  DATA          ", prepareThePath(paste(destDir[[i]], "AMS2_ISHD.MRG", sep="/"))),
          "  METHOD   REFLEVEL SUBNWS",
          "  METHOD   WIND_DIR  RANDOM",
          if(ustar) {"  METHOD   STABLEBL ADJ_U*"
          },
          paste0("  NWS_HGT    ", "WIND ", rmetObj$surf_AnenometerHeight),
          paste0("  OUTPUT        ", prepareThePath(paste(destDir[[i]], paste0("AM_",loc_years[[i]],".SFC"),
                                                          sep="/"))),
          paste0("  PROFILE        ", prepareThePath(paste(destDir[[i]], paste0("AM_",loc_years[[i]],".PFL"),
                                                          sep="/")))
        )
      )
      
          
    })
    
    S3 <- lapply(S3, function(lx){
      paste(lapply(lx, function(x) paste(x, collapse="\n")), collapse="\n")
    })
    
    rmetObj$inputText$aermet$s3 <- S3
    
  }

  
return(rmetObj)

}


