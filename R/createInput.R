#' @title createInput
#' 
#' @description
#' \code{createInput} 
#' 
#' @export



createInput.rmet <- function(rmetObj, type=c("aerminute", "aersurface")){
 # type <- match.arg(type)
  stopifnot(class(rmetObj) =="rmet")
  
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
            
            hourFile <- paste0(destDir, "/AM_", "1min_", loc_years[[i]], ".dat")
            summFile <- paste0(destDir, "/AM_", "1min_", loc_years[[i]], "_summ.dat")
            compFile <- paste0(destDir, "/AM_", "1min_", loc_years[[i]], "_comp.dat")
            
            aerminInp <- gsub("!hourfile_AM!", prepareThePath(hourFile), aerminInp)
            aerminInp <- gsub("!summfile_AM!", prepareThePath(summFile), aerminInp)
            aerminInp <- gsub("!compfile_AM!", prepareThePath(compFile), aerminInp)
            
          }
          
          if(!is.null(rmetObj$td3505_noaa[[i]])){
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
  

  
return(rmetObj)

}
