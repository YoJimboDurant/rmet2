#' createInput 
#' 
#' @description
#' 
#' createInput modifies an \code{rmet} object witht the input runstreams to run
#' AERMINUTE, AERSURFACE and AERMET based on the contents of the \code{rmetObj}.
#' Creation of the inputs can be run individually or as a vector using the 
#' \code{type} argument.
#' 
#' @returns \code{rmet} object.
#' 
#' @param rmetObj an \code{rmet} class object, created by \code{createMetProject} function.

#' @param type is vector of input files needed:
#'
#'   \itemize{ \item \dQuote{aerminute}.  
#'  
#'  This updates \code{rmetObj$inputText$aerminute} with an AERMINUTE
#'  runstream text. The function also updates \code{rmetObj$outputFiles$aerminute}
#'  with the location of the .DAT output files from AERMINUTE. 
#'  
#'  The \code{aerminTemplate} modifies the \code{aerminInp} template, to create input text 
#'  for EPA AERMINUTE for each year of the project. The file locations of \code{rmetObj$td6405_noaa}, 
#'  \code{rmetObj$td6401_noaa}, and \code{rmetObj$td3505_noaa} are scanned and 
#'  the template is updated with the information in \code{rmetObj} to update. 
#'  
#'  Once run, the code \code{cat(rmetObj$inputText$aerminute$xxxx)} where xxxx is the
#'  year of the input file, can be used to scrutinize the input text file. 
#'  
#'  If needed, the input template can be scrutinized using 
#'  \code{cat(aerminTemplate, sep = "\n")}
#'  
#'  \item \dQuote{aersurface_nws}
#'  
#'  This creates and appends the runstream text for AERSURFACE for National Weather Service
#'  Sites and appends the runstream to the \code{rmetObj}.
#'  
#'  The text of the runstream is stored in \code{rmetObj$inputText$aersurface$surface}
#'  and can be examined using \code{cat(rmetObj$inputText$aersurface$surface)}.  
#'   
#'  \item \dQuote{aersurface_os}
#'  
#'  This creates and appends the runstream text for AERSURFACE for ONSITE
#'  weather data and appends the runstream to the \code{rmetObj}.
#'  
#'  The text of the runstream is stored in \code{rmetObj$inputText$aersurface$onsite}
#'  and can be examined using \code{cat(rmetObj$inputText$aersurface$onsite)}.
#'    
#'  \item \dQuote{aermet1}
#'  
#'  This updates \code{rmetObj$inputText$aermet$s1$xxxx} where xxxx is the year
#'  of the project with runstreams for AERMET stage 1, and can be examined using 
#'  \code{cat(rmetObj$inputText$aermet$s1$xxxx)}.
#'  
#'  \item \dQuote{aermet23}
#'  
#'  This updates \code{rmetObj$inputText$aermet$s2$xxxx} where xxxx is the year
#'  of the project with runstreams for AERMET stage 2. This is combining the
#'  older stages of 2 and 3 of AERMET.The input runstreat can be examined 
#'  using \code{rmetObj$inputText$aermet$s1$xxxx}.
#'  
#'  \item \dQuote{aermet3}
#'  
#'  This is retained for backward compatibility with older projects.  

#'  
#'  
#'   }
#'   
#' @param ustar TRUE/FALSE should ustar adjustment argument be used
#' @param inputOps list of options for stages 1, 2 and 3. Note that only
#'   options for stage 3 are captured at this point. \dQuote{STABLEBL ADJ_U*} must
#'   be present is the ustar argument is set to TRUE.
#'   
#'  
#' 
#' @export



createInput.rmet <- function(rmetObj, type=c("aerminute", "aersurface_nws",
                                             "aersurface_os",
                                             "aermet1",
                                             "aermet2", "aermet3",
                                             "aermet23"), ustar = TRUE, inputOps = list(S1 =
                                                                                      list(),
                                                                                    S2 = list(),
                                                                                    S3 = list(other = list(
                                                                                      "THRESH_1MIN 0.5"
                                                                                      ),method = list(
                                                                                      "REFLEVEL SUBNWS",
                                                                                      "STABLEBL ADJ_U*"
                                                                                      ))
                                                                                    )
                             ){
 # type <- match.arg(type)
  stopifnot(class(rmetObj) =="rmet")
  stopifnot(all(type %in% c("aerminute", "aersurface", "aersurface_nws", "aersurface_os",
                            "aermet1", "aermet2", "aermet3", "aermet23")))
  missAerminute <- all(c(sapply(rmetObj$td6401_noaa, function(x) length(x)==0),
                       sapply(rmetObj$td6405_noaa, function(x) length(x)==0)))
  if(!is.null(ustar)){
    if("STABLEBL ADJ_U*" %in% inputOps$S3$method & ustar) ustar_problem = FALSE
    if(!"STABLEBL ADJ_U*" %in% inputOps$S3$method & !ustar) ustar_problem = FALSE
    if(!"STABLEBL ADJ_U*" %in% inputOps$S3$method & ustar) ustar_problem = TRUE
    if("STABLEBL ADJ_U*" %in% inputOps$S3$method & !ustar) ustar_problem = TRUE
    if(ustar_problem) stop("Incompatable USTAR arguements in stage 3.")
  } 
  
  if(missAerminute){
    warning("No TD6401 Files and TD6405 Files - no aerminute inputs created!")
    type <- type[type !="aerminute"]
  }
  
  loc_years <- locYears(rmetObj)
  
  xtz <- lubridate::tz(rmetObj$start_Date)
  if(!is.null(rmetObj$ua_WMO)){
  fslFiles <- paste(path.expand(rmetObj$project_Dir), "/", loc_years,"/", rmetObj$ua_WMO,".FSL", sep="")
  } 
  
  if(is.null(rmetObj$ua_WMO)){
    fslFiles <- rmetObj$ua_IGRA_ext 
  }
    
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
        print("Writing AERMINUTE input text:\n")
        
      
        
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
            hourFile <- paste0(destDir, "/S", rmetObj$surf_USAF, "-", 
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
  
  
  if("aersurface_nws" %in% type){
    
    print("Writing AERSURFACE NWS input text:\n")
    if("aersurface" %in% type) stop("Cannot run AERSURFACE and AERSURFACE_NWS.")
    
    stopifnot(file.exists(rmetObj$aersurface$inputFiles$lc_File))
    stopifnot(!is.null(rmetObj$aersurface$inputFiles$lc_Type))

    control = list(
      "CO STARTING",
      paste("   TITLEONE", rmetObj$project_Name, "AERSURFACE NWS INPUT CONTROL FILE", sep = "  "), 
      paste("   TITLETWO", rmetObj$project_Dir, "Directory", sep = "  "),
      "** Using default options for OPTIONS keyword and parameters", sep = "  ")
    if(is.null(c(
      rmetObj$onsite_Latitude, 
      rmetObj$onsite_Longitude,
      rmetObj$onsite_Fstring)
      )
    ){  
      control <- c(control, "   OPTIONS   PRIMARY  ZORAD")
    }else{
      control <- c(control, "   OPTIONS   SECONDARY  ZORAD") 
    }
  
    control <- c(control, 
                 "   DEBUGOPT  GRID  TIFF",
                 paste("   CENTERLL ", rmetObj$surf_Latitude, rmetObj$surf_Longitude, "NAD83")
    )
      

  
  doUdoWindows <- Sys.info()['sysname'] == "Windows"
  
  
  inputFile_mx <- matrix(rev(rmetObj$aersurface$inputFiles[!sapply(rmetObj$aersurface$inputFiles, is.null)]), ncol = 2, byrow = TRUE)
  
  inputFile_mx[,2] <- paste0("\"", inputFile_mx[,2], "\"")
  dataControl <- paste("   DATAFILE ", apply(inputFile_mx, 1, paste, collapse = "  "))
  
  control <- c(control, dataControl)
  
  control <- c(control,
               "** Use default specified km radius",
               paste("   ZORADIUS ", rmetObj$aersurface$surfaceChar$as_radius)
  )
  
  
  #Create String
  if(any(c(
    rmetObj$aersurface$surfaceChar$as_Snow == "Y",
    rmetObj$surfaceChar$as_Moisture =="W",
    rmetObj$surfaceChar$as_Moisture =="D",
    rmetObj$surfaceChar$as_Arid =="Y"
    ))){
    
    climate <- "   CLIMATE"
    
    climate <- ifelse(is.null(rmetObj$surfaceChar$as_Moisture),  paste(climate, "AVG"),
           ifelse(rmetObj$surfaceChar$as_Moisture =="D", paste(climate, "DRY"), 
                  paste(climate, "WET"))
    )
    
    climate <- ifelse(rmetObj$aersurface$surfaceChar$as_Snow =="Y",
           paste(climate, "SNOW"),
           paste(climate, "NOSNOW"))
    
    climate <- ifelse(rmetObj$aersurface$surfaceChar$as_Arid =="Y",
           paste(climate, "ARID"),
           paste(climate, "NONARID"))

     control <- c(control, climate)
   
    
  }
  
  
  control <- c(
    control, 
    paste("   FREQ_SECT", "MONTHLY", dim(rmetObj$aersurface$surfaceChar$nws_sector)[[1]], "VARYAP", sep = "  "  ),
    "**        index  start    end      "
  )
 
  
  
  freq_dfx <- cbind("   SECTOR  ", 
        rmetObj$aersurface$surfaceChar$nws_sector)
  
  freq_dfx$start <- sprintf("%7.2f", freq_dfx$start)
  freq_dfx$end <- sprintf("%7.2f", freq_dfx$end)
  freq_dfx$ap <- paste("   ", freq_dfx$ap)
  
  control <- c(control, gsub(",", "", apply(freq_dfx, 1, toString)))
  
  
  
  

  
  if(!is.null(rmetObj$aersurface$surfaceSeason$as_Winter_NS)){
    control <- c(control, 
                 paste("   SEASON   WINTERNS  ", rmetObj$aersurface$surfaceSeason$as_Winter_NS)
    )
  }else{
  
      control <- c(control, 
                   paste("   SEASON   WINTERWS  ", rmetObj$aersurface$surfaceSeason$as_Winter_WS)
      )
      
      
      
  } 
  

  
    
  
    
  control <- c( control, 
                paste("   SEASON   SPRING    ", rmetObj$aersurface$surfaceSeason$as_Spring),
                paste("   SEASON   SUMMER    ", rmetObj$aersurface$surfaceSeason$as_Summer),
                paste("   SEASON   AUTUMN    ", rmetObj$aersurface$surfaceSeason$as_Autumn),
                "   RUNORNOT  RUN",
                "CO FINISHED")
  
  ouput <- list(
    "** OUTPUT",
    "OU STARTING",
    paste0("   SFCCHAR    \"", rmetObj$project_Dir, "/aersurface/nws_sfc_chars.out\""),
    paste0("   NLCDTIFF   \"", rmetObj$project_Dir, "/aersurface/nws_lc_tif.txt\""),
    paste0("   NLCDGRID   \"", rmetObj$project_Dir, "./aersurface/nws_landcover.txt\""))
  
  if(!is.null(rmetObj$aersurface$inputFiles$imp_File)){
    ouput <- c(ouput,
    paste0("   MPRVGRID   \"", rmetObj$project_Dir, "/aersurface/nws_imp_tif_dbg.txt\""),
    paste0("   MPRVTIFF   \"", rmetObj$project_Dir, "/aersurface/nws_impervious.txt\"")
    )
  }
 if(!is.null(rmetObj$aersurface$inputFiles$cnpy_File)){
   ouput <- c(ouput,
   paste0("   CNPYGRID   \"", rmetObj$project_Dir, "/aersurface/nws_can_tif_dbg.txt\""),
   paste0("   CNPYTIFF   \"", rmetObj$project_Dir, "/aersurface/nws_canopy.txt\"")
   )             
   
  }
  ouput <- c(ouput, 
             "OU FINISHED")
  
    rmetObj$inputText$aersurface$surface <- 
      paste(
         c(control, ouput),
        collapse = "\n")
    
    if(doUdoWindows){
      
      rmetObj$inputText$aersurface$surface <- gsub("/", "\\\\", rmetObj$inputText$aersurface$surface)
      
    }
  }
  
# AERSURFACE OS
  
  if("aersurface_os" %in% type){
    print("Writing AERSURFACE OS input text:\n")
    
    if("aersurface" %in% type) stop("Cannot run AERSURFACE and AERSURFACE_os.")
    
    stopifnot(file.exists(rmetObj$aersurface$inputFiles$lc_File))
    stopifnot(!is.null(rmetObj$aersurface$inputFiles$lc_Type))
    
    control = list(
      "CO STARTING",
      paste("   TITLEONE", rmetObj$project_Name, "AERSURFACE ONSITE INPUT CONTROL FILE", sep = "  "), 
      paste("   TITLETWO", rmetObj$project_Dir, "Directory", sep = "  "),
      "** Using default options for OPTIONS keyword and parameters", sep = "  ")
      control <- c(control, "   OPTIONS   PRIMARY  ZORAD")

      control <- c(control, 
                 "   DEBUGOPT  GRID  TIFF",
                 paste("   CENTERLL ", rmetObj$onsite_Latitude, rmetObj$onsite_Longitude, "NAD83")
    )
    
    
    
    doUdoWindows <- Sys.info()['sysname'] == "Windows"
    
    
    inputFile_mx <- matrix(rev(rmetObj$aersurface$inputFiles[!sapply(rmetObj$aersurface$inputFiles, is.null)]), ncol = 2, byrow = TRUE)
    
    inputFile_mx[,2] <- paste0("\"", inputFile_mx[,2], "\"")
    dataControl <- paste("   DATAFILE ", apply(inputFile_mx, 1, paste, collapse = "  "))
    
    control <- c(control, dataControl)
    
    control <- c(control,
                 "** Use default specified km radius",
                 paste("   ZORADIUS ", rmetObj$aersurface$surfaceChar$as_radius)
    )
  
    
    
    #Create String
    if(any(c(
      rmetObj$aersurface$surfaceChar$as_Snow == "Y",
      rmetObj$surfaceChar$as_Moisture =="W",
      rmetObj$surfaceChar$as_Moisture =="D",
      rmetObj$surfaceChar$as_Arid =="Y"
    ))){
      
      climate <- "   CLIMATE"
      
      climate <- ifelse(is.null(rmetObj$surfaceChar$as_Moisture),  paste(climate, "AVG"),
                        ifelse(rmetObj$surfaceChar$as_Moisture =="D", paste(climate, "DRY"), 
                               paste(climate, "WET"))
      )
      
      climate <- ifelse(rmetObj$aersurface$surfaceChar$as_Snow =="Y",
                        paste(climate, "SNOW"),
                        paste(climate, "NOSNOW"))
      
      climate <- ifelse(rmetObj$aersurface$surfaceChar$as_Arid =="Y",
                        paste(climate, "ARID"),
                        paste(climate, "NONARID"))
      
      control <- c(control, climate)
      
      
    }
    
    
    control <- c(
      control, 
      paste("   FREQ_SECT", "MONTHLY", dim(rmetObj$aersurface$surfaceChar$os_sector)[[1]], "VARYAP", sep = "  "  ),
      "**        index  start    end      "
    )
    
    
    
    freq_dfx <- cbind("   SECTOR  ", 
                      rmetObj$aersurface$surfaceChar$os_sector)
    
    freq_dfx$start <- sprintf("%7.2f", freq_dfx$start)
    freq_dfx$end <- sprintf("%7.2f", freq_dfx$end)
    freq_dfx$ap <- paste("   ", freq_dfx$ap)
    
    control <- c(control, gsub(",", "", apply(freq_dfx, 1, toString)))
    
    if(!is.null(rmetObj$aersurface$surfaceSeason$as_Winter_NS)){
      control <- c(control, 
                   paste("   SEASON   WINTERNS  ", rmetObj$aersurface$surfaceSeason$as_Winter_NS)
      )
    }
    
    if(!is.null(rmetObj$aersurface$surfaceSeason$as_Winter_WS)){
      control <- c(control, 
                   paste("   SEASON   WINTERWS  ", rmetObj$aersurface$surfaceSeason$as_Winter_WS)
      )
      
      
      
    }  
    
    if(rmetObj$aersurface$surfaceChar$as_Snow == "Y"){
      
      control <- c(control, 
                   "   CLIMATE   SNOW")
    }else{
      control <- c(control, 
                   "   CLIMATE   NOSNOW")
    }
    
    control <- c( control, 
                  paste("   SEASON   SPRING    ", rmetObj$aersurface$surfaceSeason$as_Spring),
                  paste("   SEASON   SUMMER    ", rmetObj$aersurface$surfaceSeason$as_Summer),
                  paste("   SEASON   AUTUMN    ", rmetObj$aersurface$surfaceSeason$as_Autumn),
                  "   RUNORNOT  RUN",
                  "CO FINISHED")
    
    ouput <- list(
      "** OUTPUT",
      "OU STARTING",
      paste0("   SFCCHAR    \"", rmetObj$project_Dir, "/aersurface/os_sfc_chars.out\""),
      paste0("   NLCDTIFF   \"", rmetObj$project_Dir, "/aersurface/os_lc_tif_dbg.tif\""),
      paste0("   NLCDGRID   \"", rmetObj$project_Dir, "./aersurface/os_landcover.txt\""))
    
    if(!is.null(rmetObj$aersurface$inputFiles$imp_File)){
      ouput <- c(ouput,
                  paste0("   MPRVGRID   \"", rmetObj$project_Dir, "/aersurface/os_imp_tif_dbg.txt\""),
                  paste0("   MPRVTIFF   \"", rmetObj$project_Dir, "/aersurface/os_impervious.tif\"")
      )
    }
    if(!is.null(rmetObj$aersurface$inputFiles$cnpy_File)){
      ouput <- c(ouput,
                  paste0("   CNPYGRID   \"", rmetObj$project_Dir, "/aersurface/os_can_tif_dbg.txt\""),
                  paste0("   CNPYTIFF   \"", rmetObj$project_Dir, "/aersurface/os_canopy.tif\"")
      )             
      
    }
    ouput <- c(ouput, 
               "OU FINISHED")
    
    rmetObj$inputText$aersurface$onsite <- 
      paste(
        c(control, ouput),
        collapse = "\n")
    
    if(doUdoWindows){
      
      rmetObj$inputText$aersurface$onsite <- gsub("/", "\\\\", rmetObj$inputText$aersurface$onsite)
      
    }
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
    
    if(grepl("[.]bin\"$", as_inpFile$lcFile)){
      indx <- sapply(state.name, function(x) grepl(x, as_inpFile$lcFile, ignore.case=TRUE))
       if(any(indx)) {
         as_inpFile <- append(as_inpFile, state.abb[indx], after = 1)
       } else {
         warning("Cannot find the state appreviation - you must manually append to input.")
       }
       
    }
    
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
    
    rmetObj$inputText$aersurface$surface <- paste(as_inpFile, collapse="\n")
    
    if(!is.null(rmetObj$onsite_Latitude) & !is.null(rmetObj$onsite_Latitude)){
      as_inpFile$lat = rmetObj$onsite_Latitude
      as_inpFile$long = rmetObj$onsite_Longitude
      as_inpFile$airport <- "N"
      rmetObj$inputText$aersurface$onsite <- paste(as_inpFile, collapse="\n")
    }
    
  }
  
  if("aermet1" %in% type){
   if(!is.null(rmetObj$ua_IGRA_zip)){
    wmoID <- str_extract(rmetObj$ua_IGRA_zip, "\\d{8}")
   }
    S1 <- lapply(seq_along(loc_years), function(i) {list(
      JOB=list(
        "JOB",
        "**",
        paste0("  REPORT     ", prepareThePath(paste(destDir[[i]], "S1.RPT", sep="/"))),
        paste0("  MESSAGES   ", prepareThePath(paste(destDir[[i]], "S1.MSG", sep="/")))),
      UPPERAIR=list(
        "UPPERAIR",
        if(!is.null(rmetObj$ua_WMO)){
          paste("**          Upper air data for WMO:",
              rmetObj$ua_WMO ,"FSL format")} else{
        if(is.null(rmetObj$ua_WMO)){
          paste("**          Upper air data for IGRA:",
                wmoID ,"IGRA format")
        
        }
                },
        if(!is.null(rmetObj$ua_WMO)){
          paste0("  DATA      ", prepareThePath(fslFiles[[i]]), " FSL")
          }else{
            if(is.null(rmetObj$ua_WMO)){
              paste0("  DATA      ", prepareThePath(fslFiles[[i]]), " IGRA")
              
            }
          },
        paste0("  EXTRACT   ", prepareThePath(uaexoutFiles[[i]])),
        "  AUDIT     UATT UAWS UALR",
        paste0("  XDATES    ", xdates[[i]]),
        if(!is.null(rmetObj$ua_WMO)){
        paste0("  LOCATION  ", rmetObj$ua_WMO,"  ",convertLat(rmetObj$ua_Latitude),"   ",
               convertLong(rmetObj$ua_Longitude),"   ",-(rmetObj$ua_UTC))
          }else{
            if(is.null(rmetObj$ua_WMO)){
              paste0("  LOCATION  ", wmoID, "  ",convertLat(rmetObj$ua_Latitude),"   ",
                     convertLong(rmetObj$ua_Longitude),"   ",-(rmetObj$ua_UTC), "   ",rmetObj$ua_elevation)
            } 
          },
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
        paste("  LOCATION     ", rmetObj$surf_WBAN, convertLat(round(rmetObj$surf_Latitude,3)), 
              convertLong(round(rmetObj$surf_Longitude,3)), -(rmetObj$surf_UTC), 
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
          paste0(paste0("  ", inputOps$S3$other), collapse = "\n"),
          paste0(paste0("  METHOD        ", inputOps$S3$method),collapse = "\n"),
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

  
  
  if("aermet23" %in% type){
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
          paste0(paste0("  ", inputOps$S3$other), collapse = "\n"),
          paste0(paste0("  METHOD        ", inputOps$S3$method),collapse = "\n"),
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
    
    
    rmetObj$inputText$aermet$s2 <-
      purrr::imap(
        rmetObj$inputText$aermet$s2,
        ~ paste0(.x, gsub("JOB.+[.]MSG\"", "", 
                          gsub("DATA.+MRG\\n", "",      rmetObj$inputText$aermet$s3[[.y]])),
                 "\n", paste(rmetObj$output$aersurface$surface, collapse = "\n")
        )
      )
    
    rmetObj$inputText$aermet$s3 <- NULL
    
    
  }
  
  
return(rmetObj)

}


