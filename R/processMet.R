#' processMet
#' 
#' @description
#' \code{processMet} This executes the AERMINUTE, AERSUFACE or AERMET prcoessors
#' using appropriately initialized \code{rmet} object. This means that inputFiles 
#' have been created (typically using \code{createInput}) and written (using \code{writeInput}).
#' 
#' 
#' @export

processMet <- function(rmetObj, processor = c("aerminute", "aersurface", "aermet1",
                                              "aermet2", "aermet3"), newaersurface = TRUE){
  
  stopifnot(processor %in% c("aerminute", "aersurface", "aermet1", 
                             "aermet2", "aermet3"))
  
  if("aerminute" %in% processor) {
    sapply(seq_along(rmetObj$inputFiles$aerminute), function(i) {
      stopifnot(!is.null(rmetObj$inputFiles$aerminute[[i]]))
      system(getOption("aerminute"), input=rmetObj$inputFiles$aerminute[[i]])
      moveFiles <- c("aerminute.log",
                   "bad_records.dat",
                   "good_records.dat",
                   "check_records.dat",
                   "bad_records_5.dat",
                   "good_records_5.dat",
                   "calm_variable_records.dat"
      )
    tmp <- sapply(moveFiles, function(x) file.rename(x, paste(rmetObj$project_Dir,
                                                              locYears(rmetObj)[[i]], x, 
                                                              sep="/")))
    
  }
  )
  }
  
  if ("aersurface" %in% processor) {
    if (newaersurface) {
      system(paste(
        getOption("aersurface"),
        rmetObj$inputFiles$aersurface$surface
      ))
      
      rmetObj$output$aersurface$surface <-
        readLines(paste(rmetObj$project_Dir, "aersurface/nws_sfc_chars.out", sep =
                          "/"))
      
      
      if (!is.null(rmetObj$inputFiles$aersurface$onsite)) {
        system(paste(
          getOption("aersurface"),
          rmetObj$inputFiles$aersurface$onsite
        ))
        
        rmetObj$output$aersurface$onsite <-
          readLines(paste(rmetObj$project_Dir, "aersurface/os_sfc_chars.out",
                          sep = "/"))
        
        
      }
      
    }else{
      system(getOption("aersurface"),
             input = readLines(rmetObj$inputFiles$aersurface$surface[grepl("aersurface.inp",
                                                                           unlist(rmetObj$inputFiles$aersurface))]))
      rmetObj$output$aersurface$surface <-
        readLines(paste(rmetObj$project_Dir, "aersurface/aersurface.out", sep =
                          "/"))
      
      
      if (!is.null(rmetObj$onsite_Latitude) &
          !is.null(rmetObj$onsite_Latitude)) {
        system(getOption("aersurface"),
               input = readLines(rmetObj$inputFiles$aersurface$onsite[grepl("aersurface_onsite.inp",
                                                                            rmetObj$inputFiles$aersurface$onsite)]))
        rmetObj$output$aersurface$onsite <-
          readLines(paste(rmetObj$project_Dir, "aersurface/aersurface.out",
                          sep = "/"))
        
        
      }
      
    }
    
  }
  
  
  
  if("aermet1" %in% processor){
  
  lapply(rmetObj$inputText$aermet$s1, function(x) {
    
    xInp <- gsub("[.]RPT", ".INP", 
                 stringr::str_extract(x, 
                                      paste(rmetObj$project_Dir,
                          "[:digit:]{4}/S1.RPT", sep="/")))
    
    
    if(!is.null(rmetObj$onsite_Fstring)){
      xdates <- stringr::str_extract(
        x, "XDATES.*[:digit:]")
      Fstring <- rmetObj$onsite_Fstring
     
      Fstring[grepl("XDATES", Fstring)] <- paste0("  ",xdates)
      
      xpath <- stringr::str_extract(x, "  QAOUT.*SFQAOUT.DAT")
      
      Fstring[grepl("QAOUT", Fstring)] <- gsub("SFQAOUT.DAT", "OSQAOUT.DSK\"", xpath)

      x <- c(x, Fstring)
    }
    #browser()
    write(x, file="./AERMET.INP")
    system(getOption("aermet"))
    file.copy("AERMET.INP", xInp, overwrite = TRUE)
    
  })
  }
  
  if("aermet2" %in% processor){
    
  lapply(rmetObj$inputText$aermet$s2, function(x) {
    
    xInp <- gsub("[.]RPT", ".INP", 
                 stringr::str_extract(x, 
                                      paste(rmetObj$project_Dir,
                                            "[:digit:]{4}/S2.RPT", sep="/")))
    if(!is.null(rmetObj$onsite_Fstring)){
      
      xpath <- stringr::str_extract(x, "  QAOUT.*SFQAOUT.DAT")
      onsiteqa <- gsub("SFQAOUT.DAT", "OSQAOUT.DSK\"", xpath)
      x <- c(x, "ONSITE", onsiteqa)
    }
    write(x, file="AERMET.INP")
    system(getOption("aermet"))
    file.copy("AERMET.INP", xInp, overwrite = TRUE)
  })
  
  }
  
  if("aermet3" %in% processor){
    
  lapply(seq_along(rmetObj$inputText$aermet$s3), function(i) {
    
    xInp <- gsub("[.]RPT", ".INP", 
                 stringr::str_extract(rmetObj$inputText$aermet$s3[[i]], 
                                      paste(rmetObj$project_Dir,
                                            "[:digit:]{4}/S3.RPT", sep="/")))
    if(is.null(rmetObj$inputFiles$aersurface$onsite)){
    write(c(rmetObj$inputText$aermet$s3[[i]], rmetObj$output$aersurface$surface), file="AERMET.INP")
    }
    
    if(!is.null(rmetObj$inputFiles$aersurface$onsite)){
      
      if(newaersurface){
        
        headLines <- grep("^\\*", rmetObj$output$aersurface$onsite, value=TRUE)
        
        surfaceFreqSect <- grep("  FREQ_SECT", rmetObj$output$aersurface$surface, value=TRUE)
        onsiteFreqSect <- grep("  FREQ_SECT", rmetObj$output$aersurface$onsite, value=TRUE)
        
        surfaceSect <- grep("^   SECTOR", rmetObj$output$aersurface$surface, value=TRUE)
        onsiteSect <- grep("^   SECTOR", rmetObj$output$aersurface$onsite, 
                           value=TRUE)
        
        surfaceChar <- grep("^   SITE_CHAR", rmetObj$output$aersurface$surface, value=TRUE)
        onsiteChar <- grep("^   SITE_CHAR", rmetObj$output$aersurface$onsite, 
                           value=TRUE)
        
        
      }else{
      
      headLines <- grep("^\\*", rmetObj$output$aersurface$surface, value=TRUE)
      
      surfaceFreqSect <- grep("  FREQ_SECT", rmetObj$output$aersurface$surface, value=TRUE)
      surfaceFreqSect <- gsub("FREQ_SECT", "FREQ_SECT2", surfaceFreqSect)
      onsiteFreqSect <- grep("  FREQ_SECT", rmetObj$output$aersurface$onsite, value=TRUE)
      
      surfaceSect <- grep("^   SECTOR", rmetObj$output$aersurface$surface, value=TRUE)
      surfaceSect <- gsub("SECTOR", "SECTOR2", surfaceSect)
      
      onsiteSect <- grep("^   SECTOR", rmetObj$output$aersurface$onsite, 
                                 value=TRUE)
      
      surfaceChar <- grep("^   SITE_CHAR", rmetObj$output$aersurface$surface, value=TRUE)
      surfaceChar <- gsub("SITE_CHAR", "SITE_CHAR2", surfaceChar)
      
      onsiteChar <- grep("^   SITE_CHAR", rmetObj$output$aersurface$onsite, 
                          value=TRUE)
      }
      
      inpFile <- c(headLines[1:(length(headLines)-1)],
                   onsiteFreqSect,
                   onsiteSect,
                   surfaceFreqSect,
                   surfaceSect,
                   headLines[length(headLines)],
                   onsiteChar,
                   surfaceChar
)
      #browser()
      write(c(rmetObj$inputText$aermet$s3[[i]], inpFile), file="AERMET.INP")
    }
    system(getOption("aermet"))
    file.copy("AERMET.INP", xInp, overwrite = TRUE)
  })
  
  }
  
  
  return(rmetObj)
}
