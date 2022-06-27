#' @title qaAerminute
#' 
#' @description
#' \code{qaAerminute} reads QA Information from Aerminute Run This function returns a list object containing run log of aerminute as tables.
#' \itemize{
#' \item{\code{RecCheck} Record format QA Output, }
#' \item{\code{Flags} QA flag summary,}
#' \item{\code{qaCalms} Standard calms summary,}
#' \item{\code{qaMissing} Missing winds summary,}
#' \item{\code{qaVariable} Variable winds summary,}
#' \item{\code{qaValid} Standard valid winds summary,}
#' \item{\code{qaVarWS} Comparison of minute to hourly surface data - wind speed (variable winds),}
#' \item{\code{qaValWD} Comparison of minute to hourly surface data - wind speed (valid winds),}
#' \item{\code{qaValWD} Comparison of minute to hourly surface data - wind direction (valid winds),}
#' \item{\code{YearNumSum} Summary of available hours for model,}
#' \item{\code{monthlyData} Availability by month for all years.}
#' }
#' @param rmetObj a rmet class object
#' @export

qaAerminute = function(rmetObj){
  # library(plyr)
  # library(stringr)
  stopifnot(is.rmet(rmetObj))
  
  #read years from input
  years <- locYears(rmetObj)
  aerminRuns <- paste(rmetObj$project_Dir, years,"aerminute.log", sep = "/")
  qaAerminLines <- lapply(aerminRuns, readLines)
  
# read first 2 tables -----------------------------------------------------
  
  #Record Check Counts
  qaFormatRecCheck <- lapply(qaAerminLines, function(lz){
    RecsCheck <- lz[grepl("[Nn]umber of.*?records.*?:", lz)]
    count <- stringr::str_extract(string = RecsCheck, "[0-9]+$")
    record.type <- stringr::str_extract(string = stringr::str_trim(RecsCheck), "[[:alnum:]].*:")
    record.type <- gsub(":", "", record.type)
    data.frame(record.type,count)
    
  }
  )
  names(qaFormatRecCheck) <- years
    for(i in 1:length(years)){
    names(qaFormatRecCheck[[i]])[names(qaFormatRecCheck[[i]])=="count"] <- paste("count", years[i], sep=".")
  }
  qaRecFormatCheck <- Reduce(function(...) merge(..., by="record.type", all=TRUE, sort=FALSE), qaFormatRecCheck)
  names(qaRecFormatCheck)[1] <- "RECORD FORMAT CHECK"
    
  # flag counts
  qaFlags <- lapply(qaAerminLines, function(lz){
    
    RecsFlag <- grep("^ {1,2}[0-9]{1,2} +", lz, value=TRUE)
    count <- stringr::str_extract(string = RecsFlag, "[0-9]+$")
    record.type <- stringr::str_trim(stringr::str_extract(string = stringr::str_trim(RecsFlag), "[[:alnum:]].* {3,}"))
    data.frame(record.type,count)
  })
  
  names(qaFlags) <- years
  
  for(i in 1:length(years)){
    names(qaFlags[[i]])[names(qaFlags[[i]])=="count"] <- paste("count", years[i], sep=".")
  }
  qaFlags <- Reduce(function(...) merge(..., by="record.type", all=TRUE, sort=FALSE), qaFlags)
  names(qaFlags)[1] <- "QA FLAG SUMMARY"
  
  
# Functions for reading QC Data Tables ------------------------------------
  
#This section reads a peice of the textfile and extracts the table  
  readQASection <- function(lz, term, skip=0, sep=":", fill=TRUE, add=1){
  qaLN <- grep(term, lz)
  qaLE <- grep("^$", lz)
  qaLE <- qaLE[qaLE > qaLN[1]][1:length(qaLN)]
  qaLE <- as.numeric(as.character(qaLE)) - 1
  qaLN <- qaLN + add
  read.table(textConnection(lz[qaLN: qaLE]), sep=sep, 
             header=FALSE, skip=skip, fill=fill,
             stringsAsFactors=FALSE)
    }
  
  #this function fixes the table from readQASection (depending on widths)
  fixColNames <- function(dfx){
    for(i in 1:length(years)){
      
      if(length(names(dfx[[i]])) == 2){
      names(dfx[[i]])[names(dfx[[i]])=="V2"] <- paste("count", years[i], sep=".")
      names(dfx[[i]])[names(dfx[[i]])=="V1"] <- "parameter"
      }
      
      if(length(names(dfx[[i]])) == 3){
        idx <- is.na(dfx[[i]]$V3)
        dfx[[i]]$V3[idx] <- dfx[[i]]$V2[idx]
        dfx[[i]]$V2[idx] <- ""
        
        dfx[[i]]$V1 <- paste(dfx[[i]]$V1, dfx[[i]]$V2)
        dfx[[i]]$V2 <- dfx[[i]]$V3
        dfx[[i]]$V3 <- NULL
        names(dfx[[i]])[names(dfx[[i]])=="V2"] <- paste("count", years[i], sep=".")
        names(dfx[[i]])[names(dfx[[i]])=="V1"] <- "parameter"
      }
      
      if(length(names(dfx[[i]])) == 4){
        
        idx1 <- is.na(dfx[[i]]$V4) & is.na(dfx[[i]]$V3)
        
        dfx[[i]]$V4[idx1] <- dfx[[i]]$V2[idx1]
        dfx[[i]]$V2[idx1] <- dfx[[i]]$V3[idx1] <- ""
        idx2 <- is.na(dfx[[i]]$V4) & !is.na(dfx[[i]]$V3)
        dfx[[i]]$V4[idx2] <- dfx[[i]]$V3[idx2]
        dfx[[i]]$V3[idx2] <- ""
               
        dfx[[i]]$V1 <- paste(dfx[[i]]$V1, dfx[[i]]$V2, dfx[[i]]$V3)
        dfx[[i]]$V2 <- dfx[[i]]$V4
        dfx[[i]]$V4 <- dfx[[i]]$V3 <- NULL       

        names(dfx[[i]])[names(dfx[[i]])=="V2"] <- paste("count", years[i], sep=".")
        names(dfx[[i]])[names(dfx[[i]])=="V1"] <- "parameter"
      }
      
      
    }
    return(dfx)
  }
  
#this is the main function that calls the 2 functions above
  pullQA_ISH <- function(term, skip=0, sep=":", fill=TRUE, add=1){
    #term <- deparse(substitute(term))
    qa <- lapply(qaAerminLines, readQASection, term, skip, sep, fill)
    qa <- fixColNames(qa)
    dfx <- Reduce(function(...) merge(..., by="parameter", all=TRUE, sort=FALSE), qa)
    names(dfx)[names(dfx)=="parameter"] <- term
    dfx
  } 
  
  
# This function reads the next to last table (total hours per year)  
  readFinalSummary <- function(lz, finalTerms){
    myLines <- grep(finalTerms, lz, value=TRUE)
    dfx <- read.table(textConnection(myLines), sep=":")
    names(dfx) <- c("Total Hours", "Number(%)")
    dfx
  }
  
#This function does the monthly summary
  monthSum <- function(lz){
    LN <- grep(" YEAR      MONTH     HOURS   HOURS    HOURS     HOURS    HOURS", lz)
    LE <- grep("^$", lz)
    LE <- LE[LE>LN][1]
    dfx <- read.table(textConnection(lz[LN:LE-1]), skip=3)
    
    #label
    label1 <- unlist(stringr::str_split(stringr::str_trim(lz[(LN-1)]), pattern="  *"))
    label2 <- unlist(stringr::str_split(stringr::str_trim(gsub("YEAR|MONTH", "", lz[(LN)])), pattern="  *"))
    label <-c("YEAR", "MONTH", paste(label1, label2))
    
    names(dfx) <- label
    dfx
  }
  

  # Pull GC Section -----------------------------------------------------------
  
  #Standard Calms
  qaCalms <- pullQA_ISH("STANDARD CALMS")  
  
  #Standard Missing Winds
  qaMiss <-  pullQA_ISH("STANDARD MISSING WINDS")  

  #qaVariable Winds
  qaVar <- pullQA_ISH("STANDARD VARIABLE WINDS")
  qaVarWS <- pullQA_ISH("VARIABLE WINDS:  SPEED DIFFERENCES", skip=1, sep="", fill=TRUE)
 
  #missing standard valid winds
  qaValid <- pullQA_ISH("STANDARD VALID WINDS", skip=0, sep=":", fill=TRUE, add=0)
  
  #qaValid Winds (speed)
  qaValWS <- pullQA_ISH("VALID WINDS:  SPEED DIFFERENCES", skip=1, sep="", fill=TRUE)
  
  #qaValid Winds (direction)
  qaValWD <- pullQA_ISH(" VALID WINDS:  DIRECTION DIFFERENCES", skip=1, sep="", fill=TRUE)
  
  
  #read final summary:
  finalTerms <- "Number of total hours in data period|Number of processed hours|Number of processed non-valid hours|Number of calm hours"
  YearNumSum <- lapply(qaAerminLines, readFinalSummary, finalTerms=finalTerms)

  for(i in 1:length(years)){
    names(YearNumSum[[i]])[names(YearNumSum[[i]])== "Number(%)"] <- paste0("Number(%).", years[i])
  }
  
  YearNumSum <- Reduce(function(...) merge(..., by="Total Hours", all=TRUE, sort=FALSE), YearNumSum)
  
  
  # Monthly Summary Data ----------------------------------------------------
  
  monthSumData <- lapply(qaAerminLines, monthSum)
  
  monthSumData <- dplyr::bind_rows(monthSumData)
  
  
# list containing the different tables created  
  qaAerminInfo <- list(RecCheck =qaRecFormatCheck,
                       Flags = qaFlags, qaCalms = qaCalms,
                       qaMissing=qaMiss, qaVariable = qaVar,
                       qaVarWS = qaVarWS, qaValid=qaValid,
                       qaValWS = qaValWS, 
                       qaValWD=qaValWD, YearNumSum = YearNumSum,
                       monthlyData = monthSumData)
  
  return(qaAerminInfo)
}




  

