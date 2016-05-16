library(rmet2)

#windows intaller for binaries:
installAM(aermetExists=rep(TRUE,3))

#real programmers use Linux (or at least try to)
# options(aerminute  =  "aerminute_15272.exe")
# options(aermet = "aermet")
# options(aersurface = "aersurface")


#create object
karr <- createMetProject(
  project_Name = "CHICAGO AURORA MUNI AP",
   project_Dir="~/test/karr",
#  project_Dir="/RMET_WORKING/TEST/KARR",
  start_Date = lubridate::mdy_hm("01/01/2011 00:00", tz="Etc/GMT+6"),
  end_Date = lubridate::mdy_hm("12/31/2015 23:00", tz="Etc/GMT+6"),
  surf_UTC = -6,
  surf_WBAN = 04808,
  surf_USAF = 744655,
  surf_Call="KARR",
  surf_Latitude = 41.77,
  surf_Longitude = -88.48139,
  surf_Elevation = 216,
  surf_AnenometerHeight = 8,
  ua_WMO=74560,
  ua_UTC=-6,
  ua_Latitude = 40.15,
  ua_Longitude = -89.15,
  ifg = "Y 12 06 2002",
  lc_File = "~/lufiles/illinois.nlcd.tif",
  lc_Type = "NLCD1992",
  imp_File = NULL,
  imp_Type = NULL,
  cnpy_File = NULL,
  cnpy_Type = NULL,
  as_Snow = "Y",
  as_Arid = "N",
  as_Moisture = "A",
  as_Airport = "Y",
  as_Winter_NS ="",
  as_Winter_WS = "11 12 1 2 3",
  as_Spring ="4 5",
  as_Summer = "6 7 8",
  as_Autumn = "9 10")
 

downloadTD3505(karr) # could combine into one step
downloadTD6405(karr)
downloadTD6401(karr)
downloadFSL(karr)


karr <- createInput(karr) # probably should combine this into one step
karr <- writeInputFile(karr, c("aerminute", "aersurface"))


# protofunction to execute aerminute_exe

sapply(seq_along(karr$inputFiles$aerminute), function(i) {
  system(getOption("aerminute"), input=karr$inputFiles$aerminute[[i]])
  moveFiles <- c("aerminute.log",
                 "bad_records.dat",
                 "good_records.dat",
                 "check_records.dat",
                 "bad_records_5.dat",
                 "good_records_5.dat",
                 "calm_variable_records.dat"
                 )
  tmp <- sapply(moveFiles, function(x) file.rename(x, paste(karr$project_Dir,
                                                      locYears(karr)[[i]], x, 
                                                      sep="/")))
  return(NULL)
  }
)



# protofunction of execution of aersurface
# Since R5 guidance requires averaging snowcover and non-snowcover
# albedo bowen ratio and surface roughness by number of 
# snow cover days per month, we need to run twice
# then use rnoaa to get the number of snow cover >1" for
# county, then write the AERSURFACE parameters into
# object to be used in S3 processing in AERMET.


karr <- writeInputFile(karr, "aersurface", dewinter=TRUE)

system(getOption("aersurface"), 
       input=readLines(karr$inputFiles$aersurface[grepl("aersurface.inp", 
                                                        karr$inputFiles$aersurface)]))


system(getOption("aersurface"), 
       input=readLines(karr$inputFiles$aersurface[grepl("aersurface_dewinter.inp", 
                                                        karr$inputFiles$aersurface)]))


#rnoaa to look at snow cover
# note that you need to request a apikey from noaa 
# (see https://github.com/ropensci/rnoaa  - see section NCDC Authentication)

library(rnoaa)
x <- ncdc_stations(datatypeid='mly-snwd-avgnds-ge001wi', locationid = 'FIPS:17089')
print(x$data[c("name","id","mindate","maxdate")])


snowjobs <- lapply(x$data$id, function(x){
  ncdc(datasetid = "NORMAL_MLY", datatypeid = "mly-snwd-avgnds-ge001wi", 
                 stationid = x,
                 startdate = "2010-01-01", 
                 enddate="2010-12-31", limit=365)
}
)


snowjobs <- lapply(snowjobs, function(x) {
  x$data$value[x$data$value == -7777] <- 0
  x$data$value[x$data$value == -9999] <- NA
  x$data$value <- x$data$value/10
  return(x)
})

snowjobs <- Reduce(function(...) ncdc_combine(...), snowjobs)

ggplot(snowjobs$data, aes(x=date, y=value, shape=station, group=station)) + 
  geom_point(size=4) + geom_line() + xlab("Date") + ylab("Days with > 1 inch Snow Cover") +
  theme_light()

# metoerologist sources tupical 1 inch of cover 43 days/snow season
# http://chicagoweathercenter.com/blog/ask_tom_why_more_days_with_snow_cover_or_without_in_a_typical_chicago_winter

ddply(snowjobs$data, .(station), summarize, total = sum(value))

#looks reasonable.

snowjobs$data$value <- snowjobs$data$value/30.4

snowcover <- ddply(snowjobs$data, .(date), summarize, snowMean = round(mean(value),2))

snowcover$snowMean[snowcover$snowMean<0.01] <- 0
snowcover$month <- 1:12



asFiles <- gsub("[.]inp", ".out", karr$inputFiles$aersurface)

asData <- lapply(asFiles, function(x) {
  dfx <- read.table(x, skip=30)
  names(dfx) <- c("SITE_CHAR", "Month", "Sect", "Alb", "Bo", "Zo")
  return(dfx)
  })

names(asData) <- gsub(".*/", "", asFiles)

#winter
asData[[which(names(asData) == "aersurface.out")]][c("Alb","Bo", "Zo")] <- 
asData[[which(names(asData) == "aersurface.out")]][c("Alb","Bo", "Zo")] * 
  rep(snowcover$snowMean, each=12)

asData[[which(names(asData) == "aersurface_dewinter.out")]][c("Alb","Bo", "Zo")] <- 
  asData[[which(names(asData) == "aersurface_dewinter.out")]][c("Alb","Bo", "Zo")] * 
  rep(1-snowcover$snowMean, each=12)

asData$adjusted_aersurface.data <- data.frame(SITE_CHAR="SITE_CHAR", Month=asData[[1]]$Month, 
                                              Sect=asData[[1]]$Sect, Alb=NA, Bo=NA, Zo=NA)
  
asData$adjusted_aersurface.data[c("Alb","Bo", "Zo")] <- asData[[which(names(asData) == "aersurface_dewinter.out")]][c("Alb","Bo", "Zo")]  +
  asData[[which(names(asData) == "aersurface.out")]][c("Alb","Bo", "Zo")] 
  



headAersurf <- readLines(asFiles[[1]], n=30)
headAersurf[[grep("\\*\\* Autumn with unharvested cropland:", 
                  headAersurf)+1]] <- "** Modifed by for number snow cover >1/month based on climate normals"

surfLines <- paste("  ", asData$adjusted_aersurface.data$SITE_CHAR, "  ", 
      format(asData$adjusted_aersurface.data$Month, digits=2, just="right"),
      "    ", format(asData$adjusted_aersurface.data$Sect, digits=2, just="right"), 
      "   ", sprintf("%1.2f", asData$adjusted_aersurface.data$Alb), "   ",
      sprintf("%1.2f", asData$adjusted_aersurface.data$Bo), "   ",
      sprintf("%1.3f", asData$adjusted_aersurface.data$Zo))

karr$output$aersurface <-paste(c(headAersurf, surfLines), collapse=
                                 "\n")
write(karr$output$aersurface, file = paste(karr$project_Dir,"aersurface", "adjust_aersurface.out", sep="/"))


# Write S1, S2 and S3 input files and run. --------------------------------

lapply(karr$inputText$aermet$s1, function(x) {
  write(x, file="AERMET.INP")
  system(getOption("aermet"))
})

lapply(karr$inputText$aermet$s2, function(x) {
  write(x, file="AERMET.INP")
  system(getOption("aermet"))
})

lapply(seq_along(karr$inputText$aermet$s3), function(i) {
  write(c(karr$inputText$aermet$s3[[i]], karr$output$aersurface), file="AERMET.INP")
  system(getOption("aermet"))
})

# Make final surface and profile files ------------------------------------

makeFinal(karr)
surfCheck(karr)

