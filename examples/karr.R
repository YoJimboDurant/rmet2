karr <- createMetProject(
  project_Name = "CHICAGO AURORA MUNI AP",
  project_Dir="~/test/karr",
  start_Date = lubridate::mdy_hm("01/01/2012 00:00", tz="Etc/GMT+6"),
  end_Date = lubridate::mdy_hm("03/31/2016 23:00", tz="Etc/GMT+6"),
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
  ifg = "Y 12 06 2002",
  lc_File = "C:/local/landuse/nlcd2001_lc_N39W087/NLCD2001_LC_N39W087.tif",
  lc_Type = "NLCD2001",
  imp_File = NULL,
  imp_Type = NULL,
  cnpy_File = NULL,
  cnpy_Type = NULL,
  as_Snow = "Y",
  as_Arid = "N",
  as_Moisture = "A",
  as_Airport = "Y",
  as_Winter_NS ="",
  as_Winter_WS = "12 1 2",
  as_Spring ="3 4 5",
  as_Summer = "6 7 8",
  as_Autumn = "9 10 11")


downloadTD3505(karr) # could combine into one step
downloadTD6405(karr)
downloadTD6401(karr)
downloadFSL(karr)


karr <- createInput(karr, c("aerminute", "aersurface")) # probably should combine this into one step
karr <- writeInputFile(karr, "aerminute")


# protofunction to execute aerminute_exe
aerminute = "C:/aerminute_15272/aerminute_15272.exe"
sapply(seq_along(karr$inputFiles$aerminute), function(i) {
  system(aerminute, input=karr$inputFiles$aerminute[[i]])
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



