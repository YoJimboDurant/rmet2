karr <- createMetProject(
  project_Name = "CHICAGO AURORA MUNI AP",
  project_Dir="~/test/karr",
  start_Date = lubridate::mdy_hm("01/01/2012 00:00", tz="Etc/GMT+6"),
  end_Date = lubridate::mdy_hm("03/31/2016 23:00", tz="Etc/GMT+6"),
  surf_UTC = -6,
  surf_WBAN = 04808,
  surf_USAF = 744655,
  surf_Call="KARR",
  ua_WMO=74560,
  ua_UTC=0,
  ifg = "Y 12 06 2002")

downloadTD3505(karr)
downloadTD6405(karr)
downloadTD6401(karr)

downloadFSL(karr)


karr <- createInput(karr, "aerminute")
karr <- writeInputFile(karr, "aerminute")

aerminute = "C:/aerminute_15272/aerminute_15272.exe"
system(aerminute, input=paste0("\"",karr$inputFiles$aerminute[[1]],"\""))
