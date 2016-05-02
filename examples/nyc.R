nyc <- createMetProject(
  project_Name = "Central Park, NYC",
  project_Dir="~/test/knyc",
  start_Date = lubridate::mdy_hm("01/01/2013 00:00", tz="Etc/GMT+5"),
  end_Date = lubridate::mdy_hm("03/31/2016 23:00", tz="Etc/GMT+5"),
  surf_UTC = -5,
  surf_WBAN = 94728,
  surf_USAF = 725053,
  surf_Call="KNYC",
  ua_WMO=72249,
  ua_UTC=0)

downloadTD3505(nyc)
downloadTD6405(nyc)
downloadTD6401(nyc)
downloadFSL(nyc)


