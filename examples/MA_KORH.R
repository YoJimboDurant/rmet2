library(rmet2)
library(tidyverse)
library(FedData)
library(sf)
library(terra)
library(magrittr)


# if not blocked by a firewall you can get data from ftp site quicker:
# options(rmet.noaa.site = "ftp://ftp.ncdc.noaa.gov/pub/data/",
#         rmet.noaa.surfhist = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt"
# )

# mesoData calls the mesowest API to get some location data.
# this function is blocked by my firewall, so manually look at https://mesowest.utah.edu/cgi-bin/droman/meso_base_dyn.cgi?stn=KORH 
# to manually get the data.

# Here I create a data.frame
station_mesowest <- tibble(surf_Latitude = 42.27056,
                           surf_Longitude = -71.87306,
                           surf_Elevation = 1007 * 0.3048)

# station_mesowest  <- mesoData(station = "KORH", API_KEY = "22f95bf3922944f0af53381460bdc2a4")

# Orignal: 30.133N   93.217W 
# Google Earth surface : 30.1252332 -93.2275158
# RUC upper 30.12 -93.22


# the rootDir is where you want the files and output to go (I changed to C).
rootDir = "C:/rmet2/MA/KORH"


# Download and Check Surface Land Classification Files --------------------


# download aermap data files:

#   Create a ~10 buffer for download:
sfc_station_point <-
  data.frame(long =  station_mesowest$surf_Longitude, lat =   station_mesowest$surf_Latitude) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)


# you should always verify this location for surface station as AERSURFACE uses 30m grids to calculate roughness, bowen ratio and albedo

# plotKML will plot on google earth if you have it installed.

plotKML::plotKML(sfc_station_point)



sfc_station <- sfc_station_point %>%
  st_buffer(dist = 5500)


# Download land use data using buffer - landcover
lc_2016 <-
  get_nlcd(
    as(sfc_station, "Spatial"),
    dataset = "landcover",
    label ="landcover",
    extraction.dir = paste(rootDir, "preprocessed_data", sep = "/"),
    force.redo = T,
    year = 2016,
    raster.options =c("COMPRESS=NONE", "TFW=YES", "datatype=INT1U")
  )

# tree cover
tc_2016 <-
  get_nlcd(
    as(sfc_station, "Spatial"),
    dataset = "canopy",
    label = "canopy",
    extraction.dir = paste(rootDir, "preprocessed_data", sep = "/"),
    force.redo = T,
    year = 2016,
    raster.options =c("COMPRESS=NONE", "TFW=YES", "datatype=INT1U")
  )


# impervious cover
imp_2016 <-
  get_nlcd(
    as(sfc_station, "Spatial"),
    dataset = "impervious",
    label = "Imperv",
    extraction.dir = paste(rootDir, "preprocessed_data", sep = "/"),
    force.redo = T,
    year = 2016,
    raster.options =c("COMPRESS=NONE", "TFW=YES", "datatype=INT1U")
  )

# Add some projection information
lc_2016aea <- project(
  rast(lc_2016),
  y = " +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
  method = "near",
  res = c(30, 30),
  wopt = list(gdal = c("COMPRESS=NONE", "TFW=YES")),
  filename = gsub("[.]tif", "_rmet2.tif", raster::filename(lc_2016)),
  overwrite = T,
  datatype = "INT1U"
)

tc_2016aea <-
  project(
    rast(tc_2016),
    y = " +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    method = "near",
    res = c(30, 30),
    wopt = list(gdal = c("COMPRESS=NONE", "TFW=YES")),
    filename = gsub("[.]tif", "_rmet2.tif", raster::filename(tc_2016)),
    overwrite = T
  )
imp_2016aea <-
  project(
    rast(imp_2016),
    y = " +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    method = "near",
    res = c(30, 30),
    wopt = list(gdal = c("COMPRESS=NONE", "TFW=YES")),
    filename = gsub("[.]tif", "_rmet2.tif", raster::filename(imp_2016)),
    overwrite = T
  )

# writeRaster(lc_2016aea, raster::filename(lc_2016aea), overwrite=TRUE, wopt= list(gdal=c("COMPRESS=NONE"), datatype='INT1U'))

# check landcover file

library(leaflet)
library(leaflet.opacity)

legend_dict = c(
  '11 Open Water',
  '12 Perennial Ice/Snow',
  '21 Developed, Open Space',
  '22 Developed, Low Intensity',
  '23 Developed, Medium Intensity',
  '24 Developed High Intensity',
  '31 Barren Land (Rock/Sand/Clay)',
  '41 Deciduous Forest',
  '42 Evergreen Forest',
  '43 Mixed Forest',
  '51 Dwarf Scrub',
  '52 Shrub/Scrub',
  '71 Grassland/Herbaceous',
  '72 Sedge/Herbaceous',
  '73 Lichens',
  '74 Moss',
  '81 Pasture/Hay',
  '82 Cultivated Crops',
  '90 Woody Wetlands',
  '95 Emergent Herbaceous Wetlands'
)


# colortable for values:
colors_nlcd2016 <-
  colorFactor(
    c(
      "#466B9F",
      "#D1DEF8",
      "#DEC5C5",
      "#D99282",
      "#EB0000",
      "#AB0000",
      "#B3AC9F",
      "#68AB5F",
      "#1C5F2C",
      "#B5C58F",
      "#af963c",
      "#CCB879",
      "#DFDFC2",
      "#d1d182",
      "#a3cc51",
      "#82ba9e",
      "#DCD939",
      "#AB6C28",
      "#B8D9EB",
      "#6C9FB8"
    ),
    domain = c(
      11,
      12,
      21,
      22,
      23,
      24,
      31,
      41,
      42,
      43,
      51,
      52,
      71,
      72,
      73,
      74,
      81,
      82,
      90,
      95
    )
  )

legend_colors_nlcd2016 <-
  colorFactor(
    c(
      "#466B9F",
      "#D1DEF8",
      "#DEC5C5",
      "#D99282",
      "#EB0000",
      "#AB0000",
      "#B3AC9F",
      "#68AB5F",
      "#1C5F2C",
      "#B5C58F",
      "#af963c",
      "#CCB879",
      "#DFDFC2",
      "#d1d182",
      "#a3cc51",
      "#82ba9e",
      "#DCD939",
      "#AB6C28",
      "#B8D9EB",
      "#6C9FB8"
    ),
    domain = legend_dict
  )



leaflet() %>% addProviderTiles("Esri.WorldImagery") %>%
  addRasterImage(
    raster::ratify(lc_2016),
    opacity = 0.5,
    colors = colors_nlcd2016,
    layer = "raster"
  ) %>%
  addOpacitySlider(layerId = "raster") %>%
  addPolygons(
    data = st_transform(sfc_station, crs = 4326),
    fill = NA,
    weight = 1.5
  ) %>%
  addCircleMarkers(data = sfc_station_point, label = "KORH") %>%
  addLegend(
    "bottomright",
    pal = legend_colors_nlcd2016,
    legend_dict,
    title = "Land Use",
    opacity = 1
  )


# install processors
installAM(rootDir = rootDir)

# set options
# options(https://urldefense.proofpoint.com/v2/url?u=http-3A__rmet.noaa.site&d=DwIGaQ&c=shNJtf5dKgNcPZ6Yh64b-ALLUrcfR-4CCQkZVKC8w3o&r=5olrAxxTicif5iATw5WKfvKxJwjyGgrebTLp73BC1dE&m=11NYzCew_LwLEZj7W4Gd7Y_S_2mflfKk71camx-ycX3qsv5s9m2fZUGQjksJkcXZ&s=AQz9Nx3hGP028WvPfjaNjwp5_1Olot8k_KebYxuJxGU&e=  = "https://urldefense.proofpoint.com/v2/url?u=ftp-3A__ftp.ncdc.noaa.gov_pub_data_&d=DwIGaQ&c=shNJtf5dKgNcPZ6Yh64b-ALLUrcfR-4CCQkZVKC8w3o&r=5olrAxxTicif5iATw5WKfvKxJwjyGgrebTLp73BC1dE&m=11NYzCew_LwLEZj7W4Gd7Y_S_2mflfKk71camx-ycX3qsv5s9m2fZUGQjksJkcXZ&s=EEMv4iZARL13RAjfViBvTHq1FNBfgU02fvpeRYPMbYI&e= ",
#         rmet.noaa.surfhist = "https://urldefense.proofpoint.com/v2/url?u=ftp-3A__ftp.ncdc.noaa.gov_pub_data_noaa_isd-2Dhistory.txt&d=DwIGaQ&c=shNJtf5dKgNcPZ6Yh64b-ALLUrcfR-4CCQkZVKC8w3o&r=5olrAxxTicif5iATw5WKfvKxJwjyGgrebTLp73BC1dE&m=11NYzCew_LwLEZj7W4Gd7Y_S_2mflfKk71camx-ycX3qsv5s9m2fZUGQjksJkcXZ&s=m67uky582hkoi8yj5MkFek7kExHm-Z9EmE2IJqRExx8&e= "
# )


# Create Project ----------------------------------------------------------


# create project
# this creates an rds file with the information. IF you want to recreate or modify it you MUST delete in in the rootdir
KORH <- createMetProject(
  project_Name = "WORCESTER_RGNL_AP",
  project_Dir=rootDir,
  start_Date = lubridate::mdy_hm("01/01/2015 00:00", tz="Etc/GMT+5"),
  end_Date = lubridate::mdy_hm("12/31/2017 23:59", tz="Etc/GMT+5"),
  surf_UTC = -5,
  surf_WBAN = "94746", 
  surf_USAF =  725100,
  surf_Call="KORH",
  surf_Latitude = as.numeric(station_mesowest$surf_Latitude), 
  surf_Longitude = as.numeric(station_mesowest$surf_Longitude),
  surf_Elevation = as.numeric(station_mesowest$surf_Elevation),
  surf_AnenometerHeight = 10,
  #  ua_WMO = 74494,
  ua_WMO = 72518,
  #  ua_WMO = 74389,
  ua_UTC= -5,
  #  ua_Latitude = 41.6569,
  #  ua_Longitude = -69.9589,
  ua_Latitude = 42.7431,
  ua_Longitude = -73.8092,
  #  ua_Latitude = 43.8925,
  #  ua_Longitude =  -70.2572,
  ifg = "Y 03 28 2007",
  lc_File = sources(lc_2016aea),
  lc_Type = "NLCD2016",
  imp_File = sources(imp_2016aea),
  imp_Type = "MPRV2016",
  cnpy_File = sources(tc_2016aea),
  cnpy_Type = "CNPY2016",
  as_Snow = "Y",
  as_Arid = "N",
  as_Moisture = "A",
  as_Airport = "Y",
  as_Winter_NS = NULL,
  as_Winter_WS = "12 1 2",
  as_Spring ="3 4 5",
  as_Summer = "6 7 8",
  as_Autumn = "9 10 11"
)



# Download Surface Data ---------------------------------------------------


downloadTD6405(KORH, check=TRUE)
downloadTD6401(KORH, check=TRUE)
downloadTD3505(KORH, check=TRUE)


# AERMINUTE --------------------------------------------------------------
KORH <- createInput(KORH) %>% # probably should combine this into one step 
    writeInputFile(., c("aerminute", "aersurface")) %>%
    processMet(., processor=c("aerminute","aersurface"))

qaAerminute(KORH) %>% pander::pander(.)

aerminute.sum <- lapply(2015:2017,  function(x){
  read_csv(paste0(rootDir, "/", x, "/", "AM_1MIN_",x, "_summ.DAT"))
}) %>%
  bind_rows() %>%
  mutate(Date = lubridate::ymd_h(paste(Date,as.numeric(hr) - 1), tz = lubridate::tz(KORH$start_Date)))

# Total minutes used
aerminute.sum %>%
  ggplot(aes(x=Date, y = `total minutes`)) + geom_point(alpha = 0.4, size = 0.6) + theme_light()

#You can also look at average wind speed:

aerminute.sum %>%
  mutate(`avg speed` = ifelse(`avg speed` == 999, NA, `avg speed`)) %>%
  ggplot(aes(x=Date, y = `avg speed`)) + geom_point(alpha = 0.1, size = 0.6) + geom_smooth() + theme_light()

#And average wind direction:

aerminute.sum %>%
  mutate(`avg dir` = ifelse(`avg dir` == 999, NA, `avg dir`)) %>%
  ggplot(aes(x=Date, y = `avg dir`)) + geom_point(alpha = 0.5, size = 0.5)  + theme_light()




# AERMET ------------------------------------------------------------------
# download upper air data and create/process aermet stage 1
downloadFSL(KORH)  
KORH <- createInput(KORH, type = "aermet1")
cat(KORH$inputText$aermet$s1[[1]])
KORH <- processMet(KORH, processor = c("aermet1"))

# with the new 21112 version of aermet, we need to combine stage 2 and stage 3.
# to do this, we have to modify the text of input for stage 2 for now as suggested
# in the documentation. Here is where regular expressions have saved my life.


KORH <- createInput(KORH, type = c("aermet23"))

# use purr and gsub to combine the text files:

KORH$inputText$aermet$s2 <-
  purrr::imap(
    KORH$inputText$aermet$s2,
    ~ paste0(.x, gsub("JOB.+[.]MSG\"", "", 
                      gsub("DATA.+MRG\\n", "",      KORH$inputText$aermet$s3[[.y]])),
             "\n", paste(KORH$output$aersurface$surface, collapse = "\n")
    )
  )


cat(KORH$inputText$aermet$s2[[1]]) # and viola!
KORH <- processMet(KORH, processor = c("aermet2"))


# Check Final -------------------------------------------------------------


makeFinal.rmet(KORH, "KORH")


klch_sfc <- surfaceReader(sfile = paste(rootDir, "KORH.sfc", sep = "/"), rmetObj = KORH)
klch_sfc %>% 
  ggplot(aes(x = factor(hour),  y = monin_obukhov_length)) + 
  geom_boxplot() + theme_light()

openair::windRose(klch_sfc)
openair::windRose(klch_sfc, type = "season")
openair::windRose(klch_sfc, type = "daylight", lat = KORH$surf_Latitude, lon = KORH$surf_Longitude)

openair::polarFreq(klch_sfc, "surface_roughness")

ggplot(klch_sfc, aes(date, ws)) + geom_point(size = 0.1)


surfCheck(KORH)
