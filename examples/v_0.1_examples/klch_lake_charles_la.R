library(rmet2)
library(tidyverse)
library(FedData)
library(sf)

# USAF ID 03937
# Upper Air: 3937 72240
# 	KLCH

# mesoData calls the mesowest API to get some location data.
station_mesowest <- mesoData(station = "KLCH", API_KEY = "57f54928e49d4687858b06695f4068c6") # this is the test API from website

as.data.frame(station_mesowest)


# Orignal: 30.133N   93.217W 
# Google Earth surface : 30.1252332 -93.2275158
# RUC upper 30.12 -93.22


# the rootDir is where you want the files and output to go.
rootDir = "C:/rmet2/LA/LAKE_CHARLES_AP"


# Download and Check Surface Land Classification Files --------------------


# download aermap data files:

#   Create a ~10 buffer for download:
sfc_station_point <-
  data.frame(long =  station_mesowest$surf_Longitude, lat =   station_mesowest$surf_Latitude) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)


plotKML::plotKML(sfc_station_point)

sfc_station <- sfc_station_point %>%
  st_transform(crs = 26916) %>%
  st_buffer(dist = 5500) %>%
  st_bbox() %>%
  st_as_sfc()

# Download
lc_2016 <-
  get_nlcd(
    as(sfc_station, "Spatial"),
    label = "land_cover",
    extraction.dir = paste(rootDir, "preprocessed_data", sep = "/"),
    force.redo = T,
    year = 2016
  )
tc_2016 <-
  get_nlcd(
    as(sfc_station, "Spatial"),
    dataset = "canopy",
    label = "canopy",
    extraction.dir = paste(rootDir, "preprocessed_data", sep = "/"),
    force.redo = T,
    year = 2016
  )
imp_2016 <-
  get_nlcd(
    as(sfc_station, "Spatial"),
    dataset = "impervious",
    label = "Imperv",
    extraction.dir = paste(rootDir, "preprocessed_data", sep = "/"),
    force.redo = T,
    year = 2016
  )

# Add some projection information
lc_2016aea <- raster::projectRaster(
  lc_2016,
  crs = " +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
  method = "ngb",
  res = c(30, 30),
  options = c("COMPRESS=NONE", "TFW=YES"),
  filename = gsub("[.]tif", "_rmet.tif", raster::filename(lc_2016)),
  overwrite = T
)

tc_2016aea <-
  raster::projectRaster(
    tc_2016,
    crs = " +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    method = "ngb",
    res = c(30, 30),
    options = c("COMPRESS=NONE", "TFW=YES"),
    filename = gsub("[.]tif", "_rmet.tif", raster::filename(tc_2016)),
    overwrite = T
  )
imp_2016aea <-
  raster::projectRaster(
    imp_2016,
    crs = " +proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs",
    method = "ngb",
    res = c(30, 30),
    options = c("COMPRESS=NONE", "TFW=YES"),
    filename = gsub("[.]tif", "_rmet.tif", raster::filename(imp_2016)),
    overwrite = T
  )

# check file

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
    raster::ratify(lc_2016aea),
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
  addCircleMarkers(data = sfc_station_point, label = "KLCH") %>%
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
# options(rmet.noaa.site = "ftp://ftp.ncdc.noaa.gov/pub/data/",
#         rmet.noaa.surfhist = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt"
# )


# Create Project ----------------------------------------------------------


# create project

KLCH <- createMetProject(
  project_Name = "Lake_Charles_AP",
  project_Dir=rootDir,
  start_Date = lubridate::mdy_hm("01/01/2017 00:00", tz="Etc/GMT+6"),
  end_Date = lubridate::mdy_hm("12/31/2021 23:59", tz="Etc/GMT+6"),
  surf_UTC = -6,
  surf_WBAN = "03937",
  surf_USAF =  722400,
  surf_Call="KLCH",
  surf_Latitude = station_mesowest$surf_Latitude, 
  surf_Longitude = station_mesowest$surf_Longitude,
  surf_Elevation = station_mesowest$surf_Elevation,
  surf_AnenometerHeight = 10,
  ua_WMO = 72240,
  ua_UTC= -6,
  ua_Latitude = 30.12,
  ua_Longitude = -93.28,
  ifg = "Y 04 19 2007",
  lc_File = raster::filename(lc_2016aea),
  lc_Type = "NLCD2016",
  imp_File = raster::filename(imp_2016aea),
  imp_Type = "MPRV2016",
  cnpy_File = raster::filename(tc_2016aea),
  cnpy_Type = "CNPY2016",
  as_Snow = "N",
  as_Arid = "N",
  as_Moisture = "A",
  as_Airport = "Y",
  as_Winter_NS ="12 1 2",
  as_Winter_WS = NULL,
  as_Spring ="3 4 5",
  as_Summer = "6 7 8",
  as_Autumn = "9 10 11",
  onsite_Latitude = NULL,
  onsite_Longitude = NULL,
  onsite_Fstring = NULL
)


# Download Surface Data ---------------------------------------------------



downloadTD6405(KLCH, check=TRUE)
downloadTD6401(KLCH, check=TRUE)
downloadTD3505(KLCH, check=TRUE)


# AERMINUTE ---------------------------------------------------------------


KLCH <- createInput(KLCH, type = "aerminute")
cat(KLCH$inputText$aerminute[[1]]) # looking at individual file


KLCH$outputFiles$aerminute # where the output files are going (should be five)

KLCH <- writeInputFile(KLCH, type = "aerminute")
KLCH$inputFiles$aerminute

KLCH <- processMet(KLCH, processor="aerminute")

qaAerminute(KLCH) %>% pander::pander(.)

aerminute.sum <- lapply(2017:2021,  function(x){
  read_csv(paste0(rootDir, "/", x, "/", "AM_1MIN_",x, "_summ.DAT"))
}) %>%
  bind_rows() %>%
  mutate(Date = lubridate::ymd_h(paste(Date,as.numeric(hr) - 1), tz = lubridate::tz(KLCH$start_Date)))

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



# AERSURFACE --------------------------------------------------------------


KLCH <- createInput(KLCH, type = "aersurface_nws")
cat(KLCH$inputText$aersurface[[1]])
KLCH <- writeInputFile(KLCH, type = "aersurface")
KLCH <- processMet(KLCH, processor =  "aersurface")
KLCH$output$aersurface[[1]]



# AERMET ------------------------------------------------------------------

downloadFSL(KLCH)
KLCH <- createInput(KLCH, type = "aermet1")
cat(KLCH$inputText$aermet$s1[[1]])
KLCH <- processMet(KLCH, processor = c("aermet1"))

# with the new 21112 version of aermet, we need to combine stage 2 and stage 3.
# to do this, we have to modify the text of input for stage 2 for now as suggested
# in the documentation. Here is where regular expressions have saved my life.


KLCH <- createInput(KLCH, type = "aermet2")
KLCH <- createInput(KLCH, type = "aermet3")

# use purr and gsub to combine the text files:

KLCH$inputText$aermet$s2 <-
  purrr::imap(
  KLCH$inputText$aermet$s2,
  ~ paste0(.x, gsub("JOB.+[.]MSG\"", "", 
               gsub("DATA.+MRG\\n", "",      KLCH$inputText$aermet$s3[[.y]])),
           "\n", paste(KLCH$output$aersurface$surface, collapse = "\n")
  )
)


cat(KLCH$inputText$aermet$s2[[1]]) # and viola!
KLCH <- processMet(KLCH, processor = c("aermet2"))


# Check Final -------------------------------------------------------------


makeFinal.rmet(KLCH, "KLCH")


klch_sfc <- surfaceReader(sfile = "C:/rmet2/LA/LAKE_CHARLES_AP/KLCH.sfc", rmetObj = KLCH)
klch_sfc %>% 
  ggplot(aes(x = factor(hour),  y = monin_obukhov_length)) + 
  geom_boxplot() + theme_light()

openair::windRose(klch_sfc)
openair::windRose(klch_sfc, type = "season")
openair::windRose(klch_sfc, type = "daylight", lat = KLCH$surf_Latitude, lon = KLCH$surf_Longitude)

openair::polarFreq(klch_sfc, "surface_roughness")

ggplot(klch_sfc, aes(date, ws)) + geom_point(size = 0.1)


surfCheck(KLCH)
