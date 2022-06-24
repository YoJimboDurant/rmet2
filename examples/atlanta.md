Atlanta Example of RMET2
================

# Using R to process Air Dispersion Meteorology Data (AERMET)

    ## PhantomJS not found. You can install it with webshot::install_phantomjs(). If it is installed, please make sure the phantomjs executable can be found via the PATH variable.

<div class="figure">

<div id="htmlwidget-fe6738bbbf75f19118ce" style="width:672px;height:500px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-fe6738bbbf75f19118ce">{"x":{"diagram":"digraph {\n  graph [layout = dot, rankdir = TB]\n\n  node [shape = rectangle]\n  AERMINUTE [label = \"AERMINUTE\", style=dashed, color=grey]\n  AERMET1 [label = \"AERMET STAGE 1\"]\n  AERMET2 [label =  \"AERMET STAGE 2\"]\n  AERMET3 [label = \"AERMET STAGE 3\"]\n  AERSURFACE [label = \"AERSURFACE\", style=dashed, color=grey]\n  PROFILE [label = \"Profile File\", shape = folder]\n  SURFACE [label = \"Surface File\", shape = folder]\n  \n  # edge definitions with the node IDs\n  AERMINUTE -> AERMET2[style=dashed, color=grey]\n  AERMET1 -> AERMET2 \n  AERMET2 -> AERMET3\n  AERSURFACE -> AERMET3[style=dashed, color=grey]\n  AERMET3 -> SURFACE\n  AERMET3 -> PROFILE\n  }","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>
<p class="caption">
Meteorological File Preparation for AERMOD
</p>

</div>

**Notes:**

1.  AERMINUTE - (optional) extract and quality assure (QA) minute level
    data from National Weather Service Automated Surface Observation
    Station (NWS ASOS) and calculate hourly wind speed and direction.
2.  AERMET stage 1

-   (optional) Take output from AERMINUTE and QA.
-   Extract and QA hourly surface data from a variety of formats.
-   Extract and QA upper air data.
-   (optional) extract and QA on site data (requires user specified
    FORTRAN format codes)

3.  AERMET stage 2 - Uses output from AERMET stage 1, prepare
    intermediate files for AERMET stage 3.
4.  AERSURFACE (optional) uses National Land Cover Dataset data and
    determine Albedo, Surface Roughness, and Bowen Ratio.
5.  AERMET stage 3 - Uses output from AERMET stage 2, and AERSURFACE (or
    user specified surface characteristics) and calculate dispersion
    parameters (surface file) and wind speed and wind direction by
    height (profile file).

The meteorological processors are text based and their output is text
based. motivation for `rmet2` was to use R to perform:

-   downloading of meteorological and land use files,
-   setting up the input files, and
-   visual and tabular summaries of the intermediate and file outputs.

## rmet 2

### rmet2 object

The basic structure of `rmet2` is the `rmet` class object. It is a list
object containing the needed information and options to download the
land use and meteorology files and set up the meteorology process files.
Many `rmet2` function expect and will modify and return `rmet2` class
objects.

The class of the object will change the behavior of the functions. In
the case or `rmet2` the functions will verify that the correct
information is present in the object before the operation is performed.

During creation of the `rmet2` object, `rmet2` verifies that the needed
meteorological files exist and will return an error if files are missing
on the National Climatic Data Center site.

When `rmet2` objects are created, the object is saved in the working
directory for the project as a binary data file. If a new object is
created, the old object will not be replaced, and instead the old one
will be reloaded. This persistence encourages users to keep and preserve
records of AERMET files created and not overwrite them unless explicitly
removed by the program or the user.

### Installing rmet2

Installation from github using devtools will check and ensure that the
package is always up to date:

``` r
if(!"devtools" %in% installed.packages()) install.pacakges(devtools)
devtools::install_git("https://github.com/YoJimboDurant/rmet2", ref = "dev")
```

There are 2 problems with getting data from NOAA:

1.  Lately, access to the noaa website
    <https://www1.ncdc.noaa.gov/pub/data/noaa/> has been intermittent.
2.  You can access the the ftp site, but CDC’s firewall universally
    blocks access to ftp sites (even government sites).

You can get an exception, but it takes a lot of information and
approvals, and ITSO has to successfully configure your computer to have
a static IP address, and then the firewall people need to put in the
exception successfully.

All in all it is frankly easier to login to the guest wifi at campus and
download via that site. To set rmet2 to work with the ftp sites, you
need to change 2 options prior to loading `rmet2`:

``` r
options(rmet.noaa.site = "ftp://ftp.ncdc.noaa.gov/pub/data/",
    rmet.noaa.surfhist = "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt"
)
```

In this example we will proceed with using the www1 noaa site.

``` r
library(rmet2)
```

    ## Warning in file(con, "r"): the 'wininet' method of url() is deprecated for
    ## ftp:// URLs

    ## Warning: 1 failed to parse.

    ## Welcome to rmet! 
    ## 
    ##                         current rmet options are set to:

    ## $rmet.aermet
    ## [1] "aermet"
    ## 
    ## $rmet.aerminute
    ## [1] "aerminute"
    ## 
    ## $rmet.aersurface
    ## [1] "aersurface"
    ## 
    ## $rmet.desc
    ## list()
    ## 
    ## $rmet.desc.author
    ## [1] "\"James Durant <hzd3@cdc.gov> [aut, cre]\""
    ## 
    ## $rmet.desc.license
    ## [1] "MIT"
    ## 
    ## $rmet.install.args
    ## [1] ""
    ## 
    ## $rmet.name
    ## [1] "rmet"
    ## 
    ## $rmet.noaa.site
    ## [1] "ftp://ftp.ncdc.noaa.gov/pub/data/"
    ## 
    ## $rmet.noaa.surfhist
    ## [1] "ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.txt"

    ## Warning in file(con, "r"): the 'wininet' method of url() is deprecated for
    ## ftp:// URLs

    ## Warning in file(con, "r"): 1 failed to parse.

Besides information about the author/creator of the package, `rmet2`
will print information that will be useful for debugging:

-   options that are set (aermet, aerminute, aersurface) which is the
    location of the executables for the processors (These can be
    modified direction using the `options` function or can be modified
    by some functions within rmet2 (e.g. `installAM`)),
-   the location of the ncdc database online, and
-   the location of the history file used to determine the availability
    of surface data. This loaded dynamically every time the library is
    called.

## Example Site - Atlanta Hartsfield Jackson Airport, GA

``` r
mapstation("33.831581 -84.468445")
```

<div id="htmlwidget-f10b1abc24264fde3843" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-f10b1abc24264fde3843">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":1,"detectRetina":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addMarkers","args":[[33.63,33.778,33.767,33.878,33.867,33.917,33.917,33.917,34.013,34.013,33.917,33.63,33.867],[-84.442,-84.525,-84.517,-84.298,-84.3,-84.517,-84.517,-84.517,-84.599,-84.599,-84.517,-84.442,-84.3],null,null,null,{"interactive":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},null,null,null,null,["HARTSFIELD-JACKSON ATLANTA IN &lt;br&gt;22.53 km from your site&lt;br&gt;USAF = 722190  | WBAN = 13874 ","FULTON CO-BROWN FLD ARPT      &lt;br&gt; 7.96 km from your site&lt;br&gt;USAF = 722195  | WBAN = 03888 ","FULTON CO ARPT BROW           &lt;br&gt; 8.50 km from your site&lt;br&gt;USAF = 722195  | WBAN = 99999 ","DEKALB-PEACHTREE AIRPORT      &lt;br&gt;16.57 km from your site&lt;br&gt;USAF = 722196  | WBAN = 53863 ","DEKALB PEACHTREE              &lt;br&gt;16.05 km from your site&lt;br&gt;USAF = 722196  | WBAN = 99999 ","DOBBINS AIR RESERVE BASE AIRP &lt;br&gt;10.46 km from your site&lt;br&gt;USAF = 722270  | WBAN = 13864 ","MARIETTA/DOBBINS AF           &lt;br&gt;10.46 km from your site&lt;br&gt;USAF = 722270  | WBAN = 99999 ","ATLANTA (NAVRES)              &lt;br&gt;10.46 km from your site&lt;br&gt;USAF = 722273  | WBAN = 99999 ","COBB CO-MC COLLUM FLD ARPT    &lt;br&gt;23.44 km from your site&lt;br&gt;USAF = 747812  | WBAN = 63813 ","COBB CO MCCOLLUM FLD          &lt;br&gt;23.44 km from your site&lt;br&gt;USAF = 747812  | WBAN = 99999 ","MARIETTA DOBBINS AAF          &lt;br&gt;10.46 km from your site&lt;br&gt;USAF = 999999  | WBAN = 13864 ","ATLANTA HARTSFIELD INTL AP    &lt;br&gt;22.53 km from your site&lt;br&gt;USAF = 999999  | WBAN = 13874 ","ATLANTA NAS                   &lt;br&gt;16.05 km from your site&lt;br&gt;USAF = 999999  | WBAN = 93830 "],{"interactive":true,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]},{"method":"addCircleMarkers","args":[33.8319074217608,-84.468269082241,10,null,null,{"interactive":true,"className":"","stroke":true,"color":"red","weight":5,"opacity":0.5,"fill":true,"fillColor":"red","fillOpacity":0.2},null,null,null,null,null,{"interactive":false,"permanent":false,"direction":"auto","opacity":1,"offset":[0,0],"textsize":"10px","textOnly":false,"className":"","sticky":true},null]}],"limits":{"lat":[33.63,34.013],"lng":[-84.599,-84.298]}},"evals":[],"jsHooks":[]}</script>

For this example, we will select Atlanta Hartsfield Jackson Airport.

### NWS Station Details

I recommend gathering station information from mesonet. The page for
Atlanta is
<https://mesonet.agron.iastate.edu/sites/site.php?network=GA_ASOS&station=ATL>:

-   WBAN 13874 (Weather Bureau Army - Navy)
-   USAF 722190 (US Air Force)
-   Call: KATL
-   Latitude: 33.63010
-   Longitude: -84.44180
-   Elevation: 315

We need to check and verify if minute level data are available at NCDC:

<https://www1.ncdc.noaa.gov/pub/data/asos-onemin/6405-2020/>
<https://www1.ncdc.noaa.gov/pub/data/asos-fivemin/6401-2020/>

Finally we need to find ice free wind speed sensor date:
<https://www.weather.gov/media/asos/ASOS%20Implementation/IFW_stat.pdf>.
The significance of this is the threshold of sensitivity for the sensor.
Before the installation of the ice free sensor AERMINUTE output data are
censored below a threshold and after installation the wind speed is not
censored at all.

-   ice free winds: 3/27/2007

### Upper Air Stations

[Radiosonde](https://www.weather.gov/upperair/factsheet) data is needed
to for AERMET to understand the vertical stability of the atmosphere. So
a regional radiosonde data needs to be downloaded from
<https://ruc.noaa.gov/raobs/>. `rmet2` can facilitate this process, but
you need to identify which station you need to download data from.

Select state, then sort by station, and identify the station you want to
use. In this case, it is Peachtree City, Georgia.

-   WMO 72215
-   Latitude 33.35
-   Longitude -84.56

You only need WMO and latitude and longitude for `rmet2`. The WBAN
number is not present in some upper air stations.

### Where do you want to store all the files?

For the project, I recommend establishing a system of directories to
store your projects on. In this example I will store the directory as
`rootDir` and is specific for my project:

``` r
rootDir = "C:/rmet2/GA/ATLANTA_INT_AP"
```

### Snowcover Information

Although not an issue in Georgia, in other states, there is this strange
whitish state of water that covers the land for extended periods of
time.

This is called snow.

Since snow cover effects Bowen ratio, Albedo and surface roughness and
needs to be accounted for in AERSURFACE. You can use `rnoaa`
(<https://github.com/ropensci/rnoaa>) to get 30-year climate normals to
gather snow cover information. Note you will need to install rnoaa and
obtain an API key (see
<https://docs.ropensci.org/rnoaa/articles/rnoaa.html>). You will also
need the FIPS code for the county you are in:

``` r
library(rnoaa)
library(FedData)
library(tidyverse)
library(magrittr)
library(sf)

# you can run with FIPS:17097 for more interesting example

x <- ncdc_stations(datatypeid='mly-snwd-avgnds-ge001wi', locationid = 'FIPS:17097')
print(x$data[c("name","id","mindate","maxdate")])
```

    ##                     name                id    mindate    maxdate
    ## 1         ANTIOCH, IL US GHCND:USC00110203 1901-07-01 2010-12-31
    ## 2 BARRINGTON 3 SW, IL US GHCND:USC00110442 1962-01-01 2022-06-20
    ## 3 LAKE VILLA 2 NE, IL US GHCND:USC00114837 1985-01-01 2010-12-31
    ## 4 MUNDELEIN 4 WSW, IL US GHCND:USC00115961 1999-05-01 2022-06-20

We need to recode data to get the data into a data.frame:

``` r
snowcover <- lapply(x$data$id, function(x){
  ncdc(datasetid = "NORMAL_MLY", datatypeid = "mly-snwd-avgnds-ge001wi", 
       stationid = x,
       startdate = "2010-01-01", 
       enddate="2010-12-31", limit=365)
}
)


snowcover_lx <- lapply(snowcover, function(x) {
  x$data$value[x$data$value == -7777] <- 0
  x$data$value[x$data$value == -9999] <- NA
  x$data$value <- x$data$value/10
  return(x)
})

snowcover_dfx <- Reduce(function(...) ncdc_combine(...), snowcover_lx)$data
```

To make a plot (by station):

``` r
ggplot(snowcover_dfx, aes(x=date, y=value, shape=station, group=station)) + 
  geom_point(size=4) + geom_line() + xlab("Date") + ylab("Days with > 1 inch Snow Cover") +
  theme_light() + theme(axis.text.x = element_text(angle = 90))
```

![](atlanta_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

To make a table:

``` r
snowcover_dfx %>% group_by(station) %>%
  dplyr::summarise(total_snow = sum(value))
```

    ## # A tibble: 4 x 2
    ##   station           total_snow
    ##   <chr>                  <dbl>
    ## 1 GHCND:USC00110203       53.7
    ## 2 GHCND:USC00110442       60.1
    ## 3 GHCND:USC00114837       61.9
    ## 4 GHCND:USC00115961       58.1

``` r
snowcover_dfx %<>% mutate(rate = value/30.4) # rate of snowcover

snowrate_dfx <- snowcover_dfx %>% dplyr::group_by(date) %>% dplyr::summarise(snowMean = round(mean(rate), 2)) %>%
  mutate(snowMean = ifelse(snowMean <0.01, 0, snowMean), month = 1:12)

snowrate_dfx
```

    ## # A tibble: 12 x 3
    ##    date                snowMean month
    ##    <chr>                  <dbl> <int>
    ##  1 2010-01-01T00:00:00     0.65     1
    ##  2 2010-02-01T00:00:00     0.54     2
    ##  3 2010-03-01T00:00:00     0.22     3
    ##  4 2010-04-01T00:00:00     0.02     4
    ##  5 2010-05-01T00:00:00     0        5
    ##  6 2010-06-01T00:00:00     0        6
    ##  7 2010-07-01T00:00:00     0        7
    ##  8 2010-08-01T00:00:00     0        8
    ##  9 2010-09-01T00:00:00     0        9
    ## 10 2010-10-01T00:00:00     0       10
    ## 11 2010-11-01T00:00:00     0.06    11
    ## 12 2010-12-01T00:00:00     0.44    12

Unsurprisingly we do not normally have appreciable snowcover in Clayton
County, where the airport is located.

### Download Land Use Files

In addition to the meteorological variables (wind speed, direction,
temperature, and solar radiation), we are going to need to know
something about the land around the station collecting these
measurements. Specifically, you are going to need:

1.  [Albedo](https://climate.ncsu.edu/edu/Albedo) - proportion of solar
    energy reflected at noon.

2.  [Bowen Ratio](https://en.wikipedia.org/wiki/Bowen_ratio) - ratio of
    latent to sensible heat flux [latent and sensible
    heat](https://climate.ncsu.edu/edu/Heat).

3.[Surface roughness
length](https://en.wikipedia.org/wiki/Roughness_length) - parameter that
specifies the height that the wind speed will be theoretically zero.

Albedo and Bowen ratio deal with heat energy and it’s effects of
turbulence. The surface roughness is used to calculate the profile of
wind speeds.

To get the land use data, we can use the package `FedData`. You can also
manually download these files as well from the [Multi-Resolution Landuse
Consortium](https://www.mrlc.gov/national-land-cover-database-nlcd-2016).

For data downloaded using `FedData` you will need to project from
Mercator into Alber’s Equal Area projection for AERSURFACE to use these
files. Therefore, get a slightly larger area. For instance, if you want
a 5 kilometer radius study area, then select 5.5 kilometers.

``` r
sfc_station_point <-
  data.frame(long = -84.44180, lat =  33.63010) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

sfc_station <- sfc_station_point %>%
  st_transform(crs = 26916) %>%
  st_buffer(dist = 5500) %>%
  st_bbox() %>%
  st_as_sfc()

# need to transform back to AEA projection
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
```

You should check the input files using leaflet, or you can export to
another GIS system. Note: in this notebook I am not evaluating these
lines because of markdown issues:

``` r
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
  addMarkers(data = sfc_station_point, label = "KATL") %>%
  addLegend(
    "bottomright",
    pal = legend_colors_nlcd2016,
    legend_dict,
    title = "Land Use",
    opacity = 1
  )
```

### Installing processors

You can either manually download and unzip the meteorological processors
and use the `options` function to set the aerminute, aersurface and
aermet location. Alternatively, you can use the `installAM` function. It
is also a great idea to keep these processors as part of your project as
they are updated every few years - and you can run into problems with
reproducibility. You can use the `exists = rep(TRUE, 3)` to avoid
overwriting a current install of the metoerological processors.

``` r
installAM(rootDir = rootDir)
```

### Creating an rmet2 object

As a first step, you need to identify and gather the appropriate
information for the analysis. Depending on the needs of the project, the
first step is generally the selection of the National Weather Service
station. `rmet2` provides a function that will pull a map that will help
in this process as shown above.

To create the Atlanta Airport Project:

``` r
KATL <- createMetProject(
  project_Name = "GA_Atlanta_International_AP",
  project_Dir=rootDir,
  start_Date = lubridate::mdy_hm("06/06/2019 00:00", tz="Etc/GMT+5"),
  end_Date = lubridate::mdy_hm("12/31/2020 23:00", tz="Etc/GMT+5"),
  surf_UTC = -5,
  surf_WBAN = 13874,
  surf_USAF = 722190,
  surf_Call="KATL",
  surf_Latitude = 33.63010,
  surf_Longitude = -84.44180,
  surf_Elevation = 315,
  surf_AnenometerHeight = 10,
  ua_WMO = 72215,
  ua_UTC=-5,
  ua_Latitude =  33.35,
  ua_Longitude = -84.56,
  ifg = "Y 03 27 2007",
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
```

    ## Warning in createMetProject(project_Name = "GA_Atlanta_International_AP", :
    ## Found previous rmet project: C:/rmet2/GA/ATLANTA_INT_AP/
    ## GA_Atlanta_International_AP.rds

### AERMINUTE

Generally, I start my workflow with AERMINUTE. AERMINUTE extracts 2
minute rolling averages of wind speed and wind direction from monthly
files (either reported every minute or every five minutes). The reason
this is done is that hourly data from NOAA since 1996 is censored for
wind speeds below 3 knots.

<div id="htmlwidget-558c0e9dfe7d5a09d3ae" style="width:672px;height:500px;" class="grViz html-widget"></div>
<script type="application/json" data-for="htmlwidget-558c0e9dfe7d5a09d3ae">{"x":{"diagram":"digraph {\n  graph [layout = dot, rankdir = TB]\n\n  node [shape = folder]\n  DSI6405 [label = \"DSI 6405\nASOS 1-minute\"]\n  DSI6401 [label = \"DSI 6401\nASOS 5-minute\"]\n  DS3505 [label =  \"DS 3505\nISHD Hourly Surface\", style=dashed, color=grey]\n  AERMINUTE [label = \"AERMINUTE\", shape = rectangle]\n  HOURFILE [label = \"HOURFILE\nHourly averaged winds\"]\n  SUMMFILE [label = \"SUMMFILE\nHourly summary of minutes used and\nminimum, maximum, and average winds\"]\n  COMPFILE [label = \"COMPFILE\nCompares the standard observations \nagainst the 1-minute observations\", style=dashed, color=grey]\n\n  # edge definitions with the node IDs\n  DSI6401 -> AERMINUTE \n  DSI6405 -> AERMINUTE\n  DS3505 -> AERMINUTE[style=dashed, color=grey]\n  AERMINUTE -> HOURFILE\n  AERMINUTE -> SUMMFILE\n  AERMINUTE -> COMPFILE[style=dashed, color=grey]\n  }","config":{"engine":"dot","options":null}},"evals":[],"jsHooks":[]}</script>

Other QA files can be produced from AERMINUTE, but these are typically
what I use.

#### Downloading AERMINUTE Files

To download the files:

``` r
downloadTD6405(KATL, check=TRUE)
downloadTD6401(KATL, check=TRUE)
downloadTD3505(KATL, check=TRUE)
```

    ## Warning in readLines(con = gzcon(url(sourceFile))): the 'wininet' method of
    ## url() is deprecated for ftp:// URLs

    ## Warning in readLines(con = gzcon(url(sourceFile2))): the 'wininet' method of
    ## url() is deprecated for ftp:// URLs

    ## Warning in readLines(con = gzcon(url(sourceFile))): the 'wininet' method of
    ## url() is deprecated for ftp:// URLs

    ## Warning in readLines(con = gzcon(url(sourceFile2))): the 'wininet' method of
    ## url() is deprecated for ftp:// URLs

The check argument verifies if the files are already downloaded before
downloading.

So far we have only used rmet2 to download the files for AERMINUTE, to
create the input files, you need to:

``` r
KATL <- createInput(KATL, type = "aerminute")
```

    ## [1] "Writing AERMINUTE input text:\n"

This adds the text that will be used to write the input files for
AERMINUTE. This is appended to the KATL `rmet2` object. To View:

``` r
cat(KATL$inputText$aerminute[[2]])
```

    ## startend   01 2020 12 2020
    ## ifwgroup Y 03 27 2007
    ## 
    ## DATAFILE STARTING
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64050KATL202001.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64050KATL202002.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64050KATL202003.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64050KATL202004.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64050KATL202005.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64050KATL202006.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64050KATL202007.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64050KATL202008.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64050KATL202009.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64050KATL202010.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64050KATL202011.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64050KATL202012.dat"
    ## DATAFILE FINISHED
    ## 
    ## DAT5FILE STARTING
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64010KATL202001.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64010KATL202002.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64010KATL202003.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64010KATL202004.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64010KATL202005.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64010KATL202006.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64010KATL202007.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64010KATL202008.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64010KATL202009.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64010KATL202010.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64010KATL202011.dat"
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/64010KATL202012.dat"
    ## DAT5FILE FINISHED
    ## 
    ## SURFDATA STARTING
    ## "C:/rmet2/GA/ATLANTA_INT_AP/2020/S72219013874_2020.ISH"
    ## SURFDATA FINISHED
    ## 
    ## OUTFILES STARTING
    ## hourfile "C:/rmet2/GA/ATLANTA_INT_AP/2020/AM_1MIN_2020.DAT"
    ## summfile "C:/rmet2/GA/ATLANTA_INT_AP/2020/AM_1MIN_2020_summ.DAT"
    ## compfile "C:/rmet2/GA/ATLANTA_INT_AP/2020/AM_1MIN_2020_comp.DAT"
    ## OUTFILES FINISHED

It also specifies the output files in the `rmet2` object:

``` r
KATL$outputFiles$aerminute
```

    ## [1] "C:/rmet2/GA/ATLANTA_INT_AP/2019/AM_1MIN_2019.DAT"
    ## [2] "C:/rmet2/GA/ATLANTA_INT_AP/2020/AM_1MIN_2020.DAT"

Now we have created the input text, we need to write the input file for
AERMINUTE, and create link to where these files are:

``` r
KATL <- writeInputFile(KATL, type = "aerminute")
KATL$inputFiles$aerminute
```

    ## [1] "\"C:/rmet2/GA/ATLANTA_INT_AP/2019/AM_2019.inp\""
    ## [2] "\"C:/rmet2/GA/ATLANTA_INT_AP/2020/AM_2020.inp\""

The purpose in doing multiple steps is to allow the user to specify
existant input files that they may have received or created previously
for a processor.

And to run AERMINUTE:

``` r
KATL <- processMet(KATL, processor="aerminute")
```

AERMINUTE creates a lot of textual output, which is dumped into
AERMET.log files in each of the year files in the project directory.

It can be a little difficult to collect the key information, and detect
trends in changes in data issues with the AERMINUTE output, therefore,
we have added `qaAerminute` function.

``` r
qaAerminute(KATL) %>% pander::pander(.)
```

-   **RecCheck**:

    |           RECORD FORMAT CHECK           | count.2019 | count.2020 |
    |:---------------------------------------:|:----------:|:----------:|
    | Total number of records read from files |   295585   |   506040   |
    |       Number of processed records       |   288693   |   494529   |
    |  Number of records outside data period  |     1      |     0      |
    |  Number of records inside data period   |   288692   |   494529   |
    |     Number of non-processed records     |    6892    |   11511    |
    |     Number of records for minute 1      |    4924    |    8436    |
    |          Number of bad records          |    1968    |    3075    |
    |         Number of check records         |     0      |     0      |
    |      Number of bad minute records       |     0      |     0      |

-   **Flags**:

    |                        QA FLAG SUMMARY                        | count.2019 | count.2020 |
    |:-------------------------------------------------------------:|:----------:|:----------:|
    |       1 Non-numeric characters in columns 68 through 90       |    1966    |    3075    |
    |   2 Text string with leading zero in columns 66 through 90    |     0      |     0      |
    | 3 4-character numeric string found in columns 30 through 113  |     0      |     0      |
    |                   4 Column 30 is non-blank                    |     0      |     0      |
    |   5 No number in columns 70-74 for 2-minute wind direction    |    1966    |    3075    |
    |     6 No number in columns 76-79 for 2-minute wind speed      |    1966    |    3075    |
    | 7 No number in columns 82-84 for 5-second gust wind direction |    1184    |    1971    |
    |   8 No number in columns 87-89 for 5-second gust wind speed   |    1184    |    1971    |
    |                     9 Number in column 90                     |     0      |     5      |
    |    10 Wind speeds and directions outside allowable ranges     |     2      |     0      |
    |       11 Other flags switched on, but winds may be okay       |     0      |     0      |
    |         11 Other flags switched on, but record is bad         |    1968    |    3075    |

-   **qaCalms**:

    |            STANDARD CALMS            | count.2019 | count.2020 |
    |:------------------------------------:|:----------:|:----------:|
    | Number of standard observation calms |    267     |    461     |
    |   Number of missing 1-minute winds   |     12     |     4      |
    | Number of 1-minute winds \< 3 knots  |    234     |    437     |
    | Number of 1-minute winds \>= 3 knots |     21     |     20     |

-   **qaMissing**:

    |            STANDARD MISSING WINDS            | count.2019 | count.2020 |
    |:--------------------------------------------:|:----------:|:----------:|
    | Number of standard observation missing winds |    110     |    194     |
    |       Number of missing 1-minute winds       |     4      |     9      |
    |     Number of non-missing 1-minute winds     |    106     |    185     |

-   **qaVariable**:

    |            STANDARD VARIABLE WINDS            | count.2019 | count.2020 |
    |:---------------------------------------------:|:----------:|:----------:|
    | Number of standard observation variable winds |    101     |     93     |
    |       Number of missing 1-minute winds        |     6      |     3      |
    |     Number of non-missing 1-minute winds      |     95     |     90     |

-   **qaVarWS**:

    | VARIABLE WINDS: SPEED DIFFERENCES | count.2019 | count.2020 |
    |:---------------------------------:|:----------:|:----------:|
    |                 0                 |     70     |     61     |
    |              \<= 0.2              |     25     |     26     |
    |             0.3 - 0.5             |     0      |     0      |
    |              0.5 - 1              |     0      |     2      |
    |              1.0 - 3              |     0      |     1      |
    |              3.0 - 5              |     0      |     0      |
    |              \> 5.0               |     0      |     0      |

-   **qaValid**:

    |         STANDARD VALID WINDS         | count.2019 | count.2020 |
    |:------------------------------------:|:----------:|:----------:|
    |   Number of missing 1-minute winds   |    158     |    256     |
    | Number of non-missing 1-minute winds |    2890    |    5370    |

-   **qaValWS**:

    | VALID WINDS: SPEED DIFFERENCES | count.2019 | count.2020 |
    |:------------------------------:|:----------:|:----------:|
    |               0                |    1904    |    3386    |
    |            \<= 0.2             |    633     |    1291    |
    |           0.3 - 0.5            |    165     |    305     |
    |            0.5 - 1             |    109     |    189     |
    |            1.0 - 3             |     76     |    192     |
    |            3.0 - 5             |     2      |     7      |
    |             \> 5.0             |     1      |     0      |

-   **qaValWD**:

    | VALID WINDS: DIRECTION DIFFERENCES | count.2019 | count.2020 |
    |:----------------------------------:|:----------:|:----------:|
    |                 0                  |    2589    |    4740    |
    |               \<= 10               |    183     |    393     |
    |               \<= 20               |     55     |    118     |
    |               \<= 30               |     27     |     48     |
    |               \<= 40               |     14     |     25     |
    |               \<= 50               |     5      |     14     |
    |               \<= 60               |     1      |     6      |
    |               \<= 70               |     1      |     4      |
    |               \<= 80               |     1      |     4      |
    |               \<= 90               |     1      |     2      |
    |              \<= 100               |     1      |     0      |
    |              \<= 110               |     0      |     0      |
    |              \<= 120               |     0      |     2      |
    |              \<= 130               |     0      |     0      |
    |              \<= 140               |     0      |     2      |
    |              \<= 150               |     0      |     0      |
    |              \<= 160               |     1      |     1      |
    |              \<= 170               |     0      |     0      |
    |              \<= 180               |     0      |     0      |
    |               \> 180               |     11     |     11     |

-   **YearNumSum**:

    |             Total Hours              | Number(%).2019 | Number(%).2020 |
    |:------------------------------------:|:--------------:|:--------------:|
    | Number of total hours in data period |      5136      |      8784      |
    |      Number of processed hours       | 4961 ( 96.59%) | 8487 ( 96.62%) |
    | Number of processed non-valid hours  |   0 ( 0.00%)   |   0 ( 0.00%)   |
    |         Number of calm hours         |   0 ( 0.00%)   |   0 ( 0.00%)   |

-   **monthlyData**:

    | YEAR |   MONTH   | TOTAL HOURS | VALID HOURS | INVALID HOURS | CALM HOURS |
    |:----:|:---------:|:-----------:|:-----------:|:-------------:|:----------:|
    | 2019 |   June    |     720     |     700     |       0       |     0      |
    | 2019 |   July    |     744     |     732     |       0       |     0      |
    | 2019 |  August   |     744     |     744     |       0       |     0      |
    | 2019 | September |     720     |     671     |       0       |     0      |
    | 2019 |  October  |     744     |     719     |       0       |     0      |
    | 2019 | November  |     720     |     698     |       0       |     0      |
    | 2019 | December  |     744     |     697     |       0       |     0      |
    | 2020 |  January  |     744     |     656     |       0       |     0      |
    | 2020 | February  |     696     |     696     |       0       |     0      |
    | 2020 |   March   |     744     |     740     |       0       |     0      |
    | 2020 |   April   |     720     |     697     |       0       |     0      |
    | 2020 |    May    |     744     |     744     |       0       |     0      |
    | 2020 |   June    |     720     |     715     |       0       |     0      |
    | 2020 |   July    |     744     |     741     |       0       |     0      |
    | 2020 |  August   |     744     |     742     |       0       |     0      |
    | 2020 | September |     720     |     710     |       0       |     0      |
    | 2020 |  October  |     744     |     709     |       0       |     0      |
    | 2020 | November  |     720     |     622     |       0       |     0      |
    | 2020 | December  |     744     |     715     |       0       |     0      |

    Table continues below

    | MISSING HOURS | 5-MIN MINUTES |
    |:-------------:|:-------------:|
    |      20       |      18       |
    |      12       |      159      |
    |       0       |      37       |
    |      49       |      18       |
    |      25       |      10       |
    |      22       |       5       |
    |      47       |      16       |
    |      88       |      10       |
    |       0       |      13       |
    |       4       |      12       |
    |      23       |      15       |
    |       0       |      14       |
    |       5       |      14       |
    |       3       |      100      |
    |       2       |      54       |
    |      10       |      49       |
    |      35       |      20       |
    |      98       |      37       |
    |      29       |       6       |

<!-- end of list -->

You can also read and analyze the Summary and Comparison Files for
further analysis.

``` r
aerminute.sum <- lapply(2019:2020,  function(x){
  read_csv(paste0(rootDir, "/", x, "/", "AM_1MIN_",x, "_summ.DAT"))
}) %>%
  bind_rows() %>%
  mutate(Date = lubridate::ymd_h(paste(Date,as.numeric(hr) - 1), tz = lubridate::tz(KATL$start_Date)))
```

    ## Rows: 5136 Columns: 25
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (2): hr, flag
    ## dbl (23): Date, IFW flag, total minutes, total 5-min subs, total calms, tota...
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 8784 Columns: 25
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (2): hr, flag
    ## dbl (23): Date, IFW flag, total minutes, total 5-min subs, total calms, tota...
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
aerminute.comp <- lapply(2019:2020,  function(x){
  read_csv(paste0(rootDir, "/", x, "/", "AM_1MIN_",x, "_comp.DAT"), guess_max = 5000)
}) %>%
  bind_rows() %>%
  mutate(date = lubridate::ymd_h(paste(`date(yyyymmdd)`,as.numeric(hour) - 1), tz = lubridate::tz(KATL$start_Date)))
```

    ## Rows: 3526 Columns: 14
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (2): wind flag, qc flag
    ## dbl (12): date(yyyymmdd), hour, minute, calm flag, 1-min dir, 1-min dir10, 1...
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## Rows: 6374 Columns: 14
    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr  (2): wind flag, qc flag
    ## dbl (12): date(yyyymmdd), hour, minute, calm flag, 1-min dir, 1-min dir10, 1...
    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
aerminute.sum %>%
  ggplot(aes(x=Date, y = `total minutes`)) + geom_point() + theme_light()
```

![](atlanta_files/figure-gfm/unnamed-chunk-27-1.png)<!-- --> You can
also look at average wind speed:

``` r
aerminute.sum %>%
  mutate(`avg speed` = ifelse(`avg speed` == 999, NA, `avg speed`)) %>%
  ggplot(aes(x=Date, y = `avg speed`)) + geom_point(alpha = 0.1, size = 0.6) + geom_smooth() + theme_light()
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

    ## Warning: Removed 472 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 472 rows containing missing values (geom_point).

![](atlanta_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

And average wind direction:

``` r
aerminute.sum %>%
  mutate(`avg dir` = ifelse(`avg dir` == 999, NA, `avg dir`)) %>%
  ggplot(aes(x=Date, y = `avg dir`)) + geom_point(alpha = 0.5, size = 0.5)  + theme_light()
```

    ## Warning: Removed 472 rows containing missing values (geom_point).

![](atlanta_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

Interpretations of these graphics can be a little tricky. In general,
you are looking in sudden changes in the wind speed and wind direction
(see
<https://www.atsdr.cdc.gov/HAC/pha/TranscontinentalPipeline/TranscontinentalPipelineHC04182011.pdf>
Figure D4 for example of 2 problematic wind direction plots).

### AERSURFACE

The 3 step process of creating input and writing the input file is
similar with AERSURFACE:

``` r
KATL <- createInput(KATL, type = "aersurface_nws")
```

    ## [1] "Writing AERSURFACE NWS input text:\n"

``` r
cat(KATL$inputText$aersurface[[1]])
```

    ## CO STARTING
    ##    TITLEONE  GA_Atlanta_International_AP  AERSURFACE NWS INPUT CONTROL FILE
    ##    TITLETWO  C:\rmet2\GA\ATLANTA_INT_AP  Directory
    ## ** Using default options for OPTIONS keyword and parameters
    ##   
    ##    OPTIONS   PRIMARY  ZORAD
    ##    DEBUGOPT  GRID  TIFF
    ##    CENTERLL  33.6301 -84.4418 NAD83
    ##    DATAFILE  CNPY2016  "C:\rmet2\GA\ATLANTA_INT_AP\preprocessed_data\canopy_NLCD_Tree_Canopy_2016_rmet.tif"
    ##    DATAFILE  MPRV2016  "C:\rmet2\GA\ATLANTA_INT_AP\preprocessed_data\Imperv_NLCD_Impervious_2016_rmet.tif"
    ##    DATAFILE  NLCD2016  "C:\rmet2\GA\ATLANTA_INT_AP\preprocessed_data\land_cover_NLCD_Land_Cover_2016_rmet.tif"
    ## ** Use default specified km radius
    ##    ZORADIUS  1
    ##    FREQ_SECT  MONTHLY  12  VARYAP
    ## **        index  start    end      
    ##    SECTOR    1    0.00   30.00     AP
    ##    SECTOR    2   30.00   60.00     AP
    ##    SECTOR    3   60.00   90.00     AP
    ##    SECTOR    4   90.00  120.00     AP
    ##    SECTOR    5  120.00  150.00     AP
    ##    SECTOR    6  150.00  180.00     AP
    ##    SECTOR    7  180.00  210.00     AP
    ##    SECTOR    8  210.00  240.00     AP
    ##    SECTOR    9  240.00  270.00     AP
    ##    SECTOR   10  270.00  300.00     AP
    ##    SECTOR   11  300.00  330.00     AP
    ##    SECTOR   12  330.00  360.00     AP
    ##    SEASON   WINTERNS   12 1 2
    ##    SEASON   SPRING     3 4 5
    ##    SEASON   SUMMER     6 7 8
    ##    SEASON   AUTUMN     9 10 11
    ##    RUNORNOT  RUN
    ## CO FINISHED
    ## ** OUTPUT
    ## OU STARTING
    ##    SFCCHAR    "C:\rmet2\GA\ATLANTA_INT_AP\aersurface\nws_sfc_chars.out"
    ##    NLCDTIFF   "C:\rmet2\GA\ATLANTA_INT_AP\aersurface\nws_lc_tif.txt"
    ##    NLCDGRID   "C:\rmet2\GA\ATLANTA_INT_AP.\aersurface\nws_landcover.txt"
    ##    MPRVGRID   "C:\rmet2\GA\ATLANTA_INT_AP\aersurface\nws_imp_tif_dbg.txt"
    ##    MPRVTIFF   "C:\rmet2\GA\ATLANTA_INT_AP\aersurface\nws_impervious.txt"
    ##    CNPYGRID   "C:\rmet2\GA\ATLANTA_INT_AP\aersurface\nws_can_tif_dbg.txt"
    ##    CNPYTIFF   "C:\rmet2\GA\ATLANTA_INT_AP\aersurface\nws_canopy.txt"
    ## OU FINISHED

``` r
KATL <- writeInputFile(KATL, type = "aersurface")
KATL <- processMet(KATL, processor =  "aersurface")
```

``` r
KATL$output$aersurface[[1]]
```

    ##   [1] "** Generated by AERSURFACE, Version 20060                         06/24/22 **"                               
    ##   [2] "**                                                                13:23:48 **"                               
    ##   [3] ""                                                                                                            
    ##   [4] "** Title 1:  GA_Atlanta_International_AP  AERSURFACE NWS INPUT CONTROL FILE"                                 
    ##   [5] "** Title 2:  C:\\rmet2\\GA\\ATLANTA_INT_AP  Directory"                                                       
    ##   [6] "** Primary Site (Zo):"                                                                                       
    ##   [7] "**   Center Latitude  (decimal degrees):    33.630100"                                                       
    ##   [8] "**   Center Longitude (decimal degrees):   -84.441800"                                                       
    ##   [9] "**   Datum: NAD83"                                                                                           
    ##  [10] "** NLCD Version:  2016"                                                                                      
    ##  [11] "** NLCD DataFile: C:\\rmet2\\GA\\ATLANTA_INT_AP\\preprocessed_data\\land_cover_NLCD_Land_Cover_2016_rmet.tif"
    ##  [12] "** MPRV Version:  2016"                                                                                      
    ##  [13] "** MPRV DataFile: C:\\rmet2\\GA\\ATLANTA_INT_AP\\preprocessed_data\\Imperv_NLCD_Impervious_2016_rmet.tif"    
    ##  [14] "** CNPY Version:  2016"                                                                                      
    ##  [15] "** CNPY DataFile: C:\\rmet2\\GA\\ATLANTA_INT_AP\\preprocessed_data\\canopy_NLCD_Tree_Canopy_2016_rmet.tif"   
    ##  [16] "** Non-Airport Sector IDs: None"                                                                             
    ##  [17] "** Zo Method: ZORAD"                                                                                         
    ##  [18] "** Zo Radius (m):   1000.0"                                                                                  
    ##  [19] "** Continuous snow cover: N"                                                                                 
    ##  [20] "** Surface moisture: Average;  Arid: N"                                                                      
    ##  [21] "** Month/Season assignments: User-specified"                                                                 
    ##  [22] "** Late autumn after frost and harvest, or winter with no snow: 1  2 12"                                     
    ##  [23] "** Winter with continuous snow on the ground: "                                                              
    ##  [24] "** Transitional spring (partial green coverage, short annuals): 3  4  5"                                     
    ##  [25] "** Midsummer with lush vegetation: 6  7  8"                                                                  
    ##  [26] "** Autumn with unharvested cropland: 9 10 11"                                                                
    ##  [27] ""                                                                                                            
    ##  [28] ""                                                                                                            
    ##  [29] "   FREQ_SECT  MONTHLY 12"                                                                                    
    ##  [30] "   SECTOR   1    0.00   30.00"                                                                               
    ##  [31] "   SECTOR   2   30.00   60.00"                                                                               
    ##  [32] "   SECTOR   3   60.00   90.00"                                                                               
    ##  [33] "   SECTOR   4   90.00  120.00"                                                                               
    ##  [34] "   SECTOR   5  120.00  150.00"                                                                               
    ##  [35] "   SECTOR   6  150.00  180.00"                                                                               
    ##  [36] "   SECTOR   7  180.00  210.00"                                                                               
    ##  [37] "   SECTOR   8  210.00  240.00"                                                                               
    ##  [38] "   SECTOR   9  240.00  270.00"                                                                               
    ##  [39] "   SECTOR  10  270.00  300.00"                                                                               
    ##  [40] "   SECTOR  11  300.00  330.00"                                                                               
    ##  [41] "   SECTOR  12  330.00  360.00"                                                                               
    ##  [42] ""                                                                                                            
    ##  [43] "**           Month    Sect    Alb      Bo        Zo"                                                         
    ##  [44] "   SITE_CHAR    1       1     0.18     1.05     0.049"                                                       
    ##  [45] "   SITE_CHAR    1       2     0.18     1.05     0.040"                                                       
    ##  [46] "   SITE_CHAR    1       3     0.18     1.05     0.036"                                                       
    ##  [47] "   SITE_CHAR    1       4     0.18     1.05     0.032"                                                       
    ##  [48] "   SITE_CHAR    1       5     0.18     1.05     0.041"                                                       
    ##  [49] "   SITE_CHAR    1       6     0.18     1.05     0.040"                                                       
    ##  [50] "   SITE_CHAR    1       7     0.18     1.05     0.050"                                                       
    ##  [51] "   SITE_CHAR    1       8     0.18     1.05     0.040"                                                       
    ##  [52] "   SITE_CHAR    1       9     0.18     1.05     0.037"                                                       
    ##  [53] "   SITE_CHAR    1      10     0.18     1.05     0.032"                                                       
    ##  [54] "   SITE_CHAR    1      11     0.18     1.05     0.033"                                                       
    ##  [55] "   SITE_CHAR    1      12     0.18     1.05     0.045"                                                       
    ##  [56] "   SITE_CHAR    2       1     0.18     1.05     0.049"                                                       
    ##  [57] "   SITE_CHAR    2       2     0.18     1.05     0.040"                                                       
    ##  [58] "   SITE_CHAR    2       3     0.18     1.05     0.036"                                                       
    ##  [59] "   SITE_CHAR    2       4     0.18     1.05     0.032"                                                       
    ##  [60] "   SITE_CHAR    2       5     0.18     1.05     0.041"                                                       
    ##  [61] "   SITE_CHAR    2       6     0.18     1.05     0.040"                                                       
    ##  [62] "   SITE_CHAR    2       7     0.18     1.05     0.050"                                                       
    ##  [63] "   SITE_CHAR    2       8     0.18     1.05     0.040"                                                       
    ##  [64] "   SITE_CHAR    2       9     0.18     1.05     0.037"                                                       
    ##  [65] "   SITE_CHAR    2      10     0.18     1.05     0.032"                                                       
    ##  [66] "   SITE_CHAR    2      11     0.18     1.05     0.033"                                                       
    ##  [67] "   SITE_CHAR    2      12     0.18     1.05     0.045"                                                       
    ##  [68] "   SITE_CHAR    3       1     0.16     0.81     0.053"                                                       
    ##  [69] "   SITE_CHAR    3       2     0.16     0.81     0.045"                                                       
    ##  [70] "   SITE_CHAR    3       3     0.16     0.81     0.041"                                                       
    ##  [71] "   SITE_CHAR    3       4     0.16     0.81     0.038"                                                       
    ##  [72] "   SITE_CHAR    3       5     0.16     0.81     0.047"                                                       
    ##  [73] "   SITE_CHAR    3       6     0.16     0.81     0.046"                                                       
    ##  [74] "   SITE_CHAR    3       7     0.16     0.81     0.058"                                                       
    ##  [75] "   SITE_CHAR    3       8     0.16     0.81     0.049"                                                       
    ##  [76] "   SITE_CHAR    3       9     0.16     0.81     0.047"                                                       
    ##  [77] "   SITE_CHAR    3      10     0.16     0.81     0.038"                                                       
    ##  [78] "   SITE_CHAR    3      11     0.16     0.81     0.039"                                                       
    ##  [79] "   SITE_CHAR    3      12     0.16     0.81     0.049"                                                       
    ##  [80] "   SITE_CHAR    4       1     0.16     0.81     0.053"                                                       
    ##  [81] "   SITE_CHAR    4       2     0.16     0.81     0.045"                                                       
    ##  [82] "   SITE_CHAR    4       3     0.16     0.81     0.041"                                                       
    ##  [83] "   SITE_CHAR    4       4     0.16     0.81     0.038"                                                       
    ##  [84] "   SITE_CHAR    4       5     0.16     0.81     0.047"                                                       
    ##  [85] "   SITE_CHAR    4       6     0.16     0.81     0.046"                                                       
    ##  [86] "   SITE_CHAR    4       7     0.16     0.81     0.058"                                                       
    ##  [87] "   SITE_CHAR    4       8     0.16     0.81     0.049"                                                       
    ##  [88] "   SITE_CHAR    4       9     0.16     0.81     0.047"                                                       
    ##  [89] "   SITE_CHAR    4      10     0.16     0.81     0.038"                                                       
    ##  [90] "   SITE_CHAR    4      11     0.16     0.81     0.039"                                                       
    ##  [91] "   SITE_CHAR    4      12     0.16     0.81     0.049"                                                       
    ##  [92] "   SITE_CHAR    5       1     0.16     0.81     0.053"                                                       
    ##  [93] "   SITE_CHAR    5       2     0.16     0.81     0.045"                                                       
    ##  [94] "   SITE_CHAR    5       3     0.16     0.81     0.041"                                                       
    ##  [95] "   SITE_CHAR    5       4     0.16     0.81     0.038"                                                       
    ##  [96] "   SITE_CHAR    5       5     0.16     0.81     0.047"                                                       
    ##  [97] "   SITE_CHAR    5       6     0.16     0.81     0.046"                                                       
    ##  [98] "   SITE_CHAR    5       7     0.16     0.81     0.058"                                                       
    ##  [99] "   SITE_CHAR    5       8     0.16     0.81     0.049"                                                       
    ## [100] "   SITE_CHAR    5       9     0.16     0.81     0.047"                                                       
    ## [101] "   SITE_CHAR    5      10     0.16     0.81     0.038"                                                       
    ## [102] "   SITE_CHAR    5      11     0.16     0.81     0.039"                                                       
    ## [103] "   SITE_CHAR    5      12     0.16     0.81     0.049"                                                       
    ## [104] "   SITE_CHAR    6       1     0.17     0.80     0.055"                                                       
    ## [105] "   SITE_CHAR    6       2     0.17     0.80     0.049"                                                       
    ## [106] "   SITE_CHAR    6       3     0.17     0.80     0.046"                                                       
    ## [107] "   SITE_CHAR    6       4     0.17     0.80     0.042"                                                       
    ## [108] "   SITE_CHAR    6       5     0.17     0.80     0.052"                                                       
    ## [109] "   SITE_CHAR    6       6     0.17     0.80     0.051"                                                       
    ## [110] "   SITE_CHAR    6       7     0.17     0.80     0.065"                                                       
    ## [111] "   SITE_CHAR    6       8     0.17     0.80     0.057"                                                       
    ## [112] "   SITE_CHAR    6       9     0.17     0.80     0.056"                                                       
    ## [113] "   SITE_CHAR    6      10     0.17     0.80     0.043"                                                       
    ## [114] "   SITE_CHAR    6      11     0.17     0.80     0.043"                                                       
    ## [115] "   SITE_CHAR    6      12     0.17     0.80     0.052"                                                       
    ## [116] "   SITE_CHAR    7       1     0.17     0.80     0.055"                                                       
    ## [117] "   SITE_CHAR    7       2     0.17     0.80     0.049"                                                       
    ## [118] "   SITE_CHAR    7       3     0.17     0.80     0.046"                                                       
    ## [119] "   SITE_CHAR    7       4     0.17     0.80     0.042"                                                       
    ## [120] "   SITE_CHAR    7       5     0.17     0.80     0.052"                                                       
    ## [121] "   SITE_CHAR    7       6     0.17     0.80     0.051"                                                       
    ## [122] "   SITE_CHAR    7       7     0.17     0.80     0.065"                                                       
    ## [123] "   SITE_CHAR    7       8     0.17     0.80     0.057"                                                       
    ## [124] "   SITE_CHAR    7       9     0.17     0.80     0.056"                                                       
    ## [125] "   SITE_CHAR    7      10     0.17     0.80     0.043"                                                       
    ## [126] "   SITE_CHAR    7      11     0.17     0.80     0.043"                                                       
    ## [127] "   SITE_CHAR    7      12     0.17     0.80     0.052"                                                       
    ## [128] "   SITE_CHAR    8       1     0.17     0.80     0.055"                                                       
    ## [129] "   SITE_CHAR    8       2     0.17     0.80     0.049"                                                       
    ## [130] "   SITE_CHAR    8       3     0.17     0.80     0.046"                                                       
    ## [131] "   SITE_CHAR    8       4     0.17     0.80     0.042"                                                       
    ## [132] "   SITE_CHAR    8       5     0.17     0.80     0.052"                                                       
    ## [133] "   SITE_CHAR    8       6     0.17     0.80     0.051"                                                       
    ## [134] "   SITE_CHAR    8       7     0.17     0.80     0.065"                                                       
    ## [135] "   SITE_CHAR    8       8     0.17     0.80     0.057"                                                       
    ## [136] "   SITE_CHAR    8       9     0.17     0.80     0.056"                                                       
    ## [137] "   SITE_CHAR    8      10     0.17     0.80     0.043"                                                       
    ## [138] "   SITE_CHAR    8      11     0.17     0.80     0.043"                                                       
    ## [139] "   SITE_CHAR    8      12     0.17     0.80     0.052"                                                       
    ## [140] "   SITE_CHAR    9       1     0.17     1.05     0.053"                                                       
    ## [141] "   SITE_CHAR    9       2     0.17     1.05     0.045"                                                       
    ## [142] "   SITE_CHAR    9       3     0.17     1.05     0.041"                                                       
    ## [143] "   SITE_CHAR    9       4     0.17     1.05     0.038"                                                       
    ## [144] "   SITE_CHAR    9       5     0.17     1.05     0.047"                                                       
    ## [145] "   SITE_CHAR    9       6     0.17     1.05     0.047"                                                       
    ## [146] "   SITE_CHAR    9       7     0.17     1.05     0.059"                                                       
    ## [147] "   SITE_CHAR    9       8     0.17     1.05     0.050"                                                       
    ## [148] "   SITE_CHAR    9       9     0.17     1.05     0.048"                                                       
    ## [149] "   SITE_CHAR    9      10     0.17     1.05     0.038"                                                       
    ## [150] "   SITE_CHAR    9      11     0.17     1.05     0.039"                                                       
    ## [151] "   SITE_CHAR    9      12     0.17     1.05     0.049"                                                       
    ## [152] "   SITE_CHAR   10       1     0.17     1.05     0.053"                                                       
    ## [153] "   SITE_CHAR   10       2     0.17     1.05     0.045"                                                       
    ## [154] "   SITE_CHAR   10       3     0.17     1.05     0.041"                                                       
    ## [155] "   SITE_CHAR   10       4     0.17     1.05     0.038"                                                       
    ## [156] "   SITE_CHAR   10       5     0.17     1.05     0.047"                                                       
    ## [157] "   SITE_CHAR   10       6     0.17     1.05     0.047"                                                       
    ## [158] "   SITE_CHAR   10       7     0.17     1.05     0.059"                                                       
    ## [159] "   SITE_CHAR   10       8     0.17     1.05     0.050"                                                       
    ## [160] "   SITE_CHAR   10       9     0.17     1.05     0.048"                                                       
    ## [161] "   SITE_CHAR   10      10     0.17     1.05     0.038"                                                       
    ## [162] "   SITE_CHAR   10      11     0.17     1.05     0.039"                                                       
    ## [163] "   SITE_CHAR   10      12     0.17     1.05     0.049"                                                       
    ## [164] "   SITE_CHAR   11       1     0.17     1.05     0.053"                                                       
    ## [165] "   SITE_CHAR   11       2     0.17     1.05     0.045"                                                       
    ## [166] "   SITE_CHAR   11       3     0.17     1.05     0.041"                                                       
    ## [167] "   SITE_CHAR   11       4     0.17     1.05     0.038"                                                       
    ## [168] "   SITE_CHAR   11       5     0.17     1.05     0.047"                                                       
    ## [169] "   SITE_CHAR   11       6     0.17     1.05     0.047"                                                       
    ## [170] "   SITE_CHAR   11       7     0.17     1.05     0.059"                                                       
    ## [171] "   SITE_CHAR   11       8     0.17     1.05     0.050"                                                       
    ## [172] "   SITE_CHAR   11       9     0.17     1.05     0.048"                                                       
    ## [173] "   SITE_CHAR   11      10     0.17     1.05     0.038"                                                       
    ## [174] "   SITE_CHAR   11      11     0.17     1.05     0.039"                                                       
    ## [175] "   SITE_CHAR   11      12     0.17     1.05     0.049"                                                       
    ## [176] "   SITE_CHAR   12       1     0.18     1.05     0.049"                                                       
    ## [177] "   SITE_CHAR   12       2     0.18     1.05     0.040"                                                       
    ## [178] "   SITE_CHAR   12       3     0.18     1.05     0.036"                                                       
    ## [179] "   SITE_CHAR   12       4     0.18     1.05     0.032"                                                       
    ## [180] "   SITE_CHAR   12       5     0.18     1.05     0.041"                                                       
    ## [181] "   SITE_CHAR   12       6     0.18     1.05     0.040"                                                       
    ## [182] "   SITE_CHAR   12       7     0.18     1.05     0.050"                                                       
    ## [183] "   SITE_CHAR   12       8     0.18     1.05     0.040"                                                       
    ## [184] "   SITE_CHAR   12       9     0.18     1.05     0.037"                                                       
    ## [185] "   SITE_CHAR   12      10     0.18     1.05     0.032"                                                       
    ## [186] "   SITE_CHAR   12      11     0.18     1.05     0.033"                                                       
    ## [187] "   SITE_CHAR   12      12     0.18     1.05     0.045"

### AERMET

#### Data Files

We have already downloaded the surface file data. Now we need to get the
upper air data (radiosonde). `rmet2` automates this process.

``` r
downloadFSL(KATL)
```

    ## [1] "All files downloaded to:"
    ## [1] "C:/rmet2/GA/ATLANTA_INT_AP/2019/72215.FSL"
    ## [2] "C:/rmet2/GA/ATLANTA_INT_AP/2020/72215.FSL"

    ## NULL

#### Stage 1

Stage 1 reads in data and performs basic extraction and QA of the data:

``` r
KATL <- createInput(KATL, type = "aermet1")
cat(KATL$inputText$aermet$s1[[1]])
```

    ## JOB
    ## **
    ##   REPORT     "C:/rmet2/GA/ATLANTA_INT_AP/2019/S1.RPT"
    ##   MESSAGES   "C:/rmet2/GA/ATLANTA_INT_AP/2019/S1.MSG"
    ## UPPERAIR
    ## **          Upper air data for WMO: 72215 FSL format
    ##   DATA      "C:/rmet2/GA/ATLANTA_INT_AP/2019/72215.FSL" FSL
    ##   EXTRACT   "C:/rmet2/GA/ATLANTA_INT_AP/2019/UAEXOUT.DAT"
    ##   AUDIT     UATT UAWS UALR
    ##   XDATES    2019/6/6 TO 2020/1/1
    ##   LOCATION  72215  33.35N   84.56W   5
    ##   QAOUT     "C:/rmet2/GA/ATLANTA_INT_AP/2019/UAQAOUT.DAT"
    ##   MODIFY
    ## SURFACE
    ## **          Surface air data for WBAN: 13874 ISHD format
    ##   DATA       "C:/rmet2/GA/ATLANTA_INT_AP/2019/S72219013874_2019.ISH" ISHD
    ##   EXTRACT    "C:/rmet2/GA/ATLANTA_INT_AP/2019/SFEXOUT.DAT"
    ##   AUDIT          SLVP PRES CLHT TSKC PWTH ASKY HZVS RHUM
    ##   RANGE TMPD  -300  <=  450  999
    ##   XDATES        2019/6/6 TO 2020/1/1
    ##   LOCATION      13874 33.63N 84.442W 5 315
    ##   QAOUT        "C:/rmet2/GA/ATLANTA_INT_AP/2019/SFQAOUT.DAT"
    ##   NO_MISSING           ASKY TSKC

You can modify this text or create an input file of your own and read it
into R and add it to the inputText portion of the rmet object.

``` r
KATL <- processMet(KATL, processor  = "aermet1")
```

### Stage 2

``` r
KATL <- createInput(KATL, type = "aermet2")
cat(KATL$inputText$aermet$s2[[1]])
```

    ## JOB
    ## **
    ##   REPORT     "C:/rmet2/GA/ATLANTA_INT_AP/2019/S2.RPT"
    ##   MESSAGES   "C:/rmet2/GA/ATLANTA_INT_AP/2019/S2.MSG"
    ## UPPERAIR
    ## **
    ##   QAOUT      "C:/rmet2/GA/ATLANTA_INT_AP/2019/UAQAOUT.DAT"
    ## SURFACE
    ## **
    ##   QAOUT        "C:/rmet2/GA/ATLANTA_INT_AP/2019/SFQAOUT.DAT"
    ##   ASOS1MIN        "C:/rmet2/GA/ATLANTA_INT_AP/2019/AM_1MIN_2019.DAT"
    ## MERGE
    ## **
    ##   OUTPUT        "C:/rmet2/GA/ATLANTA_INT_AP/2019/AMS2_ISHD.MRG"
    ##   XDATES        2019/6/6 TO 2019/12/31

``` r
KATL <- processMet(KATL, processor  = "aermet2")
```

### Stage 3

``` r
KATL <- createInput(KATL, type = "aermet3")
cat(KATL$inputText$aermet$s3[[1]])
```

    ## JOB
    ## **
    ##   REPORT     "C:/rmet2/GA/ATLANTA_INT_AP/2019/S3.RPT"
    ##   MESSAGES   "C:/rmet2/GA/ATLANTA_INT_AP/2019/S3.MSG"
    ## METPREP
    ## **
    ##   XDATES        2019/6/6 TO 2019/12/31
    ##   DATA          "C:/rmet2/GA/ATLANTA_INT_AP/2019/AMS2_ISHD.MRG"
    ##   THRESH_1MIN 0.5
    ##   METHOD        REFLEVEL SUBNWS
    ##   METHOD        STABLEBL ADJ_U*
    ##   NWS_HGT    WIND 10
    ##   OUTPUT        "C:/rmet2/GA/ATLANTA_INT_AP/2019/AM_2019.SFC"
    ##   PROFILE        "C:/rmet2/GA/ATLANTA_INT_AP/2019/AM_2019.PFL"

``` r
KATL <- processMet(KATL, processor  = "aermet3")
```

``` r
makeFinal.rmet(KATL, "KATL")
```

## Checking output

You should check the message files from each of these processors for
warnings and errors. A future enhancement to `rmet2` will help with this
process, but for now you need to use a text processor for this.

### Reading in surface file

You can read in the surface file with `surfaceReader`:

``` r
katl_sfc <- surfaceReader(sfile = "C:/rmet2/GA/ATLANTA_INT_AP/KATL.sfc", rmetObj = KATL)
```

### Checking Monin-Obukhov Length

THIS IS A VERY ABSTRACT CONCEPT. But a simple way of thinking about the
Monin-Obukhav length is the height at which the the turbulence is more
generated by heat than wind sheer.

You should expect negative values during daylight and positive values
during the night. I typically use a boxplot to check this:

``` r
katl_sfc %>% 
  ggplot(aes(x = factor(hour),  y = monin_obukhov_length)) + 
  geom_boxplot() + theme_light()
```

    ## Warning: Removed 10 rows containing non-finite values (stat_boxplot).

![](atlanta_files/figure-gfm/unnamed-chunk-42-1.png)<!-- -->

By checking against hour of the day, you can verify that you have not
mispecified your time adjustment for your surface station.

### Wind Direction

You should check your wind direction plot. You are looking for sharp
changes in the wind direction, indicative of changes to the met station
or tower position. You should also look for issues of “stuck” wind vanes
as well.

``` r
katl_sfc %>%
  ggplot(aes(x = date, y = wd)) + 
  geom_point(alpha = 0.5, size = 0.5) + 
  theme_light()
```

    ## Warning: Removed 7 rows containing missing values (geom_point).

![](atlanta_files/figure-gfm/unnamed-chunk-43-1.png)<!-- -->

### Wind Speed

Similar to wind direction, you are looking for sharp changes in the wind
speed data.

``` r
katl_sfc %>%
  ggplot(aes(x = date, y = ws)) + 
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth() +
  theme_light()
```

    ## `geom_smooth()` using method = 'gam' and formula 'y ~ s(x, bs = "cs")'

![](atlanta_files/figure-gfm/unnamed-chunk-44-1.png)<!-- -->

You can also look at the wind rose:

``` r
openair::windRose(katl_sfc)
```

![](atlanta_files/figure-gfm/unnamed-chunk-45-1.png)<!-- -->

### Surface roughness

AERMOD is very sensitive to errors in surface roughness, so it is a good
idea to look and see if your surface roughness makes sense by direction.
I use a pollution rose from open air to do this.

``` r
openair::polarFreq(katl_sfc, pol = "surface_roughness", breaks = c(0.037, 0.048, 0.053, 0.060,0.71), type = "season")
```

    ## Warning in openair::polarFreq(katl_sfc, pol = "surface_roughness", breaks =
    ## c(0.037, : No statistic chosen, using mean

![](atlanta_files/figure-gfm/unnamed-chunk-46-1.png)<!-- -->

This shows overall low roughness (you can compare typical values in the
AERMET user guide). There are higher roughness to the south, which makes
sense given our position at an airport and examining the aerial
photographs.

If you have an onsite station, there is a column in the surface file
that will indicate which station the wind data is coming from.
