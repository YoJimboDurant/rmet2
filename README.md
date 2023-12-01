README
================

## Background

This package is designed to assist running US EPA’s Meteorological
Processors,
[AERMINUTE](https://www3.epa.gov/scram001/metobsdata_procaccprogs.htm),
[AERSURFACE](https://www3.epa.gov/ttn/scram/dispersion_related.htm#aersurface),
and [AERMET](https://www3.epa.gov/scram001/metobsdata_procaccprogs.htm).
Rmet2 provides the following features:

- Automatic Downloading of Meteorological Data from National
  Oceonographic and Atmospheric Administration (NOAA) websites.

- Scripted set up of runstream input files to assist in reproducibility.

- Caputuring output runstreams and files and producing tables and
  figures useful for air dispersion modelers to perform quality
  assurance checks on intermediate and final outputs of the
  meteorological preprocessors.

The package is designed to work with [R](www.r-project.org).

## Installation

In R, the following script will check for, and if needed, install the
package
[devtools](https://cran.r-project.org/web/packages/devtools/index.html),
and then attempt to install rmet2 to your system. You will only have to
install rmet2 once on your system, so for normal day-to-day use you will
not need to run the installation script:

``` r
if(!"devtools" %in% installed.packages()) install.pacakges(devtools)
if(!"rmet2" %in% installed.packages()) devtools::install_git("https://github.com/YoJimboDurant/rmet2")
```

TO use rmet2, you will type the following into the console:

``` r
library(rmet2)
```

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
    ## $rmet.noaa.1min
    ## [1] "https://www.ncei.noaa.gov/data/automated-surface-observing-system-one-minute-pg1/access/"
    ## 
    ## $rmet.noaa.5min
    ## [1] "https://www.ncei.noaa.gov/data/automated-surface-observing-system-five-minute/access/"
    ## 
    ## $rmet.noaa.site
    ## [1] "https://www1.ncdc.noaa.gov/pub/data/"
    ## 
    ## $rmet.noaa.surfhist
    ## [1] "https://www1.ncdc.noaa.gov/pub/data/noaa/isd-history.txt"

``` r
installAM()
```

    ## [1] "downloading aermet"
    ## [1] "downloading aerminute"
    ## [1] "downloading aersurface"

    ## NULL

This is the location of the executables.

``` r
sapply(c("aermet", "aerminute", "aersurface"), getOption)
```

    ##                                   aermet 
    ##               "C:/aermet_exe/aermet.exe" 
    ##                                aerminute 
    ## "C:/aerminute_15272/aerminute_15272.exe" 
    ##                               aersurface 
    ##       "C:/aersurface_exe/aersurface.exe"

## Examples usage:

These are updated to work with the new version of aermet using the
`createInput(KORH, type = c("aermet23"))`. It will create the combined
stage 2 input file.

Additionally, the script has been updated to remove dependencies on
packages that are no longer on CRAN (mainly due to rgdal issues).

[Worcester Regional
Airport](https://github.com/YoJimboDurant/rmet2/blob/master/examples/MA_KORH.R)
