% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/installAM.R
\name{installAM}
\alias{installAM}
\title{installAM}
\usage{
installAM(rootDir = "C:/", aermetExists = rep(FALSE, 3))
}
\arguments{
\item{rootDir}{us the directory where you want aermet, aerminute, and aersurface unpacked}

\item{aermetExists}{controls if you want to download the binaries again, or if you only want to set options
The first value controls downloading AERMET, the second AERMINUTE, and the third AERSURFACE.}
}
\description{
\code{installAM} Function will download and unzip files from links at \url{http://www3.epa.gov/scram001/metobsdata_procaccprogs.htm}
to a C: drive location. Sets a pathvariable for location of executables. The function also will create "options" for the 
locations of aermet, aerminute, and aersurface executables. If these exist, subsequent functions will utilize them
in deference to locally unzipped programs.
}
\examples{
installAM()
}
