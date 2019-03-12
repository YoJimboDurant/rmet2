#' @title surfCheck
#' 
#' @description
#' \code{surfCheck} Uses \code{surfReader} to read surface files then plot
#'
#' @export 

## This reads a table into a aermet surface file called surface_file_1, skipping the first line

surfCheck <- function(rmetObj, outfile="surfaceFile.pdf", file.choose=FALSE,
                      returnDfx = FALSE){
  
  
  readIt <- function(x) {
    surface_file_1 <- readr::read_fwf(x, formatSfc,
    skip=1)
  
    is.na(surface_file_1$sensible_heat_flux) <- surface_file_1$sensible_heat_flux == -999.0 
    is.na(surface_file_1$surface_friction_velocity) <- surface_file_1$surface_friction_velocity == -9.000
    is.na(surface_file_1$convective_velocity_scale) <- surface_file_1$convective_velocity_scale == -9.000 
    is.na(surface_file_1$verticle_potential_temperature) <- surface_file_1$verticle_potential_temperature == -9.000
    is.na(surface_file_1$height_conv_pbl) <- surface_file_1$height_conv_pbl == -999 
    is.na(surface_file_1$height_mech_sbl) <- surface_file_1$height_mech_sbl == -999  
    is.na(surface_file_1$monin_obukhov_length) <- surface_file_1$monin_obukhov_length == -99999.0 
    is.na(surface_file_1$temperature) <- surface_file_1$temperature == 999
    is.na(surface_file_1$relative_humidity) <- surface_file_1$relative_humidity == 999 
    is.na(surface_file_1$surface_pressure) <- surface_file_1$surface_pressure == 99999
    is.na(surface_file_1$cloud_cover) <- surface_file_1$cloud_cover == 99  
    is.na(surface_file_1$wd) <- surface_file_1$wd == 999
    is.na(surface_file_1$ws) <- surface_file_1$ws == 999
    is.na(surface_file_1$surface_roughness) <- surface_file_1$surface_roughness == -9
    #is.na(surface_file_1$ws)<- surface_file_1$ws == 0.0
    #is.na(surface_file_1$wd) <- surface_file_1$wd == 0
    
    total_hours <- length(surface_file_1$hour)
    
    seq_hour<-seq(1:total_hours)
    
    ## Need to add date field to records so we can make prettier things, I hope.
    
    # use ISOdate( ) to convert strings to dates
    # 
    adjustment <- 0
    
    
    
    date <- lubridate::ymd_h(paste(surface_file_1$year+adjustment,surface_file_1$month,surface_file_1$day, surface_file_1$hour-1), 
                             tz = lubridate::tz(rmetObj$start_Date))
   
    surface_file_1<-cbind(seq_hour, surface_file_1)
    surface_file_1<-cbind(date, surface_file_1)
    
    return(surface_file_1)
    
    
    
  }
  
  if(!file.choose) stopifnot(is(rmetObj) == "rmet")
  if(file.choose){
    sFile<-file.choose()
    surface_file_1 <- readIt(sFile)
    
  }
  
  

  else{
    fileName <- grep("[.]sfc", list.files(rmetObj$project_Dir, full=TRUE), value=TRUE)
    stopifnot(length(fileName) == 1)
    surface_file_1 <- readIt(fileName)
  }

  



## figure out minimum and maximum dates
daterange=c(as.POSIXlt(min(surface_file_1$date)),as.POSIXlt(max(surface_file_1$date)))

## Then there is a set of small functions which implement elementary
## functionality:

surface_plot_fct=function(parameter,x_dates,factor,ylab,title){
  
  plot(x_dates,parameter,xaxt="n",ylab=ylab, xlab="Date", pch=".", col="gray")   #don't plot the x axis
  
  dates<-axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%m-%Y") #label the x axis by months
  
  
  #lines(lowess(mean,consumed,f=3/10))   #this does not work if there are missing data
  #so, the following function fixes it
  lowess.na <- function(x, y = NULL, f = 2/3,...) {  #do lowess with missing data
    
    x1 <- subset(x,(!is.na(x)) &(!is.na(y)))
    y1 <- subset(y, (!is.na(x)) &(!is.na(y)))
    lowess.na <- lowess(x1,y1,f, ...)
  }
  
  lines(lowess.na(x_dates,parameter,f=factor/10))   #this does work if there are missing data
  
  title(main=title)
}
if(!is.null(outfile)){
  
if(!file.exists(paste(rmetObj$project_Dir, "output", sep="/"))) dir.create(paste(rmetObj$project_Dir, "output", sep="/"))
pdf(file=paste(rmetObj$project_Dir, "output", outfile, sep="/"))
}
openair::windRose(surface_file_1, auto.text=FALSE)
openair::windRose(surface_file_1, auto.text=FALSE, type="season")
openair::windRose(surface_file_1, auto.text=FALSE, type="daylight", latitude=rmetObj$surf_Latitude, 
                  longitude=rmetObj$surf_Longitude, local.hour.offset = rmetObj$surf_UTC)
openair::windRose(surface_file_1, auto.text=FALSE, type=c("season","daylight"), latitude=rmetObj$surf_Latitude, 
                  longitude=rmetObj$surf_Longitude, local.hour.offset = rmetObj$surf_UTC)

surface_plot_fct(surface_file_1$ws,surface_file_1$date, ylab="Wind Speed (m/s)", title="Surface File Wind Speed", factor=1)
surface_plot_fct(surface_file_1$wd,surface_file_1$date, ylab="Wind Direction", title="Wind Direction", factor=1)
if(any(!is.na(surface_file_1$sensible_heat_flux))){
surface_plot_fct(surface_file_1$sensible_heat_flux,surface_file_1$date,factor=1, ylab = expression(paste("Sensible Heat Flux ( ", W/m^2, ")", sep = "")), title="Sensible Heat Flux")
boxplot(surface_file_1$sensible_heat_flux~surface_file_1$month, ylab=expression(paste("Sensible Heat Flux (", W/m^2, ")", sep = "")), xlab="Month", main="Sensible Heat Flux by Month")
boxplot(surface_file_1$sensible_heat_flux~surface_file_1$hour, ylab=expression(paste("Sensible Heat Flux (", W/m^2, ")", sep = "")), xlab="Hour", main="Sensible Heat Flux by Hour")
}

if(any(!is.na(surface_file_1$monin_obukhov_length))){
surface_plot_fct(surface_file_1$surface_friction_velocity, surface_file_1$date, factor=1, ylab = expression(paste("Surface Friction Velocity ( ", m/s, ")", sep = "")), title="Surface Friction Velocity")
surface_plot_fct(surface_file_1$convective_velocity_scale, surface_file_1$date, factor=1, ylab = expression(paste("Convective Velocity Scale ( ", m/s, ")", sep = "")), title="Convective Velocity Scale")
surface_plot_fct(surface_file_1$verticle_potential_temperature, surface_file_1$date, factor=1, ylab = "Vertical Potential Temperature",title="Vertical Potential Temperature Gradient above PBL")
surface_plot_fct(surface_file_1$height_conv_pbl, surface_file_1$date, factor=1, ylab="Height CBL (m)", title="Height of Convective Boundary Layer")
surface_plot_fct(surface_file_1$height_mech_sbl, surface_file_1$date, factor=1, ylab="Height SBL (m)", title="Height of Stable Boundary Layer")
surface_plot_fct(surface_file_1$monin_obukhov_length, surface_file_1$date, factor=1, ylab="Monin-Obukhov Length (m)", title="Monin-Obukhov Length")
boxplot(surface_file_1$monin_obukhov_length~surface_file_1$month, ylab="Monin-Obukhovlenght(m)", xlab="Month",main="Monin-Obukhov Length by Month")
boxplot(surface_file_1$monin_obukhov_length~surface_file_1$hour, ylab="Monin-Obukhovlenght(m)", xlab="Hour",main="Monin-Obukhov Length by Hour")
# 
# with(surface_file_1, 
#      plot(1/monin_obukhov_length, log10(surface_roughness), ylim=rev(range(log10(surface_roughness))), pch="."))
golderPlot(surface_file_1)
}
surface_plot_fct(surface_file_1$temperature, surface_file_1$date, factor=1, ylab="Temperature (K)", title="Temperature")
boxplot(surface_file_1$temperature~surface_file_1$month, ylab="Temperature (K)", xlab="Month", main="Temperature by Month")
boxplot(surface_file_1$temperature~surface_file_1$hour, ylab="Temperature (K)", xlab="Hour", main="Temperature by Hour")
surface_plot_fct(surface_file_1$relative_humidity, surface_file_1$date, factor=1, ylab="Relative Humidity (%)", title="Relative Humidity")
surface_plot_fct(surface_file_1$surface_pressure, surface_file_1$date, factor=1, ylab="Surface Pressure (mb)", title="Surface Pressure")
if(any(!is.na(surface_file_1$cloud_cover))){
surface_plot_fct(surface_file_1$cloud_cover, surface_file_1$date, factor=1, ylab="Cloud Cover (tenths)", title="Cloud Cover")
}
surface_plot_fct(surface_file_1$albedo, surface_file_1$date, factor=1, ylab="Albedo", title="Albedo")
boxplot(surface_file_1$albedo~surface_file_1$month, ylab="Albedo", xlab="Month", main="Albedo by Month")
boxplot(surface_file_1$albedo~surface_file_1$hour, ylab="Albedo", xlab="Hour", main="Albedo by Hour")

if(!is.null(outfile)){
  dev.off()
}
if(returnDfx) return(surface_file_1)
if(!returnDfx) return(NULL)


}




