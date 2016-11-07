#' @title surfaceReader
#' 
#' @description
#' \code{surfaceReader} reads the surface file output of aermet
# into R, converts missing values to NA and allows further 
# graphical and numerical exploration of surface file

#' @export
#' 


surfaceReader <- function(sfile, rmetObj){
  surface_file_1<-read.table(sfile, skip=1)
  
  if(dim(surface_file_1)[[2]] ==27)
  colnames(surface_file_1)<-c(
    "year",
    "month",
    "day",
    "jday",
    "hour",
    "sensible_heat_flux",
    "surface_friction_velocity",
    "convective_velocity_scale",
    "verticle_potential_temperature",
    "height_conv_pbl",
    "height_mech_sbl",
    "monin_obukhov_length",
    "surface_roughness",
    "bowen_ratio",
    "albedo",
    "ws",
    "wd",
    "reference_height_direction",
    "temperature",
    "reference_height_temp",
    "precipitation_code",
    "precipitation_rate",
    "relative_humidity",
    "surface_pressure",
    "cloud_cover",
    "ws_adj_flag",
    "OtherInfo")
  
  if(dim(surface_file_1)[[2]] == 26)
    colnames(surface_file_1)<-c(
      "year",
      "month",
      "day",
      "jday",
      "hour",
      "sensible_heat_flux",
      "surface_friction_velocity",
      "convective_velocity_scale",
      "verticle_potential_temperature",
      "height_conv_pbl",
      "height_mech_sbl",
      "monin_obukhov_length",
      "surface_roughness",
      "bowen_ratio",
      "albedo",
      "ws",
      "wd",
      "reference_height_direction",
      "temperature",
      "reference_height_temp",
      "precipitation_code",
      "precipitation_rate",
      "relative_humidity",
      "surface_pressure",
      "cloud_cover",
      "ws_adj_flag")
  
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
  
  ## set the total hours for the file and add it sequentially to the file
  
  total_hours <- length(surface_file_1$hour)
  
  seq_hour<-seq(1:total_hours)
  
  ## Need to add date field to records so we can make prettier things, I hope.
  
  # use ISOdate( ) to convert strings to dates
  # 
  adjustment<-NULL
  
  if(min(surface_file_1$year)<50) adjustment=2000 else adjustment=1900
  
  
  
  date<-lubridate::ymd_h(paste(surface_file_1$year+adjustment,surface_file_1$month,surface_file_1$day, surface_file_1$hour-1), tz=paste0("Etc/GMT+", -rmetObj$surf_UTC))
#  date <-strptime(as.character(date), "%Y-%m-%d")   #change the date field to a internal form for time dates
  
  surface_file_1<-cbind(seq_hour, surface_file_1)
  surface_file_1<-cbind(date, surface_file_1)
  
  return(surface_file_1)
  
}