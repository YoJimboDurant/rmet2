#' csvToISD
#' 
#' @description
#' \code{csvToISD} 
#' 
#' This function takes a data.frame read from csv formatted hourly met data from \url{https://www.ncei.noaa.gov/data/global-hourly/access/}
#' and converts it to the ISD format detailed in \url{https://www.ncei.noaa.gov/data/global-hourly/doc/isd-format-document.pdf} 
#' @param csv_dfx is data.frame read in from csv file from NCEI.
#' @returns String vector in ISD format. 
#' @export

csvToISD <- function(csv_dfx){

  fixed_dfx <- csv_dfx %>%
    select(STATION:SLP) %>%
    select(-NAME) %>%
    mutate(DATE = str_replace(DATE, "T", " "),
           DATE = format(lubridate::ymd_hms(DATE), '%Y%m%d%H%M'),
           LATITUDE = sprintf("%+06i", round(as.numeric(LATITUDE),3) * 1000),
           LONGITUDE = sign(as.numeric(LONGITUDE)) * 
                              (abs(as.numeric(LONGITUDE))+ 0.0005) * 1000,
           LONGITUDE = sprintf("%07d", LONGITUDE),
           ELEVATION = sprintf("%+05i", round(as.numeric(ELEVATION,0))),
           QUALITY_CONTROL = sprintf('% 5s', QUALITY_CONTROL),
           WND = str_remove_all(WND, ","),
           CIG = str_remove_all(CIG, ","),
           VIS = str_remove_all(VIS, ","),
           TMP = str_remove_all(TMP, ","),
           DEW = str_remove_all(DEW, ","),
           SLP = str_remove_all(SLP, ",")
           ) %>%
    mutate(TD3505_fix = paste0(STATION,DATE,SOURCE, LATITUDE, LONGITUDE, 
                               REPORT_TYPE,ELEVATION, CALL_SIGN, QUALITY_CONTROL,
                               WND,CIG, 
                               VIS, TMP, DEW, SLP))
  
  variable_dfx = csv_dfx %>%
    select(AA1: REM) %>%
    as.data.frame()
  
  variable_names <- names(variable_dfx)
  true_dfx <- is.na(variable_dfx)
  
  variable_dfx[] <- lapply(1:ncol(variable_dfx), function(col) paste0(variable_names[col],variable_dfx[,col]) )
  is.na(variable_dfx) <- true_dfx
  
  variable_x <- data.frame(col1=sapply(apply(variable_dfx, 1, \(x) x[!is.na(x)]), paste, collapse=''))
  
  TD3505_fix <- 
  pull(fixed_dfx, TD3505_fix)
  
  td3505 <- paste(TD3505_fix, variable_x$col1, sep = "ADD")
  td3505 <- stringr::str_replace_all(td3505, ",", "")
  
  td3505 <- paste0(sprintf('%04i', nchar(td3505) - 101), td3505)
  return(td3505)
}

