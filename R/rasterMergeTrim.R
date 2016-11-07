#' @title rasterMergeTrim
#' 
#' @description
#' \code{rasterMergeTrim} Crops a NLCD or NED file to a radius around a fixed point.
#' The point is expected to be WGS 1984 lattitude and longitude. Buffer is in meters.
#' 
#' NLCD 1992 data is becoming challenging to obtain, at least for me. Previously,
#' I have been downloading NLCD 1992 data from Earth Explorer. These files have 
#' colortable attributes and will it will be preserved in the trimming and merging
#' process. However, it seems these files have wandered off of the Earth Explorer website,
#' so not, I am currently downloading these rasters from [Mult-Resolution Land Characteristics Consortium](http://www.mrlc.gov/nlcd92_data.php)
#' However, these tiles lack a legend and colortable attributes. This function will add the
#' a colortable for processing with AERSURFACE. 
#' 
#'  @param rasterFiles a single character string or vector of character strings specifying 
#'  the location of the rasters to be cropped merged. 
#'  
#'  @param long numeric longitude of point
#'  @param lat numeric latitude of point
#'  @buffer distance in meters for cropping around point (meters)
#'  @outFile destination of merged and cropped output
#' @export
#' 

rasterMergeTrim <- function(rasterFiles, long, lat, buffer = 5000,
                            outFile){
  stopifnot(is.numeric(long))
  stopifnot(long>-180 & long < 180)
  stopifnot(is.numeric(lat))
  stopifnot(lat>-180 & lat < 180)
  stopifnot(length(lat) + length(long) == 2)

  stopifnot(all(lapply(rasterFiles, file.exists)))
  
  
  
  x <- data.frame(lon = long, lati= lat)
  coordinates(x) <- ~lon+lati
  sp::proj4string(x) <- CRS("+init=epsg:4326")
  y <- geosphere::destPoint(x, b=1:365 ,d=buffer)
 
  y.sp <- sp::SpatialPolygons(list(Polygons(list(Polygon(y)), "R1")),
                          proj4string=CRS(proj4string(x)))
  
  
  
  if(length(rasterFiles) == 1){
    
    rasterFiles <- raster::raster(rasterFiles)

    dfx <- spTransform(y.sp, CRS(proj4string(rasterFiles)))
    
    rastXY <- raster::crop(rasterFiles, raster::extent(dfx))
    if(length(raster::colortable(rastXY)) == 0){
      rastXY@legend <- nlcdLegend
    
      }else{
        raster::colortable(rastXY) <- raster::colortable(rasterList[[1]])
    }
    
    raster::writeRaster(rastXY, filename=outFile, 
                options=c("COMPRESS=NONE", "TFW=YES"), overwrite=TRUE)
   
  }else{
    
    rasterList <- lapply(rasterFiles, raster::raster)
    stopifnot(all(lapply(rasterList, proj4string) == proj4string(rasterList[[1]])))
    
    stopifnot(all(lapply(seq_along(rasterList), function(i){
      identical(rasterList[[1]]@legend@colortable, rasterList[[i]]@legend@colortable)
        }
      )))
    
    dfx <- spTransform(y.sp, CRS(proj4string(rasterList[[1]])))
    rasterList <- lapply(seq_along(rasterList), function(i){
      rastX <- raster::crop(rasterList[[i]], raster::extent(dfx))
      is.na(raster::values(rastX)) <- raster::values(rastX) == 0
      return(rastX)
    })
    

    rasterList$overwrite = TRUE
    rastXY <- do.call(raster::merge, rasterList)
    if(length(raster::colortable(rastXY)) == 0){
      rastXY@legend <- raster::nlcdLegend
    }else{
      raster::colortable(rastXY) <- colortable(rasterList[[1]])
    }
    
    raster::writeRaster(rastXY, filename=outFile, 
                          options=c("TFW=YES", "COMPRESS=NONE"), overwrite=TRUE)
    
  }
  
    
return(rastXY)  
  
}




