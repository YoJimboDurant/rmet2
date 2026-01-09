#' Download NLCD Inputs for AERSURFACE
#'
#' Ensures that NLCD land cover, tree canopy, and impervious surface rasters
#' required by AERSURFACE exist for an \code{rmetObj}. Missing inputs are
#' downloaded, projected to the canonical AERSURFACE CRS, and written to disk.
#'
#' @param rmetObj An rmet project object.
#' @param buffer_m Buffer distance (meters) defining the AERSURFACE study area.
#'   Defaults to \code{15000}.
#' @param year NLCD year to use. Defaults to \code{2019}.
#' @param overwrite Logical; force regeneration if \code{TRUE}.
#' @param quiet Logical; suppress messages if \code{TRUE}.
#'
#' @return The updated \code{rmetObj}.
#'
#' @export
downloadNLCD <- function(
    rmetObj,
    buffer_m = 15000,
    year = 2019,
    overwrite = FALSE,
    quiet = FALSE
) {
  
  stopifnot(
    is.list(rmetObj),
    !is.null(rmetObj$surf_Latitude),
    !is.null(rmetObj$surf_Longitude)
  )
  
  outDir <- paste(rmetObj$project_Dir, "preprocessed_data", sep = "/")
  dir.create(outDir, recursive = TRUE, showWarnings = FALSE)
  
  files <- list(
    landcover  = file.path(outDir, "nlcd_landcover_aersurface.tif"),
    treecanopy = file.path(outDir, "nlcd_treecanopy_aersurface.tif"),
    impervious = file.path(outDir, "nlcd_impervious_aersurface.tif")
  )
  
  need_build <- overwrite || !all(file.exists(unlist(files)))
  
  if (need_build) {
    
    if (!quiet) message("Ensuring NLCD inputs for AERSURFACE...")
    
    template <- .BuildAERSURFACETemplate(
      rmetObj$surf_Latitude,
      rmetObj$surf_Longitude,
      buffer_m
    )
    
    nlcd_lc <- FedData::get_nlcd(
      template = template,
      year = year,
      dataset = "landcover",
      label = "aersurface_landcover",
      extraction.dir = outDir,
      force.redo = overwrite
    )
    
    nlcd_tc <- FedData::get_nlcd(
      template = template,
      year = year,
      dataset = "canopy",
      label = "aersurface_treecanopy",
      extraction.dir = outDir,
      force.redo = overwrite
    )
    
    nlcd_imp <- FedData::get_nlcd(
      template = template,
      year = year,
      dataset = "impervious",
      label = "aersurface_impervious",
      extraction.dir = outDir,
      force.redo = overwrite
    )
    
    .PrepAndWriteAERSURFACERaster(nlcd_lc,  files$landcover,  "INT1U")
    .PrepAndWriteAERSURFACERaster(nlcd_tc,  files$treecanopy, "INT1U")
    .PrepAndWriteAERSURFACERaster(nlcd_imp, files$impervious, "INT1U")
  }
  
  ## --- mutate rmetObj to designate output files ----

  
  
  rmetObj$aersurface$inputFiles$lc_File <- files$landcover
  rmetObj$aersurface$inputFiles$lc_Type <- paste0("NLCD", year)
  
  rmetObj$aersurface$inputFiles$imp_File <- files$impervious
  rmetObj$aersurface$inputFiles$imp_Type <- paste0("MPRV", year)
  
  rmetObj$aersurface$inputFiles$cnpy_File <- files$treecanopy 
  rmetObj$aersurface$inputFiles$cnpy_Type <- paste0("CNPY", year)
  
  ## ---- mutate rmetObj state (this is key) ----
  rmetObj$aersurface$nlcd <- files
  rmetObj$aersurface$buffer_m <- buffer_m
  rmetObj$aersurface$year <- year
  rmetObj$state$aersurface_nlcd <- TRUE
  
  rmetObj
}

#' @noRd
.GetAERSURFACEcrs <- function() {
  terra::crs(
    "PROJCRS[\"Albers_Conical_Equal_Area\",
      BASEGEOGCRS[\"NAD83\",
        DATUM[\"North American Datum 1983\",
          ELLIPSOID[\"GRS 1980\",6378137,298.257222101004,
            LENGTHUNIT[\"metre\",1]]],
        PRIMEM[\"Greenwich\",0,
          ANGLEUNIT[\"degree\",0.0174532925199433]],
        ID[\"EPSG\",4269]],
      CONVERSION[\"Albers Equal Area\",
        METHOD[\"Albers Equal Area\",
          ID[\"EPSG\",9822]],
        PARAMETER[\"Latitude of false origin\",23,
          ANGLEUNIT[\"degree\",0.0174532925199433],
          ID[\"EPSG\",8821]],
        PARAMETER[\"Longitude of false origin\",-96,
          ANGLEUNIT[\"degree\",0.0174532925199433],
          ID[\"EPSG\",8822]],
        PARAMETER[\"Latitude of 1st standard parallel\",29.5,
          ANGLEUNIT[\"degree\",0.0174532925199433],
          ID[\"EPSG\",8823]],
        PARAMETER[\"Latitude of 2nd standard parallel\",45.5,
          ANGLEUNIT[\"degree\",0.0174532925199433],
          ID[\"EPSG\",8824]],
        PARAMETER[\"Easting at false origin\",0,
          LENGTHUNIT[\"metre\",1],
          ID[\"EPSG\",8826]],
        PARAMETER[\"Northing at false origin\",0,
          LENGTHUNIT[\"metre\",1],
          ID[\"EPSG\",8827]]],
      CS[Cartesian,2],
        AXIS[\"easting\",east,
          ORDER[1],
          LENGTHUNIT[\"metre\",1,
            ID[\"EPSG\",9001]]],
        AXIS[\"northing\",north,
          ORDER[2],
          LENGTHUNIT[\"metre\",1,
            ID[\"EPSG\",9001]]]]"
  )
}

#' @noRd
.BuildAERSURFACETemplate <- function(lat, lon, buffer_m) {
  pt <- sf::st_sfc(sf::st_point(c(lon, lat)), crs = 4326)
  pt_aea <- sf::st_transform(pt, .GetAERSURFACEcrs())
  sf::st_buffer(pt_aea, dist = buffer_m)
}

#' @noRd
.PrepAndWriteAERSURFACERaster <- function(x, outfile, datatype) {
  
  aea_crs <- .GetAERSURFACEcrs()
  
#  if (!terra::compareGeom(x, terra::project(x, aea_crs), stopOnError = FALSE)) {
    x <- terra::project(x, aea_crs, method = "near")
# }
  
  terra::writeRaster(
    x,
    filename = outfile,
    datatype = datatype,
    NAflag = 0,
    overwrite = TRUE,
    gdal = c("COMPRESS=NONE", "TILED=NO")
  )
  
  invisible(outfile)
}
