#' Save an rmet project to disk
#'
#' @param rmetObj An rmet object
#'
#' @return Invisibly returns the rmet object
#'
#' @export
saveRmet <- function(rmetObj) {
  stopifnot(is.rmet(rmetObj))
  
  rds_file <- file.path(
    rmetObj$project_Dir,
    paste0(make.names(rmetObj$project_Name), ".rds")
  )
  
  saveRDS(rmetObj, rds_file)
  invisible(rmetObj)
}

