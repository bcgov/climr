#' Create historic input for `downscale`.
#' @param period A character vector. Label of the period to use.
#' Can be obtained from `list_period()`. Default to `list_period()`.
#' @param max_run An integer. Maximum number of model runs to include.
#' A value of 0 is `ensembleMean` only. Runs are included in the order they are found in the
#' models data untile `max_run` is reached. Default to 0L.
#' @return An object to use with `downscale`. A `SpatRaster` with, possibly, multiple layers.
#' @importFrom terra rast
#' @importFrom utils head
#' @export
historic_input <- function(period = list_period_historic()) {
  
  # Check if we have data, if not download some.
  data_check()
  
  # Get relevant files
  get_rel_files <- function(pattern) {
    res <- lapply(
      file.path(
        data_path(),
        getOption("climRpnw.historic.path", default = "inputs_pkg/historic"),
        gcm
      ),
      list.files, recursive = TRUE, full.names = TRUE, pattern = pattern
    )
    names(res) <- gcm
    res
  }
  files_tif <- get_rel_files("\\.tif$")
  
  # Load each file individually + select layers
  process_one_historic <- function(file_tif) {
    
    # Initiate raster
    r <- terra::rast(file_tif)
    #nm <- names(r)
    
    
    
    return(r)
    
  }
  
  res <- lapply(files_tif, process_one_historic)
  attr(res, "builder") <- "climRpnw" 
  
  # Return a list of SpatRaster, one element for each model
  return(res)
  
}
