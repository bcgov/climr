#' Create historic input for `downscale`.
#' @param period A character vector. Label of the period to use.
#' Can be obtained from `list_period()`. Default to `list_period()`.
#' @return An object to use with `downscale`. A `SpatRaster` with, possibly, multiple layers.
#' @importFrom terra rast
#' @importFrom utils head
#' @export

historic_input <- function(period = list_historic()[1]) {
  
  # Check if we have data, if not download some.
  data_check()
  
  # Get relevant files
  get_rel_files <- function(pattern) {
    res <- lapply(
      file.path(
        data_path(),
        getOption("climRpnw.historic.path", default = "inputs_pkg/historic"),
        period
      ),
      list.files, recursive = TRUE, full.names = TRUE, pattern = pattern
    )
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

#' List available historic periods
#' @export

list_historic <- function() {
  list.files(
    file.path(
      data_path(),
      getOption("climRpnw.historic.path", default = "inputs_pkg/historic")
    )
  )
}

# dat <- rast("../climR-pnw-data/inputs_pkg/historic/Historic_2001_2020/anom_2001_2020.nc")
# nm <- fread("../climR-pnw-data/inputs_pkg/historic/Historic_2001_2020/anom_2001_2020.csv",header = T)[['x']]
# r <- terra::rast(list.files(dir_gcm, full.names = TRUE, pattern = "\\.nc"))
# names(dat) <- nm
# 
# message(
#   "Saving uncompressed historic deltas to: ",
#   file.path(dir_hist, sprintf("gcmData.%s.deltas.tif", h))
# )
# 
# # Actual writing
# terra::writeRaster(
#   dat,
#   "../climR-pnw-data/inputs_pkg/historic/Historic_2001_2020/2001_2020.tif",
#   overwrite = TRUE,
#   gdal="COMPRESS=NONE"
# )
