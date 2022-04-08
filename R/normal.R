#' Create normal baseline input for `downscale`
#' @param normal A character or a SpatRaster. For character, label of the normal baseline to use.
#' Can be obtained from `list_normal()`. For SpatRaster, 36 layers normal climate variables with
#' names PPT01:PPT12, Tmax01:Tmax12 and Tmin01:Tmin12. Default to `list_normal()[1]`.
#' @param dem A digital elevation model SpatRaster. Only needed if normal is a SpatRaster.
#' Default to NULL.
#' @param ... Other arguments for lapse rates calculation. See `?lapse_rate`.
#' @return A normal baseline to use with `downscale`. A `SpatRaster` containing normals, lapse rates
#' and digital elevation model layers.
#' @importFrom terra rast writeRaster
#' @export
normal_input <- function(normal = list_normal()[1], dem = NULL, ...) {
  
  # Check if we have data, if not download some.
  data_check()

  # When using label
  if (!inherits(normal, "SpatRaster")) {
    # Where are normal files for this normal
    dir_normal <- file.path(
      data_path(),
      getOption("climRpnw.normal.path", default = "inputs_pkg/normal"),
      normal
    )
    file_tif <- list.files(dir_normal, full.names = TRUE, pattern = "\\.tif")
    res <- terra::rast(file_tif)
    attr(res, "builder") <- "climRpnw"
    # Return preprocessed raster
    return(res)
  }
  
  # Compute lapse rates
  lr <- lapse_rate(normal, dem, ...)
  
  # Actual writing
  f <- tempfile(fileext = "tif")
  terra::writeRaster(c(normal, lr, dem), f, overwrite = TRUE, gdal="COMPRESS=NONE")
  
  res <- terra::rast(f)
  attr(res, "builder") <- "climRpnw"
  
  return(res)
}

#' List available normal
#' @export
list_normal <- function() {
  dirs <- list.files(
    file.path(
      data_path(),
      getOption("climRpnw.normal.path", default = "inputs_pkg/normal")
    )
  )
  return(dirs)
}
