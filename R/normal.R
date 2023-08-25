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

#' Create normal baseline input for `downscale` from cloud db
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
normal_input_postgis <- function(normal = list_normal()[1], dbCon, bbox = NULL) {
  
  res <- pgGetTerra(dbCon,"normal_wna", boundary = bbox, bands = 1:73)
  names(res) <- c("PPT01", "PPT02", "PPT03", "PPT04", "PPT05", "PPT06", "PPT07", 
                  "PPT08", "PPT09", "PPT10", "PPT11", "PPT12", "Tmax01", "Tmax02", 
                  "Tmax03", "Tmax04", "Tmax05", "Tmax06", "Tmax07", "Tmax08", "Tmax09", 
                  "Tmax10", "Tmax11", "Tmax12", "Tmin01", "Tmin02", "Tmin03", "Tmin04", 
                  "Tmin05", "Tmin06", "Tmin07", "Tmin08", "Tmin09", "Tmin10", "Tmin11", 
                  "Tmin12", "lr_PPT01", "lr_PPT02", "lr_PPT03", "lr_PPT04", "lr_PPT05", 
                  "lr_PPT06", "lr_PPT07", "lr_PPT08", "lr_PPT09", "lr_PPT10", "lr_PPT11", 
                  "lr_PPT12", "lr_Tmax01", "lr_Tmax02", "lr_Tmax03", "lr_Tmax04", 
                  "lr_Tmax05", "lr_Tmax06", "lr_Tmax07", "lr_Tmax08", "lr_Tmax09", 
                  "lr_Tmax10", "lr_Tmax11", "lr_Tmax12", "lr_Tmin01", "lr_Tmin02", 
                  "lr_Tmin03", "lr_Tmin04", "lr_Tmin05", "lr_Tmin06", "lr_Tmin07", 
                  "lr_Tmin08", "lr_Tmin09", "lr_Tmin10", "lr_Tmin11", "lr_Tmin12", 
                  "dem2_WNA")
  attr(res, "builder") <- "climRpnw"
  # Return preprocessed raster
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
