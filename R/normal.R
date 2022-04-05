#' Create normal baseline input for `downscale`
#' @param normal A character. Label of the normal baseline to use. Can be obtained from
#' `list_normal()`. Default to `list_normal()[1]`.
#' @param dem A character. Label of the digital elevation model to use. Must have the same
#' extent as the normal baseline. Can be obtained from `list_dem()`. Default to `list_dem()[1]`.
#' @param use_cache Use a cached version of lapse rates if available instead of recomputing.
#' @return A normal baseline to use with `downscale`. A `SpatRaster` with a `dem` attribute and
#' a `lr` attribute..
#' @importFrom terra rast compareGeom
#' @export
normal_input <- function(normal = list_normal()[1], dem = list_dem()[1], use_cache = TRUE) {
  
  # Check if we have data, if not download some.
  data_check()

  # Where are normal files for this normal
  dir_normal <- file.path(
    data_path(),
    getOption("climRpnw.normal.path", default = "inputs_pkg/normal"),
    normal
  )  
  
  # Return precomputed if cached
  if (isTRUE(use_cache) && dir.exists(file.path(dir_normal, "cache"))) {
    
    # Directly using terra::rast does not preserve NA value from disk to memory.
    # It stores -max.int32. Workaround until fixed. Use raster::brick, do math
    # operation then use terra::rast.
    res <- terra::rast(
      list.files(file.path(dir_normal, "cache"), full.names = TRUE, pattern = "\\.tif")[1]
    ) + 0
    
    attr(res, "builder") <- "climRpnw"
    
    return(res)
    
  }
  
  # Load dem first file
  dir_dem <- file.path(
    data_path(),
    getOption("climRpnw.dem.path", default = "inputs_pkg/dem"),
    dem
  )
  # Directly using terra::rast does not preserve NA value from disk to memory.
  # It stores -max.int32. Workaround until fixed. Use raster::brick, do math
  # operation then use terra::rast.
  dem <- terra::rast(
    raster::raster(
      list.files(dir_dem, full.names = TRUE, pattern = "\\.nc")[1]
    ) - 0L
  )
  
  # Load normal files
  
  # Directly using terra::rast does not preserve NA value from disk to memory.
  # It stores -max.int32. Workaround until fixed. Use raster::brick, do math
  # operation then use terra::rast.
  # Tmax / Tmax are stored as : Real value * 10 then cast to integer
  # Recasting to real value
  nm <- data.table::fread(
    list.files(dir_normal, full.names = TRUE, pattern = "\\.csv")[1], header = TRUE
  )[["x"]]
  normal <- terra::rast(
    raster::brick(
      list.files(dir_normal, full.names = TRUE, pattern = "\\.nc")[1]
    ) / c(1L, 10L)[startsWith(nm, "T") + 1L]
  )
  names(normal) <- nm
  
  # All objects have to share the same extent for now
  # This could be modified to process all the objects to adjust them to
  # the same raster extent.
  if (!terra::compareGeom(normal, dem)) {
    stop(
      "Normal do not share the same extent, number of rows and columns, projection,",
      " resolution and origin as the referenced digital elevation model."
    )
  }
  
  lr <- lapse_rate(normal, dem)
  
  res <- c(normal, lr, dem)
  
  stopifnot(terra::nlyr(res) == 73)
  
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

#' List available digital elevation models
#' @export
list_dem <- function() {
  dirs <- list.files(
    file.path(
      data_path(),
      getOption("climRpnw.dem.path", default = "inputs_pkg/dem")
    )
  )
  for (dir in dirs) {
    # Check if multiple dem files are in one directory
    files <- list.files(
      file.path(
        data_path(),
        getOption("climRpnw.dem.path", default = "inputs_pkg/dem"),
        dir
      ),
      pattern = "\\.nc$"
    )
    if (length(files) > 1) {
      warning(
        dir, " data is available, but multiple NetCDF files found in subdirectory.",
        " Only the first one is loaded."
      )
    }
  }
  return(dirs)
}
