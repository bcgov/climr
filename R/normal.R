#' Create normal baseline input for `downscale`
#' @param normal A character. Label of the normal baseline to use. Can be obtained from
#' `list_normal()`. Default to `list_normal()[1]`.
#' @param dem A character. Label of the digital elevation model to use. Must have the same
#' extent as the normal baseline. Can be obtained from `list_dem()`. Default to `list_dem()[1]`.
#' @return A normal baseline to use with `downscale`. A `SpatRaster` with a `dem` attribute and
#' a `lapse_rates` attribute..
#' @importFrom terra rast compareGeom
#' @export
normal_input <- function(normal = list_normal()[1], dem = list_dem()[1]) {
  
  # Check if we have data, if not download some.
  data_check()
  
  # Load dem first file
  dir_dem <- file.path(
    data_path(),
    getOption("climRpnw.dem.path", default = "inputs_pkg/dem"),
    dem
  )
  dem <- terra::rast(list.files(dir_dem, full.names = TRUE)[1])
  
  # Load normal files
  dir_normal <- file.path(
    data_path(),
    getOption("climRpnw.normal.path", default = "inputs_pkg/normal"),
    normal
  )
  normal <- lapply(list.files(dir_normal, full.names = TRUE), terra::rast)
  
  # All objects have to share the same extent for now
  # This could be modified to process all the objects to adjust them to
  # the same raster extent.
  if (!terra::compareGeom(normal, dem)) {
    stop(
      "Normal do not share the same extent, number of rows and columns, projection,",
      " resolution and origin as the referenced digital elevation model."
    )
  }
  
  # Compute lapse rates and cache for same session reprocessing
  lapse_rates <- lapse_rate(normal, dem)
  
  # Set dem/lapse_rates as attribute to normal
  attr(normal, "dem") <- dem
  attr(normal, "lapse_rates") <- lapse_rates
  attr(normal, "builder") <- "climRpnw"
  
  return(normal)
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
  for (dir in dirs) {
    # Check if all months for all three variables are there
    files <- list.files(
      file.path(
        data_path(),
        getOption("climRpnw.normal.path", default = "inputs_pkg/normal"),
        dir
      )
    )
    avail <- vapply(strsplit(files, split = ".", fixed = TRUE), `[`, character(1), 1)
    vars <- c("PPT", "Tmax", "Tmin")
    months <- sprintf("%02d", 1:12)
    needed <- apply(expand.grid(vars, months), 1, paste0, collapse = "")
    if (any(miss <- !avail %in% needed)) {
       warning(
         dir, " data is available, but missing information about ",
         paste0(avail[which(miss)], collapse = ", "), "."
       )
    }
  }
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
    # Check if all months for all three variables are there
    files <- list.files(
      file.path(
        data_path(),
        getOption("climRpnw.dem.path", default = "inputs_pkg/dem"),
        dir
      )
    )
    avail <- vapply(strsplit(files, split = ".", fixed = TRUE), `[`, character(1), 1)
    if (length(avail) > 1) {
      warning(
        dir, " data is available, but multiple files found in subdirectory ",
        paste0(avail, collapse = ", "), ". Only the first one is loaded."
      )
    }
  }
  return(dirs)
}
