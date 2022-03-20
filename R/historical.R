historical_build <- function(objects, dem) {
  
  # All objects have to share the same extent for now
  
  # This could be modified to process all the objects to adjust them to
  # the same raster extent.
  
  if (!all(vapply(objects, compare_, logical(1), dem))) {
    stop("Objects do not share the same extent, number of rows and columns, projection, resolution and origin as the referenced digital elevation model.")
  }
  attr(objects, "dem") <- dem
  objects
}

historical_available <- function() {
  files <- list.files(file.path(data_path(), getOption("climRpnw.normal.path")), recursive = TRUE, full.names = TRUE)
  # Match the extent of each file
  # For each different extent
}