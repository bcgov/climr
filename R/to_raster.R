# Transform matrices into raster layers based on a reference raster

setGeneric("to_raster", def = function(x, y) standardGeneric("to_raster"))

setMethod("to_raster", signature(x = "list", "RasterLayer"), function(x, y) {
  res <- lapply(x, raster::raster)
  res <- raster::brick(res)
  raster::extent(res) <- raster::extent(y)
  return(res)
})

setMethod("to_raster", signature(x = "list", "SpatRaster"), function(x, y) {
  res <- terra::rast(lapply(x, terra::rast, extent = terra::ext(y)))
  return(res)
})