# Transform raster to matrix for faster binary operations

setGeneric("as_matrix", def = function(x) standardGeneric("as_matrix"))

setMethod("as_matrix", signature(x = "RasterLayer"),
          function(x) {raster::as.matrix(x)})

setMethod("as_matrix", signature(x = "SpatRaster"),
          function(x) {terra::as.matrix(x, wide = TRUE)})