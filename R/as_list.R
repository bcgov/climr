# Transform raster to matrix for faster binary operations

setGeneric("as_list", def = function(x) standardGeneric("as_list"))

setMethod("as_list", signature(x = "RasterBrick"),
          function(x) {attr(x, "dem") <- NULL; raster::as.list(x)})

setMethod("as_list", signature(x = "RasterLayer"),
          function(x) {attr(x, "dem") <- NULL; raster::as.list(x)})

setMethod("as_list", signature(x = "SpatRaster"),
          function(x) {attr(x, "dem") <- NULL; terra::as.list(x)})