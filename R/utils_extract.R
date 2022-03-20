setGeneric("extract_", def = function(x, pts, method, ...) standardGeneric("extract_"))

# Extract from raster
setMethod("extract_", signature(x = "Raster"), 
          function(x, pts, method, ...) {
            raster::extract(x = x, y = pts, method = method, ...)
          }
)

setMethod("extract_", signature(x = "SpatRaster"), 
          function(x, pts, method, ...) {
            terra::extract(x = x, y = pts, method = method, ...)
          }
)