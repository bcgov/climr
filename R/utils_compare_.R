# Comparing rasters

setGeneric("compare_", def = function(x, ...) { all.equal(x, ...) })

setMethod("compare_", signature(x = "RasterLayer"),
          raster::compareRaster)

setMethod("compare_", signature(x = "SpatRaster"),
          terra::compareGeom)
