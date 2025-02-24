library(terra)

dir <- "climna-tif/mosaic"
r <- terra::rast(file.path(dir, "climr_mosaic.tif"))
names(r)

for (nm in grep("^Tmin|^Tmax|^PPT", names(r), value = TRUE)) {
  terra::writeRaster(r[[nm]], file.path(dir, paste0(nm, ".tif")), gdal = "PREDICTOR=2")
}

nm <- grep("elev", names(r), value = TRUE)
terra::writeRaster(r[[nm]], file.path(dir, "elev.tif"), gdal = "PREDICTOR=2", overwrite = TRUE)

climr::get_latitude_raster(r[[1]], out = file.path(dir, "lat.tif"), gdal = "PREDICTOR=2", overwrite = TRUE)

climr::tif_folder_gen(dir, overwrite = TRUE)
