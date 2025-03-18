# Need ram

library(terra)

# Local
# tif_in <- "E:/Work/Active project/climna-tif/mosaic/climr_mosaic.tif"
# tif_mask <- "E:/Work/Active project/climna-tif/mosaic/climr_mosaic_land.tif"
# dir_out <- "E:/Work/Active project/climna-tif/mosaic/"

tif_in <- "/share/climr_mosaic.tif"
tif_mask <- "/share/climr_mosaic_land.tif"
dir_out <- "/opt/climr-tif/NA/2500m/mosaic"

dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)

r <- terra::rast(tif_in)
mask <- terra::rast(tif_mask)

for (nm in grep("^Tmin|^Tmax|^PPT", names(r), value = TRUE)) {
  r[[nm]] |>
    terra::mask(mask) |>
    terra::writeRaster(filename = file.path(dir_out, paste0(nm, ".tif")), gdal = "PREDICTOR=2", overwrite = TRUE)
}

nm <- grep("elev", names(r), value = TRUE)
r[[nm]] |>
  terra::mask(mask) |>
  terra::writeRaster(filename = file.path(dir_out, "elev.tif"), gdal = "PREDICTOR=2", overwrite = TRUE)

climr::get_latitude_raster(r[[1]]) |> 
  terra::mask(mask) |>
  terra::writeRaster(filename = file.path(dir_out, "lat.tif"), gdal = "PREDICTOR=2", overwrite = TRUE)

climr::tif_folder_gen(dir_out, overwrite = TRUE)
