## `variables` data object (external)

library(terra)
library(bcdata)
library(data.table)

vancouver <- vect("data-raw/Coast_Vancouver.shp") 
dem_vancouver <- rast("data-raw/example_VanNorthShore.tif") |>
  wrap()

BECz <- vect(bcdc_get_data("bec-zones-generalized-1-2m-"))
BECz <- project(BECz, crs(vancouver))
BECz_vancouver <- crop(BECz, ext(vancouver))

## add colours
BECcols <- fread("data-raw/BGCzone_Colorscheme.csv")
setnames(BECcols, "zone", "ZONE")
BECz_vancouver <- merge(BECz_vancouver, BECcols, by = "ZONE")

vancouver <- wrap(vancouver)
BECz_vancouver <- wrap(BECz_vancouver)

usethis::use_data(vancouver, overwrite = TRUE, internal = FALSE)
usethis::use_data(dem_vancouver, overwrite = TRUE, internal = FALSE)
usethis::use_data(BECz_vancouver, overwrite = TRUE, internal = FALSE)
usethis::use_data(BECcols, overwrite = TRUE, internal = FALSE)

