## `variables` data object (external)

library(terra)

vancouver <- vect("data-raw/Coast_Vancouver.shp") |>
  wrap()
dem_vancouver <- rast("data-raw/example_VanNorthShore.tif") |>
  wrap()


usethis::use_data(vancouver, overwrite = TRUE, internal = FALSE)
usethis::use_data(dem_vancouver, overwrite = TRUE, internal = FALSE)
