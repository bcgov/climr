## `variables` data object (external)

library(terra)
library(bcdata)
library(data.table)

## coastline of Vancouver
vancouver <- vect("data-raw/Coast_Vancouver.shp") 

## DEM of North Vancouver (smaller than coastline)
dem_vancouver <- rast("data-raw/example_VanNorthShore.tif")

## sample of points in North Vancouver
vancouver_points <- as.points(dem_vancouver)
points2sample <- which(vancouver_points$WNA_DEM_SRT_30m > 0)
set.seed(123)
samp <- sample(points2sample, 1000, replace = FALSE)
vancouver_points <- vancouver_points[samp,]
vancouver_points[, 1] <- NULL
vancouver_points$id <- 1:nrow(vancouver_points)


## a regular grid of points at coarse scale
point_grid <- as.points(project(dem_vancouver, crs(dem_vancouver), res = 0.01))
point_grid[, 1] <- NULL
point_grid$id <- 1:nrow(point_grid)

## a polygon in North Vancouver
vancouver_poly <- dem_vancouver
vancouver_poly[vancouver_poly[] == 0] <- NA
vancouver_poly[!is.na(vancouver_poly[])] <- 1
vancouver_poly <- as.polygons(vancouver_poly) |>
  disagg()

BECz <- vect(bcdc_get_data("bec-zones-generalized-1-2m-"))
BECz <- project(BECz, crs(vancouver))
BECz_vancouver <- crop(BECz, ext(vancouver))

## add colours
BECcols <- fread("data-raw/BGCzone_Colorscheme.csv")
setnames(BECcols, "zone", "ZONE")
BECz_vancouver <- merge(BECz_vancourver, BECcols, by = "ZONE")

## wrap terra objects before saving
vancouver <- wrap(vancouver)
BECz_vancouver <- wrap(BECz_vancouver)
dem_vancouver <- wrap(dem_vancouver)
vancouver_points <- wrap(vancouver_points)
vancouver_poly <- wrap(vancouver_poly)
point_grid <- wrap(point_grid)

usethis::use_data(vancouver, overwrite = TRUE, internal = FALSE)
usethis::use_data(vancouver_points, overwrite = TRUE, internal = FALSE)
usethis::use_data(vancouver_poly, overwrite = TRUE, internal = FALSE)
usethis::use_data(point_grid, overwrite = TRUE, internal = FALSE)
usethis::use_data(dem_vancouver, overwrite = TRUE, internal = FALSE)
usethis::use_data(BECz_vancouver, overwrite = TRUE, internal = FALSE)
usethis::use_data(BECcols, overwrite = TRUE, internal = FALSE)

