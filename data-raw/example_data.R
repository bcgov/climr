## `variables` data object (external)

library(terra)
library(bcdata)
library(data.table)

## coastline of Vancouver
vancouver <- vect("data-raw/Coast_Vancouver.shp") 

## DEM of North Vancouver (smaller than coastline)
dem_vancouver <- rast("data-raw/example_VanNorthShore.tif")

dem_vancouver_lowres <- project(dem_vancouver, crs(dem_vancouver), res = 0.005) 

## sample of points in North Vancouver
vancouver_points <- as.points(dem_vancouver)
points2sample <- which(vancouver_points$WNA_DEM_SRT_30m > 0)
set.seed(123)
samp <- sample(points2sample, 1000, replace = FALSE)
vancouver_points <- vancouver_points[samp,]
vancouver_points[, 1] <- NULL
vancouver_points$id <- 1:nrow(vancouver_points)

## a polygon in North Vancouver
vancouver_poly <- dem_vancouver
vancouver_poly[vancouver_poly[] == 0] <- NA
vancouver_poly[!is.na(vancouver_poly[])] <- 1
vancouver_poly <- as.polygons(vancouver_poly) |>
  disagg()

BECz <- vect(bcdc_get_data("bec-zones-generalized-1-2m-"))
BECz <- project(BECz, crs(vancouver))
BECz_vancouver <- crop(BECz, ext(vancouver))

BECz_vancouver_ras <- rasterize(BECz_vancouver, dem_vancouver, field = "ZONE")

## add colours
BECcols <- fread("data-raw/BGCzone_Colorscheme.csv")
setnames(BECcols, "zone", "ZONE")
BECz_vancouver <- merge(BECz_vancouver, BECcols, by = "ZONE")

coltb <- data.table(ZONE = BECcols$ZONE, col = BECcols$HEX)
levs <- as.data.table(levels(BECz_vancouver_ras)[[1]])
coltb <- coltb[levs, on = "ZONE"]
setnames(coltb, "ID", "value")
coltab(BECz_vancouver_ras) <- coltb[, .(value, col)]

## adjusted precipitation stations from PCIC (https://services.pacificclimate.org/met-data-portal-pcds/app/)
weather_stations <- fread("data-raw/station-metadata-by-history_BC.csv")
weather_stations$`Elevation (m)` <- as.numeric(weather_stations$`Elevation (m)`) 

## a simplified version
xyzDT <- weather_stations[, .(`Station ID`, Longitude, Latitude, `Elevation (m)`)]  
setnames(xyzDT, c("id", "lon", "lat", "elev"))
xyzDT <- unique(xyzDT)
xyzDT <- xyzDT[!duplicated(id),]  ## still many duplicated stations with slightly different coords/elev
xyzDT <- xyzDT[complete.cases(xyzDT)]

weather_stations <- weather_stations[!is.na(`Elevation (m)`)]
weather_stations <- vect(weather_stations, geom = c("Longitude", "Latitude"), crs = "EPSG:4326")

## wrap terra objects before saving
vancouver <- wrap(vancouver)
BECz_vancouver <- wrap(BECz_vancouver)
BECz_vancouver_ras <- wrap(BECz_vancouver_ras)
dem_vancouver <- wrap(dem_vancouver)
dem_vancouver_lowres <- wrap(dem_vancouver_lowres)
vancouver_points <- wrap(vancouver_points)
vancouver_poly <- wrap(vancouver_poly)
weather_stations <- wrap(weather_stations)


usethis::use_data(vancouver, overwrite = TRUE, internal = FALSE)
usethis::use_data(vancouver_points, overwrite = TRUE, internal = FALSE)
usethis::use_data(vancouver_poly, overwrite = TRUE, internal = FALSE)
usethis::use_data(dem_vancouver, overwrite = TRUE, internal = FALSE)
usethis::use_data(dem_vancouver_lowres, overwrite = TRUE, internal = FALSE)
usethis::use_data(BECz_vancouver, overwrite = TRUE, internal = FALSE)
usethis::use_data(BECz_vancouver_ras, overwrite = TRUE, internal = FALSE)
usethis::use_data(BECcols, overwrite = TRUE, internal = FALSE)
usethis::use_data(weather_stations, overwrite = TRUE, internal = FALSE)
usethis::use_data(xyzDT, overwrite = TRUE, internal = FALSE)

