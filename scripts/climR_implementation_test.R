library(raster)
library(microbenchmark)
library(sf)

gcm <- "GFDL-ESM4"
element <- "tasmin"
tasmin <- brick(paste("inputs/gcmData/gcmData", gcm, element, "nc", sep="."))
index <- read.csv(paste("inputs/gcmData/gcmIndex", gcm, element, "csv", sep="."), )
names(tasmin) <- index[,-1]

# Elevation
elev <- raster("./inputs/digitalElevationModel/dem2_WNA.asc")


# Random coordinates
n <- 100000
bbox <- st_bbox(elev)
xy <- cbind(runif(n, bbox$xmin, bbox$xmax), runif(n, bbox$ymin, bbox$ymax))

# microbenchmark({
#   tasmin <- bilinear <- raster:::.bilinearValue(tasmin, xy, 1, 12)  
# }, times = 1)

tasmin_bilinear <- raster:::.bilinearValue(tasmin, xy, 1, 12)

# NA are points over the water, can be set to 0
elev_bilinear <- raster:::.bilinearValue(elev, xy)
elev_bilinear[is.na(elev_bilinear)] <- 0L

