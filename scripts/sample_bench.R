library(terra)
library(data.table)

# do it for all 36 variables
targets <- list(
  Tmax01 = terra::rast("./inputs/Normal_1961_1990MP/Tmax01.asc"),
  Tmax02 = terra::rast("./inputs/Normal_1961_1990MP/Tmax02.asc"),
  Tmax03 = terra::rast("./inputs/Normal_1961_1990MP/Tmax03.asc"),
  Tmax04 = terra::rast("./inputs/Normal_1961_1990MP/Tmax04.asc"),
  Tmax05 = terra::rast("./inputs/Normal_1961_1990MP/Tmax05.asc"),
  Tmax06 = terra::rast("./inputs/Normal_1961_1990MP/Tmax06.asc"),
  Tmax07 = terra::rast("./inputs/Normal_1961_1990MP/Tmax07.asc"),
  Tmax08 = terra::rast("./inputs/Normal_1961_1990MP/Tmax08.asc"),
  Tmax09 = terra::rast("./inputs/Normal_1961_1990MP/Tmax09.asc"),
  Tmax10 = terra::rast("./inputs/Normal_1961_1990MP/Tmax10.asc"),
  Tmax11 = terra::rast("./inputs/Normal_1961_1990MP/Tmax11.asc"),
  Tmax12 = terra::rast("./inputs/Normal_1961_1990MP/Tmax12.asc"),
  Tmin01 = terra::rast("./inputs/Normal_1961_1990MP/Tmin01.asc"),
  Tmin02 = terra::rast("./inputs/Normal_1961_1990MP/Tmin02.asc"),
  Tmin03 = terra::rast("./inputs/Normal_1961_1990MP/Tmin03.asc"),
  Tmin04 = terra::rast("./inputs/Normal_1961_1990MP/Tmin04.asc"),
  Tmin05 = terra::rast("./inputs/Normal_1961_1990MP/Tmin05.asc"),
  Tmin06 = terra::rast("./inputs/Normal_1961_1990MP/Tmin06.asc"),
  Tmin07 = terra::rast("./inputs/Normal_1961_1990MP/Tmin07.asc"),
  Tmin08 = terra::rast("./inputs/Normal_1961_1990MP/Tmin08.asc"),
  Tmin09 = terra::rast("./inputs/Normal_1961_1990MP/Tmin09.asc"),
  Tmin10 = terra::rast("./inputs/Normal_1961_1990MP/Tmin10.asc"),
  Tmin11 = terra::rast("./inputs/Normal_1961_1990MP/Tmin11.asc"),
  Tmin12 = terra::rast("./inputs/Normal_1961_1990MP/Tmin12.asc"),
  PPT01  = terra::rast("./inputs/Normal_1961_1990MP/PPT01.asc"),
  PPT02  = terra::rast("./inputs/Normal_1961_1990MP/PPT02.asc"),
  PPT03  = terra::rast("./inputs/Normal_1961_1990MP/PPT03.asc"),
  PPT04  = terra::rast("./inputs/Normal_1961_1990MP/PPT04.asc"),
  PPT05  = terra::rast("./inputs/Normal_1961_1990MP/PPT05.asc"),
  PPT06  = terra::rast("./inputs/Normal_1961_1990MP/PPT06.asc"),
  PPT07  = terra::rast("./inputs/Normal_1961_1990MP/PPT07.asc"),
  PPT08  = terra::rast("./inputs/Normal_1961_1990MP/PPT08.asc"),
  PPT09  = terra::rast("./inputs/Normal_1961_1990MP/PPT09.asc"),
  PPT10  = terra::rast("./inputs/Normal_1961_1990MP/PPT10.asc"),
  PPT11  = terra::rast("./inputs/Normal_1961_1990MP/PPT11.asc"),
  PPT12  = terra::rast("./inputs/Normal_1961_1990MP/PPT12.asc")
)


dem <- raster::raster("./inputs/digitalElevationModel/dem2_WNA.asc")
ext <- raster::extent(dem)
size <- 10L^6L
elevmin <- min(as.matrix(dem), na.rm = TRUE)
elevmax <- max(as.matrix(dem), na.rm = TRUE)

xy <- data.table(
  x    = runif(size, ext@xmin, ext@xmax),
  y    = runif(size, ext@ymin, ext@ymax)
)

elev <- runif(size, elevmin, elevmax)

bricky <- raster::brick(targets)

system.time({a <- raster::extract(targets[[1]], xy, method = "bilinear")})

system.time({a1 <- raster::extract(bricky, xy, method = "bilinear")})


# Does it already do what I want it to do
xcell <- (xy[5]$x - ext@xmin) %/% raster::res(dem)[1]+1
ycell <- (ext@ymax - xy[5]$y) %/% raster::res(dem)[2]+1
targets[[1]][ycell:(ycell+1L), (xcell-1L):xcell]
x <- (xy[5]$x-ext@xmin) %% raster::res(dem)[1] + raster::res(dem)[1]/2
y <- (raster::res(dem)[2]-(ext@ymax - xy[5]$y) %% raster::res(dem)[2])+ + raster::res(dem)[1]/2

x1 <- x/raster::res(dem)[1]*targets[[1]][ycell, xcell]+(raster::res(dem)[1]-x)/raster::res(dem)[1]*targets[[1]][ycell, xcell-1L]
x2 <- x/raster::res(dem)[1]*targets[[1]][ycell+1L, xcell]+(raster::res(dem)[1]-x)/raster::res(dem)[1]*targets[[1]][ycell+1L, xcell-1L]
y/raster::res(dem)[2]*x1+(raster::res(dem)[2]-y)/raster::res(dem)[2]*x2
# Yes!!!


# setClass("basepoints", slots = c(
#     latitude = "numeric",
#     longitude = "numeric",
#     elevation = "numeric",
#     historical = "character",
#     future = "character"))
# 
# setGeneric("downscale", function(latitude, longitude, elevation, historical, future) {latitude})
# 
# method.skeleton("downscale", "basepoint")
# 
# setMethod("downscale", signature = c(latitude = "numeric", longitude = "numeric", 
#                                      elevation = "numeric", historical = "character", future = "character"), 
#           function(latitude, longitude, elevation, historical, future){latitude})
# 
# setMethod("downscale",
#           signature(
#               latitude = "numeric",
#               longitude = "numeric",
#               elevation = "numeric",
#               historical = "character",
#               future = "character"
#           ),
#           function (latitude, longitude, elevation, historical, future) 
#           {
#               #Validates parameters
#               
#           }
# )
# 
# available_historical_periods <- function() {
#     
# }   
# 
# available_future_models <- function() {
#     #TODO use system.file 
#     sort(unique(gsub("^[^.]+\\.([^.]+).*$", "\\1", dir("./inputs/gcmData/"))))
# }
