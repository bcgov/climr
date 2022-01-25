library(climRpnw)
library(raster)

# lapse rate ----

dem <- raster::raster("./inputs/digitalElevationModel/dem2_WNA.asc")

# do it for all 36 variables
targets <- list(
  Tmax01 = raster::raster("./inputs/Normal_1961_1990MP/Tmax01.asc"),
  Tmax02 = raster::raster("./inputs/Normal_1961_1990MP/Tmax02.asc"),
  Tmax03 = raster::raster("./inputs/Normal_1961_1990MP/Tmax03.asc"),
  Tmax04 = raster::raster("./inputs/Normal_1961_1990MP/Tmax04.asc"),
  Tmax05 = raster::raster("./inputs/Normal_1961_1990MP/Tmax05.asc"),
  Tmax06 = raster::raster("./inputs/Normal_1961_1990MP/Tmax06.asc"),
  Tmax07 = raster::raster("./inputs/Normal_1961_1990MP/Tmax07.asc"),
  Tmax08 = raster::raster("./inputs/Normal_1961_1990MP/Tmax08.asc"),
  Tmax09 = raster::raster("./inputs/Normal_1961_1990MP/Tmax09.asc"),
  Tmax10 = raster::raster("./inputs/Normal_1961_1990MP/Tmax10.asc"),
  Tmax11 = raster::raster("./inputs/Normal_1961_1990MP/Tmax11.asc"),
  Tmax12 = raster::raster("./inputs/Normal_1961_1990MP/Tmax12.asc"),
  Tmin01 = raster::raster("./inputs/Normal_1961_1990MP/Tmin01.asc"),
  Tmin02 = raster::raster("./inputs/Normal_1961_1990MP/Tmin02.asc"),
  Tmin03 = raster::raster("./inputs/Normal_1961_1990MP/Tmin03.asc"),
  Tmin04 = raster::raster("./inputs/Normal_1961_1990MP/Tmin04.asc"),
  Tmin05 = raster::raster("./inputs/Normal_1961_1990MP/Tmin05.asc"),
  Tmin06 = raster::raster("./inputs/Normal_1961_1990MP/Tmin06.asc"),
  Tmin07 = raster::raster("./inputs/Normal_1961_1990MP/Tmin07.asc"),
  Tmin08 = raster::raster("./inputs/Normal_1961_1990MP/Tmin08.asc"),
  Tmin09 = raster::raster("./inputs/Normal_1961_1990MP/Tmin09.asc"),
  Tmin10 = raster::raster("./inputs/Normal_1961_1990MP/Tmin10.asc"),
  Tmin11 = raster::raster("./inputs/Normal_1961_1990MP/Tmin11.asc"),
  Tmin12 = raster::raster("./inputs/Normal_1961_1990MP/Tmin12.asc"),
  PPT01  = raster::raster("./inputs/Normal_1961_1990MP/PPT01.asc"),
  PPT02  = raster::raster("./inputs/Normal_1961_1990MP/PPT02.asc"),
  PPT03  = raster::raster("./inputs/Normal_1961_1990MP/PPT03.asc"),
  PPT04  = raster::raster("./inputs/Normal_1961_1990MP/PPT04.asc"),
  PPT05  = raster::raster("./inputs/Normal_1961_1990MP/PPT05.asc"),
  PPT06  = raster::raster("./inputs/Normal_1961_1990MP/PPT06.asc"),
  PPT07  = raster::raster("./inputs/Normal_1961_1990MP/PPT07.asc"),
  PPT08  = raster::raster("./inputs/Normal_1961_1990MP/PPT08.asc"),
  PPT09  = raster::raster("./inputs/Normal_1961_1990MP/PPT09.asc"),
  PPT10  = raster::raster("./inputs/Normal_1961_1990MP/PPT10.asc"),
  PPT11  = raster::raster("./inputs/Normal_1961_1990MP/PPT11.asc"),
  PPT12  = raster::raster("./inputs/Normal_1961_1990MP/PPT12.asc")
)

lapse_rates <- lapse_rate(dem, targets, use_parallel = TRUE, as_raster = TRUE)

raster::writeRaster(
  x = lapse_rates,
  filename = sprintf("./inputs/lapseRates/36layersbrick.nc"),
  overwrite = TRUE,
  options=c("INTERLEAVE=BAND","COMPRESS=LZW")
)
