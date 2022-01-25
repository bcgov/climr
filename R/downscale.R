downscale <- function(object, xy, dem, elev = NULL, lapse_rates = TRUE, extra = TRUE, dem = TRUE) {
  res <- raster::extract(object, xy, method = "bilinear")
  elev_delta <- elev - raster::extract(dem, xy, method = "simple")
  lr <- elev_delta * raster::extract(lapse_rates, xy)
  res+lr
  # Compute derivatives
}
  