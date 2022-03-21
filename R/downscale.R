#' Downscale target rasters to points of interest
#' @param xyz A 3-column matrix or data.frame (x, y, z) or (lon, lat, elev).
#' @param historical Reference historical climate variables rasters.
#' @param future Future climate variables rasters. Default to NULL.
#' @param variables A character vector of variables to compute.
#' @param grouping A character vector of variables grouping. Can be `m` (monthly), `s` (seasonal) or `a` (annual).
downscale <- function(xyz, historical, future = NULL, variables, grouping = c("m", "s", "a")) {
  
  # Historical layer value extraction
  res <- terra::extract(x = historical, y = xyz[,1L:2L], method = "bilinear")
  
  # Compute lapse rates and cache for same session reprocessing
  lapse_rates <- lapse_rate(historical)
  
  # Compute elevation differences between provided points elevation and historical
  elev_delta <- xyz[,3L] - terra::extract(x = attr(historical, "dem"), y = xyz[,1L:2L], method = "simple")
  
  # Compute individual point lapse rate adjustments
  lr <- elev_delta * terra::extract(x = lapse_rates, y = xyz[,1L:2L], method = "bilinear")
  
  # Compute future differences between ...
  if (!is.null(future)) {
    future <- 0L
  }

  # Combine results
  res <- res + lr + future
  
  # Compute climate variables  
  
}





# Methods to retrieve available GCM
# Cache locally, download from github


# raster selection method that returns rasters

# library(data.table)
# system.file("inputs/", package = "climR-pnw")
