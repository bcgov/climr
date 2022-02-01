downscale <- function(object, xy, elev = NULL, lapse_rates = NULL, extra = TRUE) {
  
  res <- terra::extract(object, xy, method = "bilinear")
  
  # Lapse rates adjustment
  if (!is.null(lapse_rates)) {
    elev_delta <- elev - terra::extract(attr(lapse_rates, "dem"), xy, method = "simple")
    lr <- elev_delta * terra::extract(lapse_rates, xy)
    res+lr    
  }

  if (isTRUE(extra)) {
    # Compute derivatives (data.table?)  
  }
  
}

# Add additional methods for non terra objects