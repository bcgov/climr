#' Downscale target rasters to points of interest
#' @param xyz A 3-column matrix or data.frame (x, y, z) or (lon, lat, elev).
#' @param normal Reference normal baseline climate variables rasters.
#' @param gcm Future climate variables rasters, probably from Global Circulation Models. Default to NULL.
#' @param variables A character vector of extra variables to compute.
#' @param grouping A character vector of variables grouping. Can be `m` (monthly), `s` (seasonal) or `a` (annual).
#' @param use_cache A boolean. For lapse rate calculation, if `TRUE` and available,
#'  uses cached computation. If `FALSE`, recompute. Default to `TRUE`.
#' @examples
#' \dontrun{
#' xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10))
#' normal <- baseline()
#' gcm <- future(list_gcm()[3], list_ssp()[1], list_period()[2])
#' downscale(xyz, normal, gcm)
#' }
downscale <- function(xyz, normal, gcm = NULL, variables, grouping = c("m", "s", "a"), use_cache = TRUE) {
  
  # Make sure normal was built using baseline
  if (!isTRUE(attr(normal, "builder") == "climRpnw")) {
    stop("Please use this package `baseline` function to create `normal`. See `?baseline` for details.")
  }
  
  # Make sure gcm was built using future
  if (!is.null(gcm) && !isTRUE(attr(gcm, "builder") == "climRpnw")) {
    stop("Please use this package `future` function to create `gcm`. See `?future` for details.")
  }
  
  # Baseline value extraction
  # possible garbage output :
  # Error in (function (x)  : attempt to apply non-function
  # Error in x$.self$finalize() : attempt to apply non-function
  # Can ignore, suppressing messages for now
  # https://github.com/rspatial/terra/issues/287
  res <- suppressMessages(
    {terra::extract(x = normal, y = xyz[,1L:2L], method = "bilinear")},
    classes = c("messages", "condition")
  )
  
  # Compute lapse rates and cache for same session reprocessing
  if (isTRUE(use_cache)) {
    lapse_rates <- lapse_rate_mem(normal)  
  } else {
    lapse_rates <- lapse_rate(normal)  
  }
  
  # Compute elevation differences between provided points elevation and normal
  elev_delta <- xyz[,3L] - terra::extract(x = attr(normal, "dem"), y = xyz[,1L:2L], method = "simple")
  
  # Compute individual point lapse rate adjustments
  lr <- elev_delta * suppressMessages(
    {terra::extract(x = lapse_rates, y = xyz[,1L:2L], method = "bilinear")},
    classes = c("messages", "condition")
  )
  
  # Combine results
  res <- res + lr + future
  
  # Compute climate variables  
  
}
