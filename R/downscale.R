#' Downscale target rasters to points of interest
#' @param xyz A 3-column matrix or data.frame (x, y, z) or (lon, lat, elev).
#' @param normal Reference normal baseline climate variables rasters.
#' @param gcm Future climate variables rasters, probably from Global Circulation Models. Default to NULL.
#' @param extra_variables A character vector of extra variables to compute. Supported variables can be obtained
#' with `list_variables(FALSE)`. Definitions can be found in this package `variables` dataset.
#' Default to `character()`.
#' @param grouping A character vector of variables grouping. Can be `m` (monthly), `s` (seasonal) or `a` (annual).
#' @param use_cache A boolean. For lapse rate calculation, if `TRUE` and available,
#'  uses cached computation. If `FALSE`, recompute. Default to `TRUE`.
#' @details Couple first calls should be slower as it will cache the costly lapse rate
#' computation. After that, it should be as quick as possible.
#' @import data.table
#' @importFrom terra extract
#' @export
#' @examples
#' \dontrun{
#' xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10))
#' normal <- baseline()
#' gcm <- future(list_gcm()[3], list_ssp()[1], list_period()[2])
#' downscale(xyz, normal, gcm)
#' }
downscale <- function(xyz, normal, gcm = NULL, extra_variables = character(), grouping = c("m", "s", "a"), use_cache = TRUE) {
  
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
  # Can ignore, trying to suppress messages
  # https://github.com/rspatial/terra/issues/287
  res <- try(
    {terra::extract(x = normal, y = xyz[,1L:2L], method = "bilinear")},
    silent = TRUE
  )
  
  # Compute lapse rates and cache for same session reprocessing
  if (isTRUE(use_cache)) {
    lapse_rates <- lapse_rate_mem(normal)
  } else {
    lapse_rates <- lapse_rate(normal)
  }
  
  # Compute elevation differences between provided points elevation and normal
  elev_delta <- xyz[,3L] - try(
    {terra::extract(x = attr(normal, "dem"), y = xyz[,1L:2L], method = "simple")},
    silent = TRUE
  )[, -1L] # Remove ID column
  
  # Compute individual point lapse rate adjustments
  lr <- elev_delta * try(
    {terra::extract(x = lapse_rates, y = xyz[,1L:2L], method = "bilinear")},
    silent = TRUE
  )[,-1L]
  
  # Combine results
  res[,-1L] <- res[,-1L] + lr
  
  process_one_fut <- function(fut) {
    nm <- names(fut)
    # Extract future / gcm bilinear interpolation
    fut <- try(
      {terra::extract(x = fut, y = xyz[,1L:2L], method = "bilinear")},
      silent = TRUE
    )
    # Find matching column in baseline
    labels <- vapply(strsplit(nm, "_"), function(x) {paste0(x[2:3], collapse = "")}, character(1))
    labels <- gsub("pr", "PPT", labels)
    labels <- gsub("tas", "T", labels)
    # Add matching column to fut and return
    fut[,-1L] <- fut[,-1L] + res[,match(labels, names(res))]
    
    # Reshape (melt / dcast) to obtain final form
    ref_dt <- data.table::tstrsplit(nm, "_")
    # Recombine PERIOD
    ref_dt[[6]] <- paste(ref_dt[[6]], ref_dt[[7]], sep = "_")
    ref_dt[7] <- NULL
    # Transform labels to data.table for remerging
    data.table::setDT(ref_dt)
    data.table::setnames(ref_dt, c("GCM", "VAR", "MONTH", "SSP", "RUN", "PERIOD"))
    data.table::set(ref_dt, j = "variable", value = nm)
    data.table::set(ref_dt, j = "VAR", value = c("pr" = "PPT", "tasmin" = "Tmin", "tasmax" = "Tmax")[ref_dt[["VAR"]]])
    data.table::setkey(ref_dt, "variable")
    
    # Melt fut and set the same key for merging
    fut <- data.table::melt(data.table::setDT(fut), id.vars = "ID", variable.factor = FALSE)
    data.table::setkey(fut, "variable")
    
    # And dcast back to final form to get original 36 columns
    fut <- data.table::dcast(fut[ref_dt,], ID + GCM + SSP + RUN + PERIOD ~ VAR + MONTH, value.var = "value", sep = "")
    
    return(fut)
  }
  
  # User provided gcm
  if (!is.null(gcm)) {
    # Compute future
    res <- data.table::rbindlist(lapply(gcm, process_one_fut), use.names = TRUE)
  }
  
  # Compute extra climate variables, assign by reference
  append_calc(res, extra_variables, grouping)
  
  return(res)
  
}
