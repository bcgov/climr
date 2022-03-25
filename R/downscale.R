#' Downscale target rasters to points of interest
#' @param xyz A 3-column matrix or data.frame (x, y, z) or (lon, lat, elev).
#' @param normal Reference normal baseline input from `normal_input`.
#' @param gcm Global Circulation Models input from `gcm_input`. Default to NULL.
#' @param vars A character vector of climate variables to compute. Supported variables
#' can be obtained with `list_variables()`. Definitions can be found in this package
#' `variables` dataset. Default to monthly PPT, Tmax, Tmin.
#' @import data.table
#' @importFrom terra extract
#' @export
#' @examples
#' \dontrun{
#' xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10))
#' normal <- normal_input()
#' gcm_input <- gcm_input(list_gcm()[3], list_ssp()[1], list_period()[2])
#' downscale(xyz, normal, gcm)
#' }
downscale <- function(xyz, normal, gcm = NULL, 
                      vars = sprintf(c("PPT%02d", "Tmax%02d", "Tmin%02d"),sort(rep(1:12,3)))) {
  
  # Make sure normal was built using normal_input
  if (!isTRUE(attr(normal, "builder") == "climRpnw")) {
    stop(
      "Please use this package `normal_input` function to create `normal`.",
      " See `?normal_input` for details."
    )
  }
  
  # Make sure gcm was built using gcm_input
  if (!is.null(gcm) && !isTRUE(attr(gcm, "builder") == "climRpnw")) {
    stop(
      "Please use this package `gcm_input` function to create `gcm`.",
      " See `?gcm_input` for details."
    )
  }
  
  # Normal value extraction
  # possible garbage output :
  # Error in (function (x)  : attempt to apply non-function
  # Error in x$.self$finalize() : attempt to apply non-function
  # Can ignore, trying to suppress messages with `shush`
  # https://github.com/rspatial/terra/issues/287
  res <- shush(terra::extract(x = normal, y = xyz[,1L:2L], method = "bilinear"))
  
  # Compute elevation differences between provided points elevation and normal
  elev_delta <- xyz[,3L] - shush(
      terra::extract(x = attr(normal, "dem"), y = xyz[,1L:2L], method = "simple")
  )[,-1L] # Remove ID column
  
  # Compute individual point lapse rate adjustments
  lr <- elev_delta * shush(
    terra::extract(x = attr(normal, "lr"), y = xyz[,1L:2L], method = "bilinear")
  )[,-1L] # Remove ID column
  
  # Combine results (ignoring ID column)
  res[,-1L] <- res[,-1L] + lr
  
  # Process one GCM stacked layers
  process_one_gcm <- function(gcm_, res, xyz) {
    # Store names for later use
    nm <- names(gcm_)
    # Extract gcm bilinear interpolations
    gcm_ <- shush(terra::extract(x = gcm_, y = xyz[,1L:2L], method = "bilinear"))
    # Create match set to match with res names
    labels <- vapply(
      strsplit(nm, "_"),
      function(x) {paste0(x[2:3], collapse = "")},
      character(1)
    )
    labels <- gsub("pr", "PPT", labels)
    labels <- gsub("tas", "T", labels)
    # Add matching column to gcm_
    gcm_[,-1L] <- gcm_[,-1L] + res[,match(labels, names(res))]
    
    # Reshape (melt / dcast) to obtain final form
    ref_dt <- data.table::tstrsplit(nm, "_")
    # Recombine PERIOD into one field
    ref_dt[[6]] <- paste(ref_dt[[6]], ref_dt[[7]], sep = "_")
    ref_dt[7] <- NULL
    # Transform ref_dt to data.table for remerging
    data.table::setDT(ref_dt)
    data.table::setnames(ref_dt, c("GCM", "VAR", "MONTH", "SSP", "RUN", "PERIOD"))
    data.table::set(ref_dt, j = "variable", value = nm)
    data.table::set(
      ref_dt,
      j = "VAR",
      # This is a quick trick to replace multiple elements in a character vector
      # You can test it with c("a" = 2, "b" = 3)[c("b", "b")]
      value = c("pr" = "PPT", "tasmin" = "Tmin", "tasmax" = "Tmax")[ref_dt[["VAR"]]]
    )
    data.table::setkey(ref_dt, "variable")
    
    # Melt gcm_ and set the same key for merging
    gcm_ <- data.table::melt(
      data.table::setDT(gcm_),
      id.vars = "ID",
      variable.factor = FALSE
    )
    data.table::setkey(gcm_, "variable")
    
    # Finally, dcast back to final form to get original 36 columns
    gcm_ <- data.table::dcast(
      # The merge with shared keys is as simple as that
      gcm_[ref_dt,],
      ID + GCM + SSP + RUN + PERIOD ~ VAR + MONTH,
      value.var = "value",
      sep = ""
    )
    
    return(gcm_)
  }
  
  # In case user provided some gcm
  if (!is.null(gcm)) {
    # Process each gcm and rbind resulting tables
    res <- data.table::rbindlist(
      lapply(gcm, process_one_gcm, res = res, xyz = xyz),
      use.names = TRUE
    )
  }
  
  # Compute extra climate variables, assign by reference
  append_clim_vars(res, vars)
  
  return(res)
  
}
