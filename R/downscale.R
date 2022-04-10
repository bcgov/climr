#' Downscale target rasters to points of interest
#' @param xyz A 3-column matrix or data.frame (x, y, z) or (lon, lat, elev).
#' @param normal Reference normal baseline input from `normal_input`.
#' @param gcm Global Circulation Models input from `gcm_input`. Default to NULL.
#' @param vars A character vector of climate variables to compute. Supported variables
#' can be obtained with `list_variables()`. Definitions can be found in this package
#' `variables` dataset. Default to monthly PPT, Tmax, Tmin.
#' @param ppt_lr A boolean. Apply lapse rate adjustment to precipitations. Default to FALSE.
#' @param nthread An integer. Number of parallel threads to use to do computations. Default to 1L.
#' @import data.table
#' @importFrom terra extract rast sources ext xres yres crop
#' @importFrom parallel makeForkCluster makePSOCKcluster stopCluster splitIndices parLapply
#' @return A downscaled dataset. If `gcm` is NULL, this is just the downscaled `normal`
#' at point locations. If `gcm` is provided, this returns a downscaled dataset for each
#' point location, general circulation model, shared socioeconomic pathway, run and period.
#' @export
#' @examples
#' \dontrun{
#' xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10))
#' normal <- normal_input()
#' gcm_input <- gcm_input(list_gcm()[3], list_ssp()[1], list_period()[2])
#' downscale(xyz, normal, gcm)
#' }
downscale <- function(xyz, normal, gcm = NULL, 
                      vars = sort(sprintf(c("PPT%02d", "Tmax%02d", "Tmin%02d"),sort(rep(1:12,3)))),
                      ppt_lr = FALSE, nthread = 1L) {
  
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
  
  if (nthread > 1) {
    # initiate cluster
    if (Sys.info()['sysname'] != "Windows") {
      cl <- parallel::makeForkCluster(nthread)
    } else {
      cl <- parallel::makePSOCKcluster(nthread)
    }
    # destroy cluster on exit
    on.exit(parallel::stopCluster(cl), add = TRUE)
    
    # Pre setting ID for recycling later
    xyz[,4L] <- seq_len(nrow(xyz))
    # Reordering on y axis for smaller cropped area and faster
    # sequential reads
    xyz <- xyz[order(xyz[,2L]),]
    # Split before parallel processing
    xyz <- lapply(
      parallel::splitIndices(nrow(xyz), length(cl)),
      function(x) {xyz[x,]}
    )
    
    threaded_downscale_ <- function(xyz, normal_path, gcm_paths, vars, ppt_lr) {

      # Set DT threads to 1 in parallel to avoid overloading CPU
      # Not needed for forking, not taking any chances
      dt_nt <- data.table::getDTthreads()
      data.table::setDTthreads(1)
      on.exit(data.table::setDTthreads(dt_nt))
      
      # Reload SpatRaster pointers
      normal <- terra::rast(normal_path)
      gcm <- lapply(gcm_paths, function(x) {
        terra::rast(x[["source"]], lyrs = x[["lyrs"]])
      })
      
      # Downscale
      res <- downscale_(xyz, normal, gcm, vars, ppt_lr)
      
      return(res)
    }
    
    # Parallel processing and recombine
    res <- data.table::rbindlist(
      parallel::parLapply(
        cl = cl,
        X = xyz,
        fun = threaded_downscale_,
        normal_path = terra::sources(normal),
        gcm_paths = lapply(gcm, function(x) {
          s <- terra::sources(x, bands = TRUE)
          list(source = unique(s[["source"]]), lyrs = s[["bands"]])
        }),
        vars = vars,
        ppt_lr = ppt_lr
      ),
      use.names = TRUE
    )
    
  } else {
    
    # Downscale without parallel processing
    res <- downscale_(xyz, normal, gcm, vars, ppt_lr)
    
  }

  data.table::setkey(res, "ID")
  return(res)
  
}

#' Simple downscale
#' @noRd
downscale_ <- function(xyzID, normal, gcm, vars, ppt_lr) {
  
  # Define normal extent
  ex <- terra::ext(
    c(
      min(xyzID[,1L]) - terra::xres(normal)*2,
      max(xyzID[,1L]) + terra::xres(normal)*2,
      min(xyzID[,2L]) - terra::yres(normal)*2,
      max(xyzID[,2L]) + terra::yres(normal)*2
    )
  )
  
  # crop normal raster (while also loading it to memory)
  normal <- terra::crop(normal, ex, snap = "out")
  
  # Normal value extraction
  # possible garbage output :
  # Error in (function (x)  : attempt to apply non-function
  # Error in x$.self$finalize() : attempt to apply non-function
  # Can ignore, trying to suppress messages with `shush`
  # https://github.com/rspatial/terra/issues/287
  
  # stack before extracting
  res <- shush(
    terra::extract(
      x = normal,
      y = xyzID[,1L:2L],
      method = "bilinear"
    )
  )

  # Compute elevation differences between provided points elevation and normal
  # Dem at position 74 (ID column + 36 normal layers + 36 lapse rate layers + 1 dem layer)
  elev_delta <- xyzID[,3L] - res[, 74L]
  
  # Compute individual point lapse rate adjustments
  # Lapse rate position 38:73 (ID column + 36 normal layers + 36 lapse rate layers)
  lr <- elev_delta * res[, 38L:73L]
  
  # Replace any NAs left with 0s
  lr[is.na(lr)] <- 0L
  
  # Remove lapse rates and digital elevation model from res
  res[,38L:74L] <- NULL
  
  # Combine results (ignoring ID column)
  if (isTRUE(ppt_lr)) {
    res[,-1L] <- res[,-1L] + lr
  } else {
    ppt <- grep("^PPT", names(normal)[1L:36L], invert = TRUE)
    res[, ppt + 1L] <- res[, ppt + 1L] + lr[, ppt]
  }
  
  # Process one GCM stacked layers
  process_one_gcm <- function(gcm_, res, xyzID) {
    
    # Store names for later use
    nm <- names(gcm_)
    
    # Define gcm extent. res*2 To make sure we capture surrounding
    # cells for bilinear interpolation.
    ex <- terra::ext(
      c(
        min(xyzID[,1L]) - terra::xres(gcm_)*2,
        max(xyzID[,1L]) + terra::xres(gcm_)*2,
        min(xyzID[,2L]) - terra::yres(gcm_)*2,
        max(xyzID[,2L]) + terra::yres(gcm_)*2
      )
    )
    # Extract gcm bilinear interpolations
    # Cropping will reduce the size of data to load in memory
    gcm_ <- terra::crop(gcm_, ex, snap = "out")
    gcm_ <- shush(terra::extract(x = gcm_, y = xyzID[,1L:2L], method = "bilinear"))
    
    # Create match set to match with res names
    labels <- vapply(
      strsplit(nm, "_"),
      function(x) {paste0(x[2:3], collapse = "")},
      character(1)
    )
    
    # Add matching column to gcm_
    gcm_[,-1L] <- gcm_[,-1L] + res[,match(labels, names(res))]
    
    # Set Latitude and possibly ID
    gcm_[["Lat"]] <- xyzID[,2L]
    if (ncol(xyzID) == 4L) {
      gcm_[["ID"]] <- xyzID[, 4L]
    }
    
    # Find groupset (GCM_CLIMATEVAR_MONTH_{GROUPSET})
    groupset <- sort(unique(gsub("^[^_]+_[^_]+_[^_]+_(.*)$", "\\1", nm)))
    
    # Find groupset columns indices
    col_indices <- lapply(
      groupset,
      function(gs) {
        c(1, grep(gs, x = names(gcm_), fixed = TRUE), ncol(gcm_))
      }
    )
    
    # Rename columns to climate variables names
    names(gcm_) <- gsub("^(ID|Lat)$|^[^_]+_([^_]+)_([^_]+)_(.*)$", "\\1\\2\\3", names(gcm_))
    
    # Extract GCM label
    gcm_label <- gsub(".", "-", gsub("^([^_]+)_.*$", "\\1", nm[1]), fixed = TRUE)
    
    # Recombine extract by stacking groupset
    gcm_ <- data.table::rbindlist(
      mapply(
        function(cinx, labels) {
          dt <- data.table::setDT(gcm_[,cinx])
          
          data.table::set(
            dt,
            j = c("GCM", "SSP", "RUN", "PERIOD"),
            value = list(
              gcm_label,
              labels[1],
              labels[2],
              sprintf("%s_%s", labels[3], labels[4])
            )
          )
          
          dt
        },
        col_indices,
        strsplit(groupset, "_"),
        SIMPLIFY = FALSE
      ),
      use.names = TRUE
    )
    
    return(gcm_)
  }
  
  # In case user provided some gcm
  if (!is.null(gcm)) {
    # Process each gcm and rbind resulting tables
    res <- data.table::rbindlist(
      lapply(gcm, process_one_gcm, res = res, xyzID = xyzID),
      use.names = TRUE
    )
  } else {
    # Set Latitude and possibly ID
    res[["Lat"]] <- xyzID[,2L]
    if (ncol(xyzID) == 4L) {
      res[["ID"]] <- xyzID[, 4L]
    }
    data.table::setDT(res)
  }
  
  # Compute extra climate variables, assign by reference
  append_clim_vars(res, vars)
  
  return(res)
  
}
