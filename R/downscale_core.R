#' Change-factor downscaling of user-supplied climate data
#'
#' @description
#' `downscale_core()` is the engine for [`downscale()`].
#' It takes user-supplied high- and low-resolution rasters as input and downscales to user-specified point locations.
#' While less user-friendly than [`downscale()`], `downscale_core()` is more flexible in that users can supply their 
#' own raster inputs. For example, a user could supply their own high-resolution climate map, instead of what is 
#' available in climr, as the input to `refmap`. Another example is in downscaling a uniform warming level, as shown 
#' in the example for this function. 
#'
#' @details
#' We recommend [`downscale()`] for most purposes.
#'
#' @template xyz
#' @param refmap `SpatRaster`. Outputs from [`input_refmap()`]. The high-resolution
#'   climate maps for use as the downscaling baseline.
#' @param gcms `list` of `SpatRasters`. Outputs from [`input_gcms()`]. Global
#'   climate model data for 20-year reference periods to be downscaled. Default to `NULL`.
#' @param obs `list` of `SpatRasters`. Outputs from [`input_obs()`].
#'   Observed climate data for 20-year reference periods to be downscaled. Default to `NULL`.
#' @param gcm_ssp_ts `list` of `SpatRasters`. Outputs from [`input_gcm_ssp()`].
#'   Global climate model time series for ssps-rcp scenarios to be downscaled. Default to `NULL`.
#' @param gcm_hist_ts `list` of `SpatRasters`. Outputs from [`input_gcm_hist()`].
#'   Global climate model time series for historical scenario to be downscaled. Default to `NULL`.
#' @param obs_ts `list` of `SpatRasters`. Outputs from [`input_obs_ts()`].
#'   Observed climate time series to be downscaled. Default to `NULL`.
#' @param return_refperiod logical. Return downscaled reference period (1961-1990)? Default `TRUE`.
#' @template vars
#' @param ppt_lr logical. Apply elevation adjustment to precipitation. Default to FALSE.
#' @param nthread integer. Number of parallel threads to use to do computations. Default to 1L.
#' @param out_spatial logical. Should a SpatVector be returned instead of a
#'   `data.frame`.
#' @param plot character. If `out_spatial` is TRUE, the name of a variable to plot.
#'   If the variable exists in `reference`, then its reference values will also be plotted. 
#'   Otherwise, reference January total precipitation (PPT01) values will be plotted.
#'   Defaults to no plotting (NULL).
#'
#' @import data.table
#' @importFrom terra extract rast sources ext xres yres crop plot as.polygons setValues
#' @importFrom grDevices hcl.colors palette
#' @importFrom stats complete.cases
#'
#' @return A `data.table` or SpatVector with downscaled climate variables. If `gcms` is NULL,
#'   this is just the downscaled `reference` at point locations. If `gcms` is provided,
#'   this returns a downscaled dataset for each point location, general circulation
#'   model (GCM), shared socioeconomic pathway (SSP), run and period.
#'
#' @seealso [`input_gcms()`], [`input_obs()`], [`list_vars()`]
#'
#' @export
#' @examples
#' ## 
#' library(terra)
#' xyz <- data.frame(lon = runif(10, -130, -106), lat = runif(10, 37, 50), elev = runif(10), id = 1:10)
#'
#' ## get bounding box based on input points
#' thebb <- get_bb(xyz)
#' 
#' ## get database connection
#' dbCon <- data_connect()
#' 
#' # obtain the climatena 1961-1990 normals for the study area. 
#' refmap <- input_refmap(dbCon, thebb, reference = "refmap_climatena")
#' 
#' # obtain the low-resolution climate data for a single gcm, 20-year period, and ssp scenario. 
#' gcm_raw <- input_gcms(dbCon, thebb, list_gcms()[3], list_ssps()[1], period = list_gcm_periods()[2])
#' 
#' # downscale the GCM data
#' gcm_downscaled <- downscale_core(xyz = xyz, refmap = refmap, gcms = gcm_raw, vars = c("MAT", "PAS"))
#' 
#' # create an input of uniform warming of 2 degrees Celsius and no precipitation change, for use as a null comparison to the GCM warming
#' null <- gcm_raw #' use the gcm input object as a template
#' names(null) <- "null_2C"
#' names(null[[1]]) <-  sapply(strsplit(names(null[[1]]), "_"), function(x) paste("null2C", x[2], x[3], "NA", "NA", "NA", "NA", sep="_"))
#' for(var in names(null[[1]])){ values(null[[1]][[var]]) <- if(length(grep("PPT", var)==1)) 1 else 2 } #' repopulate with the null values
#' 
#' # downscale the null values for variables of interest
#' null_downscaled <- downscale_core(xyz = xyz, refmap = refmap, gcms = null, vars = c("MAT", "PAS"))
#' pool::poolClose(dbCon)
#' 
downscale_core <- function(xyz, refmap, gcms = NULL, obs = NULL, gcm_ssp_ts = NULL,
                      gcm_hist_ts = NULL, obs_ts = NULL, return_refperiod = TRUE,
                      vars = sort(sprintf(c("PPT_%02d", "Tmax_%02d", "Tmin_%02d"), sort(rep(1:12, 3)))),
                      ppt_lr = FALSE, nthread = 1L, out_spatial = FALSE, plot = NULL) {
  ## checks
  .checkDwnsclArgs(
    xyz, refmap, gcms, obs, gcm_ssp_ts, gcm_hist_ts,
    obs_ts, return_refperiod, out_spatial, plot, vars
  )

  expectedCols <- c("lon", "lat", "elev", "id")
  xyz <- .checkXYZ(copy(xyz), expectedCols)
  get_bb(xyz) # we don't need a bbox, but this the projection of xyz

  if (isTRUE(nthread > 1L)) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      message("nthreads is >1, but 'parallel' package is not available.")
      message("Setting nthreads to 1 and running computations in sequential mode.")
      message("If you wish to parallelise please run install.packages('parallel')")
      nthread <- 1L
    }
  }

  if (isTRUE(nthread > 1L)) {
    message("Parallelising downscaling computations across ", nthread, " threads")

    # initiate cluster
    if (Sys.info()["sysname"] != "Windows") {
      cl <- parallel::makeForkCluster(nthread)
    } else {
      cl <- parallel::makePSOCKcluster(nthread)
    }
    # destroy cluster on exit
    on.exit(parallel::stopCluster(cl), add = TRUE)

    # Reordering on y axis for smaller cropped area and faster
    # sequential reads
    xyz <- xyz[order(lat), ]

    # Split before parallel processing
    xyz <- lapply(
      parallel::splitIndices(nrow(xyz), length(cl)),
      function(x) {
        xyz[x, ]
      }
    )

    # Parallel processing and recombine

    ## pack rasters for parallelisation
    refmap <- packRasters(refmap)
    gcms <- packRasters(gcms)
    gcm_ssp_ts <- packRasters(gcm_ssp_ts)
    gcm_hist_ts <- packRasters(gcm_hist_ts)
    obs <- packRasters(obs)
    obs_ts <- packRasters(obs_ts)

    ## workaround to export function to nodes
    unpackRasters <- unpackRasters
    parallel::clusterExport(cl, c("unpackRasters"), envir = environment())

    res <- rbindlist(
      parallel::parLapply(
        cl = cl,
        X = xyz,
        fun = threaded_downscale_,
        # lapply(xyz,  ## testing
        # FUN = threaded_downscale_,
        refmap = refmap,
        gcms = gcms,
        gcm_ssp_ts = gcm_ssp_ts,
        gcm_hist_ts = gcm_hist_ts,
        obs = obs,
        obs_ts = obs_ts,
        return_refperiod = return_refperiod,
        vars = vars,
        ppt_lr = ppt_lr
      ),
      use.names = TRUE
    )
  } else {
    # Downscale without parallel processing
    res <- downscale_(
      xyz,
      refmap,
      gcms,
      gcm_ssp_ts,
      gcm_hist_ts,
      obs,
      obs_ts,
      return_refperiod,
      vars, ppt_lr
    )
  }

  IDcols <- names(res)[!names(res) %in% vars]
  setkeyv(res, IDcols)
  if (out_spatial) {
    res <- as.data.table(xyz)[res, on = "id"]

    res <- vect(res, geom = c("lon", "lat"), crs = crs(refmap, proj = TRUE))

    if (!is.null(plot)) {
      if (!plot %in% vars) {
        stop("The variable you wish to plot was not downscaled. Please pass a variable listed in 'vars'")
      }
      ## make a mask of the normals data "extent"
      msk <- refmap[[1]]
      msk[!is.na(msk[])] <- 1
      msk <- as.polygons(msk)

      ## round values
      res2 <- res
      res2[[plot]] <- round(res2[[plot]], 4)

      ## make table of available runs
      cols <- c("GCM", "SSP", "RUN", "PERIOD")
      cols <- cols[cols %in% names(res2)]
      uniqueCombos <- unique(as.data.table(res2)[, ..cols])
      uniqueCombos <- uniqueCombos[complete.cases(uniqueCombos)]

      if (nrow(uniqueCombos) > 1) {
        message("Plotting results for a single period/GCM/run/SSP")
        uniqueCombos <- uniqueCombos[1]

        res2 <- as.data.table(res2, geom = "XY")
        res2 <- res2[uniqueCombos, on = names(uniqueCombos)]
        res2 <- vect(res2, geom = c("x", "y"), crs = crs(res, proj = TRUE))
      }

      plotTitle <- paste(paste(names(uniqueCombos), uniqueCombos, sep = ": "), collapse = "; ")
      plotTitle <- paste0(plot, "\n", plotTitle)
      plot(msk, col = "grey", main = plotTitle, legend = FALSE, mar = c(3.1, 3.1, 3.1, 7.1))
      plot(res2,
        y = plot, axes = FALSE, ext = ext(msk),
        col = hcl.colors(100, "viridis"), sort = TRUE,
        add = TRUE, type = "continuous"
      )
    }
  }
  return(res)
}

#' Simple downscale
#'
#' @noRd
#' @inheritParams downscale
#' @template xyz
#'
#' @return A `data.table`.
#'
#' @import data.table
#' @importFrom terra crop ext xres yres extract
#' @noRd
downscale_ <- function(xyz, refmap, gcms, gcm_ssp_ts, gcm_hist_ts,
                       obs, obs_ts, return_refperiod,
                       vars, ppt_lr = FALSE) {
  # print(xyz)
  # Define reference extent
  ex <- ext(
    c(
      min(xyz[["lon"]]) - xres(refmap) * 2,
      max(xyz[["lon"]]) + xres(refmap) * 2,
      min(xyz[["lat"]]) - yres(refmap) * 2,
      max(xyz[["lat"]]) + yres(refmap) * 2
    )
  )

  # crop refmap raster (while also loading it to memory)
  refmap <- crop(refmap, ex, snap = "out")

  # Normal value extraction
  # possible garbage output :
  # Error in (function (x)  : attempt to apply non-function
  # Error in x$.self$finalize() : attempt to apply non-function
  # Can ignore, trying to suppress messages with `shush`
  # https://github.com/rspatial/terra/issues/287

  # stack before extracting
  res <-
    extract(
      x = refmap,
      y = xyz[, .(lon, lat)],
      method = "bilinear"
    )

  # Compute elevation differences between provided points elevation and reference
  # Dem at position 74 (ID column + 36 reference layers + 36 lapse rate layers + 1 dem layer)
  elev_delta <- xyz[["elev"]] - res[, "dem2_WNA"]
  # print(elev_delta)
  # print(res)
  # Compute individual point lapse rate adjustments
  # Lapse rate position 38:73 (ID column + 36 reference layers + 36 lapse rate layers)
  lrCols <- grep("^lr_", names(res), value = TRUE)
  lr <- elev_delta * res[, lrCols] ## do we need anything other than the lapse rate?

  # Replace any NAs left with 0s
  lr[is.na(lr)] <- 0L

  # Remove lapse rates and digital elevation model from res
  lrDemCols <- grep("^lr_|dem2_WNA", names(res), value = TRUE)
  if (length(lrCols) == length(lrDemCols)) {
    stop("Error 01: can't find DEM layer. Please contact developer and supply error code")
  }

  res[, lrDemCols] <- NULL

  # Combine results (ignoring ID column)
  res <- as.data.frame(res) ## TODO: convert code below to data.table
  if (isTRUE(ppt_lr)) {
    notIDcols <- names(res)[which(tolower(names(res)) != "id")]

    if (any(paste0("lr_", notIDcols) != names(lr))) {
      stop(
        "Error 02: lapse rates and downscale output column names do not match.",
        "\n   Please contact developers."
      )
    }

    res[, notIDcols] <- res[, notIDcols] + lr
  } else {
    notpptLRDEM <- grep("^PPT|ID|^lr_|dem2_WNA", names(res), invert = TRUE, value = TRUE)
    lr_notpptLRDEM <- grep("^lr_PPT", names(lr), invert = TRUE, value = TRUE)
    if (any(paste0("lr_", notpptLRDEM) != lr_notpptLRDEM)) {
      stop(
        "Error 02: lapse rates and downscale output column names do not match.",
        "\n   Please contact developers."
      )
    }
    res[, notpptLRDEM] <- res[, notpptLRDEM] + lr[, lr_notpptLRDEM]
  }
  res <- as.data.table(res)

  # Process one GCM stacked layers
  if (!is.null(gcms)) {
    # Process each gcms and rbind resulting tables
    res_gcm <- rbindlist(
      lapply(gcms, process_one_climaterast, res = res, xyz = xyz, type = "gcms"),
      use.names = TRUE
    )
  } else {
    res_gcm <- NULL
  }
  if (!is.null(gcm_ssp_ts)) {
    # Process each gcms and rbind resulting tables
    res_gcmts <- rbindlist(
      lapply(gcm_ssp_ts, process_one_climaterast, res = res, xyz = xyz, timeseries = TRUE, type = "gcms"),
      use.names = TRUE
    )
  } else {
    res_gcmts <- NULL
  }
  if (!is.null(gcm_hist_ts)) {
    # Process each gcms and rbind resulting tables
    res_gcm_hist <- rbindlist(
      lapply(gcm_hist_ts, process_one_climaterast, res = res, xyz = xyz, type = "gcm_hist_ts"),
      use.names = TRUE
    )
  } else {
    res_gcm_hist <- NULL
  }
  if (!is.null(obs)) {
    # print(obs)
    res_hist <- rbindlist(
      lapply(obs, process_one_climaterast, res = res, xyz = xyz, type = "obs"),
      use.names = TRUE
    )
  } else {
    res_hist <- NULL
  }
  if (!is.null(obs_ts)) {
    # print(obs)
    res_hist_ts <- rbindlist(
      lapply(obs_ts, process_one_climaterast, res = res, xyz = xyz, timeseries = TRUE, type = "obs_ts"),
      use.names = TRUE
    )
  } else {
    res_hist_ts <- NULL
  }

  if (return_refperiod) {
    nm <- names(res)[-1]
    labels <- nm
    normal_ <- res
    # Reshape (melt / dcast) to obtain final form
    #ref_dt <- tstrsplit(nm, "_")
    ref_dt <- data.table(VAR = nm)
    # setDT(ref_dt)
    # setnames(ref_dt, c("VAR"))
    set(ref_dt, j = "variable", value = nm)
    set(ref_dt, j = "PERIOD", value = "1961_1990")
    setkey(ref_dt, "variable")
    # Set Latitude elevation and ID
    normal_[["lat"]] <- xyz[["lat"]]
    normal_[["elev"]] <- xyz[["elev"]]
    normal_[["id"]] <- xyz[["id"]]

    # Melt gcm_ and set the same key for merging
    normal_ <- melt(
      setDT(normal_),
      id.vars = c("id", "lat", "elev"),
      variable.factor = FALSE
    )
    setkey(normal_, "variable")

    # Finally, dcast back to final form to get original 36 columns
    normal_ <- dcast(
      # The merge with shared keys is as simple as that
      normal_[ref_dt, ],
      id + PERIOD + lat + elev ~ VAR,
      value.var = "value",
      sep = ""
    )
  } else {
    normal_ <- NULL
  }
  res <- rbind(res_gcm, res_gcmts, res_gcm_hist, res_hist, res_hist_ts, normal_, use.names = TRUE, fill = TRUE)
  # print(names(res))
  # Compute extra climate variables, assign by reference
  append_clim_vars(res, vars)

  return(res)
}


#' Wrapper function for `downscale_` for parallelising
#'
#' @inheritParams downscale
#' @param ... further arguments passed to `downscale_`
#'
#' @return A `data.table`
#'
#' @importFrom data.table getDTthreads setDTthreads
#' @importFrom terra rast
#' @noRd
threaded_downscale_ <- function(xyz, refmap, gcms, gcm_ssp_ts, gcm_hist_ts, obs, obs_ts, ...) {
  ## unpack rasters
  refmap <- unpackRasters(refmap)
  gcms <- unpackRasters(gcms)
  gcm_ssp_ts <- unpackRasters(gcm_ssp_ts)
  gcm_hist_ts <- unpackRasters(gcm_hist_ts)
  obs <- unpackRasters(obs)
  obs_ts <- unpackRasters(obs_ts)

  # Set DT threads to 1 in parallel to avoid overloading CPU
  # Not needed for forking, not taking any chances
  dt_nt <- getDTthreads()
  setDTthreads(1)
  on.exit(setDTthreads(dt_nt))

  # Downscale
  res <- downscale_(
    xyz = xyz, refmap = refmap, gcms = gcms,
    gcm_ssp_ts = gcm_ssp_ts, gcm_hist_ts = gcm_hist_ts,
    obs = obs, obs_ts = obs_ts, ...
  )
  return(res)
}

#' TODO: fill documentation here
#'
#' @param climaterast TODO
#' @param res TODO
#' @template xyz
#' @param timeseries TODO
#' @param type TODO
#'
#' @return a `data.table`
#' @noRd
#' @importFrom stats as.formula
process_one_climaterast <- function(climaterast, res, xyz, timeseries = FALSE,
                                    type = c("gcms", "gcm_hist_ts", "obs", "obs_ts")) {
  type <- match.arg(type)

  # Store names for later use
  nm <- names(climaterast)

  # Define gcms extent. res*2 To make sure we capture surrounding
  # cells for bilinear interpolation.
  ex <- ext(
    c(
      min(xyz[["lon"]]) - xres(climaterast) * 2,
      max(xyz[["lon"]]) + xres(climaterast) * 2,
      min(xyz[["lat"]]) - yres(climaterast) * 2,
      max(xyz[["lat"]]) + yres(climaterast) * 2
    )
  )
  # Extract gcms bilinear interpolations
  # Cropping will reduce the size of data to load in memory

  climaterast <- crop(climaterast, ex, snap = "out")
  gc(reset = TRUE)  ## free unused memory
  
  climaterast <- try(extract(x = climaterast, y = xyz[, .(lon, lat)], method = "bilinear"))
  
  ## we may have run out of memory if there are MANY rasters
  ## attempt to get only unique raster cell values 
  ## (i.e. xyz may be at higher res than the climaterast leading to extracting the same values many times)
  if (is(climaterast, "try-error")) {
    if (grepl("bad_alloc", climaterast)) {
      message("System is out of memory to extract climate values for the supplied coordinates")
        stop("Insufficient memory to downscale climate data for these many points/climate layers.\n",
             "  Try reducing number of points/layers.")
      }
  } 
  
  # else { Ceres not sure what this is for but it's always causing fails
  #     stop("Climate value extraction failed.",
  #          "\n   Please contact developers with a reproducible example and the error:\n",
  #          climaterast) 
  #   }
  
  # Create match set to match with res names
  

    labels <- vapply(
      strsplit(nm, "_"),
      \(x) {
        paste0(x[2:3], collapse = "_")
      },
      character(1)
    )
  
  if (type %in% c("obs")) {
    ## Create match set to match with res names
    labels <- nm
  }

  # Add matching column to climaterast
  res <- as.data.frame(res)
  ppt_ <- grep("PPT", labels)
  ppt_next <- ppt_ + 1L
  climaterast[, ppt_next] <- climaterast[, ppt_next] * res[, match(labels[ppt_], names(res))] ## PPT
  climaterast[, -c(1L, ppt_next)] <- climaterast[, -c(1L, ppt_next)] + res[, match(labels[-ppt_], names(res))] ## Temperature
  res <- as.data.table(res)
  climaterast <- as.data.table(climaterast)

  # Reshape (melt / dcast) to obtain final form
  ref_dt <- tstrsplit(nm, "_")

  # Recombine PERIOD into one field
  if (!timeseries & type == "gcms") {
    ref_dt[[6]] <- paste(ref_dt[[6]], ref_dt[[7]], sep = "_")
    ref_dt[7] <- NULL
  }

  setDT(ref_dt)
  if (type %in% c("obs","obs_ts")) {
    if (timeseries) {
      setnames(ref_dt, c("DATASET", "VAR", "MONTH", "PERIOD"))
      set(ref_dt, j = "variable", value = nm)
    } else {
      setnames(ref_dt, c("VAR","MONTH"))
      set(ref_dt, j = "variable", value = nm)
      set(ref_dt, j = "PERIOD", value = "2001_2020")
    }
  }

  # Transform ref_dt to data.table for remerging
  if (type %in% c("gcms", "gcm_hist_ts")) {
    switch(type,
      gcms = setnames(ref_dt, c("GCM", "VAR", "MONTH", "SSP", "RUN", "PERIOD")),
      gcm_hist_ts = setnames(ref_dt, c("GCM", "VAR", "MONTH", "RUN", "PERIOD"))
    )
    set(ref_dt, j = "variable", value = nm)
    set(ref_dt, j = "GCM", value = gsub(".", "-", ref_dt[["GCM"]], fixed = TRUE))
  }

  setkey(ref_dt, "variable")

  # Set Latitude and possibly ID
  climaterast[["lat"]] <- xyz[["lat"]]
  climaterast[["elev"]] <- xyz[["elev"]]
  climaterast[["id"]] <- xyz[["id"]]

  # Melt climaterast and set the same key for merging
  climaterast <- melt(
    setDT(climaterast),
    id.vars = c("id", "lat", "elev"),
    variable.factor = FALSE
  )
  setkey(climaterast, "variable")

  # Finally, dcast back to final form to get original 36 columns
  form <- switch(type,
    gcms = quote(id + GCM + SSP + RUN + PERIOD + lat + elev ~ VAR + MONTH),
    gcm_hist_ts = quote(id + GCM + RUN + PERIOD + lat + elev ~ VAR + MONTH),
    obs = quote(id + PERIOD + lat + elev ~ VAR + MONTH),
    obs_ts = quote(id + DATASET + PERIOD + lat + elev ~ VAR + MONTH)
  )

  climaterast <- dcast(
    # The merge with shared keys is as simple as that
    climaterast[ref_dt, ],
    as.formula(form),
    value.var = "value",
    sep = "_"
  )

  return(climaterast)
}


#' Add ID columns back to `downscale` output.
#'
#' @details adds back the original ID column and removes temporary
#'   one
#'
#' @param IDCols character. ID columns to add, or NULL if none
#' @param results `data.table` or `SpatVector` output from `downscale`
#'
#' @return `results` with IDCols
#'
#' @importFrom terra vect crs
#' @importFrom data.table as.data.table
#' @importFrom methods is
#' @noRd
addIDCols <- function(IDCols, results) {
  if (!is.null(IDCols)) {
    nm_order <- names(results)
    nms <- setdiff(names(IDCols), "id")

    results2 <- as.data.table(results, geom = "XY")
    results2[IDCols, (nms) := mget(nms), on = "id"]
    results2[, id := id_orig]
    results2[, id_orig := NULL]
    # setcolorder(results2, c(nm_order,nms))
    if (is(results, "SpatVector")) {
      results2 <- vect(results2, geom = c("x", "y"), crs = crs(results, proj = TRUE))
    } else {
      suppressWarnings(results2[, `:=`(x = NULL, y = NULL)])
    }
  } else {
    results2 <- results
  }
  return(results2)
}


#' Pack rasters for parallel computing
#'
#' @param ras a `SpatRaster` or list of `SpatRasters`, or `NULL`
#'
#' @return `NULL`, a packed `SpatRaster` or list of packed `SpatRasters`
#'
#' @importFrom terra wrap
#' @noRd
packRasters <- function(ras) {
  if (!is.null(ras)) {
    if (is(ras, "SpatRaster")) {
      ras <- wrap(ras)
    }
    if (is(ras, "list")) {
      ras <- sapply(ras, wrap, USE.NAMES = TRUE, simplify = FALSE)
    }
  }
  return(ras)
}


#' Pack rasters for parallel computing
#'
#' @param ras a `SpatRaster` or list of `SpatRasters`, or `NULL`
#'
#' @return `NULL`, a `PackedSpatRaster` or list of `PackedSpatRasters`
#'
#' @importFrom terra unwrap
#' @noRd
unpackRasters <- function(ras) {
  if (!is.null(ras)) {
    if (is(ras, "PackedSpatRaster")) {
      ras <- unwrap(ras)
    }
    if (is(ras, "list")) {
      ras <- sapply(ras, unwrap, USE.NAMES = TRUE, simplify = FALSE)
    }
  }
  return(ras)
}



#' Check xyz conforms to standards
#'
#' @template xyz
#' @param expectedCols character. Columns to look for in `xyz`.
#'
#' @return NULL
#' @noRd
.checkXYZ <- function(xyz, expectedCols = c("lon", "lat", "elev", "id")) {
  if (!is(xyz, "data.table")) {
    xyz <- tryCatch(as.data.table(xyz), error = function(e) e)
    if (is(xyz, "error")) {
      stop("Can't coherce xyz to a data.table object. Please pass a data.table object or another cohercible object class.")
    }
  }

  if (length(setdiff(expectedCols, names(xyz)))) {
    stop("'xyz' must have the columns ", paste(expectedCols, collapse = ", "))
  }

  if (length(unique(xyz$id)) != nrow(xyz)) {
    stop("'xyz$id' must be a column of unique IDs")
  }
  colTypes <- c("integer", "numeric", "character", "factor")
  if (!inherits(xyz$id, colTypes)) {
    stop("'xyz$id' must be an column of type ", paste(colTypes, collapse = ", "))
  }
  
  return(xyz)
}


#' Check `downscale_core` arguments
#'
#' @inheritParams downscale_core
#'
#' @return NULL
#' @noRd
.checkDwnsclArgs <- function(xyz, refmap, gcms = NULL, obs = NULL, gcm_ssp_ts = NULL, gcm_hist_ts = NULL,
                             obs_ts = NULL, return_refperiod = FALSE,
                             out_spatial = FALSE, plot = NULL, vars = list_vars()) {
  
  notSupportedVars <- setdiff(vars, list_vars())
  if (length(notSupportedVars)) {
    stop("The following variables are not supported:", 
         "\n  ", paste(notSupportedVars, collapse = ", "),
         "\n  Please see 'list_vars' for list of supported variables.")
  }
  
  if (!return_refperiod %in% c(TRUE, FALSE)) {
    stop("'return_refperiod' must be TRUE or FALSE")
  }
  if (!out_spatial %in% c(TRUE, FALSE)) {
    stop("'out_spatial' must be TRUE or FALSE")
  }
  
  plot <- if (!is.null(plot)) {
    match.arg(plot,list_vars())
  }
  
  if (!isTRUE(attr(refmap, "builder") == "climr")) {
    stop(
      "Please use `input_refmap` function to create `refmap`.",
      " See `?input_refmap` for details."
    )
  }

  # Make sure gcms was built using input_gcms
  if (!is.null(gcms) && !isTRUE(attr(gcms, "builder") == "climr")) {
    stop(
      "Please use `input_gcms` function to create `gcms`.",
      " See `?input_gcms` for details."
    )
  }

  # Make sure gcm_ssp_ts was built using input_gcm_ssp
  if (!is.null(gcm_ssp_ts) && !isTRUE(attr(gcm_ssp_ts, "builder") == "climr")) {
    stop(
      "Please use `input_gcm_ssp` function to create `gcm_ssp_ts`.",
      " See `?input_gcm_ssp` for details."
    )
  }

  # Make sure gcm_hist_ts was built using input_gcm_hist
  if (!is.null(gcm_hist_ts) && !isTRUE(attr(gcm_hist_ts, "builder") == "climr")) {
    stop(
      "Please use `input_gcm_hist` function to create `gcm_hist_ts`.",
      " See `?input_gcm_hist` for details."
    )
  }

  # Make sure obs was built using input_obs
  if (!is.null(obs) && !isTRUE(attr(obs, "builder") == "climr")) {
    stop(
      "Please use `input_obs` function to create `obs`.",
      " See `?input_obs` for details."
    )
  }

  # Make sure obs_ts was built using input_obs_ts
  if (!is.null(obs_ts) && !isTRUE(attr(obs_ts, "builder") == "climr")) {
    stop(
      "Please use `input_obs_ts` function to create `obs_ts`.",
      " See `?input_obs_ts` for details."
    )
  }

  ## check for "silly" parameter combinations
  if (all(
    is.null(gcms), is.null(gcm_ssp_ts), is.null(gcm_hist_ts),
    is.null(obs), is.null(obs_ts)
  )) {
    warning("'gcms', 'gcm_ssp_ts', 'gcm_hist_ts', 'obs' and 'obs_ts' are missing. Nothing to downscale.")
  }

  return(invisible(NULL))
}
