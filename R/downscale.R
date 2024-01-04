#' Climate variable downscaling
#' 
#' @description
#' `climr_downscale` downscales and calculates climate variables for points of interest.
#' 
#' @details
#' Additional details... TODO.
#' 
#' @param xyz three or four column `data.frame`: long, lat, elev, (id)
#' @param which_normal character. Which normal layer to use. Default is "auto", which selects the highest resolution normal for each point
#' @param historic_period character. Which historic period. Default `NULL`
#' @param historic_ts integer. Vector of historic years requested. Must be in `1902:2015`. Default `NULL`
#' @param gcm_models character. Vector of GCM names. Options are `list_gcm()`. Used for gcm periods, gcm timeseries, and historic timeseries. Default `NULL`
#' @template ssp
#' @param gcm_period character. Requested future periods. Options are `list_gcm_period()`
#' @param gcm_ts_years character. Requested future timeseries years. Must be in `2015:2100`
#' @param gcm_hist_years character. Requested historic modelled years. Must be in `1851:2010`
#' @template max_run
#' @template return_normal
#' @param vars character. Vector of climate variables. Options are `list_vars()`
#' @param cache logical. Cache data locally? Default `TRUE`
#' @template out_spatial
#' @template plot

#' @return `data.frame` of downscaled climate variables for each location.
#'   Historic, normal, and future periods are all returned in one table.

#' @author Kiri Daust

#' @importFrom sf st_as_sf st_join
#' @importFrom pool poolClose
#' @importFrom terra rast extract sources ext xres yres crop
#' @importFrom data.table getDTthreads setDTthreads rbindlist setkey
#'
#' @examples {
#'   ## TODO.
#' }
#' @rdname downscaling
#' @export
climr_downscale <- function(xyz, which_normal = c("auto", "BC", "NorAm"), historic_period = NULL, historic_ts = NULL,
                            gcm_models = NULL, ssp = c("ssp126", "ssp245", "ssp370", "ssp585"),
                            gcm_period = NULL, gcm_ts_years = NULL, gcm_hist_years = NULL, max_run = 0L, return_normal = TRUE,
                            vars = sort(sprintf(c("PPT%02d", "Tmax%02d", "Tmin%02d"), sort(rep(1:12, 3)))), cache = TRUE,
                            out_spatial = FALSE, plot = NULL) {
  # xyz <- coords
  # which_normal = "auto"
  # historic_period = NULL
  # historic_ts = NULL
  # gcm_models = NULL
  # ssp = c("ssp245","ssp370")
  # gcm_period = c("2021_2040", "2041_2060")
  # gcm_ts_years = 2020:2030
  # max_run = 0L
  # return_normal = TRUE
  # cache = TRUE
  # vars = c("PPT","CMI","PET07")
  
  message("Welcome to climr!")
  dbCon <- data_connect()
  thebb <- get_bb(xyz) ## get bounding box based on input points
  
  xyz <- as.data.frame(xyz) ## input needs to be a data.frame, not data.table. This is something we could change in the future
  IDCols <- NULL
  if (ncol(xyz) > 3) { ## remove extraneous columns
    ##check that ids are unique
    if(length(unique(xyz[,4])) < nrow(xyz)){
      warning("ID field is not unique. Will reassign ID column. Extra columns will not be returned.")
      xyz[,4] <- 1:nrow(xyz)
    }
    if(ncol(xyz) > 4) { ##save extra variables to join back
      IDCols <- xyz[, -c(1:3)]
      colnames(IDCols)[1] <- "ID"
      setDT(IDCols)
    }
  }else{
    xyz[,4] <- 1:nrow(xyz) ##assign ID column
  }
  
  message("Getting normals...")
  if (which_normal == "NorAm") {
    normal <- normal_input(dbCon = dbCon, normal = "normal_na", bbox = thebb, cache = cache)
  } else if (which_normal == "BC") {
    normal <- normal_input(dbCon = dbCon, normal = "normal_bc", bbox = thebb, cache = cache)
  } else {
    message("Normals not specified, extracting normals for BC and keeping points with non-NA climate values")
    bc_outline <- rast(system.file("extdata", "bc_outline.tif", package = "climr"))
    pnts <- extract(bc_outline, xyz[, 1:2], method = "simple")
    bc_ids <- xyz[, 4][!is.na(pnts$PPT01)]
    if (length(bc_ids) >= 1) {
      xyz_save <- xyz
      xyz <- xyz[!is.na(pnts$PPT01),]
      thebb_bc <- get_bb(xyz)
      message("for BC...")
      normal <- normal_input(dbCon = dbCon, normal = "normal_bc", bbox = thebb_bc, cache = cache)
    } else {
      normal <- normal_input(dbCon = dbCon, normal = "normal_na", bbox = thebb, cache = cache)
    }
  }
  
  if (!is.null(historic_period)) {
    message("Getting historic...")
    historic_period <- historic_input(dbCon, bbox = thebb, period = historic_period, cache = cache)
  }
  if (!is.null(historic_ts)) { 
    historic_ts <- historic_input_ts(dbCon, bbox = thebb, years = historic_ts, cache = cache)
  }
  
  if (!is.null(gcm_models)) {
    if (!is.null(gcm_period)) {
      message("Getting GCMs...")
      gcm <- gcm_input(dbCon,
                       bbox = thebb, gcm = gcm_models,
                       ssp = ssp,
                       period = gcm_period,
                       max_run = max_run,
                       cache = cache)
    } else {
      gcm <- NULL
    }
    if (!is.null(gcm_ts_years)) {
      gcm_ts <- gcm_ts_input(dbCon,
                             bbox = thebb, gcm = gcm_models,
                             ssp = ssp,
                             years = gcm_ts_years,
                             max_run = max_run,
                             cache = cache)
    } else {
      gcm_ts <- NULL
    }
    if (!is.null(gcm_hist_years)) {
      gcm_hist <- gcm_hist_input(dbCon,
                                 bbox = thebb, gcm = gcm_models,
                                 years = gcm_hist_years,
                                 max_run = max_run,
                                 cache = cache
      )
    } else {
      gcm_hist <- NULL
    }
  } else {
    gcm <- gcm_ts <- gcm_hist <- NULL
  }
  
  message("Downscaling!!")
  results <- downscale(
    xyz = xyz,
    normal = normal,
    historic = historic_period,
    historic_ts = historic_ts,
    gcm = gcm,
    gcm_ts = gcm_ts,
    gcm_hist = gcm_hist,
    return_normal = return_normal,
    vars = vars,
    out_spatial = out_spatial,
    plot = plot
  )
  
  if (which_normal %in% c("BC", "NorAm")) {
    if(!is.null(dbCon)) poolClose(dbCon)
    results <- addIDCols(IDCols, results)
    return(results)
  } 
  if ((length(bc_ids) == nrow(xyz_save) | length(bc_ids) < 1)) {
    if(!is.null(dbCon)) poolClose(dbCon)
    results <- addIDCols(IDCols, results)
    return(results)
  } else {
    na_xyz <- xyz_save[!xyz_save[, 4] %in% bc_ids,]
    thebb <- get_bb(na_xyz)
    message("Now North America...")
    normal <- normal_input(dbCon = dbCon, normal = "normal_na", bbox = thebb, cache = cache)
    
    results_na <- downscale(
      xyz = na_xyz,
      normal = normal,
      historic = historic_period,
      historic_ts = historic_ts,
      gcm = gcm,
      gcm_ts = gcm_ts,
      gcm_hist = gcm_hist,
      return_normal = return_normal,
      vars = vars,
      out_spatial = out_spatial,
      plot = plot
    )
    
    if(!is.null(dbCon)) poolClose(dbCon)
    res_all <- rbind(results, results_na)
    res_all <- addIDCols(IDCols, res_all)
    
    return(res_all)
  }
}


#' @description `downscale` downscales target rasters to points of interest.
#' 
#' @details
#'   Additional details... TODO.
#' 
#' @param xyz A 3-column `matrix` or `data.frame` (x, y, z) or (lon, lat, elev).
#' @param normal `SpatRaster`. Reference normal baseline input from `normal_input`.
#' @param gcm `list` of `SpatRasters`. Global Circulation Models outputs from `gcm_input`. Default to `NULL`.
#' @param historic `list` of `SpatRasters`. Historic time period outputs from `historic_input`. Default to `NULL`.
#' @param gcm_ts TODO
#' @param gcm_hist `list` of `SpatRasters`. TODO
#' @param historic_ts `list` of `SpatRasters`. TODO
#' @template return_normal
#' @param vars character. A vector of climate variables to compute. Supported variables
#'   can be obtained with `list_variables()`. Definitions can be found in this package
#'  `variables` dataset. Default to monthly PPT, Tmax, Tmin.
#' @param ppt_lr logical. Apply lapse rate adjustment to precipitations. Default to FALSE.
#' @param nthread An integer. Number of parallel threads to use to do computations. Default to 1L.
#' @template out_spatial
#' @template plot
#' 
#' @import data.table
#' @importFrom terra extract rast sources ext xres yres crop plot as.polygons
#' @importFrom grDevices hcl.colors palette
#' @importFrom stats complete.cases
#' 
#' @return A `data.table` or SpatVector with downscaled climate variables. If `gcm` is NULL, 
#'   this is just the downscaled `normal` at point locations. If `gcm` is provided,
#'   this returns a downscaled dataset for each point location, general circulation 
#'   model (GCM), shared socioeconomic pathway (SSP), run and period.
#' 
#' @rdname downscaling
#' @export
#' @examples
#' \dontrun{
#' dbCon <- data_connect()
#' xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10))
#' 
#' ## get bounding box based on input points
#' thebb <- get_bb(xyz)
#' normal <- normal_input(dbCon = dbCon, bbox = thebb, cache = TRUE)
#' 
#' ## pick one GCM, one SSP and one period from the list of available options 
#' gcm <- gcm_input(dbCon, thebb, gcm = list_gcm()[3], list_ssp()[1], list_gcm_period()[2])
#' 
#' ## notice coarseness of the data
#' terra::plot(gcm[[1]])
#' 
#' downscale(xyz, normal, gcm)
#' historic <- historic_input(dbCon, thebb)
#' terra::plot(historic[[1]])
#' 
#' downscale(xyz, normal, gcm = NULL, historic = historic, ppt_lr = FALSE)
#' }
downscale <- function(xyz, normal, gcm = NULL, historic = NULL, gcm_ts = NULL, gcm_hist = NULL, historic_ts = NULL, return_normal = FALSE,
                      vars = sort(sprintf(c("PPT%02d", "Tmax%02d", "Tmin%02d"), sort(rep(1:12, 3)))),
                      ppt_lr = FALSE, nthread = 1L, out_spatial = FALSE, plot = NULL) {
  # Make sure normal was built using normal_input
  if (!isTRUE(attr(normal, "builder") == "climr")) {
    stop(
      "Please use this package `normal_input` function to create `normal`.",
      " See `?normal_input` for details."
    )
  }
  
  # Make sure gcm was built using gcm_input
  if (!is.null(gcm) && !isTRUE(attr(gcm, "builder") == "climr")) {
    stop(
      "Please use this package `gcm_input` function to create `gcm`.",
      " See `?gcm_input` for details."
    )
  }
  
  if (isTRUE(nthread > 1L)) {
    if (!requireNamespace("parallel")) {
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
    
    ## we need to add ID's before paralellising otherwise we'll get repeated IDs
    xyz$ID <- 1:nrow(xyz)
    
    # Reordering on y axis for smaller cropped area and faster
    # sequential reads
    xyz <- xyz[order(xyz[, 2L]),]
    
    # Split before parallel processing
    xyz <- lapply(
      parallel::splitIndices(nrow(xyz), length(cl)),
      function(x) {
        xyz[x,]
      }
    )
    
    # Parallel processing and recombine

    ## pack rasters for parallelisation
    normal <- packRasters(normal)
    gcm <- packRasters(gcm)
    gcm_ts <- packRasters(gcm_ts)
    gcm_hist <- packRasters(gcm_hist) 
    historic <- packRasters(historic)
    historic_ts <- packRasters(historic_ts) 
    
    ## workaround to export function to nodes
    unpackRasters <- unpackRasters
    parallel::clusterExport(cl, c("unpackRasters"), envir = environment())
    
    res <- rbindlist(
        parallel::parLapply(
        cl = cl,
        X = xyz,
      # lapply(xyz,  ## testing
        # FUN = threaded_downscale_,
        fun = threaded_downscale_,
        normal = normal, 
        gcm = gcm, 
        gcm_ts = gcm_ts, 
        gcm_hist = gcm_hist, 
        historic = historic, 
        historic_ts = historic_ts, 
        return_normal = return_normal, 
        vars = vars,
        ppt_lr = ppt_lr
      ),
      use.names = TRUE
    )
  } else {
    # Downscale without parallel processing
    res <- downscale_(xyz, 
                      normal, 
                      gcm, 
                      gcm_ts, 
                      gcm_hist, 
                      historic, 
                      historic_ts, 
                      return_normal, 
                      vars, ppt_lr)
  }
  
  IDcols <- names(res)[!names(res) %in% vars]
  setkeyv(res, IDcols)
  if (out_spatial) {
    names(xyz)[4] <- "ID"
    res <- as.data.table(xyz)[res, on = "ID"]
    
    res <- vect(res, geom = names(xyz)[1:2], crs = crs(normal, proj = TRUE))
    
    if (!is.null(plot)) {
      if (!plot %in% vars) {
        stop("The variable you wish to plot was not dowscaled. Please pass a variable listed in 'vars'")
      } 
      ## make a mask of the normals data "extent"
      msk <- normal[[1]]
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
      plot(res2, y = plot, axes = FALSE, ext = ext(msk),
           col = palette(hcl.colors(100, "viridis")), sort = TRUE,
           add = TRUE, type = "continuous")
    }
  }
  return(res)
}

#' Simple downscale
#' 
#' @noRd
#' @inheritParams downscale
#' @param xyzID same as `xyz`, but with an added "ID" column
#' 
#' @return a `data.table`.
#' 
#' @import data.table
#' @importFrom terra crop ext xres yres extract
downscale_ <- function(xyzID, normal, gcm, gcm_ts, gcm_hist, 
                       historic, historic_ts, return_normal, 
                       vars, ppt_lr = FALSE) {
  # print(xyzID)
  # Define normal extent
  ex <- ext(
    c(
      min(xyzID[, 1L]) - xres(normal) * 2,
      max(xyzID[, 1L]) + xres(normal) * 2,
      min(xyzID[, 2L]) - yres(normal) * 2,
      max(xyzID[, 2L]) + yres(normal) * 2
    )
  )
  
  # crop normal raster (while also loading it to memory)
  normal <- crop(normal, ex, snap = "out")
  
  # Normal value extraction
  # possible garbage output :
  # Error in (function (x)  : attempt to apply non-function
  # Error in x$.self$finalize() : attempt to apply non-function
  # Can ignore, trying to suppress messages with `shush`
  # https://github.com/rspatial/terra/issues/287
  
  # stack before extracting
  res <-
    extract(
      x = normal,
      y = xyzID[, 1L:2L],
      method = "bilinear"
    )
  
  
  # Compute elevation differences between provided points elevation and normal
  # Dem at position 74 (ID column + 36 normal layers + 36 lapse rate layers + 1 dem layer)
  elev_delta <- xyzID[, 3L] - res[, 74L]
  # print(elev_delta)
  # print(res)
  # Compute individual point lapse rate adjustments
  # Lapse rate position 38:73 (ID column + 36 normal layers + 36 lapse rate layers)
  lr <- elev_delta * res[, 38L:73L] ## do we need anything other than the lapse rate?
  
  # Replace any NAs left with 0s
  lr[is.na(lr)] <- 0L
  
  # Remove lapse rates and digital elevation model from res
  res[, 38L:74L] <- NULL
  
  # Combine results (ignoring ID column)
  if (isTRUE(ppt_lr)) {
    res[, -1L] <- res[, -1L] + lr
  } else {
    ppt <- grep("^PPT", names(normal)[1L:36L], invert = TRUE)
    res[, ppt + 1L] <- res[, ppt + 1L] + lr[, ppt]
  }
  
  # Process one GCM stacked layers
  if (!is.null(gcm)) {
    # Process each gcm and rbind resulting tables
    res_gcm <- rbindlist(
      lapply(gcm, process_one_gcm, res = res, xyzID = xyzID, timeseries = FALSE),
      use.names = TRUE
    )
  } else {
    res_gcm <- NULL
  }
  if (!is.null(gcm_ts)) {
    # Process each gcm and rbind resulting tables
    res_gcmts <- rbindlist(
      lapply(gcm_ts, process_one_gcm, res = res, xyzID = xyzID, timeseries = TRUE),
      use.names = TRUE
    )
  } else {
    res_gcmts <- NULL
  }
  if (!is.null(gcm_hist)) {
    # Process each gcm and rbind resulting tables
    res_gcm_hist <- rbindlist(
      lapply(gcm_hist, process_one_gcm_hist, res = res, xyzID = xyzID),
      use.names = TRUE
    )
  } else {
    res_gcm_hist <- NULL
  }
  if (!is.null(historic)) {
    # print(historic)
    res_hist <- rbindlist(
      lapply(historic, process_one_historic, res = res, xyzID = xyzID, timeseries = FALSE),
      use.names = TRUE
    )
  } else {
    res_hist <- NULL
  }
  if (!is.null(historic_ts)) {
    # print(historic)
    res_hist_ts <- rbindlist(
      lapply(historic_ts, process_one_historic, res = res, xyzID = xyzID, timeseries = TRUE),
      use.names = TRUE
    )
  } else {
    res_hist_ts <- NULL
  }
  
  if (return_normal) {
    nm <- names(res)[-1]
    labels <- nm
    normal_ <- res
    # Reshape (melt / dcast) to obtain final form
    ref_dt <- tstrsplit(nm, "_")
    setDT(ref_dt)
    setnames(ref_dt, c("VAR"))
    set(ref_dt, j = "variable", value = nm)
    set(ref_dt, j = "PERIOD", value = "1961_1990")
    setkey(ref_dt, "variable")
    # Set Latitude and possibly ID
    normal_[["Lat"]] <- xyzID[, 2L]
    normal_[["Elev"]] <- xyzID[, 3L]
    if (ncol(xyzID) >= 4L) {
      normal_[["ID"]] <- xyzID[, 4L]
    }
    
    # Melt gcm_ and set the same key for merging
    normal_ <- melt(
      setDT(normal_),
      id.vars = c("ID", "Lat", "Elev"),
      variable.factor = FALSE
    )
    setkey(normal_, "variable")
    
    # Finally, dcast back to final form to get original 36 columns
    normal_ <- dcast(
      # The merge with shared keys is as simple as that
      normal_[ref_dt,],
      ID + PERIOD + Lat + Elev ~ VAR,
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
threaded_downscale_ <- function(xyz, normal, gcm, gcm_ts, gcm_hist, historic, historic_ts, ...) {
  ## unpack rasters
  normal <- unpackRasters(normal)
  gcm <- unpackRasters(gcm)
  gcm_ts <- unpackRasters(gcm_ts)
  gcm_hist <- unpackRasters(gcm_hist) 
  historic <- unpackRasters(historic)
  historic_ts <- unpackRasters(historic_ts) 

  # Set DT threads to 1 in parallel to avoid overloading CPU
  # Not needed for forking, not taking any chances
  dt_nt <- getDTthreads()
  setDTthreads(1)
  on.exit(setDTthreads(dt_nt))
  
  # Downscale
  res <- downscale_(xyzID = xyz, normal = normal, gcm = gcm, 
                    gcm_ts = gcm_ts, gcm_hist = gcm_hist, 
                    historic = historic, historic_ts = historic_ts, ...)
  return(res)
}

#' TODO: fill documentation here
#'
#' @param gcm_ TODO
#' @param res TODO
#' @param xyzID TODO
#' @param timeseries TODO
#'
#' @return `data.table`
process_one_gcm <- function(gcm_, res, xyzID, timeseries) {
  ## gcm_ <- gcm[[1]]
  # Store names for later use
  nm <- names(gcm_)
  
  # Define gcm extent. res*2 To make sure we capture surrounding
  # cells for bilinear interpolation.
  ex <- ext(
    c(
      min(xyzID[, 1L]) - xres(gcm_) * 2,
      max(xyzID[, 1L]) + xres(gcm_) * 2,
      min(xyzID[, 2L]) - yres(gcm_) * 2,
      max(xyzID[, 2L]) + yres(gcm_) * 2
    )
  )
  # Extract gcm bilinear interpolations
  # Cropping will reduce the size of data to load in memory
  gcm_ <- crop(gcm_, ex, snap = "out")
  gcm_ <- extract(x = gcm_, y = xyzID[, 1L:2L], method = "bilinear")
  
  # Create match set to match with res names
  labels <- vapply(
    strsplit(nm, "_"),
    function(x) {
      paste0(x[2:3], collapse = "")
    },
    character(1)
  )
  
  # Add matching column to gcm_
  ppt_ <- grep("PPT", labels)
  gcm_[, ppt_ + 1L] <- gcm_[, ppt_ + 1L] * res[, match(labels[ppt_], names(res))] ## PPT
  gcm_[, -c(1L, ppt_ + 1L)] <- gcm_[, -c(1L, ppt_ + 1L)] + res[, match(labels[-ppt_], names(res))] ## Temperature
  
  # Reshape (melt / dcast) to obtain final form
  ref_dt <- tstrsplit(nm, "_")
  # Recombine PERIOD into one field
  if (!timeseries) {
    ref_dt[[6]] <- paste(ref_dt[[6]], ref_dt[[7]], sep = "_")
    ref_dt[7] <- NULL
  }
  # Transform ref_dt to data.table for remerging
  setDT(ref_dt)
  setnames(ref_dt, c("GCM", "VAR", "MONTH", "SSP", "RUN", "PERIOD"))
  set(ref_dt, j = "variable", value = nm)
  set(ref_dt, j = "GCM", value = gsub(".", "-", ref_dt[["GCM"]], fixed = TRUE))
  setkey(ref_dt, "variable")
  
  # Set Latitude and possibly ID
  gcm_[["Lat"]] <- xyzID[, 2L]
  gcm_[["Elev"]] <- xyzID[, 3L]
  if (ncol(xyzID) >= 4L) {
    gcm_[["ID"]] <- xyzID[, 4L]
  }
  
  # Melt gcm_ and set the same key for merging
  gcm_ <- melt(
    setDT(gcm_),
    id.vars = c("ID", "Lat", "Elev"),
    variable.factor = FALSE
  )
  setkey(gcm_, "variable")
  
  # Finally, dcast back to final form to get original 36 columns
  gcm_ <- dcast(
    # The merge with shared keys is as simple as that
    gcm_[ref_dt,],
    ID + GCM + SSP + RUN + PERIOD + Lat + Elev ~ VAR + MONTH,
    value.var = "value",
    sep = ""
  )
  
  return(gcm_)
}


#' TODO: fill documentation here
#'
#' @param gcm_ TODO
#' @param res TODO
#' @param xyzID TODO
#'
#' @return `data.table`
process_one_gcm_hist <- function(gcm_, res, xyzID) {
  ## gcm_ <- gcm[[1]]
  # Store names for later use
  nm <- names(gcm_)
  
  # Define gcm extent. res*2 To make sure we capture surrounding
  # cells for bilinear interpolation.
  ex <- ext(
    c(
      min(xyzID[, 1L]) - xres(gcm_) * 2,
      max(xyzID[, 1L]) + xres(gcm_) * 2,
      min(xyzID[, 2L]) - yres(gcm_) * 2,
      max(xyzID[, 2L]) + yres(gcm_) * 2
    )
  )
  # Extract gcm bilinear interpolations
  # Cropping will reduce the size of data to load in memory
  gcm_ <- crop(gcm_, ex, snap = "out")
  gcm_ <- extract(x = gcm_, y = xyzID[, 1L:2L], method = "bilinear")
  
  # Create match set to match with res names
  labels <- vapply(
    strsplit(nm, "_"),
    function(x) {
      paste0(x[2:3], collapse = "")
    },
    character(1)
  )
  
  # Add matching column to gcm_
  ppt_ <- grep("PPT", labels)
  gcm_[, ppt_ + 1L] <- gcm_[, ppt_ + 1L] * res[, match(labels[ppt_], names(res))] ## PPT
  gcm_[, -c(1L, ppt_ + 1L)] <- gcm_[, -c(1L, ppt_ + 1L)] + res[, match(labels[-ppt_], names(res))] ## Temperature
  
  # Reshape (melt / dcast) to obtain final form
  ref_dt <- tstrsplit(nm, "_")
  
  # Transform ref_dt to data.table for remerging
  setDT(ref_dt)
  setnames(ref_dt, c("GCM", "VAR", "MONTH", "RUN", "PERIOD"))
  set(ref_dt, j = "variable", value = nm)
  set(ref_dt, j = "GCM", value = gsub(".", "-", ref_dt[["GCM"]], fixed = TRUE))
  setkey(ref_dt, "variable")
  
  # Set Latitude and possibly ID
  gcm_[["Lat"]] <- xyzID[, 2L]
  gcm_[["Elev"]] <- xyzID[, 3L]
  if (ncol(xyzID) >= 4L) {
    gcm_[["ID"]] <- xyzID[, 4L]
  }
  
  # Melt gcm_ and set the same key for merging
  gcm_ <- melt(
    setDT(gcm_),
    id.vars = c("ID", "Lat", "Elev"),
    variable.factor = FALSE
  )
  setkey(gcm_, "variable")
  
  gcm_ <- dcast(
    gcm_[ref_dt,],
    ID + GCM + RUN + PERIOD + Lat + Elev ~ VAR + MONTH,
    value.var = "value",
    sep = ""
  )
  
  return(gcm_)
}

#' TODO: fill documentation here
#'
#' @param historic_ TODO
#' @param res TODO
#' @param xyzID TODO
#' @param timeseries TODO
#'
#' @return `data.table`
process_one_historic <- function(historic_, res, xyzID, timeseries) {
  # print(historic_)
  # Store names for later use
  nm <- names(historic_)
  
  # Define gcm extent. res*2 To make sure we capture surrounding
  # cells for bilinear interpolation.
  ex <- ext(
    c(
      min(xyzID[, 1L]) - xres(historic_) * 2,
      max(xyzID[, 1L]) + xres(historic_) * 2,
      min(xyzID[, 2L]) - yres(historic_) * 2,
      max(xyzID[, 2L]) + yres(historic_) * 2
    )
  )
  # Extract gcm bilinear interpolations
  # Cropping will reduce the size of data to load in memory
  historic_ <- crop(historic_, ex, snap = "out")
  historic_ <- extract(x = historic_, y = xyzID[, 1L:2L], method = "bilinear")
  
  # Create match set to match with res names
  labels <- nm
  if (timeseries) {
    labels <- gsub("_.*", "", labels)
  }
  
  ppt_ <- grep("PPT", labels)
  historic_[, ppt_ + 1L] <- historic_[, ppt_ + 1L] * res[, match(labels[ppt_], names(res))] ## PPT
  historic_[, -c(1L, ppt_ + 1L)] <- historic_[, -c(1L, ppt_ + 1L)] + res[, match(labels[-ppt_], names(res))] ## Temperature
  
  
  # Reshape (melt / dcast) to obtain final form
  ref_dt <- tstrsplit(nm, "_")
  setDT(ref_dt)
  if (timeseries) {
    setnames(ref_dt, c("VAR", "PERIOD"))
    set(ref_dt, j = "variable", value = nm)
  } else {
    setnames(ref_dt, c("VAR"))
    set(ref_dt, j = "variable", value = nm)
    set(ref_dt, j = "PERIOD", value = "2001_2020")
  }
  
  setkey(ref_dt, "variable")
  # Set Latitude and possibly ID
  historic_[["Lat"]] <- xyzID[, 2L]
  historic_[["Elev"]] <- xyzID[, 3L]
  if (ncol(xyzID) >= 4L) {
    historic_[["ID"]] <- xyzID[, 4L]
  }
  
  # Melt gcm_ and set the same key for merging
  historic_ <- melt(
    setDT(historic_),
    id.vars = c("ID", "Lat", "Elev"),
    variable.factor = FALSE
  )
  setkey(historic_, "variable")
  
  # Finally, dcast back to final form to get original 36 columns
  historic_ <- dcast(
    # The merge with shared keys is as simple as that
    historic_[ref_dt,],
    ID + PERIOD + Lat + Elev ~ VAR,
    value.var = "value",
    sep = ""
  )
  
  return(historic_)
}




#' Add ID columns to `downscale` output.
#'
#' @param IDCols character. ID columns to add, or NULL if none
#' @param results `data.table` or `SpatVector` output from `downscale`
#'
#' @return `results` with IDCols
#'
#' @importFrom terra vect crs
#' @importFrom data.table as.data.table
#' @importFrom methods is
addIDCols <- function(IDCols, results) {
  if(!is.null(IDCols)){
    nm_order <- names(results)
    nms <- names(IDCols)[-1]
    
    results2 <- as.data.table(results, geom = "XY")
    results2[IDCols, (nms) := mget(nms), on = "ID"]
    #setcolorder(results2, c(nm_order,nms))
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
#' @return `NULL`, a packed SpatRaster or list of packed SpatRaters
#'
#' @importFrom terra wrap
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
#' @return `NULL`, a packed SpatRaster or list of packed SpatRaters
#'
#' @importFrom terra unwrap
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
