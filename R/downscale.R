#' Climate variable downscaling
#' 
#' @description
#' `climr_downscale` downscales and calculates climate variables for points of interest.
#' 
#' @details
#' Additional details... TODO.
#' 
#' @template xyz
#' @param which_normal character. Which normal layer to use. 
#'   Default is "auto", which selects the highest resolution normal for each point. 
#'   Other options are one of [`list_normal()`]
#' @param historic_period character. Which historic period. Default `NULL`
#' @param historic_ts integer. Vector of historic years requested. Must be in `1902:2015`. Default `NULL`
#' @param gcm_models character. Vector of GCM names. Options are [`list_gcm()`]. Used for gcm periods, gcm timeseries, and historic timeseries. Default `NULL`
#' @template ssp
#' @param gcm_period character. Requested future periods. Options are [`list_gcm_period()`]
#' @param gcm_ts_years character. Requested future timeseries years. Must be in `2015:2100`
#' @param gcm_hist_years character. Requested historic modelled years. Must be in `1851:2010`
#' @template max_run
#' @template return_normal
#' @param vars character. Vector of climate variables. Options are [`list_vars()`]
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
#' library(data.table)
#' library(terra)
#' set.seed(123)
#' dbCon <- data_connect()
#' xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10),
#'                   id = 1:10)
#' 
#' ## get bounding box based on input points
#' thebb <- get_bb(xyz)
#' historic <- historic_input(dbCon, thebb, period = "2001_2020")
#' plot(historic[[1]][[2]])
#' 
#' ## provide or create a lon, lat, elev, and optionally id, dataframe - usually read from csv file
#' in_xyz <- data.frame(lon = c(-127.70521, -127.62279, -127.56235, -127.7162,
#'                               -127.18585, -127.1254, -126.94957, -126.95507),
#'                      lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
#'                      elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
#'                      id = 1:8,
#'                      Zone = c(rep("CWH",3), rep("CDF",5)),
#'                      Subzone = c("vm1","vm2","vs1",rep("mm",3),"dk","dc"))
#' 
#' ## historic observational time series
#' vars <- c("PPT","CMD","Tave07")
#' climate_norms_hist <- climr_downscale(xyz = in_xyz, which_normal = "auto",
#'                                       return_normal = TRUE,
#'                                       historic_period = "2001_2020",
#'                                       vars = vars,
#'                                       out_spatial = TRUE, plot = "PPT") ##specify desired variables to plot
#' 
#' ## as a data.table
#' climate_norms_hist <- climr_downscale(xyz = in_xyz, which_normal = "auto",
#'                                       return_normal = TRUE,
#'                                       vars = vars,
#'                                       out_spatial = FALSE, plot = "PPT") ##specify desired variables to plot
#' 
#' ## future projections
#' climate_norms_fut <- climr_downscale(xyz = in_xyz, which_normal = "auto",
#'                                      gcm_models = c("ACCESS-ESM1-5"),
#'                                      ssp = c("ssp370"),
#'                                      gcm_period = c("2021_2040"),
#'                                      #gcm_ts_years = 2020:2060,
#'                                      max_run = 3, #' we want 3 individual runs for each model
#'                                      vars = vars)
#' }
#' @rdname downscaling
#' @export
climr_downscale <- function(xyz, which_normal = c("auto", list_normal()), historic_period = NULL, historic_ts = NULL,
                            gcm_models = NULL, ssp = list_ssp(),
                            gcm_period = NULL, gcm_ts_years = NULL, gcm_hist_years = NULL, max_run = 0L, return_normal = TRUE,
                            vars = sort(sprintf(c("PPT%02d", "Tmax%02d", "Tmin%02d"), sort(rep(1:12, 3)))), cache = TRUE,
                            out_spatial = FALSE, plot = NULL) {
  message("Welcome to climr!")
  
  ## checks
  thiscall <- match.call()
  thiscall[[1]] <- as.name(".checkClimrDwnsclArgs")
  eval(thiscall, envir = parent.frame())
  
  expectedCols <- c("lon", "lat", "elev", "id")
  xyz <- .checkXYZ(xyz, expectedCols)
  
  dbCon <- data_connect()
  thebb <- get_bb(xyz) ## get bounding box based on input points
  
  rmCols <- setdiff(names(xyz), expectedCols)
  if (length(rmCols)) { ## remove extraneous columns
    warnings("Extra columns will be ignored")
    xyz <- xyz[, ..expectedCols]
  }
  
  ## make an integer id col to use through out
  if (inherits(xyz$id, c("integer", "numeric"))) {
    IDint <- as.integer(xyz$id)
  } else {
    if (inherits(xyz$id, c("character", "factor"))) {
      IDint <- as.integer(as.factor(xyz$id))
    }
  }
  setnames(xyz, "id", "id_orig")
  xyz[, id := IDint]
  rm(IDint)
  
  ## save original ID column to join back
  origID <- xyz[, .(id, id_orig)]
  
  xyz[, id_orig := NULL]
  
  ## input needs to be a data.frame, not data.table. This is something we could change in the future
  # xyz <- as.data.frame(xyz) 
  
  message("Getting normals...")
  if (which_normal == "NorAm") {
    normal <- normal_input(dbCon = dbCon, normal = "normal_na", bbox = thebb, cache = cache)
  } else if (which_normal == "BC") {
    normal <- normal_input(dbCon = dbCon, normal = "normal_bc", bbox = thebb, cache = cache)
  } else if (which_normal == "Colin") {
    normal <- normal_input(dbCon = dbCon, normal = "composite_normal", bbox = thebb, cache = cache)
  } else {
    #message("Normals not specified, using highest resolution available for each point")
    bc_outline <- rast(system.file("extdata", "bc_outline.tif", package = "climr"))
    pnts <- extract(bc_outline, xyz[, .(lon, lat)], method = "simple")
    bc_ids <- xyz[["id"]][!is.na(pnts$PPT01)]
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
  
  if (which_normal != "auto") {
    if(!is.null(dbCon)) poolClose(dbCon)
    results <- addIDCols(origID, results)
    return(results)
  } 
  if ((length(bc_ids) < 1 || length(bc_ids) == nrow(xyz_save))) {
    if(!is.null(dbCon)) poolClose(dbCon)
    results <- addIDCols(origID, results)
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
    res_all <- addIDCols(origID, res_all)
    
    return(res_all)
  }
}


#' @description `downscale` downscales target rasters to points of interest.
#' 
#' @details
#'   Additional details... TODO.
#' 
#' @template xyz
#' @param normal `SpatRaster`. Reference normal baseline input from `normal_input`.
#' @param gcm `list` of `SpatRasters`. Global Circulation Models outputs from [`gcm_input()`]. Default to `NULL`.
#' @param historic `list` of `SpatRasters`. Historic time period outputs from [`historic_input()`]. Default to `NULL`.
#' @param gcm_ts `list` of `SpatRasters`. TODO
#' @param gcm_hist `list` of `SpatRasters`. TODO
#' @param historic_ts `list` of `SpatRasters`. TODO
#' @template return_normal
#' @param vars character. A vector of climate variables to compute. Supported variables
#'   can be obtained with [`list_variables()`]. Definitions can be found in this package
#'  `variables` dataset. Default to monthly PPT, Tmax, Tmin.
#' @param ppt_lr logical. Apply lapse rate adjustment to precipitations. Default to FALSE.
#' @param nthread integer. Number of parallel threads to use to do computations. Default to 1L.
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
#' @seealso [`gcm_input()`], [`historic_input()`], [`list_variables()`]
#' 
#' @rdname downscaling
#' @export
#' @examples
#' \dontrun{
#' dbCon <- data_connect()
#' on.exit(try(pool::poolClose(dbCon)))
#' xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10), id = 1:10)
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
  ## checks
  thiscall <- match.call()
  thiscall[[1]] <- as.name(".checkDwnsclArgs")
  eval(thiscall, envir = parent.frame())
  
  expectedCols <- c("lon", "lat", "elev", "id")
  xyz <- .checkXYZ(xyz, expectedCols)
  
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
    
    # Reordering on y axis for smaller cropped area and faster
    # sequential reads
    xyz <- xyz[order(lat),]
    
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
    res <- as.data.table(xyz)[res, on = "id"]
    
    res <- vect(res, geom = c("lon", "lat"), crs = crs(normal, proj = TRUE))
    
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
#' @template xyz
#' 
#' @return A `data.table`.
#' 
#' @import data.table
#' @importFrom terra crop ext xres yres extract
downscale_ <- function(xyz, normal, gcm, gcm_ts, gcm_hist, 
                       historic, historic_ts, return_normal, 
                       vars, ppt_lr = FALSE) {
  # print(xyz)
  # Define normal extent
  ex <- ext(
    c(
      min(xyz[["lon"]]) - xres(normal) * 2,
      max(xyz[["lon"]]) + xres(normal) * 2,
      min(xyz[["lat"]]) - yres(normal) * 2,
      max(xyz[["lat"]]) + yres(normal) * 2
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
      y = xyz[, .(lon, lat)],
      method = "bilinear"
    )
  
  # Compute elevation differences between provided points elevation and normal
  # Dem at position 74 (ID column + 36 normal layers + 36 lapse rate layers + 1 dem layer)
  elev_delta <- xyz[["elev"]] - res[, 74L]
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
  res <- as.data.frame(res)  ## TODO: convert code below to data.table
  if (isTRUE(ppt_lr)) {
    res[, -1L] <- res[, -1L] + lr
  } else {
    ppt <- grep("^PPT", names(normal)[1L:36L], invert = TRUE)
    res[, ppt + 1L] <- res[, ppt + 1L] + lr[, ppt]
  }
  res <- as.data.table(res)
  
  # Process one GCM stacked layers
  if (!is.null(gcm)) {
    # Process each gcm and rbind resulting tables
    res_gcm <- rbindlist(
      lapply(gcm, process_one_climaterast, res = res, xyz = xyz, type = "gcm"),
      use.names = TRUE
    )
  } else {
    res_gcm <- NULL
  }
  if (!is.null(gcm_ts)) {
    # Process each gcm and rbind resulting tables
    res_gcmts <- rbindlist(
      lapply(gcm_ts, process_one_climaterast, res = res, xyz = xyz, timeseries = TRUE, type = "gcm"),
      use.names = TRUE
    )
  } else {
    res_gcmts <- NULL
  }
  if (!is.null(gcm_hist)) {
    # Process each gcm and rbind resulting tables
    res_gcm_hist <- rbindlist(
      lapply(gcm_hist, process_one_climaterast, res = res, xyz = xyz, type = "gcm_hist"),
      use.names = TRUE)
  } else {
    res_gcm_hist <- NULL
  }
  if (!is.null(historic)) {
    # print(historic)
    res_hist <- rbindlist(
      lapply(historic, process_one_climaterast, res = res, xyz = xyz, type = "historic"),
      use.names = TRUE
    )
  } else {
    res_hist <- NULL
  }
  if (!is.null(historic_ts)) {
    # print(historic)
    res_hist_ts <- rbindlist(
      lapply(historic_ts, process_one_climaterast, res = res, xyz = xyz, timeseries = TRUE, type = "historic"),
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
      normal_[ref_dt,],
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
  res <- downscale_(xyz = xyz, normal = normal, gcm = gcm, 
                    gcm_ts = gcm_ts, gcm_hist = gcm_hist, 
                    historic = historic, historic_ts = historic_ts, ...)
  return(res)
}

#' TODO: fill documentation here
#'
#' @param climaterast 
#' @param res 
#' @template xyz 
#' @param timeseries 
#' @param type 
#'
#' @return
#' @export
#'
#' @importFrom stats as.formula
process_one_climaterast <- function(climaterast, res, xyz, timeseries = FALSE, 
                                    type = c("gcm", "gcm_hist", "historic")) {
  type <- match.arg(type)
  
  # Store names for later use
  nm <- names(climaterast)
  
  # Define gcm extent. res*2 To make sure we capture surrounding
  # cells for bilinear interpolation.
  ex <- ext(
    c(
      min(xyz[["lon"]]) - xres(climaterast) * 2,
      max(xyz[["lon"]]) + xres(climaterast) * 2,
      min(xyz[["lat"]]) - yres(climaterast) * 2,
      max(xyz[["lat"]]) + yres(climaterast) * 2
    )
  )
  # Extract gcm bilinear interpolations
  # Cropping will reduce the size of data to load in memory
  climaterast <- crop(climaterast, ex, snap = "out")
  climaterast <- extract(x = climaterast, y = xyz[, .(lon, lat)], method = "bilinear")
  
  # Create match set to match with res names
  labels <- vapply(
    strsplit(nm, "_"),
    function(x) {
      paste0(x[2:3], collapse = "")
    },
    character(1)
  )
  
  if (type == "historic") {
    ## Create match set to match with res names
    labels <- nm
    if (timeseries) {
      labels <- gsub("_.*", "", labels)
    }
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
  if (!timeseries & type == "gcm") {
    ref_dt[[6]] <- paste(ref_dt[[6]], ref_dt[[7]], sep = "_")
    ref_dt[7] <- NULL
  }
  
  setDT(ref_dt)
  if (type == "historic") {
    if (timeseries) {
      setnames(ref_dt, c("VAR", "PERIOD"))
      set(ref_dt, j = "variable", value = nm)
    } else {
      setnames(ref_dt, c("VAR"))
      set(ref_dt, j = "variable", value = nm)
      set(ref_dt, j = "PERIOD", value = "2001_2020")
    }
  }
  
  # Transform ref_dt to data.table for remerging
  if (type %in% c("gcm", "gcm_hist")) {
    switch(type,
           gcm = setnames(ref_dt, c("GCM", "VAR", "MONTH", "SSP", "RUN", "PERIOD")),
           gcm_hist = setnames(ref_dt, c("GCM", "VAR", "MONTH", "RUN", "PERIOD")))
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
                 gcm = quote(id + GCM + SSP + RUN + PERIOD + lat + elev ~ VAR + MONTH),
                 gcm_hist = quote(id + GCM + RUN + PERIOD + lat + elev ~ VAR + MONTH),
                 historic = quote(id + PERIOD + lat + elev ~ VAR)) 
  
  climaterast <- dcast(
    # The merge with shared keys is as simple as that
    climaterast[ref_dt,],
    as.formula(form),
    value.var = "value",
    sep = ""
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
addIDCols <- function(IDCols, results) {
  if (!is.null(IDCols)){
    nm_order <- names(results)
    nms <- setdiff(names(IDCols), "id")
    
    results2 <- as.data.table(results, geom = "XY")
    results2[IDCols, (nms) := mget(nms), on = "id"]
    results2[, id := id_orig]
    results2[, id_orig := NULL]
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
#' @return `NULL`, a packed `SpatRaster` or list of packed `SpatRasters`
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
#' @return `NULL`, a `PackedSpatRaster` or list of `PackedSpatRasters`
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



#' Check xyz conforms to standards
#'
#' @template xyz
#' @param expectedCols character. Columns to look for in `xyz`.
#'
#' @return NULL
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



#' Check `climr_downscale` arguments
#'
#' @inheritParams climr_downscale 
#' @param ... ignored
#'
#' @return NULL
.checkClimrDwnsclArgs <- function(xyz, which_normal = NULL, normal = NULL, historic_period = NULL, historic_ts = NULL,
                            gcm_models = NULL, ssp = list_ssp(), gcm_period = NULL, gcm_ts_years = NULL, 
                            gcm_hist_years = NULL, max_run = 0L, vars = list_variables(), ...) {
  ssp <- match.arg(ssp, list_ssp(), several.ok = TRUE)
  vars <- match.arg(vars, list_variables(), several.ok = TRUE)
  
  if (!is.null(which_normal)) {
    which_normal <- match.arg(which_normal, c("auto", list_normal()))
  } 
  
  if (!is.null(historic_period)) {
    historic_period <- match.arg(historic_period, list_historic(), several.ok = TRUE)
  }
  
  if (!is.null(historic_ts)) {
    if (!all(historic_ts %in% 1902:2015)) {
      stop("'historic_ts' must be in 1902:2015")
    }
  }
  
  if (!is.null(gcm_models)) {
    gcm_models <- match.arg(gcm_models, list_gcm(), several.ok = TRUE)
  }
  
  if (!is.null(gcm_period)) {
    gcm_period <- match.arg(gcm_period, list_gcm_period(), several.ok = TRUE)
  }
  
  if (!is.null(gcm_ts_years)) {
    if (!all(gcm_ts_years %in% 2015:2100)) {
      stop("'gcm_ts_years' must be in 2015:2100")
    }
  }
  
  msg <- "'max_run' must be 0 or larger"
  if (!inherits(max_run, c("integer", "numeric"))) {
    stop(msg)
  } else if (max_run < 0) {
    stop(msg)
  }
  
  ## check for "silly" parameter combinations
  if (!is.null(gcm_models) & 
      all(is.null(gcm_hist_years), is.null(gcm_ts_years), is.null(gcm_period), is.null(ssp))) {
    message("'gcm_models' will be ignored, since 'gcm_hist_years', 'gcm_ts_years', 'gcm_period' and 'ssp' are missing")
  }
  
  if (is.null(gcm_models) & 
      any(!is.null(gcm_hist_years), !is.null(gcm_ts_years), !is.null(gcm_period), !is.null(ssp))) {
    message("'gcm_models' is missing. 'gcm_hist_years', 'gcm_ts_years', 'gcm_period' and 'ssp' will be ignored")
  }
  
  if ((!is.null(max_run) | max_run > 0) & 
      is.null(gcm_models)) {
    message("'gcm_models' is missing. 'max_run' will be ignored")
  }
  return(invisible(NULL))
}

#' Check `downscale` arguments
#'
#' @inheritParams downscale
#' @param ... ignored
#'
#' @return NULL
.checkDwnsclArgs <- function(xyz, normal, gcm = NULL, historic = NULL, gcm_ts = NULL, gcm_hist = NULL, 
                             historic_ts = NULL, return_normal = FALSE,
                             vars = list_variables(), ...) {
 
  vars <- match.arg(vars, list_variables(), several.ok = TRUE)
  
  if (!isTRUE(attr(normal, "builder") == "climr")) {
    stop(
      "Please use `normal_input` function to create `normal`.",
      " See `?normal_input` for details."
    )
  }
  
  # Make sure gcm was built using gcm_input
  if (!is.null(gcm) && !isTRUE(attr(gcm, "builder") == "climr")) {
    stop(
      "Please use `gcm_input` function to create `gcm`.",
      " See `?gcm_input` for details."
    )
  }
  
  # Make sure gcm_ts was built using gcm_ts_input
  if (!is.null(gcm_ts) && !isTRUE(attr(gcm_ts, "builder") == "climr")) {
    stop(
      "Please use `gcm_ts_input` function to create `gcm_ts`.",
      " See `?gcm_ts_input` for details."
    )
  }
  
  # Make sure gcm_hist was built using gcm_hist_input
  if (!is.null(gcm_hist) && !isTRUE(attr(gcm_hist, "builder") == "climr")) {
    stop(
      "Please use `gcm_hist_input` function to create `gcm_hist`.",
      " See `?gcm_hist_input` for details."
    )
  }
  
  # Make sure historic was built using historic_input
  if (!is.null(historic) && !isTRUE(attr(historic, "builder") == "climr")) {
    stop(
      "Please use `historic_input` function to create `historic`.",
      " See `?historic_input` for details."
    )
  }
  
  # Make sure historic_ts was built using historic_input_ts
  if (!is.null(historic_ts) && !isTRUE(attr(historic_ts, "builder") == "climr")) {
    stop(
      "Please use `historic_input_ts` function to create `historic_ts`.",
      " See `?historic_input_ts` for details."
    )
  }
  
  ## check for "silly" parameter combinations
  if (all(is.null(gcm), is.null(gcm_ts), is.null(gcm_hist), 
          is.null(historic), is.null(historic_ts))) {
    warning("'gcm', 'gcm_ts', 'gcm_hist', 'historic' and 'historic_ts' are missing. Nothing to downscale.")
  }
  
  return(invisible(NULL))
}

