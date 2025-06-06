#' Self-contained change-factor downscaling of observed and simulated climate data
#'
#' @description
#' `downscale()` provides downscaled climate variables for user-specified
#'  locations.
#'  It adapts a simple change-factor (aka "delta') downscaling
#'  approach originally implemented in \href{https://climatena.ca/}{ClimateNA}.
#'  This approach downscales climate data in three stages:
#' \enumerate{
#' \item \emph{Change-factor downscaling} of coarse-resolution (50-200km grid) monthly temperature and precipitation data from climate models or observational sources to high-resolution (800m grid);
#' \item \emph{Elevation adjustment} of temperature variables to provide scales finer than the high-resolution reference grid; and
#' \item \emph{Calculating derived variables} from the downscaled monthly temperature and precipitation variables.
#' }
#' See \code{vignette("methods_downscaling")} for a description of the downscaling methodology.
#'
#' `downscale()` is a user-friendly wrapper for `downscale_core()`
#'
#' @details
#' [`downscale_core()`] parameters can be applied in `downscale()`. For example,
#' setting `ppt_lr = TRUE` in `downscale()` will apply elevation adjustment to precipitation values.
#'
#'
#' @template xyz
#' @param which_refmap character. Which map of 1961-1990 climatological normals to use as the
#'   high-resolution reference climate map for downscaling. Default is `"refmap_climr"`.
#'   Other options are one of [`list_refmaps()`].
#' @param obs_periods character. Which historical period for observational climate
#'   data, averaged over this period. Options are [`list_obs_periods()`].
#' @param obs_years integer. Vector of years to obtain individual years or time
#'   series of observational climate data. See [`list_obs_years()`] for available years.
#' @param obs_ts_dataset character. The dataset to use for observational time series data. Options
#'   are `"climatena"` for the ClimateNA gridded time series or `"cru.gpcc"` for the combined Climatic
#'   Research Unit TS dataset (for temperature) and Global Precipitation Climatology Centre dataset
#'   (for precipitation). 
#' @param gcms character. Vector of global climate model names. Options are [`list_gcms()`]. 
#' @template ssps
#' @param gcm_periods character. 20-year reference periods for GCM simulations.
#'   Options are [`list_gcm_periods()`]. 
#' @param gcm_ssp_years character. Timeseries years for GCM simulations of future
#'   scenarios specified by `ssps`. See [`list_gcm_ssp_years()`] for available years.
#' @param gcm_hist_years character.  Timeseries years for GCM simulations of the
#'   historical scenario. See [`list_gcm_hist_years()`] for available years.
#' @template max_run
#' @template ensemble_mean
#' @template run_nm
#' @param cache logical. Cache data locally? Default `TRUE`
#' @param local logical. Is the postgres database local? Default `FALSE`
#' @param indiv_tiles logical. Only download necessary tiles instead of full bounding box?
#' This will generally be faster, but doesn't cache.
#' @param db_option character. One of `auto`, `database`, or `local`. Default `auto`. 
#' @param return_refperiod logical. Return 1961-1990 period? Default `TRUE`
#' @param ... other arguments passed to [`downscale_core()`]. Namely:
#'   `vars`, `out_spatial` and `plot`
#'   
#' @return `data.table` or `SpatRaster` of downscaled climate variables for each location.
#'   All outputs are returned in one table. If output is `SpatRaster`, each layer corresponds to a variable.
#' @importFrom sf st_as_sf st_join
#' @importFrom pool poolClose
#' @importFrom terra rast extract sources ext xres yres crop
#' @importFrom data.table getDTthreads setDTthreads rbindlist setkey
#' 
#' @details
#' The standard climr method, when `db_option = "local"` downloads and optionally caches raster data, 
#' then does the processing locally. Option `database` submits points to the climr database, 
#' and processes them on the database server. This is generally faster for a) very few points, or b) 
#' timeseries with many layers. Option `auto` attempts to intelligently combine these methods depending on the input:
#' if fewer than 5 points are submitted, all downscaling will be done on the database, otherwise period downscaling will
#' be done locally, and timeseries will be done on the database. 
#' 
#'
#' @examples
#'
#' ## provide or create a dataframe of lon, lat, elev, and id - usually read from csv file
#' in_xyz <- data.frame(
#'   lon = c(-127.7052, -127.6227, -127.5623, -127.7162, -127.1858, -127.125, -126.9495, -126.9550),
#'   lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
#'   elev = c(291, 296, 626, 377, 424, 591, 723, 633),
#'   id = 1:8
#' )
#'
#' ## historic observational time series
#' vars <- c("PPT", "CMD", "Tave_07")
#' climate_norms_hist <- downscale(
#'   xyz = in_xyz,
#'   which_refmap = "refmap_climr",
#'   return_refperiod = TRUE,
#'   obs_periods = "2001_2020",
#'   vars = vars,
#'   out_spatial = TRUE, plot = "PPT"
#' ) ## specify desired variables to plot
#'
#' ## as a data.table
#' climate_norms_hist <- downscale(
#'   xyz = in_xyz, which_refmap = "refmap_climr",
#'   return_refperiod = TRUE,
#'   vars = vars,
#'   out_spatial = FALSE, plot = "PPT"
#' ) ## specify desired variables to plot
#'
#' ## future projections for annual variables from three models
#' climate_norms_fut <- downscale(
#'   xyz = in_xyz, which_refmap = "refmap_climr",
#'   gcms = list_gcms()[c(1, 5, 6)],
#'   ssps = list_ssps()[2],
#'   gcm_periods = list_gcm_periods()[1:2],
#'   # gcm_ssp_years = 2020:2060,
#'   max_run = 3, #' we want 3 individual runs for the model
#'   vars = list_vars("Annual")
#' )
#'
#' @export
downscale <- function(xyz, which_refmap = "refmap_climr",
                      obs_periods = NULL,
                      obs_years = NULL,
                      obs_ts_dataset = NULL,
                      gcms = NULL, ssps = NULL,
                      gcm_periods = NULL, gcm_ssp_years = NULL,
                      gcm_hist_years = NULL, 
                      ensemble_mean = TRUE,
                      max_run = 0L,
                      run_nm = NULL,
                      cache = TRUE,
                      local = FALSE,
                      indiv_tiles = FALSE,
                      db_option = "auto",
                      return_refperiod = TRUE,
                      ...) {
  message("Welcome to climr!")

  ## checks
  .checkDwnsclArgs(
    xyz, which_refmap, db_option, obs_periods, obs_years, obs_ts_dataset,
    gcms, ssps, gcm_periods, gcm_ssp_years,
    gcm_hist_years, max_run, run_nm
  )

  if (inherits(xyz, "SpatRaster")) {
    if(db_option == "database") warning("Database downscaling is currently not supported for rasters. Switching to local version.")
    db_option <- "local"
    if(nlyr(xyz) > 1) stop("Please supply a single layer SpatRaster containing elevation values in metres.")
    names(xyz) <- "elev"
  } else {
    expectedCols <- c("lon", "lat", "elev", "id")
    xyz <- .checkXYZ(copy(xyz), expectedCols)
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
  }

  thebb <- get_bb(xyz) ## get bounding box based on input points
  db_ts <- db_option
  if((db_option == "auto" & nrow(xyz) < 5) | db_option == "database"){
    db_option <- "database"
  } else {
    db_option <- "local"
  }
  
  if(which_refmap %in% c("refmap_climatena","refmap_climr")) {
    reference_db <- reference <- NULL
  } else {
    stop("Unknown which_refmap parameter")
  }

  obs_periods_db <- obs_periods_reg <- obs_years_reg <- obs_years_db <- NULL
  if (!is.null(obs_periods)) {
    message("Getting observed anomalies...")
    if(db_option == "database"){
      obs_periods_db <- input_obs_db(period = obs_periods)
    } else {
      obs_periods_reg <- input_obs(bbox = thebb, period = obs_periods, cache = cache)
    }
  }
  if (!is.null(obs_years)) { ##should probably also have a local option here
    if(db_ts == "local"){
      obs_years_reg <- input_obs_ts(
                                dataset = obs_ts_dataset,
                                bbox = thebb, years = obs_years, cache = cache
      )
    } else {
      obs_years_db <- input_obs_ts_db(dataset = obs_ts_dataset, years = obs_years)
    }  
  }
  
  gcm_ssp_periods <- gcm_ssp_periods_db <- gcm_ssp_ts <- gcm_ssp_ts_reg <- gcm_hist_ts <- gcm_hist_ts_reg <- NULL
  if (!is.null(gcms)) {
    if (!is.null(gcm_periods)) {
      message("Getting GCMs...")
      if(db_option == "database"){
        gcm_ssp_periods_db <- input_gcms_db(
          gcms = gcms,
          ssps = ssps,
          period = gcm_periods,
          max_run = max_run,
          ensemble_mean = ensemble_mean,
          run_nm = run_nm
        )
      } else {
        gcm_ssp_periods <- input_gcms(bbox = thebb, gcms = gcms,
                                      ssps = ssps,
                                      period = gcm_periods,
                                      max_run = max_run,
                                      ensemble_mean = ensemble_mean,
                                      run_nm = run_nm,
                                      cache = cache
        )
      }
      
    }
    if (!is.null(gcm_ssp_years)) {
      if(db_ts == "local"){
        gcm_ssp_ts_reg <- input_gcm_ssp(bbox = thebb, gcms = gcms,
                                    ssps = ssps,
                                    years = gcm_ssp_years,
                                    max_run = max_run,
                                    ensemble_mean = ensemble_mean,
                                    cache = cache,
                                    run_nm = run_nm,
                                    fast = FALSE
        )
      } else {
        gcm_ssp_ts <- input_gcm_ssp_db(
          gcms = gcms,
          ssps = ssps,
          years = gcm_ssp_years,
          max_run = max_run,
          ensemble_mean = ensemble_mean,
          run_nm = run_nm
        )
      }

    } else {
      gcm_ssp_ts <- gcm_ssp_ts_reg <- NULL
    }
    if (!is.null(gcm_hist_years)) {
      if(db_ts == "local"){
        gcm_hist_ts_reg <- input_gcm_hist(bbox = thebb, gcms = gcms,
                                      years = gcm_hist_years,
                                      max_run = max_run,
                                      ensemble_mean = ensemble_mean,
                                      run_nm = run_nm,
                                      cache = cache
        )
      } else {
        gcm_hist_ts <- input_gcm_hist_db(
          gcms = gcms,
          years = gcm_hist_years,
          max_run = max_run,
          ensemble_mean = ensemble_mean,
          run_nm = run_nm
        )
      }
      
    }
  }

  results <- results_ts <- NULL
  if(any(!is.null(c(obs_periods_db, obs_years_db, gcm_ssp_periods_db, gcm_ssp_ts, gcm_hist_ts)))) return_refperiod <- FALSE
  if(any(!is.null(c(obs_periods_reg, obs_years_reg, gcm_ssp_periods, gcm_ssp_ts_reg, gcm_hist_ts_reg))) | (return_refperiod & db_option == "local")){
    reference <- input_refmap(reference = which_refmap, bbox = thebb, cache = cache, indiv_tiles = indiv_tiles, xyz = xyz)
    message("Downscaling...")
    results <- downscale_core(
      xyz = xyz,
      refmap = reference,
      obs = obs_periods_reg,
      obs_ts = obs_years_reg,
      gcms = gcm_ssp_periods,
      gcm_ssp_ts = gcm_ssp_ts_reg,
      gcm_hist_ts = gcm_hist_ts_reg,
      skip_check = TRUE,
      return_refperiod = return_refperiod,
      ...
    )
  }
  
  if(any(!is.null(c(obs_periods_db, obs_years_db, gcm_ssp_periods_db, gcm_ssp_ts, gcm_hist_ts)))){
    write_xyz(xyz)
    reference_db <- input_refmap_db(reference = which_refmap)
    message("Downscaling in database...")
    results_ts <- downscale_db_core(
      xyz = xyz,
      refmap = reference_db,
      obs = obs_periods_db,
      obs_ts = obs_years_db,
      gcms = gcm_ssp_periods_db,
      gcm_ssp_ts = gcm_ssp_ts,
      gcm_hist_ts = gcm_hist_ts,
      ...
    )
  }
  
  if (inherits(xyz, "SpatRaster")) return(results)
  results <- rbind(results, results_ts, fill = TRUE)
  results <- addIDCols(origID, results)
  return(results)
}


downscale_db <- function(
  xyz,
  which_refmap = "refmap_climr",
  obs_periods = NULL,
  obs_years = NULL,
  obs_ts_dataset = NULL,
  gcms = NULL,
  ssps = NULL,
  gcm_periods = NULL,
  gcm_ssp_years = NULL,
  gcm_hist_years = NULL,
  max_run = 0L,
  run_nm = NULL,
  local = FALSE,
  ...
) {
  ## checks
  .checkDwnsclArgs(
    xyz, which_refmap, obs_periods, obs_years, obs_ts_dataset,
    gcms, ssps, gcm_periods, gcm_ssp_years,
    gcm_hist_years, max_run, run_nm
  )
  
  expectedCols <- c("lon", "lat", "elev", "id")
  xyz <- .checkXYZ(copy(xyz), expectedCols)
  if (is.null(attr(xyz, "hull"))) {
    attr(xyz, "hull") <- terra::vect(xyz, geom = c("lon", "lat"), crs = "EPSG:4326") |>
      terra::convHull() |>
      terra::geom(wkt = TRUE)
  }
  #dbCon <- data_con(if (local) "local")
 
  rmCols <- setdiff(names(xyz), expectedCols)
  if (length(rmCols)) { ## remove extraneous columns
    warnings("Extra columns will be ignored")
    xyz <- xyz[, ..expectedCols]
  }

  if(which_refmap %in% c("refmap_climatena","refmap_climr")){
    reference <- input_refmap_db(reference = which_refmap)
  } else {
    stop("Unknown `which_refmap` parameter.")
  }
  if (!is.null(obs_periods)) {
    obs_periods <- input_obs_db(period = obs_periods)
  }
  if (!is.null(obs_years)) {
    obs_years <- input_obs_ts_db(dataset = obs_ts_dataset, years = obs_years)
  }

  if (!is.null(gcms)) {
    if (!is.null(gcm_periods)) {
      gcm_ssp_periods <- input_gcms_db(
        gcms = gcms,
        ssps = ssps,
        period = gcm_periods,
        max_run = max_run,
        run_nm = run_nm
      )
    } else {
      gcm_ssp_periods <- NULL
    }
    if (!is.null(gcm_ssp_years)) {
      gcm_ssp_ts <- input_gcm_ssp_db(
        gcms = gcms,
        ssps = ssps,
        years = gcm_ssp_years,
        max_run = max_run,
        run_nm = run_nm
      )
    } else {
      gcm_ssp_ts <- NULL
    }
    if (!is.null(gcm_hist_years)) {
      gcm_hist_ts <- input_gcm_hist_db(
        gcms = gcms,
        years = gcm_hist_years,
        max_run = max_run,
        run_nm = run_nm
      )
    } else {
      gcm_hist_ts <- NULL
    }
  } else {
    gcm_ssp_periods <- gcm_ssp_ts <- gcm_hist_ts <- NULL
  }
  
  write_xyz(xyz)
  
  message("Downscaling...")
  results <- downscale_db_core(
    xyz = xyz,
    refmap = reference,
    obs = obs_periods,
    obs_ts = obs_years,
    gcms = gcm_ssp_periods,
    gcm_ssp_ts = gcm_ssp_ts,
    gcm_hist_ts = gcm_hist_ts,
    return_refperiod = FALSE,
    ...
  )
  return(results)
}


#' Check `downscale` arguments
#'
#' @inheritParams downscale
#'
#' @return NULL
#' @noRd
.checkDwnsclArgs <- function(xyz, which_refmap = NULL, db_option = NULL, obs_periods = NULL, obs_years = NULL,
                             obs_ts_dataset = NULL, gcms = NULL, ssps = list_ssps(), gcm_periods = NULL, gcm_ssp_years = NULL,
                             gcm_hist_years = NULL, max_run = 0L, run_nm = NULL) {
  if (is.null(ssps) & (!is.null(gcm_periods) | !is.null(gcm_ssp_years))) {
    stop("ssps must be specified")
  }
  
  if(!db_option %in% c("auto","database","local")) {
    stop("db_option must be one of `auto`, `database` or `local`.")
  }
  
  if(!is.null(run_nm) & max_run > 1){
    warning("max_run is > 0, but run_nm is specified. Only named runs will be returned.")
  }

  if (!is.null(ssps)) {
    notSupportedSsps <- setdiff(ssps, list_ssps())
    if (length(notSupportedSsps)) {
      stop(
        "The following SSPs are not supported:",
        "\n  ", paste(notSupportedSsps, collapse = ", "),
        "\n  Please see 'list_ssps' for list of supported SSPs."
      )
    }
  }

  if (!is.null(which_refmap)) {
    which_refmap <- match.arg(which_refmap, c("auto", list_refmaps()))
  }

  if (!is.null(obs_periods)) {
    notSupportedPeriods <- setdiff(obs_periods, list_obs_periods())
    if (length(notSupportedPeriods)) {
      stop(
        "The following observed periods are not supported:",
        "\n  ", paste(notSupportedPeriods, collapse = ", "),
        "\n  Please see 'list_obs_periods' for list of supported periods."
      )
    }
  }

  if (!is.null(obs_years)) {
    if (!all(obs_years %in% list_obs_years())) {
      stop(
        "'obs_years' must be in ", range(list_obs_years())[1], ":",
        range(list_obs_years())[2]
      )
    }
  }

  if (!is.null(obs_ts_dataset)) {
    if (any(!obs_ts_dataset %in% c("mswx.blend", "cru.gpcc", "climatena"))) {
      stop("obs_ts_dataset must be one or more of 'mswx.blend', 'cru.gpcc', 'climatena'.")
    }
    if (is.null(obs_years)) {
      stop("'obs_years' must be specified")
    }
  }

  if (!is.null(gcms)) {
    notSupportedGCMs <- setdiff(gcms, list_gcms())
    if (length(notSupportedGCMs)) {
      stop(
        "The following GCMs are not supported:",
        "\n  ", paste(notSupportedGCMs, collapse = ", "),
        "\n  Please see 'list_gcms' for list of supported GCMs."
      )
    }
  }

  if (!is.null(gcm_periods)) {
    notSupportedGCMPs <- setdiff(gcm_periods, list_gcm_periods())
    if (length(notSupportedGCMPs)) {
      stop(
        "The following projected periods are not supported:",
        "\n  ", paste(notSupportedGCMPs, collapse = ", "),
        "\n  Please see 'list_gcm_periods' for list of supported periods."
      )
    }
  }

  if (!is.null(gcm_ssp_years)) {
    if (!all(gcm_ssp_years %in% list_gcm_ssp_years())) {
      stop(
        "'gcm_ssp_years' must be in ", range(list_gcm_ssp_years())[1], ":",
        range(list_gcm_ssp_years())[2]
      )
    }
  }

  msg <- "'max_run' must be 0 or larger"
  if (!inherits(max_run, c("integer", "numeric"))) {
    stop(msg)
  } else if (max_run < 0) {
    stop(msg)
  }

  ## check for "silly" parameter combinations
  if (!is.null(gcms) &
    all(is.null(gcm_hist_years), is.null(gcm_ssp_years), is.null(gcm_periods), is.null(ssps))) {
    message("'gcms' will be ignored, since 'gcm_hist_years', 'gcm_ssp_years', 'gcm_periods' and 'ssps' are missing")
  }

  if (is.null(gcms) &
    any(!is.null(gcm_hist_years), !is.null(gcm_ssp_years), !is.null(gcm_periods), !is.null(ssps))) {
    message("'gcms' is missing. 'gcm_hist_years', 'gcm_ssp_years', 'gcm_periods' and 'ssps' will be ignored")
  }

  if ((!is.null(max_run) & max_run > 0) &
    is.null(gcms)) {
    message("'gcms' is missing. 'max_run' will be ignored")
  }
  return(invisible(NULL))
}


##code for checking bounding box
# if (inherits(xyz, "SpatRaster")) {
#   refmapchck <- \(rf, bb) {
#     q <- "
#         SELECT min(ST_UpperLeftX(rast)) xmin,
#                max(ST_UpperLeftX(rast)+ST_Width(rast)*ST_PixelWidth(rast)) xmax,
#                min(ST_UpperLeftY(rast)-ST_Height(rast)*abs(ST_PixelHeight(rast))) ymin,
#                max(ST_UpperLeftY(rast)) ymax
#         FROM %s" |> sprintf(rf)
#     normalhull <- db_safe_query(q) |> 
#       unlist() |>
#       terra::ext() |>
#       terra::vect(crs = "EPSG:4326")
#     bchck <- terra::ext(bb["xmin"], bb["xmax"], bb["ymin"], bb["ymax"]) |> terra::vect(crs = "EPSG:4326")
#     terra::is.related(normalhull, bchck, relation = "contains")
#   }
#   reference <- "refmap_prism"
#   if (!refmapchck("normal_bc", thebb)) { 
#     reference <- "refmap_climr"
#   }
#   if (!refmapchck("normal_composite", thebb)) { 
#     reference <- "refmap_climatena"
#   }
#   reference <- input_refmap(dbCon = dbCon, reference = reference, bbox = thebb, cache = cache, indiv_tiles = indiv_tiles, xyz = xyz)
# } else {
#   # message("Normals not specified, using highest resolution available for each point")
#   rastFile <- system.file("extdata", "bc_outline.tif", package = "climr")
#   ## if package is loaded with devtools::load_all, file won't be found and we need to pass .libPaths
#   if (rastFile == "") {
#     rastFile <- system.file("extdata", "bc_outline.tif", package = "climr", lib.loc = .libPaths())
#   }
#   bc_outline <- rast(rastFile)
#   pnts <- extract(bc_outline, xyz[, .(lon, lat)], method = "simple")
#   bc_ids <- xyz[["id"]][!is.na(pnts[[2]])]
#   if (length(bc_ids) >= 1) {
#     xyz_save <- xyz
#     xyz <- xyz[!is.na(pnts[[2]]), ]
#     thebb_bc <- get_bb(xyz)
#     message("for BC...")
#     reference <- input_refmap(dbCon = dbCon, reference = "refmap_prism", bbox = thebb_bc, cache = cache, indiv_tiles = indiv_tiles, xyz = xyz)
#   } else {
#     reference <- input_refmap(dbCon = dbCon, reference = "refmap_climatena", bbox = thebb, cache = cache, indiv_tiles = indiv_tiles, xyz = xyz)
#   }
# }
# if ((length(bc_ids) < 1 || length(bc_ids) == nrow(xyz_save))) {
#   results <- addIDCols(origID, results)
#   return(results)
# } else {
#   na_xyz <- xyz_save[!xyz_save[, 4] %in% bc_ids, ]
#   thebb <- get_bb(na_xyz)
#   message("Now North America...")
#   reference <- input_refmap(dbCon = dbCon, reference = "refmap_climatena", bbox = thebb, cache = cache, indiv_tiles = indiv_tiles, xyz = xyz)
#   
#   results_na <- downscale_core(
#     xyz = na_xyz,
#     refmap = reference,
#     obs = obs_periods,
#     obs_ts = obs_years,
#     gcms = gcm_ssp_periods,
#     gcm_ssp_ts = gcm_ssp_ts,
#     gcm_hist_ts = gcm_hist_ts,
#     skip_check = TRUE,
#     ...
#   )
#   
#   res_all <- rbind(results, results_na)
#   res_all <- addIDCols(origID, res_all)
#   
#   return(res_all)
# }
