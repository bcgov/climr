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
#' Although `which_refmap = "auto"` is the default, users are cautioned that
#' this can produce artefacts associated with downscaling to different reference
#' climate maps within and outside the western North American boundary of `refmap_climr`.
#' We recommend that queries spanning this boundary use `which_refmap = "refmap_climatena"`.
#'
#' @template xyz
#' @param which_refmap character. Which map of 1961-1990 climatological normals to use as the
#'   high-resolution reference climate map for downscaling. Default is "auto",
#'   which selects, for each query point, the best available climatological
#'   normals map in declining order of `"refmap_prism"`, `"refmap_climr"`, and `"refmap_climatena"`.
#'   Other options are one of [`list_refmaps()`], which will provide a consistent
#'   reference map for all points.
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
#' @template run_nm
#' @param cache logical. Cache data locally? Default `TRUE`
#' @param local logical. Is the postgres database local? Default `FALSE`
#' @param indiv_tiles logical. Only download necessary tiles instead of full bounding box? This will generally be faster, but doesn't cache.
#' @param ... other arguments passed to [`downscale_core()`]. Namely: `return_refperiod`,
#'   `vars`, `out_spatial` and `plot`

#' @return `data.table` of downscaled climate variables for each location.
#'   All outputs are returned in one table.

#' @importFrom sf st_as_sf st_join
#' @importFrom pool poolClose
#' @importFrom terra rast extract sources ext xres yres crop
#' @importFrom data.table getDTthreads setDTthreads rbindlist setkey
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
#'   which_refmap = "auto",
#'   return_refperiod = TRUE,
#'   obs_periods = "2001_2020",
#'   vars = vars,
#'   out_spatial = TRUE, plot = "PPT"
#' ) ## specify desired variables to plot
#'
#' ## as a data.table
#' climate_norms_hist <- downscale(
#'   xyz = in_xyz, which_refmap = "auto",
#'   return_refperiod = TRUE,
#'   vars = vars,
#'   out_spatial = FALSE, plot = "PPT"
#' ) ## specify desired variables to plot
#'
#' ## future projections for annual variables from three models
#' climate_norms_fut <- downscale(
#'   xyz = in_xyz, which_refmap = "auto",
#'   gcms = list_gcms()[c(1, 5, 6)],
#'   ssps = list_ssps()[2],
#'   gcm_periods = list_gcm_periods()[1:2],
#'   # gcm_ssp_years = 2020:2060,
#'   max_run = 3, #' we want 3 individual runs for the model
#'   vars = list_vars("Annual")
#' )
#'
#' @export
downscale <- function(xyz, which_refmap = "auto",
                      obs_periods = NULL,
                      obs_years = NULL,
                      obs_ts_dataset = NULL,
                      gcms = NULL, ssps = NULL,
                      gcm_periods = NULL, gcm_ssp_years = NULL,
                      gcm_hist_years = NULL, max_run = 0L,
                      run_nm = NULL,
                      cache = TRUE,
                      local = FALSE,
                      indiv_tiles = FALSE,
                      ...) {
  message("Welcome to climr!")

  ## checks
  .checkDwnsclArgs(
    xyz, which_refmap, obs_periods, obs_years, obs_ts_dataset,
    gcms, ssps, gcm_periods, gcm_ssp_years,
    gcm_hist_years, max_run, run_nm
  )

  expectedCols <- c("lon", "lat", "elev", "id")
  xyz <- .checkXYZ(copy(xyz), expectedCols)

  dbCon <- data_connect(local = local)
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
  if(which_refmap %in% c("refmap_climatena","refmap_prism","refmap_climr")){
    reference <- input_refmap(dbCon = dbCon, reference = which_refmap, bbox = thebb, cache = cache, indiv_tiles = indiv_tiles, xyz = xyz)
  } else {
    # message("Normals not specified, using highest resolution available for each point")
    rastFile <- system.file("extdata", "wna_outline.tif", package = "climr")
    ## if package is loaded with devtools::load_all, file won't be found and we need to pass .libPaths
    if (rastFile == "") {
      rastFile <- system.file("extdata", "wna_outline.tif", package = "climr", lib.loc = .libPaths())
    }
    bc_outline <- rast(rastFile)
    pnts <- extract(bc_outline, xyz[, .(lon, lat)], method = "simple")
    bc_ids <- xyz[["id"]][!is.na(pnts$PPT_01)]
    if (length(bc_ids) >= 1) {
      xyz_save <- xyz
      xyz <- xyz[!is.na(pnts$PPT_01), ]
      thebb_bc <- get_bb(xyz)
      message("for BC...")
      reference <- input_refmap(dbCon = dbCon, reference = "refmap_prism", bbox = thebb_bc, cache = cache, indiv_tiles = indiv_tiles, xyz = xyz)
    } else {
      reference <- input_refmap(dbCon = dbCon, reference = "refmap_climatena", bbox = thebb, cache = cache, indiv_tiles = indiv_tiles, xyz = xyz)
    }
  }

  if (!is.null(obs_periods)) {
    message("Getting observed anomalies...")
    obs_periods <- input_obs(dbCon, bbox = thebb, period = obs_periods, cache = cache)
  }
  if (!is.null(obs_years)) {
    obs_years <- input_obs_ts(dbCon,
      dataset = obs_ts_dataset,
      bbox = thebb, years = obs_years, cache = cache
    )
  }

  if (!is.null(gcms)) {
    if (!is.null(gcm_periods)) {
      message("Getting GCMs...")
      gcm_ssp_periods <- input_gcms(dbCon,
        bbox = thebb, gcms = gcms,
        ssps = ssps,
        period = gcm_periods,
        max_run = max_run,
        run_nm = run_nm,
        cache = cache
      )
    } else {
      gcm_ssp_periods <- NULL
    }
    if (!is.null(gcm_ssp_years)) {
      gcm_ssp_ts <- input_gcm_ssp(dbCon,
        bbox = thebb, gcms = gcms,
        ssps = ssps,
        years = gcm_ssp_years,
        max_run = max_run,
        cache = cache,
        run_nm = run_nm,
        fast = TRUE
      )
    } else {
      gcm_ssp_ts <- NULL
    }
    if (!is.null(gcm_hist_years)) {
      gcm_hist_ts <- input_gcm_hist(dbCon,
        bbox = thebb, gcms = gcms,
        years = gcm_hist_years,
        max_run = max_run,
        run_nm = run_nm,
        cache = cache
      )
    } else {
      gcm_hist_ts <- NULL
    }
  } else {
    gcm_ssp_periods <- gcm_ssp_ts <- gcm_hist_ts <- NULL
  }

  message("Downscaling...")
  results <- downscale_core(
    xyz = xyz,
    refmap = reference,
    obs = obs_periods,
    obs_ts = obs_years,
    gcms = gcm_ssp_periods,
    gcm_ssp_ts = gcm_ssp_ts,
    gcm_hist_ts = gcm_hist_ts,
    ...
  )

  if (which_refmap != "auto") {
    if (!is.null(dbCon)) poolClose(dbCon)
    results <- addIDCols(origID, results)
    return(results)
  }
  if ((length(bc_ids) < 1 || length(bc_ids) == nrow(xyz_save))) {
    if (!is.null(dbCon)) poolClose(dbCon)
    results <- addIDCols(origID, results)
    return(results)
  } else {
    na_xyz <- xyz_save[!xyz_save[, 4] %in% bc_ids, ]
    thebb <- get_bb(na_xyz)
    message("Now North America...")
    reference <- input_refmap(dbCon = dbCon, reference = "refmap_climatena", bbox = thebb, cache = cache, indiv_tiles = indiv_tiles, xyz = xyz)

    results_na <- downscale_core(
      xyz = na_xyz,
      refmap = reference,
      obs = obs_periods,
      obs_ts = obs_years,
      gcms = gcm_ssp_periods,
      gcm_ssp_ts = gcm_ssp_ts,
      gcm_hist_ts = gcm_hist_ts,
      ...
    )

    if (!is.null(dbCon)) poolClose(dbCon)
    res_all <- rbind(results, results_na)
    res_all <- addIDCols(origID, res_all)

    return(res_all)
  }
}


#' Check `downscale` arguments
#'
#' @inheritParams downscale
#'
#' @return NULL
#' @noRd
.checkDwnsclArgs <- function(xyz, which_refmap = NULL, obs_periods = NULL, obs_years = NULL,
                             obs_ts_dataset = NULL, gcms = NULL, ssps = list_ssps(), gcm_periods = NULL, gcm_ssp_years = NULL,
                             gcm_hist_years = NULL, max_run = 0L, run_nm = NULL) {
  if (is.null(ssps) & (!is.null(gcm_periods) | !is.null(gcm_ssp_years))) {
    stop("ssps must be specified")
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
    if (any(!obs_ts_dataset %in% c("cru.gpcc", "climatena"))) {
      stop("obs_ts_dataset must be 'cru.gpcc', 'climatena', or 'both'")
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

  if ((!is.null(max_run) | max_run > 0) &
    is.null(gcms)) {
    message("'gcms' is missing. 'max_run' will be ignored")
  }
  return(invisible(NULL))
}
