#' Self-contained change-factor downscaling of observed and simulated climate data
#' 
#' @description
#' `climr_downscale()` provides downscaled climate variables for user-specified locations. 
#' `climr_downscale()` adapts a simple change-factor (aka "delta') downscaling approach originally implemented in \href{https://climatena.ca/}{ClimateNA}. 
#' This approach downscales climate data in three stages: 
#' \enumerate{
#' \item \emph{Change-factor downscaling} of coarse-resolution (50-200km grid) monthly temperature and precipitation data from climate models or observational sources to high-resolution (800m grid);
#' \item \emph{Elevation adjustment} of temperature variables to provide scales finer than the high-resolution reference grid; and
#' \item \emph{Calculating derived variables} from the downscaled monthly temperature and precipitation variables.
#' }
#' See our vignette \code{\link{methods_downscaling.Rmd}} for a description of the downscaling methodology. 
#'  
#' `climr_downscale()` is a user-friendly wrapper for `downscale()`
#'  
#' @details
#' [`downscale()`] parameters can be applied in `climr_downscale()`. For example, setting `ppt_lr = TRUE` in `climr_downscale()` will apply elevation adjustment to precipitation values. 
#' 
#' Although `which_normal = "auto"` is the default, users are cautioned that this can produce artefacts associated with downscaling to different reference climate maps within and outside the western North American boundary of `normal_composite`. 
#' We recommend that queries spanning this boundary use `which_normal = "normal_na"`.
#' 
#' @template xyz
#' @param which_normal character. Which climatological normals map to use as the high-resolution reference climate map for downscaling. 
#'   Default is "auto", which selects, for each query point, the best available climatological normals map in declining order of normals_bc, normals_composite, and normals_na. 
#'   Other options are one of [`list_normal()`], which will provide a consistent reference map for all points. 
#' @param historic_period character. Which historic period for observed climate data, averaged over this period. Options are [`list_historic()`]. Default `NULL`
#' @param historic_ts integer. Vector of years to obtain individual years or time series of observed climate data. Must be in `1902:2015`. Default `NULL`
#' @param gcm_models character. Vector of global climate model names. Options are [`list_gcm()`]. Used for gcm periods, gcm timeseries, and historic timeseries. Default `NULL`
#' @template ssp
#' @param gcm_period character. 20-year normal periods for GCM simulations. Options are [`list_gcm_period()`]
#' @param gcm_ts_years character. Timeseries years for GCM simulations of future scenarios specified by `ssp`. Must be in `2015:2100`
#' @param gcm_hist_years character.  Timeseries years for GCM simulations of the historical scenario. Must be in `1851:2014`
#' @template max_run
#' @template return_normal
#' @template vars
#' @param cache logical. Cache data locally? Default `TRUE`
#' @template out_spatial
#' @template plot

#' @return `data.frame` of downscaled climate variables for each location.
#'   All outputs are returned in one table.

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
#' @export
climr_downscale <- function(xyz, which_normal = c("auto", list_normal()), historic_period = NULL, historic_ts = NULL,
                            gcm_models = NULL, ssp = list_ssp(),
                            gcm_period = NULL, gcm_ts_years = NULL, gcm_hist_years = NULL, max_run = 0L, return_normal = TRUE,
                            vars = sort(sprintf(c("PPT%02d", "Tmax%02d", "Tmin%02d"), sort(rep(1:12, 3)))), cache = TRUE,
                            out_spatial = FALSE, plot = NULL, ...) {
  message("Welcome to climr!")
  
  ## checks
  .checkClimrDwnsclArgs(xyz, which_normal, historic_period, historic_ts, 
                        gcm_models, ssp, gcm_period, gcm_ts_years, 
                        gcm_hist_years, max_run, vars)
  
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
  if (which_normal == "normal_na") {
    normal <- normal_input(dbCon = dbCon, normal = "normal_na", bbox = thebb, cache = cache)
  } else if (which_normal == "normal_bc") {
    normal <- normal_input(dbCon = dbCon, normal = "normal_bc", bbox = thebb, cache = cache)
  } else if (which_normal == "normal_composite") {
    normal <- normal_input(dbCon = dbCon, normal = "normal_composite", bbox = thebb, cache = cache)
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
      normal <- normal_input(dbCon = dbCon, normal = "normal_composite", bbox = thebb_bc, cache = cache)
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
    plot = plot, 
    ...
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
      plot = plot, 
      ...
    )
    
    if(!is.null(dbCon)) poolClose(dbCon)
    res_all <- rbind(results, results_na)
    res_all <- addIDCols(origID, res_all)
    
    return(res_all)
  }
}
