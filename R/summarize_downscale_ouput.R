
#' Summarize downscale for plot inputs
#' 
#' This function will summarize downscale to prepare input for plots
#' 
#' @inheritParams downscale
#' @param downscale_results optional, if you want to pass precomputed downscale results
#' @param vars character vector of the vars that needs to be summarize
#' @param use_downscale_db Should the function `downscale_db` be used instead of `downscale`
#' @param ... Additional arguments passed to `downscale` or `downscale_db`
#'
#' @return A data.table containing averaged climate variables by period, 
#'    including monthly temperature (average, min, max) and precipitation
#'    values (Tave, Tmin, Tmax, PPT) for each of the 12 months
#' @export
#' @examples
#' \dontrun{
#' in_xyz <- data.frame(lon = -127.7052, lat = 55.3557, elev = 291, id = 1)
#' summarize_downscale_output(in_xyz)
#' summarize_downscale_output(in_xyz, max_run = 1L, 
#'   gcm_hist_years = 1950:2015, 
#'   gcm_ssp_years = 2015:2040)
#' }

summarize_downscale_output <- function(xyz = NULL,
                                 gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
                                 ssps = list_ssps()[2],
                                 max_run = 10L,
                                 obs_years = list_obs_years(),
                                 gcm_hist_years = list_gcm_hist_years(),
                                 gcm_ssp_years = list_gcm_ssp_years(), 
                                 use_downscale_db = FALSE,
                                 cache = TRUE, 
                                 vars = NULL,
                                 downscale_results = NULL,
                                 ...){
  
  dwn_dt <- downscale_results
  # only compute downscale if there are not passed as input
  if (is.null(downscale_results)) {
    if (isTRUE(use_downscale_db)){
      
      dwn_dt <- downscale_db(xyz = xyz,
                             gcms = gcms,
                             ssps = ssps,
                             max_run = max_run,
                             obs_years = obs_years,
                             gcm_hist_years = gcm_hist_years,
                             gcm_ssp_years = gcm_ssp_years,
                             vars = vars,
                             return_refperiod = FALSE,
                             ...)
    }else{
      dwn_dt <- downscale(xyz = xyz,
                          gcms = gcms,
                          ssps = ssps,
                          max_run = max_run,
                          obs_years = obs_years,
                          gcm_hist_years = gcm_hist_years,
                          gcm_ssp_years = gcm_ssp_years,
                          vars = vars,
                          return_refperiod = FALSE,
                          cache = TRUE, ...)
    }
  }
  
  # default to all vars that are not descriptive
  if (is.null(vars)) {
    vars <- setdiff(names(dwn_dt), c("GCM", "SSP", "RUN", "PERIOD", "DATASET"))
  }
  
  
  if (!is.null(dwn_dt$RUN) && any(dwn_dt$RUN == "ensembleMean")){
    avg <- dwn_dt[RUN == "ensembleMean", lapply(.SD, mean), by = PERIOD, .SDcols = vars]  
  } else{
    avg <- dwn_dt[, lapply(.SD, mean), by = PERIOD, .SDcols = vars]
  }
  
  avg
}


