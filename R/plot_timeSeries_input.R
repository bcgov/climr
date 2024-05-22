#' Input data for the time series climate change plot
#'
#' @description
#' Input data for the [`plot_timeSeries()`] function. Since these inputs are time-consuming to generate, 
#' the purpose of conducting the generation of the input table in a separate function is to allow users
#' to make multiple calls to [`plot_timeSeries()`] (e.g., for comparing different climate variables) 
#' without needing to generate the inputs each time.
#' 
#' @details
#' This function generates standardized inputs for one or multiple locations at any spatial scale.
#' If multiple locations are specified, the output is the average of the climate variables for all locations. 
#' 
#' Downloads of GCM time series take a long time. The `plot_timeSeries_input()` function can take >1hr 
#' to run for the first time it is called for a location. We are looking into ways to speed this up, but until then 
#' we recommend users dedicate some time to run this function in background. Once the time series are cached, they 
#' don't need to be downloaded again. 
#'
#' @template xyz
#' @inheritParams downscale
#'
#' @return `data.table` of average downscaled climate variables for all locations.
#'   
#' @examples
#' if(FALSE){
#' # data frame of arbitrary points
#' my_points <- data.frame(lon = c(-127.7300,-127.7500), lat = c(55.34114, 55.25), elev = c(711, 500), id = 1:2)
#' 
#' # generate the input data
#' my_data <- plot_timeSeries_input(my_points)
#' 
#' # use the input to create a plot
#' plot_timeSeries(my_data, variable1 = "Tmin_sm")
#' }
#' #' 
#' @export

plot_timeSeries_input <- function(
    xyz, 
    gcms = list_gcms(),
    ssps = list_ssps(),
    max_run = 10,
    obs_ts_dataset = c("cru.gpcc", "climatena"), 
    obs_years = 1901:2022,
    gcm_hist_years = 1850:2014, 
    gcm_ssp_years = 2015:2100, 
    vars = list_vars()
) {
  data <- downscale(xyz = xyz, 
                    gcms = gcms,
                    ssps = ssps,
                    max_run = max_run,
                    obs_ts_dataset = obs_ts_dataset, 
                    obs_years = obs_years,
                    gcm_hist_years = gcm_hist_years, 
                    gcm_ssp_years = gcm_ssp_years, 
                    vars = vars
  )
  data.agg <- data[, lapply(.SD, mean), by = .(GCM, SSP, RUN, PERIOD, DATASET), .SDcols = -c("id", "GCM", "SSP", "RUN", "PERIOD", "DATASET")]
  return(data.agg)
}

