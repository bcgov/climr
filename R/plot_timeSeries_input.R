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
#' @template xyz
#' @inheritParams climr_downscale
#'
#' @return `data.table` of average downscaled climate variables for all locations.
#'   
#' @examples
#' # data frame of arbitrary points
#' my_points <- data.frame(lon = c(-127.7300,-127.7500), lat = c(55.34114, 55.25), elev = c(711, 500), id = 1:2)
#'
#' # generate the input data
#' my_data <- plot_timeSeries_input(my_points)
#'
#' # use the input to create a plot
#' plot_timeSeries(my_data, variable1 = "Tmin_sm")
#' 
#' @export

plot_timeSeries_input <- function(
    xyz, 
    gcm_models = list_gcm(),
    ssp = list_ssp(),
    max_run = 10,
    historic_ts_dataset = "cru.gpcc", 
    historic_ts = 1901:2022,
    gcm_hist_years = 1850:2014, 
    gcm_ts_years = 2015:2100, 
    vars = list_variables()
) {
  data <- climr_downscale(xyz = xyz, 
                          gcm_models = gcm_models,
                          ssp = ssp,
                          max_run = max_run,
                          historic_ts_dataset = historic_ts_dataset, 
                          historic_ts = historic_ts,
                          gcm_hist_years = gcm_hist_years, 
                          gcm_ts_years = gcm_ts_years, 
                          vars = vars
  )
  data.agg <- data[, lapply(.SD, mean), by = .(GCM, SSP, RUN, PERIOD), .SDcols = -c("id", "GCM", "SSP", "RUN", "PERIOD")]
  return(data.agg)
}

