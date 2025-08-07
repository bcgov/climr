#' Input data for the Walter-Lieth climate diagram
#'
#' @description
#' Input data for the [`plot_WalterLieth()`] function. Since these inputs are time-consuming to
#' generate, the purpose of conducting the generation of the input table in a separate function is
#' to allow users to make multiple calls to [`plot_WalterLieth()`] (e.g., for comparing different
#' time periods) without needing to generate the inputs each time.
#'
#' @details
#' This function generates standardized inputs for one or multiple locations at any spatial scale.
#'
#' @template xyz
#' @inheritParams downscale
#' @template vars
#'
#' @return `data.table` of average downscaled climate variables for all locations.
#'
#' @examples
#' if (FALSE) {
#'   # data frame of arbitrary points
#'   my_points <- data.frame(
#'     lon = c(-127.7300, -127.7500),
#'     lat = c(55.34114, 55.25),
#'     elev = c(711, 500),
#'     id = 1:2
#'   )
#'
#'   # generate the input data
#'   my_data <- plot_WalterLieth_input(my_points, db_option = "local")
#'
#'   # use the input to create a plot
#'   plot_WalterLieth(my_data)
#' }
#' #'
#' @export

plot_WalterLieth_input <- function(
    xyz,
    obs_period = NULL,
    gcms = NULL,
    ssps = NULL,
    gcm_periods = NULL,
    db_option = "auto") {
  data <- downscale(
    xyz = xyz,
    obs_period = obs_period,
    gcms = gcms,
    ssps = ssps,
    gcm_periods = gcm_periods,
    db_option = db_option
  )
  data <- merge(data, data.frame(id = xyz$id, elev = xyz$elev), by = 'id')
  # calculate the mean value if there are multiple locations
  if(is.null(gcms)){
      data.agg <- data[, lapply(.SD, mean), by = .(PERIOD), .SDcols = -c("id", "PERIOD")]
  } else {
      data.agg <- data[, lapply(.SD, mean), by = .(GCM, SSP, RUN, PERIOD), .SDcols = -c("id", "GCM", "SSP", "RUN", "PERIOD")]
  }
  return(data.agg)
}
