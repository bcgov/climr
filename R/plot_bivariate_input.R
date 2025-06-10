#' Input data for the bivariate climate change plot
#'
#' @description
#' Input data for the [`plot_bivariate()`] function. Since these inputs are time-consuming to
#' generate, the purpose of conducting the generation of the input table in a separate function is
#' to allow users to make multiple calls to [`plot_bivariate()`] (e.g., for comparing different
#' climate variables) without needing to generate the inputs each time.
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
#'   my_data <- plot_bivariate_input(my_points)
#'
#'   # use the input to create a plot
#'   plot_bivariate(my_data, xvar = "MAT", yvar = "PAS_an")
#' }
#' #'
#' @export

plot_bivariate_input <- function(
    xyz,
    obs_period = list_obs_periods()[1],
    gcms = list_gcms(),
    ssps = list_ssps()[c(1:3)],
    gcm_periods = list_gcm_periods(),
    max_run = 10,
    vars = list_vars()) {
  data <- downscale(
    xyz = xyz,
    obs_period = obs_period,
    gcms = gcms,
    ssps = ssps,
    gcm_periods = gcm_periods,
    max_run = max_run,
    vars = vars,
    db_option = "database",
    cache = cache
  )
  data <- na.omit(data)
  return(data)
}