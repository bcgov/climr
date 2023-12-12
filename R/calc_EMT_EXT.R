#' Calculate Extreme Minimum Temperature (EMT)
#'
#' @param t_min_list list of monthly minimum temperature for each month in order
#' @param td difference between the mean warmest monthly temperature and the mean coldest monthly temperature
#'
#' @return Extreme Minimum Temperature
#'
calc_EMT <- function(t_min_list, td) {
  tmin1 <- t_min_list[[1]]
  tmin12 <- t_min_list[[12]]

  year_min <- do.call(pmin, t_min_list)


  -23.02164 + 0.77908 * tmin1 + 0.67048 * tmin12 + 0.01075 * year_min^2 + 0.11565 * td
}



#' Calculate Extreme Maximum Temperature
#'
#' @param t_max_list list of monthly maximum temperature for each month in order
#' @param td difference between the mean warmest monthly temperature and the mean coldest monthly temperature
#'
#' @return Extreme Maximum Temperature
#'
calc_EXT <- function(t_max_list, td) {
  tmax7 <- t_max_list[[7]]
  tmax8 <- t_max_list[[8]]

  year_max <- do.call(pmax, t_max_list)


  10.64245 + -1.92005 * tmax7 + 0.04816 * tmax7^2 + 2.51176 * tmax8 - 0.03088 * tmax8^2 - 0.01311 * year_max^2 + 0.33167 * td - 0.001 * td^2
}
