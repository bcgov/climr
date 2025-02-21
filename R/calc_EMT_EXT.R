#' Calculate Extreme Minimum Temperature (EMT)
#' @template t_min_list
#' @template t_max_list
#' @template mcmt
#' @template mwmt
#' @template td
#' @export
#' @rdname climatevar
calc_EMT <- function(t_min_list, mcmt, td) {
  tmin1 <- t_min_list[[1]]
  tmin12 <- t_min_list[[12]]
  -23.02164 + 0.77908 * tmin1 + 0.67048 * tmin12 + 0.01075 * mcmt^2 + 0.11565 * td
}

#' Calculate Extreme Maximum Temperature
#' @export
#' @rdname climatevar
calc_EXT <- function(t_max_list, mwmt, td) {
  tmax7 <- t_max_list[[7]]
  tmax8 <- t_max_list[[8]]
  10.64245 + -1.92005 * tmax7 + 0.04816 * tmax7^2 + 2.51176 * tmax8 - 0.03088 * tmax8^2 - 0.01311 * mwmt^2 + 0.33167 * td - 0.001 * td^2
}

#' Mean Temperature of the Coldest Month (MCMT)
#' @export
#' @rdname climatevar
calc_MCMT <- function(t_min_list) {
  if (inherits(t_min_list[[1]], "SpatRaster")) {
    do.call(c, unname(t_min_list)) |> min()
  } else {
    do.call(pmin, t_min_list)
  }
}

#' Mean Temperature of the Warmest Month (MWMT)
#' @export
#' @rdname climatevar
calc_MWMT <- function(t_max_list) {
  if (inherits(t_max_list[[1]], "SpatRaster")) {
    do.call(c, unname(t_max_list)) |> max()
  } else {
    do.call(pmax, t_max_list)
  }
}

#' Continentality (TD)
#' @export
#' @rdname climatevar
calc_TD <- function(mcmt, mwmt) {
  mwmt - mcmt
}