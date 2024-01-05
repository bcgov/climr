#' Calculate Precipitation As Snow (PAS)
#'
#' @template m
#' @param tm min temperature for that month
#' @param ppt numeric. Precipitation in mm.
#'
#' @return numeric. Precipitation As Snow
#'
#' @examples
#' \dontrun{
#' climr:::calc_PAS(4, 2, 600)
#' }
calc_PAS <- function(m, tm, ppt) {
  if (FALSE) {
    T0 <- a <- b <- NULL
  }

  param[["PAS"]][m, ppt * a / (1 + exp(-(tm - T0) / b))]
}
