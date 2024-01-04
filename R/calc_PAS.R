#' Calculate Precipitation As Snow (PAS)
#'
#' @template m
#' @template tmin
#' @template PPT
#'
#' @return numeric. Precipitation As Snow
#'
#' @examples
#' \dontrun{
#' climr:::calc_PAS(4, 2, 600)
#' }
calc_PAS <- function(m, tmin, ppt) {
  if (FALSE) {
    T0 <- a <- b <- NULL
  }

  param[["PAS"]][m, ppt * a / (1 + exp(-(tmin - T0) / b))]
}
