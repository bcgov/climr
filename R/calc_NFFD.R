#' Calculate Number of Frost Free Days (NFFD)
#'
#' @template m
#' @param tm min temperature for that month
#'
#' @return numeric. Number of Frost Free Days
#'
#' @examples
#' \dontrun{
#' climr:::calc_NFFD(3, 2.05)
#' }
calc_NFFD <- function(m, tm) {
  if (FALSE) {
    a <- T0 <- b <- NULL
  }
  param[["NFFD"]][m, a / (1 + exp(-(tm - T0) / b))]
}
