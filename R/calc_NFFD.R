#' Calculate Number of Frost Free Days (NFFD)
#'
#' @template m
#' @template tmin
#'
#' @return numeric. Number of Frost Free Days
#'
#' @examples
#' \dontrun{
#' climr::calc_NFFD(3, 2.05)
#' }
#' @export
#' @rdname climatevar
calc_NFFD <- function(m, tmin) {
  if (FALSE) {
    a <- T0 <- b <- NULL
  }
  param[["NFFD"]][m, a / (1 + exp(-(tmin - T0) / b))]
}
