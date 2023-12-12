#' Calculate Precipitation As Snow (PAS)
#'
#' @param m month of the year
#' @param tm min temperature for that month
#' @param ppt precipitation mm
#'
#' @return Precipitation As Snow
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
