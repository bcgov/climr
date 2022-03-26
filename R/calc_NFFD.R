#' Calculate Number of Frost Free Days (NFFD)
#'
#' @param m month of the year
#' @param tm min temperature for that month
#'
#' @return Number of Frost Free Days
#' 
#' @examples
#' \dontrun{
#' calc_NFFD(3, 2.05)
#' }
calc_NFFD <- function(m, tm) {
  
  if (FALSE) { a <- T0 <- b <- NULL }
  param[["NFFD"]][m, a/(1 + exp(-(tm - T0)/b))]
  
}