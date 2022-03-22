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
  
  match_lines <- match(m, param$NFFD$Month)
  
  a <- param$NFFD$a[match_lines]
  b <- param$NFFD$b[match_lines]
  t0 <- param$NFFD$T0[match_lines]
  
  a/(1 + exp(-(tm - t0)/b))
}