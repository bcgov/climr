#' Calculate Number of Frost Free Days (NFFD)
#'
#' @param m month of the year
#' @param tm min temperature for that month
#'
#' @return Number of Frost Free Days
#' @export
#' 
#' @examples
#' calc_NFFD(3, 2.05)
#' 
calc_NFFD <- function(m, tm) {
  
  match_lines <- match(m, param$nffd$Month)
  
  a <- param$nffd$a[match_lines]
  b <- param$nffd$b[match_lines]
  t0 <- param$nffd$T0[match_lines]
  
  a/(1 + exp(-(tm - t0)/b))
}