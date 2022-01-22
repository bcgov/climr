#' Calculate Precipitation As Snow (PAS)
#'
#' @param m month of the year
#' @param tm min temperature for that month
#'
#' @return Precipitation As Snow
#' @export
#'
#' @examples
#' calc_PAS(4, 2)
#' 
calc_PAS <- function(m, tm) {
  
  match_lines <- match(m, param$PAS$Month)
  
  b <- param$PAS$b[match_lines]
  t0 <- param$PAS$T0[match_lines]
  
  1/(1 + exp(-(tm - t0)/b))
}
