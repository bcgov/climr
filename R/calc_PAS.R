#' Calculate Precipitation As Snow (PAS)
#'
#' @param m month of the year
#' @param tm min temperature for that month
#'
#' @return Precipitation As Snow
#'
#' @examples
#' \dontrun{
#' calc_PAS(4, 2)
#' }
calc_PAS <- function(m, tm, ppt) {
  
  if (FALSE) {T0 <- b <- NULL}
  
  param[["PAS"]][m, ppt * a / (1 + exp(-(tm - b) / T0))]

}
