#' Calculate Relative Humidity (RH)
#'
#' @param tmin monthly mean minimum air temperature
#' @param tmax monthly mean maximum air temperature
#'
#' @return Relative Humidity
#'
#' @examples
#' \dontrun{
#' calc_RH(tmin_mean = 10, tmax_mean = 40)
#' }
calc_RH <- function(tmin, tmax) {
  
  es_tmin <- calc_es(tmin)
  es_tmax <- calc_es(tmax)
  es_avg = (es_tmin + es_tmax)/2
  return(100 * es_tmin/es_avg)
  
}


# es: saturated vapour pressure at a temperature t
# t: air temperature
calc_es <- function(t) {
  
  svp <- calc_SVP(t)
  i <- which(t < 0)
  svp[i] <- svp[i] * (1 + ( t[i] * 0.01) )
  return(svp)
  
}

# Saturated Vapour Pressure
calc_SVP <- function(t){
  
  return(0.6105 * exp((17.273*t)/(t+237.3)))
  
}
