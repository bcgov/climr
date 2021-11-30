#' Calculate Relative Humidity (RH)
#'
#' @param tmin_mean monthly mean minimum air temperature
#' @param tmax_mean monthly mean maximum air temperature
#'
#' @return Relative Humidity
#' @export
#'
#' @examples
#' calc_RH(tmin_mean = 10, tmax_mean = 40)
calc_RH <- function(tmin_mean, tmax_mean) {
  
  es_tmin <- calc_es(tmin_mean)
  es_tmax <- calc_es(tmax_mean)
  es_avg = (es_tmin + es_tmax)/2
  
  100 * es_tmin/es_avg
}


# es: saturated vapour pressure at a temperature t
# t: air temperature
calc_es <- function(t) {
  
  svp <- calc_SVP(t)
  
  which_minus_0 <- which(t < 0)
  
  svp[which_minus_0] <- svp[which_minus_0] * (1 + ( t[which_minus_0] * 0.01) )
  
  svp
}

# Saturated Vapour Pressure
calc_SVP <- function(t){
  0.6105 * exp((17.273*t)/(t+237.3))
}
