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

##based on simplified Penman - Monteith method fro Hogg 1997
calc_CMI <- function(ppt, tave, tmin, tmax, alt){
  D <- 0.5*(calc_SVP(tmax) - calc_SVP(tmin)) - calc_SVP(tmin - 2.5)
  pet <- data.table::fifelse(tave > 10, 93 * D * exp(alt/9300),
                             data.table::fifelse(tave > -5, (6.2 * tave + 31) * D * exp(alt/9300), 0))
  return(ppt - pet)
}

# calc_cmi <- function(ppt, pet){
#   return(ppt - pet)
# }

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
