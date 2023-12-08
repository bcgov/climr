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

#' Calculate Potential Evapotranspiration
#' 
#' Based on simplified Penman - Monteith method from Hogg (1997)
#' @references Hogg, E.H. (1997). Temporal scaling of moisture and the forest-grassland boundary in western Canada. Agricultural and Forest Meteorology, Research on Forest Environmental Influences in a Changing World, 84, 115–122.
#' @importFrom data.table fifelse
calc_PET <- function(tave, tmin, tmax, alt){
  D <- 0.5*(calc_SVP(tmax) - calc_SVP(tmin)) - calc_SVP(tmin - 2.5)
  pet <- fifelse(tave > 10, 93 * D * exp(alt/9300),
                 fifelse(tave > -5, (6.2 * tave + 31) * D * exp(alt/9300), 0))
  return(pet)
}

# calc_cmi <- function(ppt, pet){
#   return(ppt - pet)
# }

#' Calculate Saturated Vapour Pressure at a temperature t
#' @template t
calc_es <- function(t) {
  
  svp <- calc_SVP(t)
  i <- which(t < 0)
  svp[i] <- svp[i] * (1 + ( t[i] * 0.01) )
  return(svp)
  
}

#' @template t
calc_SVP <- function(t){
  
  return(0.6105 * exp((17.273*t)/(t+237.3)))
  
}
