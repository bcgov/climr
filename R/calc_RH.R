#' Calculate Relative Humidity (RH)
#'
#' @template tmmin
#' @template tmmax
#'
#' @return numeric. Relative Humidity
#'
#' @examples
#' \dontrun{
#' climr::calc_RH(tmmin = 10, tmmax = 40)
#' }
#' @export
#' @rdname climatevar
calc_RH <- function(tmmin, tmmax) {
  es_tmin <- calc_SVP(tmmin)
  es_tmax <- calc_SVP(tmmax)
  es_avg <- (es_tmin + es_tmax) / 2
  return(100 * es_tmin / es_avg)
}

#' Calculate Potential Evapotranspiration
#'
#' Based on simplified Penman - Monteith method from Hogg (1997)
#'
#' @param tave numeric. Monthly average air temperature.
#' @template tmmin
#' @template tmmax
#' @param alt numeric. Altitude in m.
#'
#' @references Hogg, E.H. (1997). Temporal scaling of moisture and the forest-grassland boundary in western Canada. Agricultural and Forest Meteorology, Research on Forest Environmental Influences in a Changing World, 84, 115–122.
#' @importFrom data.table fifelse
#' @export
#' @rdname climatevar
calc_PET <- function(tave, tmmin, tmmax, alt) {
  D <- 0.5 * (.calc_SVP(tmmax) + .calc_SVP(tmmin)) - .calc_SVP(tmmin - 2.5)
  test1 <- tave > 10
  test2 <- tave > -5
  pet <- test1 * (93 * D * exp(alt / 9300)) + (!test1) * test2 * ((6.2 * tave + 31) * D * exp(alt / 9300))
  return(pet)
}

# calc_cmi <- function(ppt, pet) {
#   return(ppt - pet)
# }

#' Calculate Saturated Vapour Pressure at a temperature t
#' @template t
#' @export
#' @rdname climatevar
calc_SVP <- function(t) {
  svp <- .calc_SVP(t)
  test1 <- t < 0
  svp <- test1 * svp * (1 + (t * 0.01)) + (!test1) * svp
  return(svp)
}


#' internal utility function
#' Magnus-Tetens empirical formula for saturation vapour pressure, with units of degrees C and kPa
#' obtained from: https://andrewsforest.oregonstate.edu/sites/default/files/lter/data/studies/ms01/dewpt_vpd_calculations.pdf
#' @template t
#' @noRd
.calc_SVP <- function(t) {
  return(0.6105 * exp((17.273 * t) / (t + 237.3)))
}
