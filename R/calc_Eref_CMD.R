#' Calculate Hargreaves' reference evaporation
#' @template m
#' @template tmmin
#' @template tmmax
#' @template latitude
#' @return numeric. Reference evaporation (Eref)
#' @export
#' @rdname climatevar
calc_Eref <- function(m, tmmin, tmmax, latitude) {
  tmean <- (tmmax + tmmin) / 2
  test1 <- tmean >= 0
  day_month <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  day_julian <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  # Paper unclear, 1.18 - 0.0065 in Appendix, 1.18 - 0.0067 in paper
  # Wangetal2012_ClimateWNA_JAMC-D-11-043.pdf
  # Probably missing
  Eref <- test1 * (
    0.0023 * day_month[m] *
    calc_S0_I(day_julian[m], tmean, latitude) *
    (tmean + 17.8) * sqrt(tmmax - tmmin) *
    (1.18 - 0.0065 * latitude)
  )
  return(Eref)
}

#' Calculate climatic moisture deficit (CMD)
#' @param Eref Reference evaporation
#' @template PPT
#' @return numeric. Climatic moisture deficit
#' @export
#' @rdname climatevar
calc_CMD <- function(Eref, PPT) {
  test1 <- Eref > PPT
  CMD <- test1 * (Eref - PPT)
  return(CMD)
}

#' PROGRAM I From Hargreaves 1985
#' @param d numeric. Julian day of the year (January 1 = 1, December 31 = 365).
#' @param tm numeric. Monthly mean temperature for the corresponding month.
#' @template latitude
#' @return numeric. Extraterrestrial radiation estimation in mm/day
#' @noRd
calc_S0_I <- function(d, tm, latitude) {
  # BASIC COMPUTER PROGRAM FOR ESTIMATING DAILY RA VALUES
  # D=JULIAN DAY (JANUARY 1=1)
  # DEC=DECLINATION OF THE SUN IN RADIANS
  # ES=MEAN MONTHLY DISTANCE OF THE SUN TO THE EARTH DIVIDED BY THE MEAN ANNUAL DISTANCE
  # LD=LATITUDES IN DEGREES
  # LDM=MINUTES OF LATITUDES
  # RA=MEAN MONTHLY EXTRATERRESTRIAL RADIATION IN MM/DAY
  # RAL=MEAN MONTHLY EXTRATERRESTRIAL RADIATION IN LANGLEYS/DAY
  # TC=MEAN DAILY TEMPERATURE IN DEGREE CELSIUS
  Y <- cos(0.0172142 * (d + 192L))
  DEC <- 0.40876 * Y
  ES <- 1.0028 + 0.03269 * Y
  XLR <- latitude / 57.2958
  Z <- -tan(XLR) * tan(DEC)
  OM <- -atan(Z / sqrt(-Z * Z + 1)) + pi / 2
  # -----------
  # code added Hargreaves' program to provide finite values above the arctic circle. 
  lim <- if (d %in% 91:274) 3.1 else 0
  if (inherits(OM, "SpatRaster")) {
    OM <- ifel(is.finite(OM), OM, lim)
  } else {
    OM <- ifelse(is.finite(OM), OM, lim)
  }
  # -----------end of added code-----------
  # CALCULATE THE DAILY EXTRATERRESTRIAL RADIATION IN LANGLEYS/DAY
  DL <- OM / 0.1309
  RAL <- 120 * (DL * sin(XLR) * sin(DEC) + 7.639 * cos(XLR) * cos(DEC) * sin(OM)) / ES
  # CALCULATE THE EXTRATERRESTRIAL RADIATION IN MM/DAY
  RA <- RAL * 10 / (595.9 - 0.55 * tm)
  return(RA)
}

#' PROGRAM II From Hargreaves 1985
#' @template m
#' @template tm
#' @template latitude
#' @return numeric. Extraterrestrial radiation estimation in mm/day
#' @noRd
calc_S0_II <- function(m, tm, latitude) {
  # BASIC COMPUTER PROGRAM FOR ESTIMATING MONTHLY RA VALUES
  # DEC=DECLINATION OF THE SUN IN RADIANS
  # ES=MEAN MONTHLY DISTANCE OF THE SUN TO THE EARTH DIVIDED BY THE MEAN ANNUAL DISTANCE
  # LD=LATITUDES IN DEGREES
  # LDM=MINUTES OF LATITUDES
  # RA=MEAN MONTHLY EXTRATERRESTRIAL RADIATION IN MM/DAY
  # RAL=MEAN MONTHLY EXTRATERRESTRIAL RADIATION IN LANGLEYS/DAY
  # TC=MEAN MONTHLY TEMPERATURE IN DEGREE CELSIUS
  # DEC <- numeric(12)
  # ES <- numeric(12)
  DEC <- -0.00117 - 0.40117 * cos(pi * m) -
    0.042185 * sin(pi * m) + 0.00163 *
      cos(2 * pi * m) + 0.00208 * sin(2 * pi * m)
  # CALCULATE THE RELATIVE DISTANCE OF THE SUN TO THE EARTH
  ES <- 1.00016 - 0.032126 * cos(pi * m) -
    0.0033535 * sin(pi * m)
  XLR <- latitude / 57.2958
  Z <- -tan(XLR) * tan(DEC)
  OM <- -atan(Z / sqrt(-Z * Z + 1)) + pi / 2
  # CALCULATE THE DAILY EXTRATERRESTRIAL RADIATION IN LANGLEYS/DAY
  RAL <- 916.732 * (OM * sin(XLR) * sin(DEC) + cos(XLR) * cos(DEC) * sin(OM)) / ES
  # CALCULATE THE EXTRATERRESTRIAL RADIATION IN MM/DAY
  RA <- RAL * 10 / (595.9 - 0.55 * tm)

  return(RA)
}
