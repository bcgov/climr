## *** NFFD  ***

nffd_param <- read.csv(file = "../optimizationTables/param_NFFD.csv", sep=',', header = TRUE)

# nffd: number of frost free days
# m: month of the year
# tm: min temperature for that month
nffd <- function(m, tm) {
  
  optimized_params <- nffd_param[nffd_param$Month == m,]

  a <- optimized_params$a
  b <- optimized_params$b
  t0 <- optimized_params$T0

  return( a/(1 + exp(-(tm - t0)/b)))
}

## *** FFP, bFFP and eFFP ***

# bffp: the day of the yearon which FFP begins
# t_min_list: named list of monthly minimum temperature for each month
# td: difference between the mean warmest monthly temperature and the mean coldest monthly temperature
# nffd: number of frost-free days.
bffp <- function(td, nffd, t_min_list) {
  
  tmin4 <- t_min_list[["4"]]
  tmin6 <- t_min_list[["6"]]
    
  return(352.1358994 + -0.021715653 * tmin4^2 + -3.542187618 * tmin6 + 0.020359471 * tmin6^2 - 4.897998097 * td + 0.033521327 * td^2 - 2.164862277 * nffd + 0.006767633 * nffd^2 - 0.00000929 * nffd^3 + 0.043516586 * (td * nffd) - 0.00000253 * (td * nffd)^2)
}

# effp: the day of the year on which FFP ends 
# t_min_list: named list of monthly minimum temperature for each month
# td: difference between the mean warmest monthly temperature and the mean coldest monthly temperature
effp <- function(nffd, t_min_list) {
  
  tmin9 <- t_min_list[["9"]]
  tmin10 <- t_min_list[["10"]]
  tmin11 <- t_min_list[["11"]]
    
  return(243.7752209 + 4.134210825 * tmin9 - 0.162876448 * tmin9^2 + 1.248649021 * tmin10 + 0.145073612 * tmin10^2 + 0.004319892 * tmin10 + -0.005753127 * tmin10^2 - 0.06296471 * nffd + 0.000399177 * nffd^2)
  
}

ffp <-function(effp,bffp) {
  return(effp - bffp)
}

## *** PAS ***
pas_param <- read.csv(file = "../optimizationTables/param_PAS.csv", sep=',', header = TRUE)

# pas: precipitation as snow
# m: month of the year
# tm: min temperature for that month
pas <- function(m, tm) {
  
  optimized_params <- pas_param[pas_param$Month == m,]

  b <- optimized_params$b
  t0 <- optimized_params$T0

  return( 1/(1 + exp(-(tm - t0)/b)))
}

## *** EMT, EXT ***

# emt: extreme minimum temperature
# t_min_list: named list of monthly minimum temperature for each month
# td: difference between the mean warmest monthly temperature and the mean coldest monthly temperature
emt <- function(t_min_list, td) {
  
  tmin1 <- t_min_list[["1"]]
  tmin12 <- t_min_list[["12"]]

  # tminx: minimum temperature over the year
  tminx <- min(sapply(t_min_list, min))

  
  return(-23.02164 + 0.77908 * tmin1 + 0.67048 * tmin12 + 0.01075 * tminx^2 + 0.11565 * td)
}

# ext: extreme maximum temperature
# t_max_list: named list of monthly maximum temperature for each month
# td: difference between the mean warmest monthly temperature and the mean coldest monthly temperature
ext <- function(t_max_list, td) {
  
  tmax7 <- t_max_list[["7"]]
  tmax8 <- t_max_list[["8"]]

  # tmaxx: maximum temperature over the year
  tmaxx <- max(sapply(t_max_list, max))

  
  return(10.64245 + -1.92005 * tmax7 + 0.04816 * tmax7^2 + 2.51176 * tmax8 - 0.03088 * tmax8^2 - 0.01311 * tmaxx^2 + 0.33167 * td - 0.001 * td^2)
}

## *** RH ***

# es: saturated vapour pressure at a temperature t
# t: air temperature
es <- function(t) {
  
  svp <- 0.6105 * exp((17.273*t)/(t+237.3))
  
  if(t < 0) {
    return(svp*(1 + (t*0.01)))
  } else {
    return(svp)
  }
}

# *******************************************
# Question, should this be tmin or tmin_mean?
# *******************************************

# rh: relative humidity
# tmin_mean: monthly mean minimum air temperature
# tmax_mean: monthly mean maximum air temperature
rh <- function(tmin_mean, tmax_mean) {
  es_avg = (es(tmin_mean)+ es(tmax_mean))/2
  
  return((100 * es(tmin_mean)/es_avg))
}

# *** DD ***
dd_param_below_0 <- read.csv(file = "../optimizationTables/param_DD_S1.csv", sep=',', header = TRUE)
dd_param_above_5 <- read.csv(file = "../optimizationTables/param_DD_S2.csv", sep=',', header = TRUE)
dd_param_below_18 <- read.csv(file = "../optimizationTables/param_DD_S3.csv", sep=',', header = TRUE)
dd_param_above_18 <- read.csv(file = "../optimizationTables/param_DD_S4.csv", sep=',', header = TRUE)

dd <- function(m, tm) {
  
# ***********************************************
# Question, I think it should be dd < 5 (not > 5)
# ***********************************************
  dd_param <- ''

  if(tm < 0) {
    dd_param <- dd_param_below_0
  } else if(tm < 5) {
    dd_param <- dd_param_above_5[dd_param_above_5$Region == "All"]
  } else if(tm < 18) {
    dd_param <- dd_param_below_18
  } else {
    dd_param <- dd_param_above_18
  }
  
  optimized_params <- dd_param[dd_param$Month == m,]
    
  k <- optimized_params$a
  a <- optimized_params$a
  b <- optimized_params$b
  t0 <- optimized_params$T0
  c <- optimized_params$a
  beta <- optimized_params$a
  
  if(tm > k) {
    return( a/(1 + exp(-(tm - t0)/b)))
  } else {
    return(c + (beta * tm))
  }
}
