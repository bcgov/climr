#' Plot Climate Diagram
#' 
#' This is a wrapper function that will create the climate diagram input and 
#' then will call the function to generate the climate diagram. 
#' 
#' @inheritParams create_climate_diagram_input
#' @export
#' @examples
#' \dontrun{
#' in_xyz <- data.frame(lon = -127.7052, lat = 55.3557, elev = 291, id = 1)
#' plot_climate_diagram(in_xyz)
#' 
#' plot_climate_diagram(in_xyz, 
#'   gcms = "CanESM5",
#'   ssp = "ssp245",
#'   obs_period = "2001_2020",
#'   gcm_period = "2021_2040",
#'   max_run = 1,
#'   cache = TRUE
#'   )
#' }

plot_climate_diagram <- function(xyz,
                                 gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
                                 ssps = list_ssps()[2],
                                 obs_periods = list_obs_periods()[1],
                                 gcm_periods = list_gcm_periods(),
                                 max_run = 10,
                                 cache = TRUE, 
                                 ...){
  
  
  plot_input <- create_climate_diagram_input(xyz = xyz,
                                             gcms = gcms,
                                             ssps = ssps,
                                             obs_periods = obs_periods,
                                             gcm_periods = gcm_periods,
                                             max_run = max_run,
                                             cache = cache, 
                                             ...)
  
  
  create_climate_diagram(temp = plot_input[["Tave"]], precip = plot_input[["PPT"]], elev = plot_input[["elev"]])
  
  
}



#' Create input for Climate Diagram
#' 
#' This function will downscale and return a list of the montly Tmin, Tmax, Tave, PPT and the elevation.
#' @inheritParams downscale
#' @param use_downscale_db Should the function `downscale_db` be used instead of `downscale`
#' @export
#' @examples
#' \dontrun{
#' in_xyz <- data.frame(lon = -127.7052, lat = 55.3557, elev = 291, id = 1)
#' create_climate_diagram_input(in_xyz)
#' }

create_climate_diagram_input <- function(xyz,
                                         gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
                                         ssps = list_ssps()[2],
                                         obs_periods = list_obs_periods()[1],
                                         gcm_periods = list_gcm_periods(),
                                         max_run = 10L,
                                         cache = TRUE, 
                                         use_downscale_db = FALSE,
                                         ...) {
  
  if (isTRUE(use_downscale_db)){
    data <- downscale_db(xyz,
                         gcms = gcms,
                         ssps = ssps,
                         obs_periods = obs_periods,
                         gcm_periods = gcm_periods,
                         vars = c(sprintf("PPT_%02d", 1:12), sprintf("Tmin_%02d", 1:12), sprintf("Tmax_%02d", 1:12), sprintf("Tave_%02d", 1:12)),
                         return_refperiod = FALSE,
                         max_run = max_run,
                         ...)
    
    
  } else{
    
    data <- downscale(xyz,
                      gcms = gcms,
                      ssps = ssps,
                      obs_periods = obs_periods,
                      gcm_periods = gcm_periods,
                      vars = c(sprintf("PPT_%02d", 1:12), sprintf("Tmin_%02d", 1:12), sprintf("Tmax_%02d", 1:12), sprintf("Tave_%02d", 1:12)),
                      return_refperiod = FALSE,
                      max_run = max_run,
                      cache = cache, 
                      ...)
  }
 

  
  data <- merge(data, data.frame(id = xyz$id, elev = xyz$elev), by = 'id')
  
  if (any(data$RUN == "ensembleMean")){
    
    Tmin <-  data[RUN !=  "ensembleMean", lapply(.SD, min, na.rm = TRUE), .SDcols = patterns("Tmin_[0-9]")]
    Tmax <-  data[RUN !=  "ensembleMean", lapply(.SD, max, na.rm = TRUE), .SDcols = patterns("Tmax_[0-9]")]
    Tave <-  data[RUN ==  "ensembleMean", lapply(.SD, mean, na.rm = TRUE), .SDcols = patterns("Tave_[0-9]")]
    PPT <- data[RUN == "ensembleMean", lapply(.SD, mean, na.rm = TRUE), .SDcols = patterns("PPT_[0-9]")]
    
  } else { #use historical data
    Tmin <-  data[, lapply(.SD, min, na.rm = TRUE), .SDcols = patterns("Tmin_[0-9]")]
    Tmax <-  data[, lapply(.SD, max, na.rm = TRUE), .SDcols = patterns("Tmax_[0-9]")]
    Tave <-  data[, lapply(.SD, mean, na.rm = TRUE), .SDcols = patterns("Tave_[0-9]")]
    PPT <- data[, lapply(.SD, mean, na.rm = TRUE), .SDcols = patterns("PPT_[0-9]")]
  }
  
  list(Tmin = c(Tmin[["Tmin_01"]], Tmin[["Tmin_02"]], Tmin[["Tmin_03"]], Tmin[["Tmin_04"]], Tmin[["Tmin_05"]], Tmin[["Tmin_06"]], 
                Tmin[["Tmin_07"]], Tmin[["Tmin_08"]], Tmin[["Tmin_09"]], Tmin[["Tmin_10"]], Tmin[["Tmin_11"]], Tmin[["Tmin_12"]]),
       Tmax = c(Tmax[["Tmax_01"]], Tmax[["Tmax_02"]], Tmax[["Tmax_03"]], Tmax[["Tmax_04"]], Tmax[["Tmax_05"]], Tmax[["Tmax_06"]], 
                Tmax[["Tmax_07"]], Tmax[["Tmax_08"]], Tmax[["Tmax_09"]], Tmax[["Tmax_10"]], Tmax[["Tmax_11"]], Tmax[["Tmax_12"]]), 
       Tave = c(Tave[["Tave_01"]], Tave[["Tave_02"]], Tave[["Tave_03"]], Tave[["Tave_04"]], Tave[["Tave_05"]], Tave[["Tave_06"]], 
                Tave[["Tave_07"]], Tave[["Tave_08"]], Tave[["Tave_09"]], Tave[["Tave_10"]], Tave[["Tave_11"]], Tave[["Tave_12"]]), 
       PPT = c(PPT[["PPT_01"]], PPT[["PPT_02"]], PPT[["PPT_03"]], PPT[["PPT_04"]], PPT[["PPT_05"]], PPT[["PPT_06"]], 
               PPT[["PPT_07"]], PPT[["PPT_08"]], PPT[["PPT_09"]], PPT[["PPT_10"]], PPT[["PPT_11"]], PPT[["PPT_12"]]), 
       elev = mean(xyz$elev, na.rm = TRUE))

}





