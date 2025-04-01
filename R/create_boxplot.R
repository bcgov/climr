#' Create a Boxplot from Climate Data
#'
#' @description Creates a boxplot visualizing monthly climate data (temperature or precipitation)
#' across different periods using ggplot2.
#'
#' @param dt A data.table containing the climate data with columns for monthly values
#'           and optionally a RUN column.
#' @param var Character string specifying the variable to plot. One of:
#'            "Tmax" (maximum temperature), "Tmin" (minimum temperature),
#'            "PPT" (precipitation), or "Tave" (average temperature).
#'
#' @return A ggplot object containing a boxplot of monthly values by period.
#'
#' @details 
#' The function processes climate data by calculating means by period using ensemble means 
#' if present and then create the boxplot 
#'
#' @examples
#' 
#' # Sample data
#' dt <- data.frame(
#'   PERIOD = rep(c("2000-2010", "2011-2020"), each = 2),
#'   Tmax_01 = c(10, 12, 11, 13),  
#'   Tmax_02 = c(15, 16, 14, 17),  
#'   Tmax_03 = c(18, 19, 17, 20),  
#'   Tmax_04 = c(22, 23, 21, 24),  
#'   Tmax_05 = c(25, 26, 24, 27),  
#'   Tmax_06 = c(28, 29, 27, 30),  
#'   Tmax_07 = c(30, 31, 29, 32), 
#'   Tmax_08 = c(29, 30, 28, 31),  
#'   Tmax_09 = c(26, 27, 25, 28),  
#'   Tmax_10 = c(22, 23, 21, 24), 
#'   Tmax_11 = c(17, 18, 16, 19), 
#'   Tmax_12 = c(12, 13, 11, 14)  
#'   )
#'   
#' # Create boxplot
#' create_boxplot(dt, "Tmax")
#' 
#'
#' @importFrom data.table setnames melt
#' @importFrom ggplot2 ggplot aes geom_boxplot xlab ylab theme_classic
#' @export
create_boxplot <- function(dt, var = c("Tmax", "Tmin", "PPT", "Tave")){
  
  #Remove CRAN check warnings
  if (FALSE) { variable <- value <- NULL}
  
  setDT(dt)
  
  var_nm <- sprintf("%s_%02d", match.arg(var), 1:12)
  
  var <- match.arg(var)
  
  if (!is.null(dt$RUN) && any(dt$RUN == "ensembleMean")){
    avg <- dt[RUN == "ensembleMean", lapply(.SD, mean), by = PERIOD, .SDcols = var_nm]  
  } else{
    avg <- dt[, lapply(.SD, mean), by = PERIOD, .SDcols = var_nm]
  }
  
  
  setnames(avg, old = var_nm, 
           new = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
  
  melted_dt <- melt(avg, id.vars = "PERIOD", 
                    measure.vars = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
  
  if (var == "PPT"){
    y_var_lab = "Precipitation (mm)"
  } else {
    y_var_lab = "Temperature (\u00B0C)"
  }
  
  ggplot(melted_dt, aes(variable, value)) + 
    geom_boxplot(fill = "#F28C82", colour = "#4C8C7C") + 
    xlab("") + ylab(y_var_lab) + 
    theme_classic()
}




#' Create input for boxplot
#' 
#' This function will downscale and return a data.table used to call `create_boxplot`.
#' 
#' @inheritParams downscale
#' @param downscale_results optional, if you want to pass precomputed downscale results
#' @param use_downscale_db Should the function `downscale_db` be used instead of `downscale`
#' @param ... Additional arguments passed to `downscale` or `downscale_db`
#'
#' @return A data.table containing averaged climate variables by period, 
#'    including monthly temperature (average, min, max) and precipitation
#'    values (Tave, Tmin, Tmax, PPT) for each of the 12 months
#' @export
#' @examples
#' \dontrun{
#' in_xyz <- data.frame(lon = -127.7052, lat = 55.3557, elev = 291, id = 1)
#' create_boxplot_input(in_xyz)
#' create_boxplot_input(in_xyz, max_run = 1L, 
#'   gcm_hist_years = 1950:2015, 
#'   gcm_ssp_years = 2015:2040)
#' }

create_boxplot_input <- function(xyz = NULL,
                                 gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
                                 ssps = list_ssps()[2],
                                 max_run = 10L,
                                 obs_years = list_obs_years(),
                                 gcm_hist_years = list_gcm_hist_years(),
                                 gcm_ssp_years = list_gcm_ssp_years(), 
                                 use_downscale_db = FALSE,
                                 cache = TRUE, 
                                 downscale_results = NULL,
                                 ...){
  
  
  vars <- c(sprintf("%s_%02d", "Tave", 1:12),
            sprintf("%s_%02d", "Tmin", 1:12),
            sprintf("%s_%02d", "Tmax", 1:12),
            sprintf("%s_%02d", "PPT", 1:12))
    
  summarize_downscale_output(xyz,
                            gcms = gcms,
                            ssps = ssps,
                            max_run = max_run,
                            obs_years = obs_years,
                            gcm_hist_years = gcm_hist_years,
                            gcm_ssp_years = gcm_ssp_years, 
                            use_downscale_db = use_downscale_db,
                            cache = cache, 
                            vars = vars,
                            downscale_results = downscale_results,
                            ...)
}


