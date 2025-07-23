
#' Create a Walter-Lieth climate diagram
#'
#' This function creates a Walter-Lieth climate diagram, which summarizes mean monthly 
#' temperature and precipitation to visualize seasonal climate patterns.
#' It supports both the classical version using monthly average temperature
#' and an enhanced version using monthly mean daily minimum (`Tmin`) and maximum (`tmax`) temperatures.
#'
#' The diagram uses the following conventions:
#' - **Temperature**: red line showing mean monthly temperature. If `tmin` and `tmax` are supplied, they define a red ribbon showing the temperature range.
#' - **Precipitation**: blue line showing monthly precipitation, scaled at 2 mm per °C. For months >100 mm, the scale changes to 20 mm per °C to avoid diagram distortion.
#' - **Humid periods**: blue shading where precipitation exceeds temperature (P > T).
#' - **Dry periods**: red shading where temperature exceeds precipitation (T > P).
#' - **Wet periods**: dark grey shading where precipitation > 100 mm.
#' - **Freezing months**: marked with a blue polygon along the x-axis when temperature < 0°C.
#'
#' This plot helps identify wet/dry seasons, frost months, and temperature-precipitation balance.
#'
#' @param X  A `data.table` object produced using the function [`plot_bivariate_input()`]. This
#' table can include more models, scenarios, and variables than are used in individual calls to
#' [`plot_bivariate()`].
#' @param diurnal Logical. If `TRUE`, the range between `Tmin` and `Tmax` is shown, representing the diurnal range for each month.
#' @param obs_period Character (optional). One of the observation periods (e.g., `"1961_1990"`).
#' @param gcm Character (optional). A specific global climate model (GCM). If unspecified, then the mean of all global climate models will be shown for the specified `ssp` and `gcm_period`.  
#' @param ssp Character (optional). A Shared Socioeconomic Pathway scenario (SSP), required when a `gcm_period` is specified.
#' @param gcm_period Character (optional). A GCM simulation period (e.g., `"2041_2060"`), used for ensemble means or GCM-specific plots. Required when a `gcm_period` is specified.
#' @param location Character (optional). Include the location in plot title.
#' @param app Logical. If `TRUE`, it will increase the font size.
#'
#' @return A `ggplot2` object showing the Walter-Lieth climate diagram.
#'
#' @details 
#' The Walter-Lieth diagram was introduced by Heinrich Walter and Helmut Lieth in the 1960s to provide a compact visual representation of climate normals. It is particularly valuable in ecology and biogeography for characterizing climate zones and inferring environmental conditions suitable for vegetation types or agricultural use.
#'
#' Interpretation tips:
#' - **Shaded blue areas** indicate months where water supply exceeds potential evapotranspiration, often conducive to plant growth.
#' - **Shaded red areas** suggest moisture stress periods.
#' - **Shaded black areas** denote very wet months, which may correspond to tropical monsoon seasons or strong winter rains.
#' - **Polygon blocks on the baseline** signal freezing months and potential frost periods.
#' 
#' @importFrom data.table fcase fifelse
#' @importFrom ggplot2 aes geom_hline geom_line geom_ribbon geom_polygon
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous sec_axis
#' @importFrom ggplot2 annotate coord_cartesian labs theme_classic theme element_text
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' in_xyz <- data.frame(lon = -120.3273, lat = 50.6745, elev = 384, id = 1) # Kamloops, BC
#' 
#' # Obtain the input data for the plot
#' my_data <- plot_WalterLieth_input(in_xyz, 
#'                                obs_period = list_obs_periods()[1],
#'                                gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
#'                                ssps = list_ssps()[2],
#'                                gcm_periods = list_gcm_periods()
#'                                )
#' 
#' # Plot 1961-1990 climate normals
#' plot_WalterLieth(my_data) 
#' 
#' # include the monthly diurnal range
#' plot_WalterLieth(my_data, diurnal = TRUE) 
#' 
#' # Plot 2001-2020 period climate normals
#' plot_WalterLieth(my_data, obs_period = list_obs_periods()[1])
#'
#' # Plot a single GCM projection
#' plot_WalterLieth(my_data, gcm = "ACCESS-ESM1-5", ssp = "ssp245", gcm_period = "2041_2060")
#'
#' # Plot ensemble mean projection for an SSP and period
#' plot_WalterLieth(my_data, ssp = "ssp245", gcm_period = "2041_2060")
#' }
#' 

plot_WalterLieth <- function(X, diurnal = FALSE,
                             obs_period = NULL,
                             gcm = NULL,
                             ssp = NULL,
                             gcm_period = NULL,
                             location = NULL,
                             app = FALSE) {
  
  if (!inherits(X, "data.table")) X <- as.data.table(X)
  
  # Ensure SSP and GCM period are specified together
  if (xor(is.null(ssp), is.null(gcm_period))) {
    stop("You must supply both 'ssp' and 'gcm_period' together if either is specified.")
  }
  
  # Select focal data in a logical sequence
  if(!is.null(gcm)){ # if a GCM is specified, use that GCM
    data <- X[GCM==gcm & SSP==ssp & PERIOD==gcm_period, .SD, .SDcols = !c("GCM", "SSP", "RUN", "PERIOD")]
  } else if(!is.null(gcm_period)){ #if no GCM is specified, but a simulation time period is specified, use the average across the GCM ensemble
    data <- X[SSP==ssp & PERIOD==gcm_period] 
    data <- data[, lapply(.SD, mean, na.rm = TRUE), .SDcols = !c("GCM", "SSP", "RUN", "PERIOD")]
  } else if(!is.null(obs_period)){ 
    if ("GCM" %in% names(X)) {
      data <- X[is.na(GCM) & PERIOD==obs_period]
    } else {
      data <- X[PERIOD==obs_period]
    }
  } else {
    data <- X[PERIOD=="1961_1990"]
  }
  
  ppt <- as.numeric(unlist(data[, grep("^PPT_", names(data), value = TRUE), with = FALSE]))
  tmin <- as.numeric(unlist(data[, grep("^Tmin_", names(data), value = TRUE), with = FALSE]))
  tmax <- as.numeric(unlist(data[, grep("^Tmax_", names(data), value = TRUE), with = FALSE]))
  tave <- (tmin + tmax) / 2
  if ("elev" %in% names(data)) {
    elev <- as.numeric(data[, elev])
  } else {
    elev <- NULL
  }
  
  # Remove CRAN check warnings
  if (FALSE) {
    y_tave <- y_precip <- y_100 <- x <- y <- NULL
  }
  
  month <- 1:12
  y_precip_rel <- fifelse(ppt > 100, (ppt - 100)/20 + 50, ppt / 2)
  period_type <- fcase(ppt > 100, 'Wet',
                       y_precip_rel > tave, 'Humid', 
                       default = 'Dry')
  
  monthly_df <- data.frame(month = month, y_precip = y_precip_rel, y_tave = tave, period_type = period_type)
  
  intersect_xy <- find_intersect_humid_dry(tave, y_precip_rel)
  
  if (nrow(intersect_xy)) {
    intersect_df <- data.frame(month = intersect_xy$x, y_precip = intersect_xy$y, y_tave = intersect_xy$y)
    df_humid <- rbind(monthly_df[which(period_type %in% c('Humid', 'Wet')),], cbind(intersect_df, period_type = 'Humid'))
    df_dry <- rbind(monthly_df[which(period_type == 'Dry'),], cbind(intersect_df, period_type = 'Dry'))
  } else {
    df_humid <- monthly_df[which(period_type %in% c('Humid', 'Wet')),]
    df_dry <- monthly_df[which(period_type == 'Dry'),]
  }
  
  all_info <- unique(rbind(df_humid, df_dry))
  
  gg <- ggplot(data = all_info, aes(x = month)) + 
    geom_line(aes(month, y_tave), colour = 'red', linewidth = 1) +
    geom_line(aes(month, y_precip), colour = 'blue', linewidth = 1) +
    geom_hline(yintercept = 50) +
    geom_hline(yintercept = 0) +
    geom_ribbon(data = df_humid, aes(ymax = y_precip, ymin = y_tave), fill = "blue", alpha = 0.25) +
    geom_ribbon(data = df_dry, aes(ymax = y_tave, ymin = y_precip), fill = "red", alpha = 0.25)
  
  # Add Tmin/Tmax ribbon if present
  if (diurnal) {
    df_range <- data.frame(month = 1:12, tmin = tmin, tmax = tmax)
    gg <- gg + geom_ribbon(data = df_range,
                           aes(x = month, ymin = tmin, ymax = tmax),
                           fill = "red", alpha = 0.2)
  }
  
  gg <- gg +
    scale_x_continuous(breaks = 1:12,
                       labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
                                  'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'),
                       name = 'Month') +
    scale_y_continuous("Temperature (°C)",
                       sec.axis = sec_axis(
                         transform = ~(.*2)*(.<=50) + (20*(.-50)+100)*(.>50),
                         name = "Precipitation (mm)"
                       )) +
    coord_cartesian(xlim = c(1,12),
                    ylim = c(min(-2, tave, tmin, na.rm=TRUE),
                             max(y_precip_rel, tave, tmax, 50, na.rm=TRUE)),
                    clip = 'off',
                    expand = FALSE) +
    labs(title = sprintf('Walter-Lieth Climate Diagram     (MAT = %.1f\u00B0C; MAP = %1.fmm )', mean(tave), sum(ppt)),
         subtitle = ifelse(is.null(elev), sprintf("(Loc. = %s, Period = %s)", location, obs_period), sprintf("(Loc. = %s, Elev. = %1.fm, Period = %s)", location, elev, obs_period))) +
    theme_classic()
  if (app) {
    gg <- gg + theme(
      plot.title = element_text(size = 20),
      plot.subtitle = element_text(size = 15),
      axis.title.x = element_text(size = 18),
      axis.title.y = element_text(size = 18),
      axis.title.y.right = element_text(size = 18),
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.text.y.right = element_text(size = 15)
    )
  }
  
  # Wet Periods
  if (any(period_type == 'Wet')) {
    which_wet <- which(period_type == 'Wet')
    intersections_100 <- do.call(cbind, lapply(which_wet, find_intersect_wet_period, y_precip_rel = y_precip_rel))
    intersections_100 <- data.frame(intersect_x = intersections_100[1,], intersect_y = intersections_100[2,])
    
    df_wet <- data.frame(
      month = c(month[which_wet], intersections_100$intersect_x),
      y_precip = c(y_precip_rel[which_wet], intersections_100$intersect_y), 
      y_100 = 50
    )
    
    gg <- gg + geom_ribbon(data = df_wet, aes(ymax = y_precip, ymin = y_100), fill = 'black', alpha = 0.50) 
  }
  
  # Freeze Periods
  freeze <- which(tave < 0)
  for(i in freeze){
    df_freeze <- data.frame(
      x = c(pmax(1, i - 0.5), pmax(1, i - 0.5), pmin(i + 0.5, 12), pmin(i + 0.5, 12)),
      y = c(0, -1, -1, 0)
    )
    gg <- gg + geom_polygon(data = df_freeze, aes(x = x, y = y), fill = "blue", colour = "black", alpha = 0.6)
  }
  
  return(gg)
}




find_intersect_wet_period <- function(x, y_precip_rel){
  
  if (x == 1){
    if (y_precip_rel[2] > 50){ # We don't cross threshold between 1 and 2
      matrix(data = c(1, y_precip_rel[1]))
    } else{
      cbind(matrix(data = c(1, y_precip_rel[1])), 
            find_intersect_xy(1:2, y1 = c(50, 50), y2 = y_precip_rel[1:2]))
    }
    
  } else if (x == 12){
    if (y_precip_rel[11] > 50){ # We don't cross threshold between 11 and 12
      matrix(data = c(12, y_precip_rel[12]))
    } else {
      cbind(find_intersect_xy(11:12, y1 = c(50, 50), y2 = y_precip_rel[11:12]), 
            matrix(data = c(12, y_precip_rel[12])))
    }
    
  } else{
    if (y_precip_rel[x-1] < 50 & y_precip_rel[x+1] < 50){
      cbind(find_intersect_xy((x-1):x, y1 = c(50, 50), y2 = y_precip_rel[(x-1):x]), 
            find_intersect_xy(x:(x+1), y1 = c(50, 50), y2 = y_precip_rel[x:(x+1)]))
    } else if (y_precip_rel[x-1] < 50 & y_precip_rel[x+1] > 50) {
      cbind(find_intersect_xy((x-1):x, y1 = c(50, 50), y2 = y_precip_rel[(x-1):x]), 
            matrix(data = c(x, y_precip_rel[x])))
    } else if (y_precip_rel[x-1] > 50 & y_precip_rel[x+1] < 50) {
      cbind(matrix(data = c(x, y_precip_rel[x])), 
            find_intersect_xy(x:(x+1), y1 = c(50, 50), y2 = y_precip_rel[x:(x+1)]))
    } else {
      matrix(data = c(x, y_precip_rel[x]))
    }
    
  }
}

#' @importFrom stats coef lm
find_intersect_xy <- function(x, y1, y2){
  coef_1 <- coef(lm(y1~x))
  coef_2 <- coef(lm(y2~x))
  #find x
  inter_x <- as.numeric((coef_2[1] - coef_1[1]) / (coef_1[2] - coef_2[2]))
  c(inter_x, as.numeric(coef_1[1] + inter_x*coef_1[2]))
} 

find_intersect_humid_dry <- function(tave, y_precip_rel){
  
  #Compare to previous month to know if period type changed between months.
  ## January won't change because it has no prior state so we assign FALSE.
  period_type_change <- c(FALSE, (y_precip_rel[-1] > tave[-1]) != (y_precip_rel[-12] > tave[-12]))
  
  intersect_xy <- vapply(which(period_type_change), \(x) find_intersect_xy((x-1):x, y1=tave[(x-1):x], y2=y_precip_rel[(x-1):x]), numeric(2))
  
  data.frame(x = intersect_xy[1,], y = intersect_xy[2,])
  
}