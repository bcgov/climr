#' Create Climate Diagram Plot

#' @param temp vector of temperatures (in Celsius) for each month
#' @param precip vector of precipitation (in mm) for each month
#' @param elev elevation (in m). Optional, if NULL no elevation will be shown in output. 
#' 
#' @importFrom data.table fcase fifelse
#' @importFrom ggplot2 aes geom_hline geom_line geom_ribbon ggplot geom_polygon
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous sec_axis
#' @importFrom ggplot2 annotate coord_cartesian labs theme_classic
#' @export
#' @examples
#' 
#' temperature <- c(14, 14, 17, 20, 24, 28, 31, 31, 27, 23, 18, 15)
#' precipitations <- c(93.1, 62.1, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 73.8, 68, 155.3, 76.3)
#' create_climate_diagram(temp = temperature, precip = precipitations,  elev = 25)
#' 
#' create_climate_diagram(temp = c(-3, 14, 17, 20, 24, 28, 31, 31, 27, 23, 18, -5), 
#'    precip = c(93.1, 62.1, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 73.8, 68, 155.3, 76.3), 
#'    elev = 25 )
#'    
#' \dontrun{  
#'  in_xyz <- data.frame(lon = -127.7052, lat = 55.3557, elev = 291, id = 1)
#'  diagram_info <- create_climate_diagram_input(in_xyz)
#'  create_climate_diagram(temp = diagram_info[["Tave"]], 
#'     precip = diagram_info[["PPT"]], 
#'     elev = diagram_info[["elev"]])
#' }
#' 
create_climate_diagram <- function(temp, precip, elev = NULL){
  
  #Remove CRAN check warnings
  if (FALSE) {
    y_temp <- y_precip <- y_100 <- x <- y <- NULL
  }

  stopifnot(length(temp)==12)
  stopifnot(length(precip)==12)
  stopifnot(!anyNA(c(temp, precip)))

  month <- 1:12
  
  # when monthly precipitation is greater than 100 mm, the scale is increased from 2 mm/Celsius to 20 mm/Celsius to avoid too high diagrams in very wet locations.
  y_precip_rel <- fifelse(precip > 100, (precip-100)/20 + 50, precip/2)
  
  period_type <- fcase(precip > 100, 'Wet',
                       y_precip_rel > temp, 'Humid', 
                       default = 'Dry')
  
  monthly_df <- data.frame(month = month, y_precip = y_precip_rel, y_temp = temp, period_type = period_type)
  
  intersect_xy <- find_intersect_humid_dry(temp, y_precip_rel)
  
  if (nrow(intersect_xy)){
    intersect_df <- data.frame(month = intersect_xy$x, y_precip = intersect_xy$y, y_temp = intersect_xy$y)

    df_humid <- rbind(monthly_df[which(period_type %in% c('Humid', 'Wet')),], cbind(intersect_df, period_type = 'Humid'))
    df_dry <- rbind(monthly_df[which(period_type == 'Dry'),], cbind(intersect_df, period_type = 'Dry'))
  } else {
    df_humid <- monthly_df[which(period_type %in% c('Humid', 'Wet')),]
    df_dry <- monthly_df[which(period_type == 'Dry'),]
  }
  
  all_info <- unique(rbind(df_humid, df_dry))
   
  gg <- ggplot(data = all_info, mapping = aes(x = month)) + 
    geom_line(aes(month, y_temp), colour = 'red') + 
    geom_line(aes(month, y_precip), colour = 'blue') + 
    geom_hline(yintercept = 50) +
    geom_hline(yintercept = 0) +
    geom_ribbon(data = all_info[which(all_info$period_type %in% c('Humid', 'Wet')),], aes(ymax = y_precip, ymin = y_temp), fill = "blue", alpha = 0.25) +
    geom_ribbon(data = all_info[which(all_info$period_type == "Dry"),], aes(ymax = y_temp, ymin = y_precip), fill = "red", alpha = 0.25) +
    scale_x_continuous(breaks = 1:12, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), name = 'Month') +
    scale_y_continuous("Temperature (C)", sec.axis = sec_axis(transform = ~(.*2)*(.<=100)+(20*(.-100)+100)*(.>100), name = "Precipitation (mm)")) +
    coord_cartesian(xlim = c(1,12), ylim = c(min(-2, temp), max(y_precip_rel, temp, 50)), clip = 'off', expand = FALSE ) + #, expand = FALSE to start at Jan
    #annotate("text", x = 11.75, y = max(y_precip_rel, temp, 50)+4, label = paste0(trunc(sum(precip)), " mm")) + 
    #annotate("text", x = 10, y = max(y_precip_rel, temp, 50)+4, label = paste0(trunc(mean(temp)*10)/10, " C")) + 
    labs(title = sprintf('Walter-Lieth Climate Diagram           ( %.1f\u00B0C )      ( %1.fmm )', mean(temp), sum(precip)) , 
         subtitle = ifelse(is.null(elev),"", sprintf("( %1.fm )", elev))) +
    theme_classic()
  
  # Add Wet Periods (precip > 100) ----
  if (any(period_type == 'Wet')){

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
  

  # Add Freeze Periods  ----
  freeze <- which(temp < 0)
   
  for(i in seq_along(freeze)){
    df_freeze <- data.frame(x = c(pmax(1,rep(freeze[i]-0.5, 2)), pmin(rep(freeze[i]+0.5, 2), 12)),
                            y= c(0,-1, -1, 0))
    
    gg <- gg + geom_polygon(data = df_freeze, aes(x = x, y = y), fill = "blue", colour = "black", alpha = 0.6)
  }
  
  
  gg
  
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

find_intersect_humid_dry <- function(temp, y_precip_rel){

  #Compare to previous month to know if period type changed between months.
  ## January won't change because it has no prior state so we assign FALSE.
  period_type_change <- c(FALSE, (y_precip_rel[-1] > temp[-1]) != (y_precip_rel[-12] > temp[-12]))

  intersect_xy <- vapply(which(period_type_change), \(x) find_intersect_xy((x-1):x, y1=temp[(x-1):x], y2=y_precip_rel[(x-1):x]), numeric(2))

  data.frame(x = intersect_xy[1,], y = intersect_xy[2,])

}