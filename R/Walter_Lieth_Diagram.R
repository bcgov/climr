



find_intersect_xy <- function(x, y1, y2){
  coef_1 <- coef(lm(y1~x))
  coef_2 <- coef(lm(y2~x))
  #find x
  inter_x <- as.numeric((coef_2[1] - coef_1[1]) / (coef_1[2] - coef_2[2]))
  c(inter_x, as.numeric(coef_1[1] + inter_x*coef_1[2]))
} 



# Make Graph continuous between Dec and Jan ? 
# Create zone above 100?  --> wet period
# Separate Sure Frost vs Probable Frost (we are currently based on average temp (sure forst) instead of Tmin (probable frost))
# Add info on top of Graph 


plot_climate_diagram <- function(temp, precip, elev = NULL, period = NULL, location = NULL){
  
  month <- 1:12
  
  # when monthly precipitation is greater than 100 mm, the scale is increased from 2 mm/Celsius to 20 mm/Celsius to avoid too high diagrams in very wet locations.
  y_precip_rel <- ifelse(precip > 100, (precip-100)/20+50, precip/2)
  
  period_type <- ifelse(y_precip_rel > temp, ifelse(precip > 100, 'Wet' ,'Humid'), 'Dry')
  
  humid_period_switch <- c(FALSE, (y_precip_rel[2:12] > temp[2:12]) != (y_precip_rel[1:11] > temp[1:11])) #TODO 1st value not necessarily FALSE ?
  
  intersections <- vapply(which(humid_period_switch), 
                          \(x) find_intersect_xy((x-1):x, y1=temp[(x-1):x], y2=y_precip_rel[(x-1):x]), 
                          numeric(2))
  
  
  which_humid <- which(period_type %in% c('Humid', 'Wet'))
  
  df_humid = data.frame(
    month = c(intersections[1,],  month[which_humid]),                        
    y_precip = c(intersections[2,],  y_precip_rel[which_humid]), 
    y_temp = c(intersections[2,],  temp[which_humid]), 
    period_type = "Humid"
  )
  
  which_dry <- which(period_type %in% c('Dry'))
  df_dry = data.frame(
    month = c(intersections[1,],  month[which_dry]),                        
    y_precip = c(intersections[2,],  y_precip_rel[which_dry]), 
    y_temp = c(intersections[2,],  temp[which_dry]), 
    period_type = "Dry"
  )
  
  which_wet <- which(precip>100)
  
  #TODO Handle case when more than 1 continyous points above 100 
  intersections_100 <- cbind(vapply(which_wet, \(x) find_intersect_xy((x-1):x, y1 = c(50, 50), y2 = y_precip_rel[(x-1):x]), numeric(2)), 
                             vapply(which_wet, \(x) find_intersect_xy(x:(x+1), y1 = c(50, 50), y2 = y_precip_rel[x:(x+1)]), numeric(2)))
  
  intersections_100 <- data.frame(intersect_x = intersections_100[1,], intersect_y = intersections_100[2,])
  
  
  df_wet <- data.frame(
    month = c(month[which_wet], intersections_100$intersect_x),
    y_precip = c(y_precip_rel[which_wet], intersections_100$intersect_y), 
    y_100 = 50
  )
  
  
  all_info <- unique(rbind(df_humid, df_dry))
  
  title <- paste0(location, " (", elev, "m)")
  
  gg <- ggplot(data = all_info, mapping = aes(x = month)) + 
    geom_line(aes(month, y_temp), colour = 'red') + 
    geom_line(aes(month, y_precip), colour = 'blue') + 
    geom_hline(yintercept = 50) +
    geom_hline(yintercept = 0) +
    geom_ribbon(data = all_info[which(all_info$period_type %in% c('Humid', 'Wet')),], aes(ymax = y_precip, ymin = y_temp), fill = "blue", alpha = 0.25) +
    geom_ribbon(data = all_info[which(all_info$period_type == "Dry"),], aes(ymax = y_temp, ymin = y_precip), fill = "red", alpha = 0.25) +
    geom_ribbon(data = df_wet, aes(ymax = y_precip, ymin = y_100), fill = 'black', alpha = 0.50) +
    scale_x_continuous(breaks = 1:12, labels = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'), name = 'Month') +
    scale_y_continuous("Temperature (C)", sec.axis = sec_axis(transform = ~(.*2)*(.<=100)+(20*(.-100)+100)*(.>100), name = "Precipitation (mm)")) +
    coord_cartesian(xlim = c(1,12), ylim = c(min(-2, temp), max(y_precip_rel, temp, 50)), clip = 'off', expand = FALSE ) + #, expand = FALSE to start at Jan
    annotate("text", x = 11.75, y = max(y_precip_rel, temp, 50)+4, label = paste0(trunc(sum(precip)), " mm")) + 
    annotate("text", x = 10, y = max(y_precip_rel, temp, 50)+4, label = paste0(trunc(mean(temp)*10)/10, " C")) + 
    labs(title = title, subtitle = paste0("(", period, ")")) +
    theme_classic()
  
  
  freeze <- which(temp < 0)
   
  for(i in seq_along(freeze)){
    df_freeze <- data.frame(x = c(pmax(1,rep(freeze[i]-0.5, 2)), pmin(rep(freeze[i]+0.5, 2), 12)),
                            y= c(0,-1, -1, 0))
    
    gg <- gg + geom_polygon(data = df_freeze, aes(x = x, y = y), fill = "blue", colour = "black", alpha = 0.6)
  }
  
  
  gg
  
} 


temp <- c(14, 14, 17, 20, 24, 28, 31, 31, 27, 23, 18, 15)
precip <- c(93.1, 62.1, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 73.8, 68, 155.3, 76.3)
library(ggplot2)

plot_climate_diagram(temp = temp, precip = precip,  elev = 25, period = '1961-2009', location = 'Napoli')

plot_climate_diagram(temp = c(-3, 14, 17, 20, 24, 28, 31, 31, 27, 23, 18, -5), 
                     precip = c(93.1, 62.1, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 73.8, 68, 155.3, 76.3), 
                     elev = 25, 
                     period = '1961-2009', 
                     location = 'Napoli')

# Current nmaes in repo 
##PPT
##Tmin
##Tmax

