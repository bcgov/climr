#' Bivariate climate change plots
#' 
#' @description
#' Bivariate plots of 21st century climate change for user-selected locations and climate variables.
#'  
#' @details
#' The input table `xyz` can be a single location or multiple locations. If multiple locations, the plot provides the mean of the anomalies for these locations. 
#' 
#' The climate change trajectories provided by `show_trajectories` are connected with an interpolation spline when the x variable is monotonic; otherwise the trajectory points are connected by straight lines. 
#' 
#' @template xyz
#' @param xvar character. x-axis variable. options are `list_variables()`.
#' @param yvar character. y-axis variable. options are `list_variables()`.
#' @param period_focal character. The 20-year period for which to plot the ensemble detail. options are `list_gcm_period()`.
#' @param legend_pos character. Position of the legend. Options are c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"). 
#' @param show_runs logical. If TRUE, the individual runs of the model are plotted (for `period_focal` only) in addition to the single-model ensemble mean.
#' @param show_ensMean logical. If TRUE, the multi-model ensemble mean is plotted (for `period_focal` only). 
#' @param show_observed logical. If TRUE, the 2001-2020 observed climate is plotted. 
#' @param show_trajectories logical. If TRUE, the values of the single-model ensemble mean are plotted for all 20-year periods in `list_gcm_period()`, connected by an interpolation spline. 
#' @param interactive logical. If TRUE, an interactive plot is generated using plotly(). If FALSE, a plot is generated using base graphics. 
#' 
#' @return TODO 
#'
#' @importFrom data.table TODO: find out what i need to do to get this command right. 
#' @importFrom stinepack stinterp TODO: add this package to dependencies
#'
#' @examples {
#' library(data.table)
#' TODO: add example usage
#' }
#' @export

# Test_Locations_VanIsl <- read.csv("data-raw/Test_Locations_VanIsl.csv")
# xyz <- data.frame(Test_Locations_VanIsl, id=1:dim(Test_Locations_VanIsl)[1])
# names(xyz) <- c("lat", "lon", "elev", "id")
plot_bivariate <- function(
    xyz, 
    xvar = "Tave_sm", 
    yvar = "PPT_sm", 
    # percent_x = NULL, TODO: set up an override for ratio variables being expressed as percent anomalies
    # percent_y = NULL, TODO: set up an override for ratio variables being expressed as percent anomalies
    period_focal = list_gcm_period()[1], 
    gcm_models = list_gcm()[c(1,4,5,6,7,10,11,12)], ## TODO: check that i am passing this to climr_downscale properly
    ssp = list_ssp()[2], ## TODO: check that i am passing this to climr_downscale properly
    legend_pos = "topright", 
    show_runs = T, 
    show_ensMean = T,
    show_observed = T,
    show_trajectories = T,
    interactive = F, ## TODO: add a plotly version of the plot
    ...
) {
  
  variables_lookup <- read.csv("data-raw/variables_lookup.csv")
  
  # variable types for default scaling (percent or absolute)
  xvar_type <- variables_lookup$Type[which(variables_lookup$Code==xvar)]
  yvar_type <- variables_lookup$Type[which(variables_lookup$Code==yvar)]
  
  colors = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)][-1]
  set.seed(2)
  ColScheme <- sample(colors,length(gcm_models)) # TODO select better colors

  # generate the climate data
  data <- climr_downscale(xyz, 
                          which_normal = "auto",
                          historic_period = list_historic()[1],
                          gcm_models = list_gcm()[c(1,4,5,6,7,10,11,12)], ## TODO: this is wrong. need to recieve this as passed from plot_bivariate()
                          ssp = list_ssp()[2], ## TODO: this is wrong. need to receive this as passed from plot_bivariate()
                          gcm_period = list_gcm_period(), 
                          max_run = 10, 
                          vars = c(xvar, yvar)
                          )
  
  # convert absolute values to anomalies
  data[, xanom := if(xvar_type=="ratio") (get(xvar)/get(xvar)[1]-1) else (get(xvar) - get(xvar)[1]), by = id]
  data[, yanom := if(yvar_type=="ratio") (get(yvar)/get(yvar)[1]-1) else (get(yvar) - get(yvar)[1]), by = id]
  
  # collapse the points down to a mean anomaly
  data <- data[, .(xanom = mean(xanom), yanom = mean(yanom)), by = .(GCM, SSP, RUN, PERIOD)]
  # ensemble mean for the selected period
  ensMean <- data[!is.na(GCM) & RUN == "ensembleMean" & PERIOD == period_focal, .(xanom = mean(xanom), yanom = mean(yanom)), ]
  # observed climate
  obs <- data[is.na(GCM) & PERIOD == period_focal]
  
  if(interactive == F){
    # BASE PLOT
    # initiate the plot
    par(mar=c(3,4,0,1), mgp=c(1.25, 0.25,0), cex=1.5)
    plot(data_mean$xanom,data_mean$yanom,col="white", tck=0, xaxt="n", yaxt="n", ylab="",
         xlab=paste("Change in", variables_lookup$Variable[which(variables_lookup$Code==xvar)]) 
    )
    par(mgp=c(2.5,0.25, 0))
    title(ylab=paste("Change in", variables_lookup$Variable[which(variables_lookup$Code==yvar)]))
    lines(c(0,0), c(-99,99), lty=2, col="gray")
    lines(c(-99,99), c(0,0), lty=2, col="gray")
    
    if(show_ensMean) points(ensMean$xanom,ensMean$yanom, pch=43, col="gray", cex=3)
    if(show_observed) points(obs$xanom ,obs$yanom, pch=22, bg="gray", cex=2.5)
    # text(x1,y1, "2001-2020", cex=1.15, font=2, pos=4, col="gray", offset=0.9)  
    
    # GCM projections
    gcm=list_gcm()[1]
    for(gcm in gcm_models){
      i=which(gcm_models==gcm)
      x2 <- c(0, data[GCM==gcm & RUN == "ensembleMean", xanom])
      y2 <- c(0, data[GCM==gcm & RUN == "ensembleMean", yanom])
      x.runs <- data[GCM==gcm & RUN != "ensembleMean" & PERIOD == period_focal, xanom]
      y.runs <- data[GCM==gcm & RUN != "ensembleMean" & PERIOD == period_focal, yanom]
      if(show_trajectories){
        if(length(unique(sign(diff(x2))))==1){
          x3 <- if(unique(sign(diff(x2)))==-1) rev(x2) else x2
          y3 <- if(unique(sign(diff(x2)))==-1) rev(y2) else y2
          s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(range(data$xanom))/500)) # way better than interpSpline, not prone to oscillations
          lines(s, col=ColScheme[i], lwd=2)
        } else lines(x2, y2, col=ColScheme[i], lwd=2)
        points(x2,y2, pch=16, col=ColScheme[i], cex=0.5)
      }
      j=which(list_gcm_period()==period_focal)+1
      points(x2[j],y2[j], pch=21, bg=ColScheme[i], cex=2.5)
      if(show_runs) points(x.runs,y.runs, pch=21, bg=ColScheme[i], cex=1)
      text(x2[j],y2[j], substr(gcm_models, 1, 2)[i], cex=0.5, font=2)
    }
    
    # axis labels
    axis(1, at=pretty(data_mean$xanom), labels=if(xvar_type=="ratio") paste(pretty(data_mean$xanom)*100, "%", sep="") else pretty(data_mean$xanom), tck=0)
    axis(2, at=pretty(data_mean$yanom), labels=if(yvar_type=="ratio") paste(pretty(data_mean$yanom)*100, "%", sep="") else pretty(data_mean$yanom), las=2, tck=0)
    
    # Legend 
    s <- c(show_observed, show_runs, T, show_trajectories, show_ensMean)
    legend(legend_pos, 
           legend = c("Observed climate (2001-2020)", "individual GCM simulation", "GCM mean", "GCM mean trajectory", "Ensemble mean")[s], 
           pch = c(22, 21, 21, 16, 43)[s], 
           pt.cex = c(2.5, 1, 2.5, 0.5, 3)[s], 
           pt.bg = c("gray", "gray", "gray", NA, NA)[s],
           col = c(1,1,1,"gray","gray")[s], 
           lty = c(NA, NA, NA, 1, NA)[s],
           lwd = c(NA, NA, NA, 2, NA)[s],
           bty = "n", cex=0.8
    )
  } else {
    # PLOTLY PLOT
  }
}

