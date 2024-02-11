#' Bivariate plots of 21st century climate change at user-selected locations
#' 
#' @description
#' 
#'  
#' @details
#' 
#' 
#' @template xyz
#'
#' @return `data.frame` of downscaled climate variables for each location.
#'   All outputs are returned in one table.
#'
#' @importFrom data.table getDTthreads setDTthreads rbindlist setkey
#' @importFrom stinepack stinterp TODO: add this package to dependencies
#'
#' @examples {
#' library(data.table)
#' 
#' }
#' @export

Test_Locations_VanIsl <- read.csv("data-raw/Test_Locations_VanIsl.csv")
xyz <- Test_Locations_VanIsl
names(xyz) <- c("lat", "lon", "elev")
xyz <- data.frame(Test_Locations_VanIsl, id=1:dim(Test_Locations_VanIsl)[1])
names(xyz) <- c("lat", "lon", "elev", "id")
plot_bivariate <- function(
    xyz, 
    xvar = "Tave_sm", 
    yvar = "PPT_sm", 
    percent_x = NULL,
    percent_y = NULL,
    period_focal = list_gcm_period()[1], 
    gcm_models = list_gcm()[c(1,4,5,6,7,10,11,12)], ## TODO: check that i am passing this to climr_downscale properly
    ssp = list_ssp()[2], ## TODO: check that i am passing this to climr_downscale properly
    legend_pos = "topright", 
    show_runs = T, 
    show_ensMean = T,
    show_observed = T,
    show_trajectories = T,
    interactive = F,
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
  
  # initiate the plot
  par(mar=c(3,4,0,1), mgp=c(1.25, 0.25,0), cex=1.5)
  plot(data_mean$xanom,data_mean$yanom,col="white", tck=0, xaxt="n", yaxt="n", ylab="",
       xlab=paste("Change in", variables_lookup$Variable[which(variables_lookup$Code==xvar)]) 
  )
  par(mgp=c(2.5,0.25, 0))
  title(ylab=paste("Change in", variables_lookup$Variable[which(variables_lookup$Code==yvar)]))
  lines(c(0,0), c(-99,99), lty=2, col="gray")
  lines(c(-99,99), c(0,0), lty=2, col="gray")
  
  # ensemble mean for the selected period
  ensMean <- data[!is.na(GCM) & RUN == "ensembleMean" & PERIOD == period_focal, .(xanom = mean(xanom), yanom = mean(yanom)), ]
  points(ensMean$xanom,ensMean$yanom, pch=43, col="gray", cex=3)
  
  # observed climate
  obs <- data[is.na(GCM) & PERIOD == period_focal]
  points(obs$xanom ,obs$yanom, pch=22, bg="gray", cex=2.5)
  # text(x1,y1, "1991-2019", cex=1.15, font=2, pos=4, col="gray", offset=0.9)  
  
  # GCM projections
  gcm=list_gcm()[1]
  for(gcm in gcm_models){
    i=which(gcm_models==gcm)
    x <- c(0, data[GCM==gcm & RUN == "ensembleMean", xanom])
    y <- c(0, data[GCM==gcm & RUN == "ensembleMean", yanom])
    x.runs <- data[GCM==gcm & RUN != "ensembleMean" & PERIOD == period_focal, xanom]
    y.runs <- data[GCM==gcm & RUN != "ensembleMean" & PERIOD == period_focal, yanom]
    if(length(unique(sign(diff(x))))==1){
      x3 <- if(unique(sign(diff(x)))==-1) rev(x) else x
      y3 <- if(unique(sign(diff(x)))==-1) rev(y) else y
      s <- stinterp(x3,y3, seq(min(x3),max(x3), diff(range(data$xanom))/500)) # way better than interpSpline, not prone to oscillations
      lines(s, col=ColScheme[i], lwd=2)
    } else lines(x, y, col=ColScheme[i], lwd=2)
    j=which(list_gcm_period()==period_focal)+1
    points(x,y, pch=16, col=ColScheme[i], cex=0.5)
    points(x[j],y[j], pch=21, bg=ColScheme[i], cex=2.5)
    points(x.runs,y.runs, pch=21, bg=ColScheme[i], cex=1)
    text(x[j],y[j], substr(gcm_models, 1, 2)[i], cex=0.5, font=2)
  }
  
  # axis labels
  axis(1, at=pretty(data_mean$xanom), labels=if(xvar_type=="ratio") paste(pretty(data_mean$xanom)*100, "%", sep="") else pretty(data_mean$xanom), tck=0)
  axis(2, at=pretty(data_mean$yanom), labels=if(yvar_type=="ratio") paste(pretty(data_mean$yanom)*100, "%", sep="") else pretty(data_mean$yanom), las=2, tck=0)
  
  # Legend TODO: modify for user selection of plot elements. 
  legend(legend_pos, 
         legend = c("Observed historical climate", "individual GCM simulation", "GCM mean", "GCM mean trajectory", "Ensemble mean"), 
         pch = c(22, 21, 21, 16, 43), 
         pt.cex = c(2.5, 1, 2.5, 0.5, 3), 
         pt.bg = c("gray", "gray", "gray", NA, NA),
         col = c(1,1,1,"gray","gray"), 
         lty = c(NA, NA, NA, 1, NA),
         lwd = c(NA, NA, NA, 2, NA),
         bty = "n", cex=0.8
  )
  
}

