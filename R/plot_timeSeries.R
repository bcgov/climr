#' Time series climate change plots
#'
#' @description
#' Time series plots of 20th and 21st century climate change for user-selected locations and climate variables.
#' The purposes of the plot are to
#' \enumerate{
#'   \item view differences in interannual variability and climate change trends among global climate models (GCMs),
#'   \item view the differences between multiple simulations of each model, and
#'   \item compare simulated and observed climate change from 1901 to present.
#'   \item compare time series of two different variables.
#' }
#' All global climate model anomalies are bias-corrected to the 1961-1990 reference period normals.
#'
#' @details
#' The input table `xyz` can be a single location or multiple locations. If multiple
#' locations, the plot provides the mean of the climate values across these locations.
#' 
#'
#' @template xyz
#' @param var character. y-axis variable. options are `list_variables()`.
#' @param legend_pos character. Position of the legend. Options are `c("bottomright",
#'   "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")`.
#' @param interactive logical. If TRUE, an interactive plot is generated using `{plotly}`.
#'   If FALSE, a plot is generated using base graphics.
#'
#' @return NULL. Draws a plot in the active graphics device.
#'
#' @importFrom graphics axis legend lines par points text title
#'
#' @examples
#' # data frame of arbitrary points
#' my_points <- data.frame(lon = -127.7300, lat = 55.34114, elev = 711, id = 1)
#'
#' # draw the plot
#' plot_timeSeries(my_points)
#'
#' # export plot to a temporary directory
#' figDir <- tempdir()
#' png(
#'   filename = file.path(figDir, "plot_test.png"), type = "cairo", units = "in",
#'   width = 6, height = 5, pointsize = 10, res = 300
#' )
#' plot_timeSeries(my_points)
#' dev.off()
#'
#' @export



# Temporary variable definitions
library(climr)
library(data.table)
library(stinepack)
library(scales)
xyz = my_points
obs.datasets = c("cru_gpcc") 
variable1 = "Tmin_sm"
variable2 = NULL
gcms.ts = list_gcm()[c(1,4,5,7,10,11,12)]
ssps = list_ssp()[1:4]
showrange = T 
yfit = T
cex = 1
showmean = T
compile = T
simplify = T
refline = T
label.endyear = F
yearlines = F
mode = "Ensemble"
legend_pos = "topleft"
interactive = FALSE
cache = TRUE
  
# generate the climate data (TODO we will have to move this outside the function because it takes too long)
data <- climr_downscale(xyz, 
                        gcm_models = list_gcm(),
                        ssp = list_ssp(),
                        max_run = 10,
                        historic_ts_dataset = "climate_na", #TODO uncomment this once we fix the error in the devl branch.
                        historic_ts = 1902:2015,
                        gcm_hist_years = 1850:2014, 
                        gcm_ts_years = 2015:2100, 
                        vars = list_variables()
)

plot_timeSeries <- function(
    xyz,
    obs.datasets = c("cru_gpcc"), #TODO we will have to resolve the inconsistencies with the dataset naming convention in climr (ideally changing the climr naming)
    variable1 = "Tmax_sm",
    variable2 = NULL,
    gcms.ts = list_gcm()[c(1,4,5,7,10,11,12)],
    ssps = list_ssp()[1:3],
    showrange = T, 
    yfit = T,
    cex = 1,
    showmean = T,
    compile = T,
    simplify = T,
    refline = T,
    label.endyear = T, 
    yearlines = T,
    mode = "Ensemble",
    legend_pos = "topleft",
    interactive = FALSE,
    cache = TRUE) {
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("package scales must be installed to use this function")
  } else  
    if (!requireNamespace("stinepack", quietly = TRUE)) {
    stop("package stinepack must be installed to use this function")
  } else {
    # data("variables", envir = environment()) #TODO temporary until we can resolve the error in the devl branch
    variables <- fread("data-raw/derivedVariables/Variables_climateBC.csv") #TODO temporary until we can resolve the error in the devl branch
    
    # Scenario definitions
    scenarios.selected <- c("historical", ssps)
    scenarios <- c("historical", list_ssp())
    scenario.names <- c("Historical simulations", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
    scenario.colScheme <- c("gray60", "dodgerblue4", "seagreen", "darkorange3", "darkred")  # these roughly match the IPCC standard scenario colours. 
    
    # yeartime definitions
    monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
    seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)],4, byrow=T)
    seasons <- c("wt", "sp", "sm", "at")
    season.names <- c("Winter", "Spring", "Summer", "Autumn")
    yeartimes <- c(seasons, monthcodes)
    yeartime.names <- c(season.names, month.name)
    
    # ensemble statistics definitions
    ensstats <- c("ensmin", "ensmax", "ensmean")
    
    ## Assemble the data that will be used in the plot (for setting the ylim)
    alldata <- data[, if(is.null(variable2)) get(variable1) else c(get(variable1), get(variable2))] # a vector of all potential data on the plot for setting the ylim (y axis range)
    visibledata <- data[GCM%in%c(NA, gcms.ts) & SSP%in%c(NA, ssps), (if(is.null(variable2)) get(variable1) else c(get(variable1), get(variable2)))] # a vector of all visible data on the plot for setting the ylim (y axis range)
    
    # components of the variable (note this will not work for monthly variables until we change the variable naming convention to underscore delimitated (e.g., Tave_01 instead of the current Tave01))
    nums <- if(is.null(variable2)) 1 else 1:2 #nums is the set of variable numbers (this is used later on as well)
    for(num in nums){ 
      variable <- get(paste("variable",num,sep=""))
      variable.components <- unlist(strsplit(variable, "_"))
      assign(paste0("yeartime", num), if(length(variable.components)==1) NA else variable.components[length(variable.components)]) #do by length because some elements have an underscore in them
      assign(paste0("element", num), if(length(grep("DD_0|DD_18", variable))==1) paste(variable.components[1:2], collapse="_") else variable.components[1])
    }
    
    # PLOT
    par(mfrow=c(1,1), mar=c(3,3,0.1,3), mgp=c(1.75, 0.25, 0), cex=1.5*cex)
    # y axis title. 
    if(is.null(variable2)){ #if there is no second variable
      ylab <- variables[Code==variable1, "Variable"]
    } else 
      if(element1==element2){ #if both variables have the same element
        ylab <- variables[Code==variable1, "Element"]
      } else 
        if(yeartime1==yeartime2){ #if both variables have the same element
          ylab <- paste(yeartime.names[which(yeartimes==yeartime1)], variables[Code==variable1, "Element"], "or", variables[Code==variable2, "Element"])
        } else { #if variables1 and 2 have different elements and yeartimes
          ylab <- paste(yeartime.names[which(yeartimes==yeartime1)], variables[Code==variable1, "Element"], "or", variables[Code==variable2, "Element"])
        }
    plot(0, col="white", xlim=c(1900, 2100), ylim=range(if(yfit==T) visibledata else alldata, na.rm = T), xaxs="i", xaxt="n", tck=0, xlab="", ylab=ylab)
    axis(1, at=seq(1850,2100,25), labels = seq(1850,2100,25), tck=0)
    
    num <- 1
    for(num in nums){
      yeartime <- get(paste("yeartime",num,sep=""))
      element <- get(paste("element",num,sep=""))
      variable <- get(paste("variable",num,sep=""))
      
      # function for plotting time series for gcm or compiled ensemble
      # gcm <- gcms.ts[1]
      # x <- data[GCM%in%gcms.ts, c("PERIOD", "SSP", "RUN", variable), with=F] # for testing only
      
      plot.ensemble <- function(x) {
        # scenario <- scenarios.selected[1]
        temp.historical <- x[is.na(SSP), c("PERIOD", "RUN", variable), with=F]
        x.historical <- as.numeric(temp.historical[, .(min = min(get(variable))), by = PERIOD][["PERIOD"]])
        ensmin.historical <- temp.historical[RUN!="ensembleMean", .(min = min(get(variable), na.rm=T)), by = PERIOD][["min"]]
        ensmax.historical <- temp.historical[RUN!="ensembleMean", .(max = max(get(variable), na.rm=T)), by = PERIOD][["max"]]
        ensmean.historical <- temp.historical[RUN=="ensembleMean", .(mean = mean(get(variable), na.rm=T)), by = PERIOD][["mean"]] #calculate mean using the single-model ensembleMean, to ensure that the mean isn't biased towards models with more runs
        
        for(scenario in scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])][-1]){
            temp <- x[SSP==scenario, c("PERIOD", "RUN", variable), with=F]
            x.temp <- as.numeric(temp[, .(min = min(get(variable))), by = PERIOD][["PERIOD"]])
            ensmin.temp <- temp[RUN!="ensembleMean", .(min = min(get(variable), na.rm=T)), by = PERIOD][["min"]]
            ensmax.temp <- temp[RUN!="ensembleMean", .(max = max(get(variable), na.rm=T)), by = PERIOD][["max"]]
            ensmean.temp <- temp[RUN=="ensembleMean", .(mean = mean(get(variable), na.rm=T)), by = PERIOD][["mean"]] #calculate mean using the single-model ensembleMean, to ensure that the mean isn't biased towards models with more runs
            assign(paste("x", scenario, sep="."), c(x.historical[length(x.historical)], x.temp)) # add last year of historical runs. this is necessary to get a seamless transition from the historical polygon to the future polygon. 
            assign(paste("ensmin", scenario, sep="."), c(ensmin.historical[length(ensmin.historical)], ensmin.temp)) # add last year of historical runs. this is necessary to get a seamless transition from the historical polygon to the future polygon. 
            assign(paste("ensmax", scenario, sep="."), c(ensmax.historical[length(ensmax.historical)], ensmax.temp)) # add last year of historical runs. this is necessary to get a seamless transition from the historical polygon to the future polygon. 
            assign(paste("ensmean", scenario, sep="."), c(ensmean.historical[length(ensmean.historical)], ensmean.temp)) # add last year of historical runs. this is necessary to get a seamless transition from the historical polygon to the future polygon. 
        }

        if(showrange==T) {
          if(simplify==F){
            for(scenario in scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])]){
              x3 <- get(paste("x", scenario, sep="."))
              polygon(c(x3, rev(x3)), c(get(paste("ensmin", scenario, sep=".")), rev(get(paste("ensmax", scenario, sep=".")))), col=alpha(scenario.colScheme [which(scenarios==scenario)], 0.35), border=scenario.colScheme[which(scenarios==scenario)])
            }
          } else {
            scenarios.select <- scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])][-1]
            for(scenario in scenarios.select){
              if(scenario==scenarios.select[1]){ # we need to run spline through the historical/projected transition
                x4 <- c(x.historical, get(paste("x", scenario, sep="."))[-1])
                y.ensmin <- c(ensmin.historical, get(paste("ensmin", scenario, sep="."))[-1])
                y.ensmax <- c(ensmax.historical, get(paste("ensmax", scenario, sep="."))[-1])
                s.ensmin <- smooth.spline(x4,y.ensmin, df=8) 
                s.ensmax <- smooth.spline(x4,y.ensmax, df=8) 
                subset.hist <- which(x4%in%x.historical)
                subset.proj <- which(x4%in%get(paste("x", scenario, sep=".")))
                polygon(c(s.ensmin$x[subset.hist], rev(s.ensmax$x[subset.hist])), c(s.ensmin$y[subset.hist], rev(s.ensmax$y[subset.hist])), col=alpha(scenario.colScheme[which(scenarios=="historical")], 0.35), border=NA)
                polygon(c(s.ensmin$x[subset.proj], rev(s.ensmax$x[subset.proj])), c(s.ensmin$y[subset.proj], rev(s.ensmax$y[subset.proj])), col=alpha(scenario.colScheme[which(scenarios==scenario)], 0.35), border=NA)
              } else { # this second routine uses interpolation splines so that the starting point for all scenarios is the same
                x5 <- c(x.historical, get(paste("x", scenario, sep="."))[-1])
                y.ensmin2 <- c(ensmin.historical, get(paste("ensmin", scenario, sep="."))[-1])
                y.ensmax2 <- c(ensmax.historical, get(paste("ensmax", scenario, sep="."))[-1])
                s.ensmin2 <- smooth.spline(x5,y.ensmin2, df=8) 
                s.ensmax2 <- smooth.spline(x5,y.ensmax2, df=8) 
                knots.hist <- which(x5%in%c(seq(1860, 2000, 20), 2014))
                knots.proj <- which(x5%in%c(seq(2030, 2090, 20), 2100))
                s.ensmin3 <- stinterp(x5[c(knots.hist, knots.proj)],c(s.ensmin$y[knots.hist], s.ensmin2$y[knots.proj]), x5)
                s.ensmax3 <- stinterp(x5[c(knots.hist, knots.proj)],c(s.ensmax$y[knots.hist], s.ensmax2$y[knots.proj]), x5)
                polygon(c(s.ensmin3$x[subset.proj], rev(s.ensmax3$x[subset.proj])), c(s.ensmin3$y[subset.proj], rev(s.ensmax3$y[subset.proj])), col=alpha(scenario.colScheme[which(scenarios==scenario)], 0.35), border=NA)
              }
            }
          }
        }
        
        if(refline==T){
          ref.temp <- mean(ensmean.historical[which(x.historical%in%1961:1990)])
          lines(1961:1990, rep(ref.temp, 30), lwd=2)
          lines(c(1990,2100), rep(ref.temp, 2), lty=2)
        }
        
        # overlay the ensemble mean lines on top of all polygons
        if(showmean==T){
          for(scenario in scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])]){
            if(simplify==F) lines(x=get(paste("x", scenario, sep=".")), y=get(paste("ensmean", scenario, sep=".")), col=scenario.colScheme[which(scenarios==scenario)], lwd=2)
            
            # calculate a spline through the time series (used for plotting and the text warming value)
            if(scenario=="historical"){ # need to run spline through the historical/projected transition
              x4 <- c(x.historical, get(paste("x", scenarios.selected[2], sep=".")))
              y4 <- c(ensmean.historical, get(paste("ensmean", scenarios.selected[2], sep=".")))
            } else {
              x4 <- c(x.historical, get(paste("x", scenario, sep=".")))
              y4 <- c(ensmean.historical, get(paste("ensmean", scenario, sep=".")))
            }
            s4 <- smooth.spline(x4,y4, df=10) 
            subset <- which(x4%in%get(paste("x", scenario, sep=".")))
            
            # plot the spline
            if(simplify==T){
              lines(x=s4$x[subset], y=s4$y[subset], col=scenario.colScheme[which(scenarios==scenario)], lwd=2)
            }
            
            # text of warming value
            if(scenario != "historical"){
              par(xpd=T)
              baseline <- mean(ensmean.historical[which(x.historical%in%1961:1990)])
              projected <- s4$y[length(s4$y)]
              if(element=="PPT"){
                change <- round(projected/baseline-1,2)
                if(is.na(change)==F) text(2098,projected, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=scenario.colScheme[which(scenarios==scenario)], pos=4, font=2, cex=1)
              } else {
                change <- round(projected-baseline,1)
                if(is.na(change)==F) text(2098,projected, if(change>0) bquote("+" * .(change) * degree * C) else bquote(.(change) * degree * C), col=scenario.colScheme[which(scenarios==scenario)], pos=4, font=2, cex=1)
              }
              par(xpd=F)
            }
            
            # Text to identify the time of year
            if(!is.null(variable2)){ #if there is no second variable
              if(element1==element2){
              label <- yeartime.names[which(yeartimes==yeartime)]
            } else {
              label <- paste(yeartime.names[which(yeartimes==yeartime)], element)
            }
            temp <- get("ensmax.historical")
            text(1925,mean(temp[10:40]), label, col="black", pos=3, font=2, cex=1)
            }
          }
        }
      }
      
      if(compile==T){ #this plots a single envelope for the ensemble as a whole
        temp.data <- data[GCM%in%gcms.ts, c("PERIOD", "SSP", "RUN", variable), with=F]
        plot.ensemble(temp.data)
        
      } else for(gcm in gcms.ts){ #this plots of individual GCM ensembles. 
        temp.data <- data[GCM==gcm, c("PERIOD", "SSP", "RUN", variable), with=F]
        plot.ensemble(temp.data)
        
        print(gcm)
      }
      
      # overlay the 5-year lines on top of all polygons
      if(yearlines){
        for(n in seq(1905, 2095, 5)){
          lines(c(n, n), c(-9999, 9999), col="grey", lty=2)
        }
      }
      
      # add in observations
      obs.colors <- c("black", "blue", "red")
      obs.options <- c("climate_na", "cru_gpcc", "era5")
      for(obs.dataset in obs.datasets){ #TODO update this code block once i know how the datasets are identified in the climr output
        obs.color <- obs.colors[which(obs.options==obs.dataset)]
        x.obs <- as.numeric(data[is.na(GCM) & PERIOD%in%1900:2100, "PERIOD"][[1]])
        y.obs <- data[is.na(GCM) & PERIOD%in%1900:2100, get(variable)]
        recent.obs <- mean(y.obs[which(x.obs%in%2014:2023)], na.rm=T)
        end <- max(which(!is.na(y.obs)))
        lines(x.obs[which(x.obs<1951)], y.obs[which(x.obs<1951)], lwd=3, lty=3, col=obs.color)
        lines(x.obs[which(x.obs>1949)], y.obs[which(x.obs>1949)], lwd=4, col=obs.color)
        points(x.obs[which(x.obs>1949)], y.obs[which(x.obs>1949)], pch=21, bg="white", cex=0.4)
        points(x.obs[which(x.obs>1949)[seq(1,999,5)]], y.obs[which(x.obs>1949)[seq(1,999,5)]], pch=21, bg="white", cex=0.7)
        if(label.endyear){
          points(x.obs[end], y.obs[end], pch=16, cex=1, col=obs.color)
          text(x.obs[end], y.obs[end], x.obs[end], pos= 4, offset = 0.25, col=obs.color, cex=1)
        }
      }
      print(num)
    }

    #legend
    a <- if("climate_na"%in%obs.datasets) 1 else NA
    b <- if("cru_gpcc"%in%obs.datasets) 2 else NA
    c <- if("era5"%in%obs.datasets) 3 else NA
    d <- if(length(gcms.ts>0)) 4 else NA
    s <- !is.na(c(a,b,c,d))
    legend.GCM <- if(mode=="Ensemble") paste("Simulated (", length(gcms.ts), " GCMs)", sep="")  else paste("Simulated (", gcms.ts, ")", sep="")
    legend(legend_pos, title = "", legend=c("Observed (ClimateNA)", "Observed (CRU/GPCC)", "Observed (ERA5)", legend.GCM)[s], bty="n",
           lty=c(1,1,1,1)[s], 
           col=c(obs.colors, "gray")[s], 
           lwd=c(4,4,4,2)[s], 
           pch=c(NA,NA,NA,NA)[s], 
           pt.bg = c(NA, NA,NA,NA)[s], 
           pt.cex=c(NA,NA,NA,NA)[s])
    
    s <- rev(which(scenarios[-1]%in%scenarios.selected))
    legend(c("top", "bottom")[if(length(grep("top", legend_pos))==1) 1 else 2], title = "Scenarios", legend=c("Historical", scenario.names[-1][s]), bty="n",
           lty=c(NA,NA,NA,NA,NA)[c(1,s+1)], col=scenario.colScheme[c(1,s+1)], lwd=c(NA,NA,NA,NA,NA)[c(1,s+1)], pch=c(22,22,22,22,22)[c(1,s+1)], pt.bg = alpha(scenario.colScheme[c(1,s+1)], 0.35), pt.cex=c(2,2,2,2,2)[c(1,s+1)])
    
    mtext(paste(" Created using climr (https://bcgov.github.io/climr/)"), side=1, line=-1.35, adj=0.0, font=1, cex=1.1, col="gray")
    
    box()
    
      }
}
