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
#' # data frame of arbitrary points on Vancouver Island
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

plot_timeSeries <- function(
    xyz,
    observations = c("climateNA"),
    variable1 = "Tmax_sm",
    variable2 = NULL,
    gcms.ts = list_gcm()[c(1,4,5,7,10,11,12)],
    gcms.compare = NA,
    ssps = list_ssp()[1:3],
    nums = c(1),
    showrange = T, 
    yfit = T,
    cex = 1,
    compare.ensemble = "None",
    showmean = T,
    compile = T,
    simplify = T,
    refline = T,
    yearlines = T,
    mode = "Ensemble",
    interactive = FALSE,
    cache = TRUE) {
  if (!requireNamespace("stinepack", quietly = TRUE)) {
    stop("package stinepack must be installed to use this function")
  } else {
    data("variables", envir = environment())
    
    # Scenario definitions
    scenarios.selected <- c("historical", ssps)
    scenarios <- c("historical", list_ssp())
    scenario.names <- c("Historical simulations", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
    
    # yeartime definitions
    monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
    seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)],4, byrow=T)
    seasons <- c("wt", "sp", "sm", "at")
    season.names <- c("Winter", "Spring", "Summer", "Autumn")
    yeartimes <- c(seasons, monthcodes)
    yeartime.names <- c(season.names, month.name)
    
    # ensemble statistics definitions
    ensstats <- c("ensmin", "ensmax", "ensmean")
    
    # generate the climate data
    data <- climr_downscale(xyz, 
                            gcm_models = list_gcm(),
                            ssp = list_ssp(),
                            max_run = 10,
                            historic_ts = 1902:2015,
                            gcm_hist_years = 1901:2014, 
                            gcm_ts_years = 2015:2100, 
                            vars = list_variables()
    )
    
    ## Assemble the data that will be used in the plot
    alldata <- vector() # a vector of all data on the plot for setting the ylim (y axis range)
    visibledata <- vector() # a vector of all visible data on the plot for setting the ylim (y axis range)
    num <- 1
    for(num in nums){
      
      # components of the variable (note this will not work for monthly variables until we change the variable naming convention to underscore delimitated (e.g., Tave_01 instead of the current Tave01))
      variable <- get(paste("variable",num,sep=""))
      variable.components <- unlist(strsplit(variable, "_"))
      yeartime <- if(length(variable.components)==1) NA else variable.components[length(variable.components)] #do by length because some elements have an underscore in them
      element <- if(length(grep("DD_0|DD_18", variable))==1) paste(variable.components[1:2], collapse="_") else variable.components[1]
      
      # data for observations
      x1 <- data[is.na(GCM) & PERIOD%in%1901:2100, PERIOD] # specified up to 2100 to allow for future updates to the time series data
      y1 <- data[is.na(GCM) & PERIOD%in%1901:2100, get(variable)]
      baseline.obs <- mean(y1[which(x1%in%1961:1990)])
      alldata <- c(alldata, y1) #store values in a big vector for maintaining a constant ylim
      visibledata <- c(visibledata, y1) #store values in a big vector for maintaining a constant ylim
      
      #compile the ensemble statistics (mean, min, max)  into a wide-format table by year and GCM
      # ensstat <- ensstats[1]
      for(ensstat in ensstats){ 
        temp.historical <- data[!is.na(GCM) & is.na(SSP), get(variable)]
        # scenario <- scenarios[2]
        for(scenario in scenarios.selected){
          temp <- data[!is.na(GCM) & SSP==scenario, get(variable)]
          if(scenario != "historical"){
            temp <- rbind(temp.historical[dim(temp.historical)[1],match(names(temp), names(temp.historical))], temp) # add last year of historical runs
          }
          if(scenario == "historical") if(ensstat=="ensmean") baseline.mod <- apply(temp[which(temp[,1]%in%1961:1990),-1], 2, mean)
          
          alldata <- c(alldata, as.vector(unlist(temp[-1]))) #store values in a big vector for maintaining a constant ylim
          temp$compile <- if(length(gcms.ts)==0) rep(NA, dim(temp)[1]) else if(length(gcms.ts)==1) temp[,which(names(temp)==gcms.ts)] else apply(temp[,which(names(temp)%in%gcms.ts)], 1, substr(ensstat, 4, nchar(ensstat)), na.rm=T)
          if(is.na(gcms.compare)!=T) temp$compare <- apply(temp[,which(names(temp)%in%gcms.compare)], 1, substr(ensstat, 4, nchar(ensstat)), na.rm=T)
          assign(paste(ensstat, scenario, num, sep="."), temp)
          if(showrange==T | ensstat==ensstats[3]) visibledata <- c(visibledata, temp$compile, if(is.na(gcms.compare)!=T) temp$compare ) #store values in a big vector for maintaining a constant ylim
        }
      }
    }
    
    # PLOT
    par(mfrow=c(1,1), mar=c(3,3,0.1,3), mgp=c(1.75, 0.25, 0), cex=1.5*cex)
    if(element1==element2){
      ylab <- element.names.units[[which(elements==element1)]]
    } else {
      ylab <- if("PPT"%in%c(element1, element2)) bquote(Precipitation~"("*mm*")"~or~Mean ~ temperature ~ "(" * degree * C * ")") else element.names.units[[1]]
    }
    plot(0, col="white", xlim=c(1900, 2100), ylim=range(if(yfit==T) visibledata else alldata, na.rm = T), xaxs="i", xaxt="n", tck=0, xlab="", ylab=ylab)
    axis(1, at=seq(1850,2100,25), labels = seq(1850,2100,25), tck=0)
    
    num <- 1
    for(num in nums){
      yeartime <- get(paste("yeartime",num,sep=""))
      element <- get(paste("element",num,sep=""))
      variable <- get(paste("variable",num,sep=""))
      
      # data for observations
      x.climatebc <- unique(obs.ts.mean[,1])
      y.climatebc <- obs.ts.mean[,which(names(obs.ts.mean)==variable)]
      baseline.obs <- mean(y.climatebc[which(x.climatebc%in%1961:1990)])
      baseline.obs.1981 <- mean(y.climatebc[which(x.climatebc%in%1981:2010)]) # for ERA5, because it has different bias prior to 1979
      recent.climatebc <- mean(y.climatebc[which(x.climatebc%in%2013:2022)])
      
      # data for era5
      if("era5"%in%observations){
        era5.ts.mean <- read.csv(paste("data/ts.era5.mean.", ecoprov, ".csv", sep=""))
        x.era5 <- unique(era5.ts.mean[,1])
        y.era5 <- era5.ts.mean[,which(names(era5.ts.mean)==variable)]
        baseline.era5 <- mean(y.era5[which(x.era5%in%1981:2010)]) # for ERA5, because it has different bias prior to 1979
        bias.era5 <- baseline.obs.1981 - baseline.era5
        recent.era5 <- mean(y.era5[which(x.era5%in%2013:2022)], na.rm=T)
      }
      
      # data for era5land
      if("era5land"%in%observations){
        era5land.ts.mean <- read.csv(paste("data/ts.era5land.mean.", ecoprov, ".csv", sep=""))
        x.era5land <- unique(era5land.ts.mean[,1])
        y.era5land <- era5land.ts.mean[,which(names(era5land.ts.mean)==variable)]
        baseline.era5land <- mean(y.era5land[which(x.era5land%in%1981:2010)]) # for ERA5, because it has different bias prior to 1979
        bias.era5land <- baseline.obs.1981 - baseline.era5land
        recent.era5land <- mean(y.era5land[which(x.era5land%in%2013:2022)], na.rm=T)
      }
      
      # data for pcic
      if("pcic"%in%observations){
        pcic.ts.mean <- read.csv(paste("data/ts.pcic.mean.", ecoprov, ".csv", sep=""))
        x.pcic <- unique(pcic.ts.mean[,1])
        y.pcic <- if(element=="PPT") pcic.ts.mean[,which(names(pcic.ts.mean)==variable)]*mean(y.climatebc[which(x.climatebc%in%1981:2010)]) + mean(y.climatebc[which(x.climatebc%in%1981:2010)]) else pcic.ts.mean[,which(names(pcic.ts.mean)==variable)] + mean(y.climatebc[which(x.climatebc%in%1981:2010)])  # apply faron's anomalies to the 1981-2010 absolute value of climatebc time series. 
        baseline.pcic <- mean(y.pcic[which(x.pcic%in%1961:1990)])
        y.pcic <- if(element=="PPT") y.pcic*(baseline.obs/baseline.pcic) else y.pcic+(baseline.obs-baseline.pcic)   # bias correct to 1961-1990 period
        recent.pcic <- mean(y.pcic[which(x.pcic%in%2012:2021)], na.rm=T)
      }
      
      # # data for cru/gpcc
      # if("cru"%in%observations){
      #   cru.ts.mean <- read.csv(paste("data/ts.cru.mean.", ecoprov, ".csv", sep=""))
      #   x.cru <- unique(cru.ts.mean[,1])
      #   y.cru <- cru.ts.mean[,which(names(cru.ts.mean)==variable)]
      #   baseline.cru <- mean(y.cru[which(x.cru%in%1961:1990)])
      #   bias.cru <- baseline.obs - baseline.cru
      #   recent.cru <- mean(y.cru[which(x.cru%in%2014:2023)], na.rm=T)
      # }
      
      # data for GISTEMP
      if("giss"%in%observations){
        giss.ts.mean <- read.csv(paste("data/ts.giss.mean.", ecoprov, ".csv", sep=""))
        x.giss <- unique(giss.ts.mean[,1])
        y.giss <- giss.ts.mean[,which(names(giss.ts.mean)==variable)]
        baseline.giss <- mean(y.giss[which(x.giss%in%1961:1990)])
        bias.giss <- baseline.obs - baseline.giss
        recent.giss <- mean(y.giss[which(x.giss%in%2013:2022)], na.rm=T)
      }
      
      # time series for the comparison ensemble
      colScheme <- c("gray60", "dodgerblue4", "seagreen", "darkorange3", "darkred")
      if(compare.ensemble!="None"){
        # scenario <- scenarios.selected[1]
        for(scenario in scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])]){
          
          for(ensstat in ensstats){
            temp <- get(paste(ensstat, scenario, num, sep="."))
            x <- temp[,1]
            temp <- temp$compare
            assign(ensstat, temp)
            assign(paste("x", scenario, sep="."), x)
            assign(paste(ensstat, scenario, sep="."), temp)
          }
          
          if(showrange==T) polygon(c(x, rev(x)), c(ensmin, rev(ensmax)), col=alpha(colScheme[which(scenarios==scenario)], 0.25), border=colScheme[which(scenarios==scenario)], lty=2)
          
          if(scenario != "historical"){
            par(xpd=T)
            baseline <- mean(ensmean.historical[111:140])
            projected <- mean(ensmean[(length(x)-5):(length(x))])
            if(element=="PPT"){
              change <- round(projected/baseline-1,2)
              if(is.na(change)==F) text(2098,projected, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=colScheme[which(scenarios==scenario)], pos=4, font=1, cex=0.8)
            } else {
              change <- round(projected-baseline,1)
              if(is.na(change)==F) text(2098,projected, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=colScheme[which(scenarios==scenario)], pos=4, font=1, cex=0.8)
            }
            par(xpd=F)
          }
          
          print(scenario)
        }
        
        # overlay the ensemble mean lines on top of all polygons
        for(scenario in scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])]){
          if(showmean==T) lines(get(paste("x", scenario, sep=".")), get(paste("ensmean", scenario, sep=".")), col=colScheme[which(scenarios==scenario)], lwd=2, lty=2)
        }
      }
      
      # time series for selected ensemble
      if(compile==T) gcms.ts <- "compile" #this prevents the plotting of individual GCM projections and plots a single envelope for the ensemble as a whole. 
      gcm <- gcms.ts[1]
      for(gcm in gcms.ts){
        # scenario <- scenarios.selected[1]
        for(scenario in scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])]){
          
          for(ensstat in ensstats){
            temp <- get(paste(ensstat, scenario, num, sep="."))
            x <- temp[,1]
            temp <- temp[,which(names(temp)==gcm)]
            assign(ensstat, temp)
            assign(paste("x", scenario, sep="."), x)
            assign(paste(ensstat, scenario, sep="."), temp)
          }
          
          print(scenario)
        }
        
        if(showrange==T) {
          if(simplify==F){
            for(scenario in scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])]){
              x <- get(paste("x", scenario, sep="."))
              polygon(c(x, rev(x)), c(get(paste("ensmin", scenario, sep=".")), rev(get(paste("ensmax", scenario, sep=".")))), col=alpha(colScheme[which(scenarios==scenario)], if(gcm=="ensemble") 0.35 else 0.35), border=colScheme[which(scenarios==scenario)])
            }
          } else {
            scenarios.select <- scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])][-1]
            for(scenario in scenarios.select){
              if(scenario==scenarios.select[1]){ # need to run spline through the historical/projected transition
                x4 <- c(x.historical, get(paste("x", scenario, sep="."))[-1])
                y.ensmin <- c(ensmin.historical, get(paste("ensmin", scenario, sep="."))[-1])
                y.ensmax <- c(ensmax.historical, get(paste("ensmax", scenario, sep="."))[-1])
                s.ensmin <- smooth.spline(x4,y.ensmin, df=8) 
                s.ensmax <- smooth.spline(x4,y.ensmax, df=8) 
                subset.hist <- which(x4%in%x.historical)
                subset.proj <- which(x4%in%get(paste("x", scenario, sep=".")))
                polygon(c(s.ensmin$x[subset.hist], rev(s.ensmax$x[subset.hist])), c(s.ensmin$y[subset.hist], rev(s.ensmax$y[subset.hist])), col=alpha(colScheme[which(scenarios=="historical")], if(gcm=="ensemble") 0.35 else 0.35), border=NA)
                polygon(c(s.ensmin$x[subset.proj], rev(s.ensmax$x[subset.proj])), c(s.ensmin$y[subset.proj], rev(s.ensmax$y[subset.proj])), col=alpha(colScheme[which(scenarios==scenario)], if(gcm=="ensemble") 0.35 else 0.35), border=NA)
              } else { # this second routine uses interpolation splines so that the starting point for all scenarios is the same
                x5 <- c(x.historical, get(paste("x", scenario, sep="."))[-1])
                y.ensmin2 <- c(ensmin.historical, get(paste("ensmin", scenario, sep="."))[-1])
                y.ensmax2 <- c(ensmax.historical, get(paste("ensmax", scenario, sep="."))[-1])
                s.ensmin2 <- smooth.spline(x5,y.ensmin2, df=8) 
                s.ensmax2 <- smooth.spline(x5,y.ensmax2, df=8) 
                knots.hist <- c(1, 20, 40, 80, 100, 120, 140, 165)
                knots.proj <- c(190, 210, 230, 250, length(x5))
                s.ensmin3 <- stinterp(x5[c(knots.hist, knots.proj)],c(s.ensmin$y[knots.hist], s.ensmin2$y[knots.proj]), x5)
                s.ensmax3 <- stinterp(x5[c(knots.hist, knots.proj)],c(s.ensmax$y[knots.hist], s.ensmax2$y[knots.proj]), x5)
                polygon(c(s.ensmin3$x[subset.proj], rev(s.ensmax3$x[subset.proj])), c(s.ensmin3$y[subset.proj], rev(s.ensmax3$y[subset.proj])), col=alpha(colScheme[which(scenarios==scenario)], if(gcm=="ensemble") 0.35 else 0.35), border=NA)
              }
            }
          }
        }
        
        if(refline==T){
          ref.temp <- mean(ensmean.historical[111:140])
          lines(1961:1990, rep(ref.temp, 30), lwd=2)
          lines(c(1990,2100), rep(ref.temp, 2), lty=2)
        }
        
        # overlay the ensemble mean lines on top of all polygons
        if(showmean==T){
          for(scenario in scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])]){
            if(simplify==F) lines(x=get(paste("x", scenario, sep=".")), y=get(paste("ensmean", scenario, sep=".")), col=colScheme[which(scenarios==scenario)], lwd=2)
            
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
              lines(x=s4$x[subset], y=s4$y[subset], col=colScheme[which(scenarios==scenario)], lwd=2)
            }
            
            # text of warming value
            if(scenario != "historical"){
              par(xpd=T)
              baseline <- mean(ensmean.historical[111:140])
              projected <- s4$y[length(s4$y)]
              if(element=="PPT"){
                change <- round(projected/baseline-1,2)
                if(is.na(change)==F) text(2098,projected, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=colScheme[which(scenarios==scenario)], pos=4, font=2, cex=1)
              } else {
                change <- round(projected-baseline,1)
                if(is.na(change)==F) text(2098,projected, if(change>0) bquote("+" * .(change) * degree * C) else bquote(.(change) * degree * C), col=colScheme[which(scenarios==scenario)], pos=4, font=2, cex=1)
              }
              par(xpd=F)
            }
          }
        }
        print(gcm)
      }
      
      # overlay the 5-year lines on top of all polygons
      if(yearlines==T){
        for(n in seq(1905, 2095, 5)){
          lines(c(n, n), c(-9999, 9999), col="grey", lty=2)
        }
      }
      
      # Text to identify the time of year
      # if(input$compare==T){
      if(element1==element2){
        label <- yeartime.names[which(yeartimes==yeartime)]
      } else {
        label <- paste(yeartime.names[which(yeartimes==yeartime)], get(paste("element", num, sep="")))
      }
      temp <- get(paste("ensmax.historical", num, sep="."))
      text(1925,mean(temp$compile[60:80]), label, col="black", pos=3, font=2, cex=1)
      # }
      
      # add in PCIC observations
      pcic.color <- "blue"
      if("pcic"%in%observations){
        end <- max(which(!is.na(y.pcic)))
        lines(x.pcic[which(x.pcic<1951)], y.pcic[which(x.pcic<1951)], lwd=3, lty=3, col=pcic.color)
        lines(x.pcic[which(x.pcic>1949)], y.pcic[which(x.pcic>1949)], lwd=3, col=pcic.color)
        points(x.pcic[end], y.pcic[end], pch=16, cex=1, col=pcic.color)
        text(x.pcic[end], y.pcic[end], x.pcic[end], pos= 4, offset = 0.25, col=pcic.color, cex=1)
        if(element=="PPT"){
          change <- round(recent.pcic/baseline.obs-1,2)
          # text(2021,recent.pcic, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=pcic.color, pos=4, font=2, cex=1)
        } else {
          change <- round(recent.pcic-baseline.obs,1)
          # text(2021,recent.pcic, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=pcic.color, pos=4, font=2, cex=1)
        }
        lines(1961:1990, rep(baseline.obs, 30), lwd=1, col=pcic.color)
        # lines(c(1990,2021), rep(baseline.obs, 2), lty=2, col=pcic.color)
        # lines(c(2012,2021), rep(recent.pcic, 2), lty=2, col=pcic.color)
      }
      
      # add in ClimateBC observations
      obs.color <- "black"
      if("climatebc"%in%observations){
        end <- max(which(!is.na(y.climatebc)))
        lines(x.climatebc[which(x.climatebc<1951)], y.climatebc[which(x.climatebc<1951)], lwd=1.5, lty=3, col=obs.color)
        lines(x.climatebc[which(x.climatebc>1949)], y.climatebc[which(x.climatebc>1949)], lwd=1.5, col=obs.color)
        points(x.climatebc[end], y.climatebc[end], pch=16, cex=1, col=obs.color)
        text(x.climatebc[end], y.climatebc[end], x.climatebc[end], pos= 4, offset = 0.25, col=obs.color, cex=1)
        # if(!("pcic"%in%observations)){
        if(element=="PPT"){
          change <- round(recent.climatebc/baseline.obs-1,2)
          # text(2022,recent.climatebc, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=obs.color, pos=4, font=2, cex=1)
        } else {
          change <- round(recent.climatebc-baseline.obs,1)
          # text(2022,recent.climatebc, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=obs.color, pos=4, font=2, cex=1)
        }
        # }
        # lines(1961:1990, rep(baseline.obs, 30), lwd=1, col=obs.color)
        # lines(c(1990,2021), rep(baseline.obs, 2), lty=2, col=obs.color)
        # lines(c(2013,2022), rep(recent.climatebc, 2), lty=2, col=obs.color)
      }
      
      # add in era5 observations
      era5.color <- "red"
      if("era5"%in%observations){
        end <- max(which(!is.na(y.era5)))
        lines(x.era5, y.era5, col=era5.color, lwd=2)
        points(x.era5[end], y.era5[end], pch=16, cex=1, col=era5.color)
        text(x.era5[end], y.era5[end], x.era5[end], pos= 4, offset = 0.25, col=era5.color, cex=1)
        if(element=="PPT"){
          change <- round(recent.era5/baseline.obs-1,2)
          # text(2021,recent.era5, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=era5.color, pos=4, font=2, cex=1)
        } else {
          change <- round(recent.era5-baseline.obs,1)
          # text(2021,recent.era5, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=era5.color, pos=4, font=2, cex=1)
        }
        lines(1961:1990, rep(baseline.obs, 30), lwd=1, col=era5.color)
        # lines(c(1990,2021), rep(baseline.obs, 2), lty=2, col=era5.color)
        # lines(c(2012,2021), rep(recent.era5, 2), lty=2, col=era5.color)
      }
      
      # add in era5land observations
      era5land.color <- "darkorange"
      if("era5land"%in%observations){
        end <- max(which(!is.na(y.era5land)))
        lines(x.era5land, y.era5land, col=era5land.color, lwd=2)
        points(x.era5land[end], y.era5land[end], pch=16, cex=1, col=era5land.color)
        text(x.era5land[end], y.era5land[end], x.era5land[end], pos= 4, offset = 0.25, col=era5land.color, cex=1)
        if(element=="PPT"){
          change <- round(recent.era5land/baseline.obs-1,2)
          # text(2021,recent.era5land, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=era5land.color, pos=4, font=2, cex=1)
        } else {
          change <- round(recent.era5land-baseline.obs,1)
          # text(2021,recent.era5land, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=era5land.color, pos=4, font=2, cex=1)
        }
        lines(1961:1990, rep(baseline.obs, 30), lwd=1, col=era5land.color)
        # lines(c(1990,2021), rep(baseline.obs, 2), lty=2, col=era5land.color)
        # lines(c(2012,2021), rep(recent.era5land, 2), lty=2, col=era5land.color)
      }
      
      # add in GISTEMP observations
      giss.color <- "black"
      if("giss"%in%observations){
        end <- max(which(!is.na(y.giss)))
        lines(x.giss[which(x.giss<1951)], y.giss[which(x.giss<1951)], lwd=2, lty=3, col=obs.color)
        lines(x.giss[which(x.giss>1949)], y.giss[which(x.giss>1949)], lwd=2, col=obs.color)
        points(x.giss[end], y.giss[end], pch=16, cex=1, col=giss.color)
        text(x.giss[end], y.giss[end], x.giss[end], pos= 4, offset = 0.25, col=giss.color, cex=1)
        if(element=="PPT"){
          change <- round(recent.giss/baseline.obs-1,2)
          # text(2021,recent.giss, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=giss.color, pos=4, font=2, cex=1)
        } else {
          change <- round(recent.giss-baseline.obs,1)
          # text(2021,recent.giss, if(change>0) paste("+",change,"C", sep="") else paste(change,"C", sep=""), col=giss.color, pos=4, font=2, cex=1)
        }
        lines(1961:1990, rep(baseline.obs, 30), lwd=1, col=giss.color)
        # lines(c(1990,2021), rep(baseline.obs, 2), lty=2, col=giss.color)
        # lines(c(2013,2022), rep(recent.giss, 2), lty=2, col=giss.color)
      }
      
      #legend
      a <- if("pcic"%in%observations) 1 else NA
      b <- if("climatebc"%in%observations) 2 else NA
      c <- if("era5"%in%observations) 3 else NA
      d <- if("era5land"%in%observations) 4 else NA
      e <- if("giss"%in%observations) 5 else NA
      f <- if(length(gcms.ts>0)) 6 else NA
      g <- if(compare.ensemble!="None") 7 else NA
      s <- !is.na(c(a,b,c,d,e,f,g))
      legend.GCM <- if(mode=="Ensemble") paste("Simulated (", length(gcms.ts), " GCMs)", sep="")  else paste("Simulated (", gcms.ts, ")", sep="")
      legend.compare <- paste("Simulated (", length(gcms.compare), " GCMs)", sep="")  
      legend("topleft", title = "", legend=c("Observed (PCIC)", "Observed (ClimateBC)", "ERA5 reanalysis", "ERA5-land reanalysis", "Observed (GISTEMP)", legend.GCM, legend.compare)[s], bty="n",
             lty=c(1,1,1,1,1,1,2)[s], 
             col=c(pcic.color, obs.color, era5.color, era5land.color, giss.color, "gray", "gray")[s], 
             lwd=c(3,1.5,2,2,2,2,2)[s], 
             pch=c(NA,NA,NA,NA,NA,NA,NA)[s], 
             pt.bg = c(NA, NA,NA,NA,NA,NA,NA)[s], 
             pt.cex=c(NA,NA,NA,NA,NA,NA,NA)[s])
      
      s <- rev(which(scenarios[-1]%in%scenarios.selected))
      legend("top", title = "Scenarios", legend=c("Historical", scenario.names[-1][s]), bty="n",
             lty=c(NA,NA,NA,NA,NA)[c(1,s+1)], col=colScheme[c(1,s+1)], lwd=c(NA,NA,NA,NA,NA)[c(1,s+1)], pch=c(22,22,22,22,22)[c(1,s+1)], pt.bg = alpha(colScheme[c(1,s+1)], 0.35), pt.cex=c(2,2,2,2,2)[c(1,s+1)])
      
      mtext(ecoprov.names[which(ecoprovs==ecoprov)], side=1, line=-1.5, adj=0.95, font=2, cex=1.4)
      
      mtext(paste(" Created using https://bcgov-env.shinyapps.io/cmip6-BC\n", "Contact: Colin Mahony colin.mahony@gov.bc.ca"), side=1, line=-1.35, adj=0.0, font=1, cex=1.1, col="gray")
      
      print(num)
    }
    box()
    
      }
}
