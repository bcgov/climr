#' Time series plots of climate change
#'
#' @description
#' Time series plots of 20th and 21st century climate change for user-selected locations and climate variables.
#' The purposes of the plot are to:
#' \enumerate{
#'   \item view differences in interannual variability and climate change trends among global climate models (GCMs),
#'   \item view the differences between multiple simulations of each model,
#'   \item compare simulated and observed climate change from 1901 to present, and
#'   \item compare time series of two different variables.
#' }
#' All global climate model anomalies are bias-corrected to the 1961-1990 reference period normals.
#'
#' @details
#' The input table `X` provides climate data for a single location or the average of multiple locations. 
#' The purpose of conducting the generation of the input table in a separate function is to allow users
#' to make multiple calls to [`plot_timeSeries()`] without needing to generate the inputs each time. 
#' 
#' Some combinations of `variable1` and `variable2` are not compatible or meaningful. 
#' Examples of meaningful combinations are winter vs summer values of the same climate variable or minimum vs. 
#' maximum temperatures. 
#'
#' @param X  A `data.table` object produced using the function [`plot_timeSeries_input()`]. This 
#' table can include more models, scenarios, and variables than are used in individual calls to 
#' [`plot_timeSeries()`].
#' @inheritParams climr_downscale
#' @param variable1 character. A climate variable. options are [`list_variables()`].
#' @param variable2 character. A second climate variable to plot in combination with `variable1`. 
#' options are [`list_variables()`].
#' @param showObserved logical. Plot a time series of observed climate.  
#' @param showrange logical. Plot a shaded region indicating the minimum and maximum of the 
#' selected ensemble of GCM simulations for each selected scenario. 
#' @param yfit logical. Set the range of the y axis to the range of the visible data. If `FALSE` 
#' the y axis is the range of all values of `variable1` (and `variable2` if applicable) in the 
#' input table defined by `X`. 
#' @param cex Numeric. The magnification factor for text size. Default is 1.
#' @param mar A numerical vector of length 4, giving the margin sizes in number of lines of text: c(bottom, left, 
#' top, right). The default is c(3,3,0.1,4).
#' @param showmean logical. Plot the ensemble mean time series. Multi-model ensemble means are 
#' calculated from the mean of simulations for each model. 
#' @param compile logical. Compile multiple global climate models into a multi-model ensemble. 
#' If `FALSE` the single-model ensembles are plotted individually. 
#' @param simplify logical. Simplify the ensemble range and mean using a smoothing spline. 
#' @param refline logical. Plot the 1961-1990 reference period mean for the selected variable
#' and extend this line to the year 2100 as a visual reference. 
#' @param refline.obs logical. Plot the 1961-1990 reference period mean for the observational data. 
#' This should be the same as the reference line for the GCM time series.  
#' @param pal character. color palette. Options are "scenario", for use when comparing scenarios, 
#' and "gcm", for use when comparing GCMs. 
#' @param label.endyear logical. Add a label of the final year of the observational time series. 
#' @param endlabel character. Add a label to the end of each simulated time series. Options 
#' are "change", to indicate the change in year 2100 relative to the 1961-1990 baseline, or "gcm"
#' to indicate the global climate model. 
#' @param yearmarkers logical. Add white points to the observational time series as a visual aid. 
#' @param yearlines logical. Add vertical lines on every fifth year as a visual reference
#' @param legend_pos character. Position of the legend. Viable options are c("bottomright",
#'   "bottomleft", "topleft", and "topright").
#'
#' @return NULL. Draws a plot in the active graphics device.
#'
#' @examples
#' # data frame of arbitrary points
#' my_points <- data.frame(lon = c(-127.7300,-127.7500), lat = c(55.34114, 55.25), elev = c(711, 500), id = 1:2)
#'
#' # generate the input data
#' my_data <- plot_timeSeries_input(my_points)
#'
#' # use the input to create a plot
#' plot_timeSeries(my_data, variable1 = "Tmin_sm")
#'
#' # compare observational time series
#' plot_timeSeries(my_data, variable1 = "Tmin_sm", historic_ts_dataset = c("cru.gpcc", "climatena"))
#' 
#' # compare mean daily minimum and maximum temperatures
#' plot_timeSeries(my_data, variable1 = "Tmin_sm", variable2 = "Tmax_sm")
#'
#' # compare summer and winter temperatures (without simplifying the ensemble range)
#' plot_timeSeries(my_data, variable1 = "Tmax_sm", variable2 = "Tmax_wt", simplify = FALSE)
#'
#' # compare global climate models
#' plot_timeSeries(my_data, gcm_models = list_gcm()[c(7,13)], pal = "gcm", ssp = list_ssp()[2], showmean = FALSE, compile = FALSE, simplify = FALSE, endlabel = "gcm", mar=c(3,3,0.1,6), showObserved = FALSE)
#'
#' # export plot to a temporary directory, including a title
#' figDir <- tempdir()
#' png(
#'   filename = file.path(figDir, "plot_test.png"), type = "cairo", units = "in",
#'   width = 6, height = 5, pointsize = 10, res = 300
#' )
#' plot_timeSeries(my_data, variable1 = "Tmin_sm", mar=c(3,3,2,4))
#' title("Historical and projected summer night-time warming in the Bulkley Valley, BC")
#' dev.off()
#'
#' @importFrom scales alpha
#' @importFrom stinepack stinterp
#'
#' @export

plot_timeSeries <- function(
    X,
    variable1 = "Tmin_sm",
    variable2 = NULL,
    showObserved = TRUE,
    historic_ts_dataset = "climatena",
    gcm_models = list_gcm()[c(1,4,5,7,10,11,12)], #TODO add GFDL once the data are in climr
    ssps = list_ssp()[1:3],
    showrange = TRUE, 
    yfit = TRUE,
    cex = 1,
    mar=c(3,3,0.1,4), 
    showmean = TRUE,
    compile = TRUE,
    simplify = TRUE,
    refline = FALSE,
    refline.obs = TRUE,
    pal = "scenario",
    label.endyear = FALSE, 
    endlabel = "change", 
    yearmarkers = TRUE,
    yearlines = FALSE,
    legend_pos = "topleft") {
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("package scales must be installed to use this function")
  } else   if (!requireNamespace("stinepack", quietly = TRUE)) {
    stop("package stinepack must be installed to use this function")
  } else {
      data("variables", envir = environment()) 

      # Scenario definitions
      scenarios.selected <- c("historical", ssps)
      scenarios <- c("historical", list_ssp())
      scenario.names <- c("Historical simulations", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
      pal.scenario <- c("gray60", "dodgerblue4", "seagreen", "darkorange3", "darkred")  # these roughly match the IPCC standard scenario colours. 
      
      # GCM color palette (from https://mk.bcgsc.ca/colorblind/)
      pal.gcm <- c("#004949","#009292","#ff6db6","#ffb6db", "#490092","#006ddb","#b66dff","#6db6ff","#b6dbff", "#920000","#924900","#db6d00","#24ff24")
      
      # function for specifying the color
      colSelect <- function(scenario, gcm){
        if(missing(gcm)){ 
          col = pal.scenario[which(scenarios==scenario)]
        } else {
          col = if(pal=="gcm") pal.gcm[which(list_gcm()==gcm)] else pal.scenario[which(scenarios==scenario)]
        }
        return(col)
      }
      
      # yeartime definitions
      monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
      seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)],4, byrow=TRUE)
      seasons <- c("wt", "sp", "sm", "at")
      season.names <- c("Winter", "Spring", "Summer", "Autumn")
      yeartimes <- c(seasons, monthcodes)
      yeartime.names <- c(season.names, month.name)
      
      # ensemble statistics definitions
      ensstats <- c("ensmin", "ensmax", "ensmean")
      
      ## Assemble the data that will be used in the plot (for setting the ylim)
      alldata <-  X[, if(is.null(variable2)) get(variable1) else c(get(variable1), get(variable2))] # a vector of all potential data on the plot for setting the ylim (y axis range)
      visibledata <-  X[GCM%in%c(NA, gcm_models) & SSP%in%c(NA, ssps), (if(is.null(variable2)) get(variable1) else c(get(variable1), get(variable2)))] # a vector of all visible data on the plot for setting the ylim (y axis range)
      
      # components of the variable (note this will not work for monthly variables until we change the variable naming convention to underscore delimitated (e.g., Tave_01 instead of the current Tave01))
      nums <- if(is.null(variable2)) 1 else 1:2 #nums is the set of variable numbers (this is used later on as well)
      for(num in nums){ 
        variable <- get(paste("variable",num,sep=""))
        # # Code for parsing element and yeartime from variable name. TODO activate when the variable naming conventions are applied. 
        # variable.components <- unlist(strsplit(variable, "_"))
        # assign(paste0("yeartime", num), if(length(variable.components)==1) NA else variable.components[length(variable.components)]) #do by length because some elements have an underscore in them
        # assign(paste0("element", num), if(length(grep("DD_0|DD_18", variable))==1) paste(variable.components[1:2], collapse="_") else variable.components[1])
        assign(paste0("yeartime", num), as.character(variables[Code == variable, "Code_Time"]) )
        assign(paste0("element", num), as.character(variables[Code == variable, "Code_Element"]) )
      }
      
      # PLOT
      par(mfrow=c(1,1), mar=mar, mgp=c(1.75, 0.25, 0), cex=cex)
      # y axis title. 
      if(is.null(variable2)){ #if there is no second variable
        ylab <- paste(yeartime.names[which(yeartimes==yeartime1)], variables[Code==variable1, "Element"])
      } else 
        if(element1==element2){ #if both variables have the same element
          ylab <- variables[Code==variable1, "Element"]
        } else 
          if(yeartime1==yeartime2){ #if both variables have the same yeartime
            ylab <- paste(yeartime.names[which(yeartimes==yeartime1)], variables[Code==variable1, "Element"], "or", variables[Code==variable2, "Element"])
          } else { #if variables1 and 2 have different elements and yeartimes
            ylab <- paste(yeartime.names[which(yeartimes==yeartime1)], variables[Code==variable1, "Element"], "or", variables[Code==variable2, "Element"])
          }
      plot(0, col="white", xlim=c(1900, 2100), ylim=range(if(yfit) visibledata else alldata, na.rm = TRUE), xaxs="i", xaxt="n", tck=0, xlab="", ylab=ylab)
      axis(1, at=seq(1850,2100,25), labels = seq(1850,2100,25), tck=0)
      
      num <- 1
      for(num in nums){
        yeartime <- get(paste("yeartime",num,sep=""))
        element <- get(paste("element",num,sep=""))
        variable <- get(paste("variable",num,sep=""))
        
        # function for plotting time series for gcm or compiled ensemble
        plot.ensemble <- function(x) {
          # scenario <- scenarios.selected[1]
          temp.historical <- x[is.na(SSP), c("PERIOD", "RUN", variable), with=FALSE]
          x.historical <- as.numeric(temp.historical[, .(min = min(get(variable))), by = PERIOD][["PERIOD"]])
          ensmin.historical <- temp.historical[RUN!="ensembleMean", .(min = min(get(variable), na.rm=TRUE)), by = PERIOD][["min"]]
          ensmax.historical <- temp.historical[RUN!="ensembleMean", .(max = max(get(variable), na.rm=TRUE)), by = PERIOD][["max"]]
          ensmean.historical <- temp.historical[RUN=="ensembleMean", .(mean = mean(get(variable), na.rm=TRUE)), by = PERIOD][["mean"]] #calculate mean using the single-model ensembleMean, to ensure that the mean isn't biased towards models with more runs
          
          for(scenario in scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])][-1]){
            temp <- x[SSP==scenario, c("PERIOD", "RUN", variable), with=FALSE]
            x.temp <- as.numeric(temp[, .(min = min(get(variable))), by = PERIOD][["PERIOD"]])
            ensmin.temp <- temp[RUN!="ensembleMean", .(min = min(get(variable), na.rm=TRUE)), by = PERIOD][["min"]]
            ensmax.temp <- temp[RUN!="ensembleMean", .(max = max(get(variable), na.rm=TRUE)), by = PERIOD][["max"]]
            ensmean.temp <- temp[RUN=="ensembleMean", .(mean = mean(get(variable), na.rm=TRUE)), by = PERIOD][["mean"]] #calculate mean using the single-model ensembleMean, to ensure that the mean isn't biased towards models with more runs
            assign(paste("x", scenario, sep="."), c(x.historical[length(x.historical)], x.temp)) # add last year of historical runs. this is necessary to get a seamless transition from the historical polygon to the future polygon. 
            assign(paste("ensmin", scenario, sep="."), c(ensmin.historical[length(ensmin.historical)], ensmin.temp)) # add last year of historical runs. this is necessary to get a seamless transition from the historical polygon to the future polygon. 
            assign(paste("ensmax", scenario, sep="."), c(ensmax.historical[length(ensmax.historical)], ensmax.temp)) # add last year of historical runs. this is necessary to get a seamless transition from the historical polygon to the future polygon. 
            assign(paste("ensmean", scenario, sep="."), c(ensmean.historical[length(ensmean.historical)], ensmean.temp)) # add last year of historical runs. this is necessary to get a seamless transition from the historical polygon to the future polygon. 
          }
          
          if(showrange) {
            if(simplify==FALSE){
              for(scenario in scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])]){
                x3 <- get(paste("x", scenario, sep="."))
                polygon(c(x3, rev(x3)), c(get(paste("ensmin", scenario, sep=".")), rev(get(paste("ensmax", scenario, sep=".")))), col=alpha(colSelect(scenario, gcm), 0.35), border=colSelect(scenario, gcm))
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
                  polygon(c(s.ensmin$x[subset.hist], rev(s.ensmax$x[subset.hist])), c(s.ensmin$y[subset.hist], rev(s.ensmax$y[subset.hist])), col=alpha(if(pal=="gcm") colSelect(scenario, gcm) else pal.scenario[which(scenarios=="historical")], 0.35), border=NA)
                  polygon(c(s.ensmin$x[subset.proj], rev(s.ensmax$x[subset.proj])), c(s.ensmin$y[subset.proj], rev(s.ensmax$y[subset.proj])), col=alpha(colSelect(scenario, gcm), 0.35), border=NA)
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
                  polygon(c(s.ensmin3$x[subset.proj], rev(s.ensmax3$x[subset.proj])), c(s.ensmin3$y[subset.proj], rev(s.ensmax3$y[subset.proj])), col=alpha(colSelect(scenario, gcm), 0.35), border=NA)
                }
              }
            }
          }
          
          if(refline){
            ref.temp <- mean(ensmean.historical[which(x.historical%in%1961:1990)])
            lines(1961:1990, rep(ref.temp, 30), lwd=2)
            lines(c(1990,2100), rep(ref.temp, 2), lty=2)
          }
          
          # overlay the ensemble mean lines on top of all polygons
          for(scenario in scenarios.selected[order(c(1,4,5,3,2)[which(scenarios%in%scenarios.selected)])]){
            
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
            
            # plot the ensemble mean
            if(showmean){
              if(simplify){
                lines(x=s4$x[subset], y=s4$y[subset], col=colSelect(scenario, gcm), lwd=2)
              } else lines(x=get(paste("x", scenario, sep=".")), y=get(paste("ensmean", scenario, sep=".")), col=colSelect(scenario, gcm), lwd=2)
            }
            
            # end labels
            if(is.null(endlabel)==FALSE){
              if(scenario != "historical"){
                par(xpd=TRUE)
                baseline <- mean(ensmean.historical[which(x.historical%in%1961:1990)])
                projected <- s4$y[length(s4$y)]
                if(endlabel=="change"){
                  if(element%in%c("PPT", "PAS", "CMD", "MAP", "MSP", "DD_18", "DD18", "DD_0", "DD5", "Eref")){
                    change <- round(projected/baseline-1,2)
                    if(is.na(change)==FALSE) text(2099,projected, if(change>0) paste("+",change*100,"%", sep="") else paste(change*100,"%", sep=""), col=colSelect(scenario, gcm), pos=4, font=2, cex=1)
                  } else if(element%in%c("Tave", "Tmin", "Tmax", "MCMT", "MWMT", "EXT", "EMT", "MAT")){
                    change <- round(projected-baseline,1)
                    if(is.na(change)==FALSE) text(2099,projected, if(change>0) bquote("+" * .(change) * degree * C) else bquote(.(change) * degree * C), col=colSelect(scenario, gcm), pos=4, font=2, cex=1)
                  } else if(element%in%c("RH")){
                    change <- round(projected-baseline,1)
                    if(is.na(change)==FALSE) text(2099,projected, if(change>0) paste("+",change,"%", sep="") else paste(change,"%", sep=""), col=colSelect(scenario, gcm), pos=4, font=2, cex=1)
                  } else {
                    change <- round(projected-baseline,1)
                    if(is.na(change)==FALSE) text(2099,projected, if(change>0) paste("+",change, sep="") else paste(change, sep=""), col=colSelect(scenario, gcm), pos=4, font=2, cex=1)
                  }
                }
                if(endlabel=="gcm"){
                  text(2099,projected, if(compile) "ensemble" else gcm, col=colSelect(scenario, gcm), pos=4, font=1, cex=1)
                }
                par(xpd=FALSE)
              }
            }
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
        
        if(compile){ #this plots a single envelope for the ensemble as a whole
          temp.data <-  X[GCM%in%gcm_models, c("PERIOD", "SSP", "RUN", variable), with=FALSE]
          plot.ensemble(temp.data)
          
        } else for(gcm in gcm_models){ #this plots of individual GCM ensembles. 
          temp.data <-  X[GCM==gcm, c("PERIOD", "SSP", "RUN", variable), with=FALSE]
          plot.ensemble(temp.data)
          
          print(gcm)
        }
        
        # overlay the 5-year lines on top of all polygons
        if(yearlines){
          for(n in seq(1905, 2095, 5)){
            lines(c(n, n), c(-9999, 9999), col="grey", lty=2)
          }
        }
        
        if(showObserved){
          # add in observations
          obs.colors <- c("black", "blue", "red")
          obs.options <- c("climatena", "cru.gpcc", "era5")
          for(obs.dataset in historic_ts_dataset){ #TODO update this code block once i know how the datasets are identified in the climr output
            obs.color <- obs.colors[which(obs.options==obs.dataset)]
            x.obs <- as.numeric(X[DATASET==obs.dataset & PERIOD%in%1900:2100, "PERIOD"][[1]])
            y.obs <-  X[DATASET==obs.dataset & PERIOD%in%1900:2100, get(variable)]
            recent.obs <- mean(y.obs[which(x.obs%in%2014:2023)], na.rm=TRUE)
            baseline.obs <- mean(y.obs[which(x.obs%in%1961:1990)], na.rm=TRUE)
            end <- max(which(!is.na(y.obs)))
            lines(x.obs[which(x.obs<1951)], y.obs[which(x.obs<1951)], lwd=3, lty=3, col=obs.color)
            lines(x.obs[which(x.obs>1949)], y.obs[which(x.obs>1949)], lwd=4, col=obs.color)
            if(yearmarkers){
              points(x.obs[which(x.obs>1949)], y.obs[which(x.obs>1949)], pch=21, bg="white", col=obs.color, cex=0.4)
              points(x.obs[which(x.obs>1949)[seq(1,999,5)]], y.obs[which(x.obs>1949)[seq(1,999,5)]], pch=21, bg="white", col=obs.color, cex=0.7)
            }
            if(label.endyear){
              points(x.obs[end], y.obs[end], pch=16, cex=1, col=obs.color)
              text(x.obs[end], y.obs[end], x.obs[end], pos= 4, offset = 0.25, col=obs.color, cex=1,)
            }
            if(refline.obs){
              lines(1961:1990, rep(baseline.obs, 30), lwd=1, col=obs.color)
              lines(c(1990,2100), rep(baseline.obs, 2), lty=2, col=obs.color)
            }
          }
        }
        print(num)
      }
      
      if(showObserved){
        # Sources legend
        a <- if("climatena"%in%historic_ts_dataset) 1 else NA
        b <- if("cru.gpcc"%in%historic_ts_dataset) 2 else NA
        c <- if("era5"%in%historic_ts_dataset) 3 else NA
        d <- if(length(gcm_models>0)) 4 else NA
        s <- !is.na(c(a,b,c,d))
        legend.GCM <- if(length(gcm_models)>1) paste("Simulated (", length(gcm_models), " GCMs)", sep="")  else paste("Simulated (", gcm_models, ")", sep="")
        legend(legend_pos, title = "", legend=c("Observed (ClimateNA)", "Observed (CRU/GPCC)", "Observed (ERA5)", legend.GCM)[s], bty="n",
               lty=c(1,1,1,1)[s], 
               col=c(obs.colors, "gray")[s], 
               lwd=c(4,4,4,2)[s], 
               pch=c(NA,NA,NA,NA)[s], 
               pt.bg = c(NA, NA,NA,NA)[s], 
               pt.cex=c(NA,NA,NA,NA)[s])
      }
      
      #Scenario legend
      if(pal=="gcm"){
        s <- which(list_gcm()%in%gcm_models)
        legend(c("top", "bottom")[if(length(grep("top", legend_pos))==1) 1 else 2], title = "GCMs", legend=gcm_models, bty="n",
               col=pal.gcm[s], pch=22, pt.bg = alpha(pal.gcm[s], 0.35), pt.cex=2)
      } else {
        s <- rev(which(scenarios[-1]%in%scenarios.selected))
        legend(c("top", "bottom")[if(length(grep("top", legend_pos))==1) 1 else 2], title = "Scenarios", legend=c("Historical", scenario.names[-1][s]), bty="n",
               lty=c(NA,NA,NA,NA,NA)[c(1,s+1)], col=pal.scenario[c(1,s+1)], lwd=c(NA,NA,NA,NA,NA)[c(1,s+1)], pch=c(22,22,22,22,22)[c(1,s+1)], pt.bg = alpha(pal.scenario[c(1,s+1)], 0.35), pt.cex=c(2,2,2,2,2)[c(1,s+1)])
      }
      
      # mtext(paste(" Created using climr (https://bcgov.github.io/climr/)"), side=1, line=1.5, adj=0.0, font=1, cex=1.1, col="gray")
      
      box()
      
    }
}
