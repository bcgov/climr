#' Time series plots of climate change
#'
#' @description
#' Time series plots of 20th and 21st century climate change for user-selected
#' locations and climate variables.
#' 
#' The purposes of the plot are to:
#' \enumerate{
#'   \item view differences in interannual variability and climate change trends among global
#' climate models (GCMs),
#'   \item view the differences between multiple simulations of each model,
#'   \item compare simulated and observed climate change from 1901 to present, and
#'   \item compare time series of two different variables.
#' }
#' All global climate model anomalies are bias-corrected to the 1961-1990 reference period normals.
#'
#' @details
#' The input table `X` provides climate data for a single location or the average of multiple
#' locations. The purpose of conducting the generation of the input table in a separate function is
#' to allow users to make multiple calls to [`plot_timeSeries()`] without needing to generate the
#' inputs each time.
#'
#' Some combinations of `var1` and `var2` are not compatible or meaningful.
#' Examples of meaningful combinations are winter vs summer values of the same climate var
#' or minimum vs. maximum temperatures.
#'
#' Downloads of GCM time series take some time. The `plot_timeSeries_input()` function can take
#' ~5 minutes to run for the first time it is called for a location. Once the time series are
#' cached, they don't need to be downloaded again.
#'
#' @param X  A `data.table` object produced using the function [`plot_timeSeries_input_preprocess()`].
#' @inheritParams downscale
#' @param var1 character. A climate var. options are [`list_vars()`].
#' @param var2 character. A second climate var to plot in combination with `var1`.
#' options are [`list_vars()`].
#' @param showObserved logical. Plot a time series of observed climate.
#' @param showrange logical. Plot a shaded region indicating the minimum and maximum of the
#' selected ensemble of GCM simulations for each selected scenario.
#' @param yfit logical. Set the range of the y axis to the range of the visible data. If `FALSE`
#' the y axis is the range of all values of `var1` (and `var2` if applicable) in the
#' input table defined by `X`.
#' @param cex Numeric. The magnification factor for text size. Default is 1.
#' @param mar A numerical vector of length 4, giving the margin sizes in number of lines of text:
#' c(bottom, left, top, right). The default is c(3,3,0.1,4).
#' @param showmean logical. Plot the ensemble mean time series. Multi-model ensemble means are
#' calculated from the mean of simulations for each model.
#' @param compile logical. Compile multiple global climate models into a multi-model ensemble.
#' If `FALSE` the single-model ensembles are plotted individually.
#' @param simplify logical. Simplify the ensemble range and mean using a smoothing spline.
#' @param refline logical. Plot the 1961-1990 reference period mean for the selected var
#' and extend this line to the year 2100 as a visual reference.
#' @param refline.obs logical. Plot the 1961-1990 reference period mean for the observational data.
#' This should be the same as the reference line for the GCM time series.
#' @param pal character. color palette. Options are "scenario", for use when comparing scenarios,
#' and "gcms", for use when comparing GCMs.
#' @param label.endyear logical. Add a label of the final year of the observational time series.
#' @param endlabel character. Add a label to the end of each simulated time series. Options
#' are "change", to indicate the change in year 2100 relative to the 1961-1990 baseline, or "gcms"
#' to indicate the global climate model.
#' @param yearmarkers logical. Add white points to the observational time series as a visual aid.
#' @param yearlines logical. Add vertical lines on every fifth year as a visual reference
#' @param legend_pos character. Position of the legend. Viable options are `c("bottomright",
#'   "bottomleft", "topleft", "topright")`.
#'
#' @return NULL. Draws a plot in the active graphics device.
#'
#'
#' @importFrom scales alpha
#' @importFrom stinepack stinterp
#' @importFrom utils data
#' @importFrom graphics box
#'
#' @export

plot_timeSeries_preprocess <- function(
    X,
    var1 = "Tmin_sm",
    var2 = NULL,
    showObserved = TRUE,
    obs_ts_dataset = "mswx.blend",
    gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
    ssps = list_ssps()[1:3],
    showrange = TRUE,
    yfit = TRUE,
    cex = 1,
    mar = c(3, 3, 0.1, 4),
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
    legend_pos = "topleft",
    app = FALSE) {

  ## checks
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("package scales must be installed to use this function")
  } 
  
  if (!requireNamespace("stinepack", quietly = TRUE)) {
    stop("package stinepack must be installed to use this function")
  }
  
  legopts <- c("bottomright", "bottomleft", "topleft", "topright")
  if (!legend_pos %in% legopts) {
    stop("'legend_pos' must be one of: ", 
         paste(legopts, collapse = ", "))
  }
  
  data("variables", envir = environment())
  
  # Scenario definitions
  scenarios.selected <- c("historical", ssps)
  scenarios <- c("historical", list_ssps())
  scenario.names <- c("Historical simulations", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
  pal.scenario <- c("gray60", "dodgerblue4", "seagreen", "darkorange3", "darkred") # these roughly match the IPCC standard scenario colours.
  
  # GCM color palette (from https://mk.bcgsc.ca/colorblind/)
  pal.gcms <- c("#004949", "#009292", "#ff6db6", "#ffb6db", "#490092",
                "#006ddb", "#b66dff", "#6db6ff", "#b6dbff", "#920000",
                "#924900", "#db6d00", "#24ff24")
  
  # yeartime definitions
  monthcodes <- as.character(c(paste0("0", 1:9), 10:12))
  seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)], 4, byrow = TRUE)
  seasons <- c("", "wt", "sp", "sm", "at")
  season.names <- c("Annual", "Winter", "Spring", "Summer", "Autumn")
  yeartimes <- c(seasons, monthcodes)
  yeartime.names <- c(season.names, month.name)
  
  # # ensemble statistics definitions
  # ensstats <- c("ensmin", "ensmax", "ensmean")
  
  # components of the var
  nums <- if (is.null(var2)) 1 else 1:2 # nums is the set of var numbers (var1 or var2) (this is used later on as well)
  for (num in nums) {
    var <- get(paste("var", num, sep = ""))
    assign(paste0("yeartime", num), as.character(variables[Code == var, "Code_Time"]))
    assign(paste0("element", num), as.character(variables[Code == var, "Code_Element"]))
  }
  
  # PLOT
  par(mfrow = c(1, 1), mar = mar, mgp = c(1.75, 0.25, 0), cex = cex)
  # y axis title.
  if (is.null(var2)) { # if there is no second var
    ylab <- stringi::stri_unescape_unicode(paste(yeartime.names[which(yeartimes == yeartime1)], variables[Code == var1, "Element"]))
    if (variables[Code == var1, "Unit"] != "") {
      ylab <- stringi::stri_unescape_unicode(paste0(ylab, " (", variables[Code == var1, "Unit"], ")"))
    }
  } else if (element1 == element2) { # if both variables have the same element
    ylab <- stringi::stri_unescape_unicode(variables[Code == var1, "Element"])
  } else if (yeartime1 == yeartime2) { # if both variables have the same yeartime
    ylab <- stringi::stri_unescape_unicode(paste(yeartime.names[which(yeartimes == yeartime1)], variables[Code == var1, "Element"], "or", variables[Code == var2, "Element"]))
  } else { # if variables1 and 2 have different elements and yeartimes
    ylab <- stringi::stri_unescape_unicode(paste(yeartime.names[which(yeartimes == yeartime1)], variables[Code == var1, "Element"], "or", variables[Code == var2, "Element"]))
    
  }
  plot(0, col = "white", xlim = c(1900, 2100), 
       ylim = range(as.integer(X[,VAL]), na.rm = TRUE), 
       xaxs = "i", xaxt = "n", tck = 0, xlab = "", ylab = ylab, cex.lab = if (app) 1.5 else 1, cex.axis = if (app) 1.25 else 1)
  axis(1, at = seq(1850, 2100, 25), labels = seq(1850, 2100, 25), tck = 0, cex.axis = if (app) 1.25 else 1)
  
  num <- 1
  for (num in nums) {
    yeartime <- get(paste("yeartime", num, sep = ""))
    element <- get(paste("element", num, sep = ""))
    var <- get(paste("var", num, sep = ""))
    
    if(!is.null(ssps)){
      if (compile) { # this plots a single envelope for the ensemble as a whole
        temp.data <- X[is.na(DATASET) & !is.na(PERIOD) & !is.na(VAL)]
        plot_preprocess_ensemble(temp.data,
                                 var = var, var2 = var2,
                                 refline = refline, showmean = showmean,
                                 endlabel = endlabel, element = element,
                                 element1 = element1, element2 = element2,
                                 compile = compile, yeartime.names = yeartime.names,
                                 yeartimes = yeartimes, yeartime = yeartime,
                                 gcm = NULL, pal = pal, pal.scenario = pal.scenario,
                                 pal.gcms = pal.gcms,
                                 scenarios.selected = scenarios.selected, scenarios = scenarios,
                                 showrange = showrange, simplify = simplify)
      } else {
        for (gcm in gcms) { # this plots individual GCM ensembles.
          stop(sprintf("Error: This function is not currently set up to handle individual GCM ensembles."))
        }
      }
      
      # overlay the 5-year lines on top of all polygons
      if (yearlines) {
        for (n in seq(1905, 2095, 5)) {
          lines(c(n, n), c(-9999, 9999), col = "grey", lty = 2)
        }
      }
    }
    
    if (showObserved) {
      # add in observations
      obs.colors <- c("black", "blue", "red")
      obs.options <- c("mswx.blend", "cru.gpcc", "climatena") 
      for (obs.dataset in obs_ts_dataset) { 
        obs.color <- obs.colors[which(obs.options == obs.dataset)]
        x.obs <- as.numeric(X[DATASET == obs.dataset & PERIOD %in% 1900:2100, "PERIOD"][[1]])
        y.obs <- as.numeric(X[DATASET == obs.dataset & PERIOD %in% 1900:2100, VAL])
        recent.obs <- mean(y.obs[which(x.obs %in% 2014:2023)], na.rm = TRUE)
        baseline.obs <- mean(y.obs[which(x.obs %in% 1961:1990)], na.rm = TRUE)
        end <- max(which(!is.na(y.obs)))
        lines(x.obs[which(x.obs < 1951)], y.obs[which(x.obs < 1951)], lwd = 3, lty = 3, col = obs.color)
        lines(x.obs[which(x.obs > 1949)], y.obs[which(x.obs > 1949)], lwd = 4, col = obs.color)
        
        if (yearmarkers) {
          points(x.obs[which(x.obs > 1949)], 
                 y.obs[which(x.obs > 1949)], 
                 pch = 21, bg = "white", col = obs.color, cex = 0.4)
          points(x.obs[which(x.obs > 1949)[seq(1, 999, 5)]], 
                 y.obs[which(x.obs > 1949)[seq(1, 999, 5)]], 
                 pch = 21, bg = "white", col = obs.color, cex = 0.7)
        }
        
        if (label.endyear) {
          points(x.obs[end], y.obs[end], pch = 16, cex = 1, col = obs.color)
          text(x.obs[end], y.obs[end], x.obs[end], pos = 4, offset = 0.25,
               col = obs.color, cex = 1, )
        }
        
        if (refline.obs) {
          lines(1961:1990, rep(baseline.obs, 30), lwd = 1, col = obs.color)
          lines(c(1990, 2100), rep(baseline.obs, 2), lty = 2, col = obs.color)
        }
      }
    }
  }
  
  if (showObserved) {
    # Sources legend
    a <- if ("mswx.blend" %in% obs_ts_dataset) 1 else NA
    b <- if ("cru.gpcc" %in% obs_ts_dataset) 2 else NA
    c <- if ("climatena" %in% obs_ts_dataset) 3 else NA
    d <- if (length(gcms > 0) & !is.null(ssps)) 4 else NA
    s <- !is.na(c(a, b, c, d))
    legend.GCM <- if (length(gcms) > 1) { 
      paste("Simulated (", length(gcms), " GCMs)", sep = "") 
    } else {
      paste("Simulated (", gcms, ")", sep = "")
    }
    legend(legend_pos,
           title = "", 
           legend = c("Observed (MSWX blend)", "Observed (CRU/GPCC)", "Observed (ClimateNA)", legend.GCM)[s],
           bty = "n",
           lty = rep(1, 4)[s],
           col = c(obs.colors, "gray")[s],
           lwd = c(4, 4, 4, 2)[s],
           pch = rep(NA, 4)[s],
           pt.bg = rep(NA, 4)[s],
           pt.cex = rep(NA, 4)[s],
           cex = if (app) 1.25 else 1
    )
  }
  
  # Scenario legend
  if(!is.null(ssps)){
    if (pal == "gcms") {
      s <- which(list_gcms() %in% gcms)
      legend(ifelse(grepl("top", legend_pos), "top", "bottom"),
             title = "GCMs", legend = gcms, bty = "n",
             col = pal.gcms[s], pch = 22, pt.bg = alpha(pal.gcms[s], 0.35), pt.cex = 2, cex = if (app) 1.25 else 1
      )
    } else {
      s <- rev(which(scenarios[-1] %in% scenarios.selected))
      legend(ifelse(grepl("top", legend_pos), "top", "bottom"),
             title = "Scenarios", legend = c("Historical", scenario.names[-1][s]), bty = "n",
             lty = rep(NA, 5)[c(1, s + 1)], col = pal.scenario[c(1, s + 1)], 
             lwd = rep(NA, 5)[c(1, s + 1)], pch = rep(22, 5)[c(1, s + 1)], 
             pt.bg = alpha(pal.scenario[c(1, s + 1)], 0.35), 
             pt.cex = rep(2, 5)[c(1, s + 1)], cex = if (app) 1.25 else 1
      )
    }
  }
  box()
}


#' Function for plotting time series for gcms or compiled ensemble
#'
#' @param x climate data
#' @param var TODO
#' @param scenarios.selected TODO
#' @param scenarios  TODO
#' @param gcm  TODO
#' @param pal.scenario  TODO
#' @param pal.gcms TODO
#' @param element  TODO
#' @param element1  TODO
#' @param element2  TODO
#' @param yeartime.names  TODO
#' @param yeartimes  TODO
#' @param yeartime  TODO
#' @inheritParams plot_timeSeries
#'
#' @importFrom graphics polygon
#' @importFrom stats smooth.spline
plot_preprocess_ensemble <- function(x, var, scenarios.selected, scenarios,
                          showrange = TRUE, simplify = TRUE, gcm = NULL,
                          pal, pal.scenario, pal.gcms, refline = FALSE, showmean = TRUE,
                          endlabel = "change", element,
                          compile = TRUE, var2 = NULL, element1, element2,
                          yeartime.names, yeartimes, yeartime) {

  if (showrange) {
    if (isFALSE(simplify)) {
      stop(sprintf("Error: This function is not currently set up to handle simplify == FALSE."))
    } else {
      scenarios.select <- scenarios.selected[order(c(1, 4, 5, 3, 2)[which(scenarios %in% scenarios.selected)])][-1]
      for (scenario in scenarios.select) {
        if (scenario == scenarios.select[1]) { # we need to run spline through the historical/projected transition
          hist.ensmin.x <- x[SSP == "historical" & TYPE == "ensmin", PERIOD]
          hist.ensmin.y <- x[SSP == "historical" & TYPE == "ensmin", VAL]
          
          hist.ensmax.x <- x[SSP == "historical" & TYPE == "ensmax", PERIOD]
          hist.ensmax.y <- x[SSP == "historical" & TYPE == "ensmax", VAL]
          
          colSel <- colSelect(scenario, gcm, pal.scenario, scenarios, pal, pal.gcms)
          
          polygon(c(hist.ensmin.x, rev(hist.ensmax.x)),
                  c(hist.ensmin.y, rev(hist.ensmax.y)), 
                  col = alpha(ifelse(pal == "gcms", colSel, pal.scenario[which(scenarios == "historical")]), 0.35),
                  border = NA
          )
          
          proj.ensmin.x <- x[SSP == scenario & TYPE == "ensmin", PERIOD]
          proj.ensmin.y <- x[SSP == scenario & TYPE == "ensmin", VAL]
          
          proj.ensmax.x <- x[SSP == scenario & TYPE == "ensmax", PERIOD]
          proj.ensmax.y <- x[SSP == scenario & TYPE == "ensmax", VAL]
          
          polygon(c(proj.ensmin.x, rev(proj.ensmax.x)), 
                  c(proj.ensmin.y, rev(proj.ensmax.y)),
                  col = alpha(colSel, 0.35),
                  border = NA
          )
        } else { # this second routine uses interpolation splines so that the starting point for all scenarios is the same
          proj.ensmin.x <- x[SSP == scenario & TYPE == "ensmin", PERIOD]
          proj.ensmin.y <- x[SSP == scenario & TYPE == "ensmin", VAL]
          
          proj.ensmax.x <- x[SSP == scenario & TYPE == "ensmax", PERIOD]
          proj.ensmax.y <- x[SSP == scenario & TYPE == "ensmax", VAL]
          
          colSel <- colSelect(scenario, gcm, pal.scenario, scenarios, pal, pal.gcms)
          
          polygon(c(proj.ensmin.x, rev(proj.ensmax.x)),
                  c(proj.ensmin.y, rev(proj.ensmax.y)),
                  col = alpha(colSel, 0.35),
                  border = NA
          )
        }
      }
    }
  }
  
  if (refline) {
    stop(sprintf("Error: This function is not currently set up to handle refline == TRUE."))
  }
  
  # overlay the ensemble mean lines on top of all polygons
  for (scenario in scenarios.selected[order(c(1, 4, 5, 3, 2)[which(scenarios %in% scenarios.selected)])]) {
    # plot the ensemble mean
    ensmean.x <- x[SSP == scenario & TYPE == "ensmean", PERIOD]
    ensmean.y <- x[SSP == scenario & TYPE == "ensmean", VAL]
    
    colSel <- colSelect(scenario, gcm, pal.scenario, scenarios, pal, pal.gcms)
    if (showmean) {
      if (simplify) {
        lines(x = ensmean.x, y = ensmean.y, col = colSel, lwd = 2)
      } else {
        stop(sprintf("Error: This function is not currently set up to handle simplify == FALSE."))
      }
    }
    
    # end labels
    if (!is.null(endlabel)) {
      if (scenario != "historical") {
        par(xpd = TRUE)
        # baseline <- mean(ensmean.historical[which(x.historical %in% 1961:1990)])
        # projected <- s4$y[length(s4$y)]
        ensmean.hist <- x[SSP == "historical" & TYPE == "ensmean", .(PERIOD, VAL)]
        baseline <- mean(as.numeric(ensmean.hist[PERIOD %in% 1961:1990, VAL]))
        s <- x[SSP == scenario & TYPE == "ensmean", VAL]
        projected <- as.numeric(s[length(s)])
        
        colSel <- colSelect(scenario, gcm, pal.scenario, scenarios, pal, pal.gcms)
        
        if (endlabel == "change") {
          if (element %in% c("PPT", "PAS", "CMD", "MAP", "MSP", "DDsub18", "DD18", "DDsub0", "DD5", "Eref")) {
            change <- round(projected / baseline - 1, 2)
            if (is.na(change) == FALSE) { 
              txt <- ifelse(change > 0, paste0("+", change * 100, "%"), paste0(change * 100, "%"))
              text(2099, projected, txt, col = colSel, pos = 4, font = 2, cex = 1)
            }
          } else if (element %in% c("Tave", "Tmin", "Tmax", "MCMT", "MWMT", "EXT", "EMT", "MAT")) {
            change <- round(projected - baseline, 1)
            if (is.na(change) == FALSE) {
              ## ifelse won't work with bquote.
              txt <- if (change > 0) bquote("+" * .(change) * degree * C) else bquote(.(change) * degree * C)
              text(2099, projected, txt, col = colSel, pos = 4, font = 2, cex = 1)
            }
          } else if (element %in% c("RH")) {
            change <- round(projected - baseline, 1)
            if (is.na(change) == FALSE) {
              txt <- ifelse(change > 0, paste0("+", change, "%"), paste(change, "%"))
              text(2099, projected, txt, col = colSel, pos = 4, font = 2, cex = 1)
            } 
          } else {
            change <- round(projected - baseline, 1)
            if (is.na(change) == FALSE) {
              txt <- ifelse(change > 0, paste0("+", change), paste0(change))
              text(2099, projected, txt, col = colSel, pos = 4, font = 2, cex = 1)
            }
          }
        }
        if (endlabel == "gcms") {
          txt <- ifelse(compile, "ensemble", gcm)
          text(2099, projected, txt, col = colSel, pos = 4, font = 1, cex = 1)
        }
        par(xpd = FALSE)
      }
    }
  }
  
  # Text to identify the time of year
  if (!is.null(var2)) { # if there is no second var
    if (element1 == element2) {
      label <- yeartime.names[which(yeartimes == yeartime)]
    } else {
      label <- paste(yeartime.names[which(yeartimes == yeartime)], element)
    }
    temp <- get("ensmax.historical")
    text(1925, mean(temp[10:40]), label, col = "black", pos = 3, font = 2, cex = 1)
  }
}


#' function for specifying the color
#'
#' @param scenario TODO
#' @inheritParams plot_preprocess_ensemble
#' @keywords internal
colSelect <- function(scenario, gcm, pal.scenario, scenarios, pal, pal.gcms) {
  if (is.null(gcm)) {
    col <- pal.scenario[which(scenarios == scenario)]
  } else {
    col <- if (pal == "gcms") pal.gcms[which(list_gcms() == gcm)] else pal.scenario[which(scenarios == scenario)]
  }
  return(col)
}
