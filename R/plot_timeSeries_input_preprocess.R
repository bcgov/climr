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
#' @param X  A `data.table`.
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
#' This should be the same as the reference line for the GCM time series..
#'
#' @return NULL.
#' @keywords internal
#'
#' @importFrom scales alpha
#' @importFrom stinepack stinterp
#' @importFrom utils data
#' @importFrom graphics box
#' @importFrom data.table data.table
#'
#' @export

plot_timeSeries_input_preprocess <- function(
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
    refline.obs = TRUE) {

  ## checks
  if (!requireNamespace("scales", quietly = TRUE)) {
    stop("package scales must be installed to use this function")
  } 
  
  if (!requireNamespace("stinepack", quietly = TRUE)) {
    stop("package stinepack must be installed to use this function")
  }
  
  # only keep variables of interest in X
  if (is.null(var2)) {
    X <- as.data.table(X)
    X <- X[, .SD, .SDcols = c("GCM", "SSP", "RUN", "PERIOD", "DATASET", var1)]
  }
  
  ## create empty data table to store processed data
  dt <- data.table(
    PERIOD = character(),
    SSP = character(),
    DATASET = character(),
    TYPE = character(),
    VAR = character(),
    VAL = character()
  )
  
  # Scenario definitions
  scenarios.selected <- c("historical", ssps)
  scenarios <- c("historical", list_ssps())
  scenario.names <- c("Historical simulations", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")
  
  # yeartime definitions
  monthcodes <- as.character(c(paste0("0", 1:9), 10:12))
  seasonmonth.mat <- matrix(monthcodes[c(12, 1:11)], 4, byrow = TRUE)
  seasons <- c("wt", "sp", "sm", "at")
  season.names <- c("Winter", "Spring", "Summer", "Autumn")
  yeartimes <- c(seasons, monthcodes)
  yeartime.names <- c(season.names, month.name)
  
  # # ensemble statistics definitions
  # ensstats <- c("ensmin", "ensmax", "ensmean")
  # 
  # ## Assemble the data that will be used in the plot (for setting the ylim)
  # alldata <- X[, if (is.null(var2)) get(var1) else c(get(var1), get(var2))] # a vector of all potential data on the plot for setting the ylim (y axis range)
  # visibledata <- X[GCM %in% c(NA, gcms) & SSP %in% c(NA, ssps), 
  #                  (if (is.null(var2)) get(var1) else c(get(var1), get(var2)))] # a vector of all visible data on the plot for setting the ylim (y axis range)
  
  # components of the var
  nums <- if (is.null(var2)) 1 else 1:2 # nums is the set of var numbers (var1 or var2) (this is used later on as well)
  for (num in nums) {
    var <- get(paste("var", num, sep = ""))
    assign(paste0("yeartime", num), as.character(variables[Code == var, "Code_Time"]))
    assign(paste0("element", num), as.character(variables[Code == var, "Code_Element"]))
  }

  num <- 1
  for (num in nums) {
    yeartime <- get(paste("yeartime", num, sep = ""))
    element <- get(paste("element", num, sep = ""))
    var <- get(paste("var", num, sep = ""))
    
    if (compile) { # this plots a single envelope for the ensemble as a whole
      temp.data <- X[GCM %in% gcms, c("PERIOD", "SSP", "RUN", var), with = FALSE]
      dt <- process_ensemble(dt = dt, x = temp.data,
                    var = var, var2 = var2,
                    refline = refline, showmean = showmean, element = element,
                    element1 = element1, element2 = element2,
                    compile = compile, yeartime.names = yeartime.names,
                    yeartimes = yeartimes, yeartime = yeartime,
                    gcm = NULL, scenarios.selected = scenarios.selected, scenarios = scenarios,
                    showrange = showrange, simplify = simplify)
    } else {
      stop(sprintf("Error: This function is not currently set up to handle individual GCM ensembles."))
    }
    
    if (showObserved) {
      # add in observations
      obs.options <- c("mswx.blend", "cru.gpcc", "climatena") 
      for (obs.dataset in obs_ts_dataset) { # TODO update this code block once i know how the datasets are identified in the climr output
        x.obs <- as.numeric(X[DATASET == obs.dataset & PERIOD %in% 1900:2100, "PERIOD"][[1]])
        y.obs <- X[DATASET == obs.dataset & PERIOD %in% 1900:2100, get(var)]
        recent.obs <- mean(y.obs[which(x.obs %in% 2014:2023)], na.rm = TRUE)
        baseline.obs <- mean(y.obs[which(x.obs %in% 1961:1990)], na.rm = TRUE)
        end <- max(which(!is.na(y.obs)))
        
        # add data for datasets
        if (length(x.obs) == length(y.obs)) {
          new_rows <- data.table(
            PERIOD = x.obs,
            SSP = NA_character_,
            DATASET = rep(obs.dataset, length(y.obs)),
            TYPE = rep("dataset", length(y.obs)),
            VAR = rep(var, length(y.obs)),
            VAL = y.obs
          )
          dt <- rbind(dt, new_rows)
        }
      }
    }
  }
  return(dt)
}


#' Function for processing time series data for gcms or compiled ensemble
#'
#' @param dt empty data table
#' @param x climate data
#' @param var TODO
#' @param scenarios.selected TODO
#' @param scenarios  TODO
#' @param gcm  TODO
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
#' @keywords internal
process_ensemble <- function(dt, x, var, scenarios.selected, scenarios,
                          showrange = TRUE, simplify = TRUE, gcm = NULL,
                          refline = FALSE, showmean = TRUE,
                          element, compile = TRUE, var2 = NULL, element1, element2,
                          yeartime.names, yeartimes, yeartime) {

  temp.historical <- x[is.na(SSP), c("PERIOD", "RUN", var), with = FALSE]
  x.historical <- as.numeric(temp.historical[, .(min = min(get(var))), by = PERIOD][["PERIOD"]])
  ensmin.historical <- temp.historical[RUN != "ensembleMean", .(min = min(get(var), na.rm = TRUE)), by = PERIOD][["min"]]
  ensmax.historical <- temp.historical[RUN != "ensembleMean", .(max = max(get(var), na.rm = TRUE)), by = PERIOD][["max"]]
  ensmean.historical <- temp.historical[RUN == "ensembleMean", .(mean = mean(get(var), na.rm = TRUE)), by = PERIOD][["mean"]] # calculate mean using the single-model ensembleMean, to ensure that the mean isn't biased towards models with more runs
  
  for (scenario in scenarios.selected[order(c(1, 4, 5, 3, 2)[which(scenarios %in% scenarios.selected)])][-1]) {
    temp <- x[SSP == scenario, c("PERIOD", "RUN", var), with = FALSE]
    x.temp <- as.numeric(temp[, .(min = min(get(var))), by = PERIOD][["PERIOD"]])
    ensmin.temp <- temp[RUN != "ensembleMean", .(min = min(get(var), na.rm = TRUE)), by = PERIOD][["min"]]
    ensmax.temp <- temp[RUN != "ensembleMean", .(max = max(get(var), na.rm = TRUE)), by = PERIOD][["max"]]
    ensmean.temp <- temp[RUN == "ensembleMean", .(mean = mean(get(var), na.rm = TRUE)), by = PERIOD][["mean"]] # calculate mean using the single-model ensembleMean, to ensure that the mean isn't biased towards models with more runs
    assign(paste("x", scenario, sep = "."), c(x.historical[length(x.historical)], x.temp)) # add last year of historical runs. this is necessary to get a seamless transition from the historical polygon to the future polygon.
    assign(paste("ensmin", scenario, sep = "."), c(ensmin.historical[length(ensmin.historical)], ensmin.temp)) # add last year of historical runs. this is necessary to get a seamless transition from the historical polygon to the future polygon.
    assign(paste("ensmax", scenario, sep = "."), c(ensmax.historical[length(ensmax.historical)], ensmax.temp)) # add last year of historical runs. this is necessary to get a seamless transition from the historical polygon to the future polygon.
    assign(paste("ensmean", scenario, sep = "."), c(ensmean.historical[length(ensmean.historical)], ensmean.temp)) # add last year of historical runs. this is necessary to get a seamless transition from the historical polygon to the future polygon.
  }
  
  if (showrange) {
    if (isFALSE(simplify)) {
      stop(sprintf("Error: This function is not currently set up to handle simplify == FALSE."))
    } else {
      scenarios.select <- scenarios.selected[order(c(1, 4, 5, 3, 2)[which(scenarios %in% scenarios.selected)])][-1]
      for (scenario in scenarios.select) {
        if (scenario == scenarios.select[1]) { # we need to run spline through the historical/projected transition
          x4 <- c(x.historical, get(paste("x", scenario, sep = "."))[-1])
          y.ensmin <- c(ensmin.historical, get(paste("ensmin", scenario, sep = "."))[-1])
          y.ensmax <- c(ensmax.historical, get(paste("ensmax", scenario, sep = "."))[-1])
          s.ensmin <- smooth.spline(x4, y.ensmin, df = 8)
          s.ensmax <- smooth.spline(x4, y.ensmax, df = 8)
          x.hist <- x.historical[(x.historical %% 5 == 0 & x.historical >= 1855 & x.historical <= 2014) | x.historical == 2014]
          subset.hist <- which(x4 %in% x.hist)
          y.proj <- get(paste("x", scenario, sep = "."))
          y.proj <- y.proj[(y.proj %% 5 == 0 & y.proj >= 2014 & y.proj <= 2100) | y.proj == 2014]
          subset.proj <- which(x4 %in% y.proj)
          
          ## add data for historical & projected envelope
          
          # historical ensmin
          if (length(s.ensmin$x[subset.hist]) == length(s.ensmin$y[subset.hist])) {
            new_rows <- data.table(
              PERIOD = s.ensmin$x[subset.hist],
              SSP = rep("historical", length(s.ensmin$x[subset.hist])),
              DATASET = NA_character_,
              TYPE = rep("ensmin", length(s.ensmin$x[subset.hist])),
              VAR = rep(var, length(s.ensmin$x[subset.hist])),
              VAL = s.ensmin$y[subset.hist]
            )
            dt <- rbind(dt, new_rows)
          }
          
          # historical ensmax
          if (length(s.ensmax$x[subset.hist]) == length(s.ensmax$y[subset.hist])) {
            new_rows <- data.table(
              PERIOD = s.ensmax$x[subset.hist],
              SSP = rep("historical", length(s.ensmax$x[subset.hist])),
              DATASET = NA_character_,
              TYPE = rep("ensmax", length(s.ensmax$x[subset.hist])),
              VAR = rep(var, length(s.ensmax$x[subset.hist])),
              VAL = s.ensmax$y[subset.hist]
            )
            dt <- rbind(dt, new_rows)
          }
          
          # projected ensmin
          if (length(s.ensmin$x[subset.proj]) == length(s.ensmin$y[subset.proj])) {
            new_rows <- data.table(
              PERIOD = s.ensmin$x[subset.proj],
              SSP = rep(scenario, length(s.ensmin$x[subset.proj])),
              DATASET = NA_character_,
              TYPE = rep("ensmin", length(s.ensmin$x[subset.proj])),
              VAR = rep(var, length(s.ensmin$x[subset.proj])),
              VAL = s.ensmin$y[subset.proj]
            )
            dt <- rbind(dt, new_rows)
          }
          
          # projected ensmax
          if (length(s.ensmax$x[subset.proj]) == length(s.ensmax$y[subset.proj])) {
            new_rows <- data.table(
              PERIOD = s.ensmax$x[subset.proj],
              SSP = rep(scenario, length(s.ensmax$x[subset.proj])),
              DATASET = NA_character_,
              TYPE = rep("ensmax", length(s.ensmax$x[subset.proj])),
              VAR = rep(var, length(s.ensmax$x[subset.proj])),
              VAL = s.ensmax$y[subset.proj]
            )
            dt <- rbind(dt, new_rows)
          }
          
        } else { # this second routine uses interpolation splines so that the starting point for all scenarios is the same
          x5 <- c(x.historical, get(paste("x", scenario, sep = "."))[-1])
          y.ensmin2 <- c(ensmin.historical, get(paste("ensmin", scenario, sep = "."))[-1])
          y.ensmax2 <- c(ensmax.historical, get(paste("ensmax", scenario, sep = "."))[-1])
          s.ensmin2 <- smooth.spline(x5, y.ensmin2, df = 8)
          s.ensmax2 <- smooth.spline(x5, y.ensmax2, df = 8)
          knots.hist <- which(x5 %in% c(seq(1860, 2000, 20), 2014))
          knots.proj <- which(x5 %in% c(seq(2030, 2090, 20), 2100))
          s.ensmin3 <- stinterp(x5[c(knots.hist, knots.proj)], c(s.ensmin$y[knots.hist], s.ensmin2$y[knots.proj]), x5)
          s.ensmax3 <- stinterp(x5[c(knots.hist, knots.proj)], c(s.ensmax$y[knots.hist], s.ensmax2$y[knots.proj]), x5)
          
          ## add data for projected envelope
          
          # projected ensmin
          if (length(s.ensmin3$x[subset.proj]) == length(s.ensmin3$y[subset.proj])) {
            new_rows <- data.table(
              PERIOD = s.ensmin3$x[subset.proj],
              SSP = rep(scenario, length(s.ensmin3$x[subset.proj])),
              DATASET = NA_character_,
              TYPE = rep("ensmin", length(s.ensmin3$x[subset.proj])),
              VAR = rep(var, length(s.ensmin3$x[subset.proj])),
              VAL = s.ensmin3$y[subset.proj]
            )
            dt <- rbind(dt, new_rows)
          }
          
          # projected ensmax
          if (length(s.ensmax3$x[subset.proj]) == length(s.ensmax3$y[subset.proj])) {
            new_rows <- data.table(
              PERIOD = s.ensmax3$x[subset.proj],
              SSP = rep(scenario, length(s.ensmax3$x[subset.proj])),
              DATASET = NA_character_,
              TYPE = rep("ensmax", length(s.ensmax3$x[subset.proj])),
              VAR = rep(var, length(s.ensmax3$x[subset.proj])),
              VAL = s.ensmax3$y[subset.proj]
            )
            dt <- rbind(dt, new_rows)
          }
        }
      }
    }
  }
  
  if (refline) { 
    stop(sprintf("Error: This function is not currently set up to handle refline == TRUE."))
  }
  
  # overlay the ensemble mean lines on top of all polygons
  for (scenario in scenarios.selected[order(c(1, 4, 5, 3, 2)[which(scenarios %in% scenarios.selected)])]) {
    # calculate a spline through the time series (used for plotting and the text warming value)
    if (scenario == "historical") { # need to run spline through the historical/projected transition
      x4 <- c(x.historical, get(paste("x", scenarios.selected[2], sep = ".")))
      y4 <- c(ensmean.historical, get(paste("ensmean", scenarios.selected[2], sep = ".")))
    } else {
      x4 <- c(x.historical, get(paste("x", scenario, sep = "."))[-1])
      y4 <- c(ensmean.historical, get(paste("ensmean", scenario, sep = "."))[-1])
    }
    s4 <- smooth.spline(x4, y4, df = 10)
    years <- get(paste("x", scenario, sep = "."))
    years <- years[(years %% 5 == 0 & years >= 1855 & years <= 2100) | years == 2014]
    subset <- which(x4 %in% years)
    
    # add data for ensemble means for all scenarios
    if (length(s4$x[subset]) == length(s4$y[subset])) {
      new_rows <- data.table(
        PERIOD = s4$x[subset],
        SSP = rep(scenario, length(s4$x[subset])),
        DATASET = NA_character_,
        TYPE = rep("ensmean", length(s4$x[subset])),
        VAR = rep(var, length(s4$x[subset])),
        VAL = s4$y[subset]
      )
      dt <- rbind(dt, new_rows)
    }
  }
  return(dt)
}
