#' Bivariate climate change plots
#'
#' @description
#' Bivariate plots of 21st century climate change for user-selected locations and climate variables.
#' The purposes of the plot are to:
#' \enumerate{
#'   \item show differences in climate change trends among global climate models (GCMs);
#'   \item show the differences between multiple simulations of each model; and
#'   \item compare simulated climate change to observed climate change in the 2001-2020 period.
#' }
#' All climate changes are relative to the 1961-1990 reference period normals.
#'
#' @details
#' The input table `X` provides climate data for a single location or the average of multiple
#' locations. The purpose of conducting the generation of the input table in a separate function is
#' to allow users to make multiple calls to [`plot_timeSeries()`] without needing to generate the
#' inputs each time.
#' 
#' The climate change trajectories provided by `show_trajectories` are points for
#' each of the five 20-year periods specified by `list_gcm_periods()`. These points
#' are connected with an interpolation spline when the x variable is monotonic;
#' otherwise the trajectory points are connected by straight lines.
#' This plot is designed to be used with a single SSP scenario. If multiple scenarios
#' are passed to the plot, the GCM means and ensemble mean are averaged across the
#' scenarios, but the individual runs for all scenarios are plotted separately.
#'
#' @param X  A `data.table` object produced using the function [`plot_bivariate_input()`]. This
#' table can include more models, scenarios, and variables than are used in individual calls to
#' [`plot_bivariate()`].
#' @param xvar character. x-axis variable. options are `list_vars()`.
#' @param yvar character. y-axis variable. options are `list_vars()`.
#' @param period_focal character. The 20-year period for which to plot the ensemble
#'   detail. options are `list_gcm_periods()`.
#' @param ssp character. A single SSP-RCP scenario (representative concentration pathways paired with shared socioeconomic pathways).
#'   Options are [`list_ssps()`]. Defaults to SSP2-4.5.
#' @param obs_period character. A single 20-year period for observed climate data.
#'   Options are `list_obs_periods()`.
#' @inheritParams downscale
#' @param legend_pos character. Position of the legend. Options are `c("bottomright",
#'   "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")`.
#' @param show_runs logical. If TRUE, the individual runs of the model are plotted (for
#' `period_focal` only) in addition to the single-model ensemble mean.
#' @param show_ensMean logical. If TRUE, the multi-model ensemble mean is plotted (for
#' `period_focal` only).
#' @param show_observed logical. If TRUE, the 2001-2020 observed climate is plotted.
#' @param show_trajectories logical. If TRUE, the values of the single-model ensemble
#'   mean are plotted for all 20-year periods in `list_gcm_periods()`, connected by an
#'   interpolation spline.
#' @param interactive logical. If TRUE, an interactive plot is generated using `{plotly}`.
#'   If FALSE, a plot is generated using base graphics.
#'
#' @return NULL. Draws a plot in the active graphics device.
#'
#' @importFrom graphics axis legend lines par points text title
#' @importFrom magrittr "%>%"
#'
#' @examples
#' # data frame of arbitrary points on Vancouver Island
#' my_points <- data.frame(
#'   lon = c(-123.4404, -123.5064, -124.2317),
#'   lat = c(48.52631, 48.46807, 49.21999),
#'   elev = c(52, 103, 357),
#'   id = seq_len(3)
#' )
#'
#' # draw the plot
#' plot_bivariate(my_points)
#'
#' # draw an interactive (plotly) plot
#' plot_bivariate(my_points, xvar="MAT", yvar="PAS", interactive = TRUE)
#'
#' # export plot to a temporary directory
#' figDir <- tempdir()
#' png(
#'   filename = file.path(figDir, "plot_test.png"), type = "cairo", units = "in",
#'   width = 6, height = 5, pointsize = 10, res = 300
#' )
#' plot_bivariate(my_points)
#' dev.off()
#'
#' @export

plot_bivariate <- function(
    X,
    xvar = "Tave_sm",
    yvar = "PPT_sm",
    # percent_x = NULL, TODO: set up an override for ratio variables being expressed as percent anomalies
    # percent_y = NULL, TODO: set up an override for ratio variables being expressed as percent anomalies
    period_focal = list_gcm_periods()[1],
    gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
    ssp = list_ssps()[2],
    obs_period = list_obs_periods()[1],
    gcm_periods = list_gcm_periods(),
    max_run = 10,
    legend_pos = "bottomleft",
    show_runs = TRUE,
    show_ensMean = TRUE,
    show_observed = TRUE,
    show_trajectories = TRUE,
    interactive = FALSE,
    cache = TRUE) {
  if (!requireNamespace("stinepack", quietly = TRUE)) {
    stop("package stinepack must be installed to use this function")
  } else {
    data("variables", envir = environment())

    # variable types for default scaling (percent or absolute)
    xvar_type <- variables$Type[which(variables$Code == xvar)]
    yvar_type <- variables$Type[which(variables$Code == yvar)]

    colors <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#1e90ff", "#B15928", "#FFFF99")
    ColScheme <- colors[1:length(gcms)]

    # extract info for xvar and yvar from downscaled data
    data <- X[, c("id", "GCM", "SSP", "RUN", "PERIOD", xvar, yvar), with = FALSE]

    # convert absolute values to anomalies
    data[, xanom := if (xvar_type == "ratio") (get(xvar) / get(xvar)[1] - 1) else (get(xvar) - get(xvar)[1]), by = id]
    data[, yanom := if (yvar_type == "ratio") (get(yvar) / get(yvar)[1] - 1) else (get(yvar) - get(yvar)[1]), by = id]

    # collapse the points down to a mean anomaly
    data.all <- copy(data[, .(xanom = mean(xanom), yanom = mean(yanom)), by = .(GCM, SSP, RUN, PERIOD)])
    # collapse the SSP field to calculate a single-model ensemble mean
    data <- copy(data.all[, .(xanom = mean(xanom), yanom = mean(yanom)), by = .(GCM, RUN, PERIOD)])
    # ensemble mean for the selected period
    ensMean <- data[!is.na(GCM) & RUN == "ensembleMean" & PERIOD == period_focal, .(xanom = mean(xanom), yanom = mean(yanom)), ]
    # observed climate
    obs <- data[is.na(GCM) & PERIOD == obs_period]

    if (interactive == FALSE) {
      # BASE PLOT
      # initiate the plot
      par(mar = c(3, 4, 0.5, 0.5), mgp = c(1.25, 0.25, 0), cex = 1.5)
      plot(data.all$xanom, data.all$yanom,
        col = "white", tck = 0, xaxt = "n", yaxt = "n", ylab = "",
        xlab = paste("Change in", variables$Variable[which(variables$Code == xvar)])
      )
      par(mgp = c(2.5, 0.25, 0))
      title(ylab = paste("Change in", variables$Variable[which(variables$Code == yvar)]))
      lines(c(0, 0), c(-99, 99), lty = 2, col = "gray")
      lines(c(-99, 99), c(0, 0), lty = 2, col = "gray")

      if (show_ensMean) points(ensMean$xanom, ensMean$yanom, pch = 43, col = "gray", cex = 3)
      if (show_observed) points(obs$xanom, obs$yanom, pch = 22, bg = "gray", cex = 2.5)
      # text(x1,y1, "2001-2020", cex=1.15, font=2, pos=4, col="gray", offset=0.9)

      # plot individual runs
      if (show_runs) {
        for (gcm in gcms) {
          i <- which(gcms == gcm)
          x.runs <- data.all[GCM == gcm & RUN != "ensembleMean" & PERIOD == period_focal, xanom]
          y.runs <- data.all[GCM == gcm & RUN != "ensembleMean" & PERIOD == period_focal, yanom]
          points(x.runs, y.runs, pch = 21, bg = ColScheme[i], cex = 1)
        }
      }

      # plot model means and trajectories
      for (gcm in gcms) {
        i <- which(gcms == gcm)
        x2 <- c(0, data[GCM == gcm & RUN == "ensembleMean", xanom])
        y2 <- c(0, data[GCM == gcm & RUN == "ensembleMean", yanom])
        if (show_trajectories) {
          if (length(unique(sign(diff(x2)))) == 1) {
            x3 <- if (unique(sign(diff(x2))) == -1) rev(x2) else x2
            y3 <- if (unique(sign(diff(x2))) == -1) rev(y2) else y2
            s <- stinepack::stinterp(x3, y3, seq(min(x3), max(x3), diff(range(data.all$xanom)) / 500))
            lines(s, col = ColScheme[i], lwd = 2)
          } else {
            lines(x2, y2, col = ColScheme[i], lwd = 2)
          }
          points(x2, y2, pch = 16, col = ColScheme[i], cex = 0.5)
        }
        j <- which(list_gcm_periods() == period_focal) + 1
        points(x2[j], y2[j], pch = 21, bg = ColScheme[i], cex = 2.5)
        text(x2[j], y2[j], substr(gcms, 1, 2)[i], cex = 0.5, font = 2)
      }

      # axis labels
      axis(1, at = pretty(data.all$xanom), labels = if (xvar_type == "ratio") paste(pretty(data.all$xanom) * 100, "%", sep = "") else pretty(data.all$xanom), tck = 0)
      axis(2, at = pretty(data.all$yanom), labels = if (yvar_type == "ratio") paste(pretty(data.all$yanom) * 100, "%", sep = "") else pretty(data.all$yanom), las = 2, tck = 0)

      # Legend
      s <- c(show_observed, show_runs, TRUE, show_trajectories, show_ensMean)
      legend(legend_pos,
             legend = c("Observed climate (2001-2020)", 
                        paste0("Individual GCM simulation (", gsub("_", "-", period_focal), ")"), 
                        paste0("GCM mean (", gsub("_", "-", period_focal), ")"), 
                        paste0("GCM mean trajectory (2001-2100)"), 
                        paste0("Ensemble mean (", gsub("_", "-", period_focal), ")")
             )[s],
             pch = c(22, 21, 21, 16, 43)[s],
             pt.cex = c(2, .8, 2, 0.5, 2.2)[s],
             pt.bg = c("gray", "gray", "gray", NA, NA)[s],
             col = c(1, 1, 1, "gray", "gray")[s],
             lty = c(NA, NA, NA, 1, NA)[s],
             lwd = c(NA, NA, NA, 2, NA)[s],
             bty = "n", cex = 0.8
      )
    } else {
      if (!requireNamespace("plotly", quietly = TRUE)) {
        stop("package 'plotly' must be installed when 'interactive==TRUE'")
      } else {
        # PLOTLY PLOT

        # initiate the plot
        fig <- plotly::plot_ly(x = data.all$xanom, y = data.all$yanom, type = "scatter", mode = "markers", marker = list(color = "lightgray", size = 5), hoverinfo = "none", color = "All models/scenarios/runs/periods")

        # axis titles
        fig <- fig %>% plotly::layout(
          # xaxis = list(title = paste("Change in", variables$Variable[which(variables$Code == xvar)]), range = range(data.all$xanom)),
          xaxis = list(title = stringi::stri_unescape_unicode(paste("Change in", variables$Variable[which(variables$Code == xvar)])), range = range(data.all$xanom)),
          # yaxis = list(title = paste("Change in", variables$Variable[which(variables$Code == yvar)]), range = range(data.all$yanom))
          yaxis = list(title = stringi::stri_unescape_unicode(paste("Change in", variables$Variable[which(variables$Code == yvar)])), range = range(data.all$yanom))
          
        )

        # observed climate
        fig <- fig %>% plotly::add_markers(obs$xanom, obs$yanom,
          name = "Observed Climate (2001-2020)", text = "observed\n(2001-2020)", hoverinfo = "text",
          marker = list(size = 25, color = "grey", symbol = 1)
        )

        # ensemble mean
        fig <- fig %>% plotly::add_markers(ensMean$xanom, ensMean$yanom,
          name = "Ensemble mean", text = "Ensemble mean", hoverinfo = "text",
          marker = list(size = 20, color = "grey", symbol = 3)
        )

        # plot individual runs
        if (show_runs) {
          for (gcm in gcms) {
            i <- which(gcms == gcm)
            x.runs <- data.all[GCM == gcm & RUN != "ensembleMean" & PERIOD == period_focal, xanom]
            y.runs <- data.all[GCM == gcm & RUN != "ensembleMean" & PERIOD == period_focal, yanom]
            runs <- data.all[GCM == gcm & RUN != "ensembleMean" & PERIOD == period_focal, RUN]
            ssps <- data.all[GCM == gcm & RUN != "ensembleMean" & PERIOD == period_focal, SSP]
            fig <- fig %>% plotly::add_markers(
              x = x.runs, y = y.runs, color = ColScheme[i], name = "Individual GCM runs", text = paste(gcms[i], ssps, runs), hoverinfo = "text", showlegend = if (i == 1) TRUE else FALSE,
              marker = list(size = 7, color = ColScheme[i], line = list(color = "black", width = 1)), legendgroup = paste("group", i, sep = "")
            )
          }
        }

        # GCM mean trajectories
        # plot model means and trajectories
        gcm <- gcms[2]
        for (gcm in gcms) {
          i <- which(gcms == gcm)
          x2 <- c(0, data[GCM == gcm & RUN == "ensembleMean", xanom])
          y2 <- c(0, data[GCM == gcm & RUN == "ensembleMean", yanom])
          if (show_trajectories) {
            if (length(unique(sign(diff(x2)))) == 1) {
              x3 <- if (unique(sign(diff(x2))) == -1) rev(x2) else x2
              y3 <- if (unique(sign(diff(x2))) == -1) rev(y2) else y2
              s <- stinepack::stinterp(x3, y3, seq(min(x3), max(x3), diff(range(data.all$xanom)) / 500)) # way better than interpSpline, not prone to oscillations
              fig <- fig %>% plotly::add_trace(x = s$x, y = s$y, color = ColScheme[i], type = "scatter", mode = "lines", line = list(color = ColScheme[i], width = 2), marker = NULL, legendgroup = paste("group", i, sep = ""), showlegend = FALSE)
            } else {
              fig <- fig %>% plotly::add_trace(x = x2, y = y2, color = ColScheme[i], type = "scatter", mode = "lines", line = list(color = ColScheme[i], width = 2), marker = NULL, legendgroup = paste("group", i, sep = ""), showlegend = FALSE)
            }
            fig <- fig %>% plotly::add_markers(
              x = x2, y = y2, color = ColScheme[i], text = gcms[i], hoverinfo = "text",
              marker = list(size = 8, color = ColScheme[i]), legendgroup = paste("group", i, sep = ""), showlegend = FALSE
            )
          }
          j <- which(list_gcm_periods() == period_focal) + 1
          fig <- fig %>% plotly::add_markers(x2[j], y2[j],
            color = gcms[i], colors = ColScheme[i],
            marker = list(size = 20, color = ColScheme[i], line = list(color = "black", width = 1)),
            legendgroup = paste("group", i, sep = "")
          )

          fig <- fig %>% plotly::add_annotations(
            x = x2[j], y = y2[j], text = sprintf("<b>%s</b>", substr(gcms, 1, 2)[i]), xanchor = "center", yanchor = "center", showarrow = FALSE,
            legendgroup = paste("group", i, sep = "")
          )
        }

        if (xvar_type == "ratio") fig <- fig %>% plotly::layout(xaxis = list(tickformat = ".2%"))
        if (yvar_type == "ratio") fig <- fig %>% plotly::layout(yaxis = list(tickformat = ".2%"))

        fig
      }
    }
  }
}
