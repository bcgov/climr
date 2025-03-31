#' Plot climate stripes 
#'
#' @description
#' Plot temperature change relative to average over time 
#'
#'
#' @param X A `data.table` object that contains the change in temperature relative to the average
#' and the corresponding years.
#' @param mode A character string. Either `warming_stripes`, `labelled_stripes`, `bars` or 
#' `bars_with_scale`.
#' 
#'
#' @return plotly object, Draws a plot in the active graphics device.
#'
#' @examples
#' if (FALSE) {
#' }
#'
#' @importFrom plotly plot_ly layout
#' @importFrom grDevices colorRamp rgb
#' @importFrom stats runif
#' @import data.table
#' @importFrom scales rescale
#'
#' @export
plot_climate_stripes <- function(X, mode = c("warming_stripes", "labelled_stripes", "bars", "bars_with_scale")) {
  
  #Remove NSE CRAN check warnings
  if (FALSE){ x <- y <- marker_color  <- NULL}
  
  mode <- match.arg(mode)
  
  # options
  negative_climate_color_ramp <- colorRamp(colors = c("#0a306b","#08519c","#2270b5","#4192c5","#95bcd0","#c6daef","#d5e4f4"))
  positive_climate_color_ramp <- colorRamp(colors = c("#fee0d2","#fcbba1","#fb6a49","#ee3a2c","#a50e15","#48151a"))
  
  #TODO handle input data
  # mock data
  if (missing(x)) {
    X <- data.table(x = seq(1850, 2023, by = 2))
    X[, y := 0.00867052*x - 16.99 + runif(.N, -0.1, 0.1)]
    X[y <= 0, marker_color := convert_rgb_to_col(negative_climate_color_ramp(rescale(y, to = c(0,1), from = c(min(y), 0))))]
    X[y >0, marker_color := convert_rgb_to_col(positive_climate_color_ramp(rescale(y, to = c(0,1), from = c(0, max(y)))))]
  }
  # TODO plot 
  if (mode %in% "warming_stripes") {
    plotly::plot_ly(X) |>
      plotly::add_bars(x = ~x, y = ~1, marker = list(color = ~marker_color)) |>
      plotly::layout(bargap = 0, xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
  }
  if (mode %in% "labelled_stripes") {
    plotly::plot_ly(X) |>
      plotly::add_bars(x = ~x, y = ~1, marker = list(color = ~marker_color)) |>
      plotly::layout(title = list(text = "Global temperature change (1850-2023)"),bargap = 0, xaxis = list(showgrid = FALSE, showline = FALSE, title = FALSE), yaxis = list(visible = FALSE))
  }
  if (mode %in% "bars") {
    plotly::plot_ly(X) |>
      plotly::add_bars(x = ~x, y = ~y, marker = list(color = ~marker_color)) |>
      plotly::layout(title = list(text = paste0("Global temperature have increase by over ", X[, round(max(y)- min(y),1)], "\u00B0C")),bargap = 0, xaxis = list(visible = FALSE), yaxis = list(visible = FALSE))
    
  }
  if (mode %in% "bars_with_scale") {
    plotly::plot_ly(X) |>
      plotly::add_bars(x = ~x, y = ~y, marker = list(color = ~marker_color)) |>
      plotly::layout(bargap = 0, xaxis = list(showgrid = FALSE,showline = TRUE, title = FALSE), yaxis = list(showgrid = FALSE, showline = TRUE, title = FALSE))
  }

 }

#' @noRd
convert_rgb_to_col <- function(rgb_matrix) {
  rgb(rgb_matrix[,1], rgb_matrix[,2], rgb_matrix[,3], maxColorValue = 255) 
}
