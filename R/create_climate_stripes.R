#' create climate stripes 
#'
#' @description
#' Plot temperature change relative to average over time 
#'
#'
#' @param dt A `data.table` object that contains the change in temperature relative to the average
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
create_climate_stripes <- function(dt, mode = c("warming_stripes", "labelled_stripes", "bars", "bars_with_scale")) {
  
  #Remove NSE CRAN check warnings
  if (FALSE){ x <- y <- marker_color  <- NULL}
  
  mode <- match.arg(mode)
  
  # options
  negative_climate_color_ramp <- colorRamp(colors = c("#0a306b","#08519c","#2270b5","#4192c5","#95bcd0","#c6daef","#d5e4f4"))
  positive_climate_color_ramp <- colorRamp(colors = c("#fee0d2","#fcbba1","#fb6a49","#ee3a2c","#a50e15","#48151a"))
  
  # create color by band
  dt[y <= 0, marker_color := convert_rgb_to_col(negative_climate_color_ramp(rescale(y, to = c(0,1), from = c(min(y), 0))))]
  dt[y >0, marker_color := convert_rgb_to_col(positive_climate_color_ramp(rescale(y, to = c(0,1), from = c(0, max(y)))))]
  
  if (mode %in% "warming_stripes") {
    return(plotly::plot_ly(dt) |>
             plotly::add_bars(x = ~x, y = ~1, marker = list(color = ~marker_color)) |>
             plotly::layout(bargap = 0, xaxis = list(visible = FALSE), yaxis = list(visible = FALSE)))
  }
  if (mode %in% "labelled_stripes") {
    return(plotly::plot_ly(dt) |>
             plotly::add_bars(x = ~x, y = ~1, marker = list(color = ~marker_color)) |>
             plotly::layout(title = list(text = "Global temperature change (1850-2023)"),
                            bargap = 0,
                            xaxis = list(showgrid = FALSE, showline = FALSE, title = FALSE),
                            yaxis = list(visible = FALSE)))
  }
  if (mode %in% "bars") {
    return(plotly::plot_ly(dt) |>
             plotly::add_bars(x = ~x, y = ~y, marker = list(color = ~marker_color)) |>
             plotly::layout(title = list(text = paste0("Global temperature have increase by over ", dt[, round(max(y)- min(y),1)], "\u00B0C")),
                            bargap = 0, 
                            xaxis = list(visible = FALSE),
                            yaxis = list(visible = FALSE)))
    
  }
  if (mode %in% "bars_with_scale") {
    return(plotly::plot_ly(dt) |>
             plotly::add_bars(x = ~x, y = ~y, marker = list(color = ~marker_color)) |>
             plotly::layout(bargap = 0,
                            xaxis = list(showgrid = FALSE,showline = TRUE, title = FALSE),
                            yaxis = list(showgrid = FALSE, showline = TRUE, title = FALSE)))
  }
  
}

#' @noRd
convert_rgb_to_col <- function(rgb_matrix) {
  rgb(rgb_matrix[,1], rgb_matrix[,2], rgb_matrix[,3], maxColorValue = 255) 
}


#' Create input for climate stripes
#' 
#' This function will downscale and return a data.table used to call `create_climate_stripes`.
#' 
#' @inheritParams downscale
#' @param use_downscale_db Should the function `downscale_db` be used instead of `downscale`
#' @param downscale_results optional, if you want to pass precomputed downscale results
#' @param ... Additional arguments passed to `downscale` or `downscale_db`
#'
#' @return A data.table containing averaged climate variables by period, 
#'    including yearly temperature (average)
#' @export
#' @examples
#' \dontrun{
#' in_xyz <- data.frame(lon = -127.7052, lat = 55.3557, elev = 291, id = 1)
#' create_climate_stripes_input(in_xyz)
#' create_climate_stripes_input(in_xyz, max_run = 1L, 
#'   gcm_hist_years = 1950:2015, 
#'   gcm_ssp_years = 2015:2040)
#' }

create_climate_stripes_input <- function(xyz = NULL,
                                 gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
                                 ssps = list_ssps()[2],
                                 max_run = 10L,
                                 obs_years = list_obs_years(),
                                 gcm_hist_years = list_gcm_hist_years(),
                                 gcm_ssp_years = list_gcm_ssp_years(), 
                                 use_downscale_db = FALSE,
                                 cache = TRUE, 
                                 downscale_results = NULL,
                                 ...){
  
  
  vars <- c("Tave")
  
  dwn_dt <- summarize_downscale_input(xyz,
                            gcms = gcms,
                            ssps = ssps,
                            max_run = max_run,
                            obs_years = obs_years,
                            gcm_hist_years = gcm_hist_years,
                            gcm_ssp_years = gcm_ssp_years, 
                            use_downscale_db = use_downscale_db,
                            cache = cache, 
                            vars = vars,
                            downscale_results = downscale_results,
                            ...)
  
  # calc mean temperature over all period and difference to mean
  dwn_dt[ , avg_temp := mean(Tave)]
  dwn_dt[ , y := Tave - avg_temp]
  dwn_dt[ , x := PERIOD]

  return(dwn_dt)
}


