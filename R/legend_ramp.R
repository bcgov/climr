#' Draw a color ramp legend that handles logarithmic scaling
#'
#' This function draws a color ramp legend in two modes:
#' 1. **Raster mode**: relative to a raster's spatial extent (if a `terra` raster is supplied).
#' 2. **Device mode**: using normalized device coordinates (0–1) if no raster is supplied,
#'    allowing placement in empty plots or arbitrary plot regions.
#'
#' The function can automatically determine horizontal vs vertical orientation
#' and supports optional margins around the legend.
#'
#' @param r Optional. A `terra` SpatRaster. If supplied, `pos` is interpreted relative to the raster extent.
#' @param title Character. Title for the legend.
#' @param ColScheme Character vector of colors for the legend.
#' @param breaks Numeric vector with length = length(ColScheme) + 1.
#' @param pos Numeric vector length 4: \code{c(xmin, xmax, ymin, ymax)}.  
#'   Interpreted relative to raster extent if `r` is provided; otherwise as 0–1 device coordinates.
#' @param log Numeric or NULL. Base of logarithm to transform labels.
#' @param log.relative Logical. If TRUE, labels show percent change from 1 (useful for relative log scales).
#' @param horizontal Logical or NULL. If NULL, orientation is chosen automatically based on bounding box shape.
#' @param title.height Numeric. Scaling factor for the title offset relative to the legend size.
#' @param margin Numeric. Proportion of legend width/height used as padding (default 0.02).
#'
#' @return NULL. Draws a legend on the current plot device.
#'
#' @details
#' This function can be used to add a legend to maps plotted with `terra::plot()` or to any plot window.
#' When `r` is provided, the legend is placed proportionally inside the raster extent.
#' When `r` is `NULL`, the legend uses normalized device coordinates (0–1), so it can be placed
#' in an empty plot window or layout panel. Margins prevent labels or title from touching the edges.
#'
#' @examples
#' ## DEVICE MODE: Empty plot
#' plot.new()
#' plot.window(xlim=c(0,1), ylim=c(0,1))
#' cols <- hcl.colors(20, "GnBu")
#' brks <- seq(0,1,length.out=21)
#' legend_ramp(
#'   r = NULL,
#'   title = "Device Legend",
#'   ColScheme = cols,
#'   breaks = brks,
#'   pos = c(0.1, 0.9, 0.1, 0.18),
#'   horizontal = NULL,
#'   margin = 0.01
#' )
#'
#' ## RASTER MODE: If a raster is available
#' # get the sample digital elevation model (dem) provided with `climr`
#' dem <- get(data("dem_vancouver")) |> terra::unwrap()
#' # A simple climr query.
#' # This will return the observed 1961-1990 normals for the raster grid points.
#' var <- "MAP"
#' clim <- downscale(dem, which_refmap = "refmap_climr", vars = var, ppt_lr = TRUE)
#' # log-transform precipitation for more meaningful scaling
#' clim <- log2(clim)
#' # increment for the ramp
#' inc=diff(range(terra::values(clim)))/500
#' # color breaks
#' breaks=seq(min(terra::values(clim))-inc, max(terra::values(clim))+inc, inc)
#' # color scheme
#' ColScheme <- rev(hcl.colors(length(breaks)-1, "GnBu"))
#' terra::plot(clim, col=ColScheme, breaks=breaks, legend=FALSE, main="", mar=NA)
#' legend_ramp(
#'   clim,
#'   title = paste(var, "(mm)"),
#'   ColScheme = ColScheme,
#'   breaks = breaks,
#'   pos=c(0.05, 0.45, 0.1, 0.125),
#'   log = 2,
#'   horizontal = TRUE
#' )
#' 
#' @export
#' 
legend_ramp <- function(r = NULL, title, ColScheme, breaks,
                        pos = c(0.2, 0.23, 0.1, 0.5),
                        log = NULL, log.relative = FALSE,
                        horizontal = NULL,
                        title.height = 1,
                        margin = 0.02) {
  
  # --- Determine bounding box ---------------------------------------------------
  if (!is.null(r)) {
    e <- terra::ext(r)
    xmin <- e[1] + (e[2] - e[1]) * pos[1]
    xmax <- e[1] + (e[2] - e[1]) * pos[2]
    ymin <- e[3] + (e[4] - e[3]) * pos[3]
    ymax <- e[3] + (e[4] - e[3]) * pos[4]
  } else {
    x_user <- function(x) grconvertX(x, from = "ndc", to = "user")
    y_user <- function(y) grconvertY(y, from = "ndc", to = "user")
    xmin <- x_user(pos[1])
    xmax <- x_user(pos[2])
    ymin <- y_user(pos[3])
    ymax <- y_user(pos[4])
  }
  
  # Apply margin
  x_pad <- (xmax - xmin) * margin
  y_pad <- (ymax - ymin) * margin
  xmin <- xmin + x_pad
  xmax <- xmax - x_pad
  ymin <- ymin + y_pad
  ymax <- ymax - y_pad
  
  # Determine orientation if horizontal is NULL
  if (is.null(horizontal)) {
    horizontal <- (xmax - xmin) > (ymax - ymin)
  }
  
  n_colors <- length(ColScheme)
  
  # Tick values and labels
  legend.values <- pretty(breaks)
  legend.values <- legend.values[legend.values >= min(breaks) &
                                   legend.values <= max(breaks)]
  legend.labels <- if (is.null(log)) {
    legend.values
  } else if (log.relative) {
    paste0(round(log^legend.values * 100 - 100, 1), "%")
  } else {
    round(log^legend.values)
  }
  
  par(xpd = TRUE)
  
  # --- Internal helper function to draw legend ---------------------------------
  draw_legend <- function(xmin, xmax, ymin, ymax, ColScheme, legend.values,
                          legend.labels, horizontal, title, title.height) {
    
    n_colors <- length(ColScheme)
    
    if (horizontal) {
      x_positions <- seq(xmin, xmax, length.out = n_colors + 1)
      rect(head(x_positions, -1), ymin,
           tail(x_positions, -1), ymax,
           col = ColScheme, border = NA)
      
      label_positions <- scales::rescale(legend.values, to = c(xmin, xmax),
                                         from = range(legend.values))
      text(label_positions, ymin, labels = legend.labels, pos = 1)
      text((xmin + xmax) / 2, ymax, labels = title, pos = 3, font = 2)
      
    } else {
      y_positions <- seq(ymin, ymax, length.out = n_colors + 1)
      rect(xmin, head(y_positions, -1),
           xmax, tail(y_positions, -1),
           col = ColScheme, border = NA)
      
      label_positions <- scales::rescale(legend.values, to = c(ymin, ymax),
                                         from = range(legend.values))
      text(xmax, label_positions, labels = legend.labels, pos = 4)
      text(xmin - (xmax - xmin) / (6 / title.height),
           (ymin + ymax) / 2,
           labels = title, srt = 90, font = 2)
    }
    
    # Outline
    rect(xmin, ymin, xmax, ymax, col = NA)
  }
  
  # Call the helper
  draw_legend(xmin, xmax, ymin, ymax, ColScheme, legend.values,
              legend.labels, horizontal, title, title.height)
  
  par(xpd = FALSE)
}
