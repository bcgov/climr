
#' Legend for color ramp maps 
#'
#' This function plots a map legend that handles logarithmic scaling
#'
#' @param r A spatial raster object, typically from the `terra` package.
#' @param title Character. The title for the legend.
#' @param ColScheme Character vector. Specifies the colors used for the legend. Typically corresponds 
#'   to the color scheme applied to the raster data visualization.
#' @param breaks Numeric vector. Defines the breakpoints for the legend, determining how the range 
#'   of the raster is divided into intervals corresponding to the colors in `ColScheme`.
#' @param pos Numeric vector of length 4. Defines the position of the legend as 
#'   proportions of the raster extent. The format is `c(xmin, xmax, ymin, ymax)`.
#' @param log Numeric or NULL. The base of the logarithm used to compute legend labels. 
#'   If `NULL`, values are displayed as-is.
#' @param horizontal Logical. Should the legend be drawn horizontally? Default is `FALSE`.
#' @param title.height Numeric. Scaling factor for the height of the legend title relative to the legend size.
#'
#' @details
#' The function uses the extent of the raster object to determine the positioning and size of 
#' the legend. The legend can be oriented either horizontally or vertically, and logarithmic 
#' scaling is applied to the labels if the `log` parameter is specified.
#' @keywords internal
#'
#' @importFrom terra ext
#' @importFrom scales rescale
#' @importFrom graphics rect
#'
#' @return NULL. A plot of the legend is drawn in the current plotting device. The function does not return any value.
#' 
#' @examples
#' ## get the sample digital elevation model (dem) provided with `climr`
#' dem <- get(data("dem_vancouver")) |> terra::unwrap()
#' ## A simple climr query.
#' ## This will return the observed 1961-1990 normals for the raster grid points.
#' var <- "MAP"
#' clim <- downscale(dem, which_refmap = "refmap_climr", vars = var)
#' ## log-transform precipitation for more meaningful scaling
#' clim <- log2(clim)
#' ## increment for the ramp
#' inc=diff(range(terra::values(clim)))/500
#' ## color breaks
#' breaks=seq(min(terra::values(clim))-inc, max(terra::values(clim))+inc, inc)
#' ## color scheme
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

# Function for plotting a color ramp legend with optional log scaling
legend_ramp <- function(r, title, ColScheme, breaks,
                       pos = c(0.2, 0.23, 0.1, 0.5), 
                       log = NULL, 
                       horizontal = FALSE, 
                       title.height = 1) {
  
  # Define legend position
  xmin <- ext(r)[1] + diff(ext(r)[1:2]) * pos[1]  # Left boundary of the legend
  xmax <- ext(r)[1] + diff(ext(r)[1:2]) * pos[2]  # Right boundary of the legend
  ymin <- ext(r)[3] + diff(ext(r)[3:4]) * pos[3]  # Bottom boundary of the legend
  ymax <- ext(r)[3] + diff(ext(r)[3:4]) * pos[4]  # Top boundary of the legend
  
  # Draw the color ramp
  n_colors <- length(ColScheme)
  
  # Define legend values and labels
  legend.values <- pretty(breaks)
  legend.values <- legend.values[which(legend.values >= min(breaks) & legend.values <= max(breaks) )]
  legend.labels <- if(is.null(log)) legend.values else round(log^legend.values) 
  
  par(xpd=TRUE) # allow plotting outside the plotting window. 
  
  if (horizontal) {
    # Horizontal legend setup
    x_positions <- seq(xmin, xmax, length.out = n_colors + 1)
    rect(
      head(x_positions, -1), ymin,  # Left edges
      tail(x_positions, -1), ymax,  # Right edges
      col = ColScheme,
      border = NA
    )
    
    # Map legend values to positions on the x-axis
    label_positions <- scales::rescale(legend.values, to = c(xmin, xmax), from = range(breaks))
    
    # Add the labels
    text(
      x = label_positions,         # X-axis positions for labels
      y = ymin,  
      labels = legend.labels,
      pos = 1,
      adj = 0.5                    # Centered alignment
    )
    
    # Add the title
    text(
      x = (xmin + xmax) / 2,       # Centered horizontally on the legend
      y = ymax,
      labels = title,
      pos = 3,
      adj = 0.5,                  # Centered alignment
      font = 2                    # Bold font
    )
    
  } else {
    # Vertical legend setup
    y_positions <- seq(ymin, ymax, length.out = n_colors + 1)
    rect(
      xmin, head(y_positions, -1),  # Bottom edges
      xmax, tail(y_positions, -1),  # Top edges
      col = ColScheme,
      border = NA
    )
    
    # Map legend values to positions on the y-axis
    label_positions <- scales::rescale(legend.values, to = c(ymin, ymax), from = range(breaks))
    
    # Add the labels
    text(
      x = xmax,         # Slightly to the right of the legend
      y = label_positions,                      # Y-axis positions for labels
      labels = legend.labels,
      pos = 4
    )
    
    # Add a rotated title
    text(
      x = xmin - diff(ext(r)[1:2]) / (50/title.height),        # Slightly to the left of the legend
      y = (ymin + ymax) / 2,                    # Centered vertically on the legend
      labels = title,   
      srt = 90,                                 # Rotate text 90 degrees
      font = 2                                  # Bold font
    )
  }
  
  # Draw a border around the legend
  rect(xmin, ymin, xmax, ymax, col = NA)
  
  par(xpd=FALSE) # reset to default
  
}
