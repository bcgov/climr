#' Be quiet
#'
#' Suppresses messaging.
#'
#' @param expr expression to evaluate in quite mode.
#'
#' @noRd
shush <- function(expr) {
  suppressMessages(
    try(expr, silent = TRUE),
    classes = c("message", "condition")
  )
}


#' Check that new BB is in old BB
#'
#' @param newbb TODO
#' @param oldbb TODO
#'
#' @return logical
#' @noRd
is_in_bbox <- function(newbb, oldbb) {
  if (newbb[1] <= oldbb[1] & newbb[2] >= oldbb[2] & newbb[3] <= oldbb[3] & newbb[4] >= oldbb[4]) {
    TRUE
  } else {
    FALSE
  }
}

#' Find bounding box of data
#'
#' @param in_xyz `data.table` (or `data.frame`) of points to downscale
#'  with columns "lon", "lat", "elev" and "id"
#' @return numeric vector. Bounding box coordinates with order ymax,ymin,xmax,xmin (e.g. `c(51, 50, -121, -122)`).
#' @export
get_bb <- function(in_xyz) {
  .checkXYZ(copy(in_xyz))
  thebb <- c(max(in_xyz[, "lat"]), min(in_xyz[, "lat"]), max(in_xyz[, "lon"]), min(in_xyz[, "lon"]))

  if (any(is.na(thebb))) {
    stop("Couldn't guess bounding box. Are there NA's in 'xyz'?")
  }

  .check_bb(thebb)

  return(thebb)
}


#' Check that BB is within the lat/long EPSG bounding box
#'
#' @template bbox
#'
#' @return NULL
#' @noRd
.check_bb <- function(bbox) {
  ## check projection
  minLon <- -179.0625
  maxLon <- -51.5625
  minLat <- 14.375
  maxLat <- 83.125

  if (any(
    (bbox[4] < minLon), (bbox[3] > maxLon),
    (bbox[2] < minLat), (bbox[1] > maxLat)
  )) {
    stop("input fields lon and lat are not in lat/long coordinates, or extent is outside ext(-179.0625, -51.5625, 14.375, 83.125)")
  }
}

#' Process a tif folder of TminXX.tif, TmaxXX.tif, PPTXX.tif
#' to create a set of climate variables tifs
#' @param dir A folder path.
#' @export
tif_folder_gen <- function(dir, overwrite = FALSE) {
  tif <- list.files(dir, full.names = TRUE)
  names(tif) <- basename(tif)
  v <- function(nm) {
    terra::rast(tif["%s.tif" |> sprintf(nm)])
  }
  for (nm in names(.calc_def)) {
    tnm <- "%s.tif" |> sprintf(nm)
    f <- file.path(dir, tnm)
    if (file.exists(f) && !overwrite) next
    r <- .calc_def[[nm]](v)
    names(r) <- nm
    terra::writeRaster(r, f, gdal=c("PREDICTOR=2"), datatype="FLT4S", overwrite = TRUE)
    tif <- c(tif, setNames(f, tnm))
  }
}

#' Create a latitude raster
#' @param r A SpatRaster.
#' @param out A file path out for `terra::writeRaster`. Default NULL.
#' @param ... Additional parameters to `terra::writeRaster`.
#' @examples
#' get_latitude_raster("Tmin_01.tif", out = "alt.tif", gdal = "PREDICTOR=2", datatype="FLT4S", overwrite = TRUE)
#' @export
get_latitude_raster <- function(r, out = NULL, ...) {
  r <- terra::rast(r)
  # Check if the input raster is in a longitude/latitude CRS
  if (terra::is.lonlat(r)) {
    # Get the y-coordinates (latitudes) for each row
    y_coords <- terra::yFromRow(r, 1:nrow(r))
    # Create a vector of latitude values, repeating each row's latitude across columns
    values_vector <- rep(y_coords, each = ncol(r))
    # Create a new raster with the same properties as the input, filled with latitude values
    lat_raster <- terra::rast(
      nrows = nrow(r),
      ncols = ncol(r),
      nlyrs = 1,
      extent = terra::ext(r),
      crs = terra::crs(r),
      vals = values_vector
    )
  } else {
    # Define a function to compute latitudes for a vector of cell indices
    lat_fun <- function(cell) {
      # Get the x, y coordinates of the cells in the input CRS
      xy <- terra::xyFromCell(r, seq_len(cell))
      # Project coordinates to WGS84 geographic CRS (EPSG:4326)
      geo_xy <- terra::project(xy, from = crs(r), to = "EPSG:4326")
      # Return the latitude (second column)
      return(geo_xy[, 2])
    }
    # Initialize a new raster with the latitude values computed for each cell
    lat_raster <- terra::init(r, fun = lat_fun)
  }
  names(lat_raster) <- "latitude"
  if (!is.null(out)) {
    terra::writeRaster(lat_raster, out, ...)
    return(invisible(lat_raster))
  }
  # Return the resulting latitude raster
  return(lat_raster)
}

#' Create an elevation raster from a template raster
#' @param r A SpatRaster. The template.
#' @param elev An elevation raster such as (northamerica_elevation_cec_2023.tif) from
#' http://www.cec.org/files/atlas_layers/0_reference/0_03_elevation/elevation_tif.zip
#' @param out A file path out for `terra::writeRaster`. Default NULL.
#' @param ... Additional parameters to `terra::writeRaster`.
#' @examples
#' r <- terra::rast(nrows=108, ncols=21, xmin=0, xmax=10)
#' elev <- system.file("ex/elev.tif", package="terra") |> terra::rast()
#' get_elev_raster(r, elev, out = "elev.tif", gdal = "PREDICTOR=2", datatype="FLT4S", overwrite = TRUE)
#' 
#' @export
get_elev_raster <- function(r, elev, out = NULL, ...) {
  elev <- terra::rast(elev)
  r <- terra::rast(r)
  prj <- terra::project(elev, r, method = "near")
  names(prj) <- "elevation"
  if (!is.null(out)) {
    terra::writeRaster(prj, out, ...)
    return(invisible(prj))
  } else {
    return(prj)
  }
}