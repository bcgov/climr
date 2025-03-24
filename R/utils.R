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
#' @param overwrite A logical. Replace existing files. Default to `FALSE`.
#' @importFrom stats setNames
#' @keywords internal
#' @export
tif_folder_gen <- function(dir, overwrite = FALSE) {
  tif <- list.files(dir, full.names = TRUE)
  names(tif) <- basename(tif)
  v <- function(nm) {
    terra::rast(tif["%s.tif" |> sprintf(nm)])
  }
  for (nm in names(.calc_def)) {
    tnm <- "%s.tif" |> sprintf(nm)
    fnm <- file.path(dir, tnm)
    if (file.exists(fnm) && !overwrite) next
    unlink(fnm)
    r <- .calc_def[[nm]](v)
    names(r) <- nm
    terra::writeRaster(x = r, filename = fnm, gdal=c("PREDICTOR=2"), datatype="FLT4S", overwrite = TRUE)
    tif <- c(tif, setNames(fnm, tnm))
  }
}

#' Create a latitude raster
#' @param r A SpatRaster.
#' @param out A file path out for `terra::writeRaster`. Default NULL.
#' @param ... Additional parameters to `terra::writeRaster`.
#' @examples
#' get_latitude_raster(system.file("ex/elev.tif", package="terra") |> terra::rast())
#' @keywords internal
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
#' r <- terra::rast(
#'   nrows=108, ncols=21,
#'   xmin=5.75, xmax=6.5,
#'   ymin = 49.5, ymax = 50,
#'   crs = "EPSG:4326"
#' )
#' elev <- system.file("ex/elev.tif", package="terra") |> terra::rast()
#' get_elev_raster(r, elev)
#' @keywords internal
#' @export
get_elev_raster <- function(r, elev, out = NULL, ...) {
  prj <- terra::project(elev, r, method = "near")
  names(prj) <- "elevation"
  if (!is.null(out)) {
    terra::writeRaster(prj, out, ...)
    return(invisible(prj))
  } else {
    return(prj)
  }
}


#' Bilinear interpolation point extraction from raster bands
#' @param dbCon A postgres database connection.
#' @param rastertbl The name of the raster table to extract from.
#' @param layers A data.table with column `var_nm` (names of the bands) and
#' @param VAR In case of rasters split across different tables. One VAR in each.
#' The first VAR will be used for replacement in labels names. Assuming all
#' tables have the same structure, only different VAR in them.
#' @param hull An optional hull well-known text geometry to prefilter
#' raster table before merging points projected in srid 4326.
#' `laynum` (index number of the bands to extract).
#' @return A data.frame one row per point id in xyz with an ID
#' column + one columns for each `bands` in `rastertbl` name `nm`.
#' @importFrom stringi stri_replace_all_fixed
#' @keywords internal
#' @export
extract_db <- function(
  dbCon,
  rastertbl,
  layers = data.table::data.table(var_nm = character(), laynum = integer()),
  VAR = NULL,
  hull = NULL
) {
  colorder <- c("ID", layers[["var_nm"]])
  layers <- layers[order(laynum)]
  if (!is.null(VAR) && length(VAR)) {
    rastertbl <- vapply(
      tolower(VAR),
      FUN = gsub,
      FUN.VALUE = character(1),
      pattern = "VAR",
      x = rastertbl
    )
    for (v in VAR[-1]) {
      colorder <- c(colorder, gsub(VAR[1], v, layers[["var_nm"]], fixed = TRUE))
    }
  }
  bands <- {
    if (!length(layers[["laynum"]])) {
      "NULL::integer[]"
    } else {
      layers[["laynum"]] |> paste0(collapse = ",") |> sprintf(fmt ="ARRAY[%s]")
    }
  }
  q <- paste0("
    WITH pixel_sizes as (
      SELECT pw,
             ph,
    		     pw/2 pw2,
    		     ph/2 ph2
      FROM \"%s\"
      CROSS JOIN LATERAL ST_PixelWidth(rast) pw, abs(ST_PixelHeight(rast)) ph
      LIMIT 1
    ), tmp_metadata AS (
      SELECT m3.id,
             m3.rid,
             m3.xr - (m3.xcell - m3.xdir + 0.5) AS xr,
             m3.yr - (m3.ycell - m3.ydir + 0.5) AS yr,
             m3.xcell,
             m3.ycell,
             m3.xdir,
             m3.ydir
      FROM (
        SELECT m2.*,
               (CASE WHEN m2.xr < (m2.xcell + 0.5) THEN 1 ELSE 0 END)::integer AS xdir,
               (CASE WHEN m2.yr < (m2.ycell + 0.5) THEN 1 ELSE 0 END)::integer AS ydir
        FROM (
          SELECT m1.*,
                 floor(m1.xr)::integer AS xcell,
                 floor(m1.yr)::integer AS ycell
          FROM (
            SELECT p.id,
                   r.rid,
                   (ST_X(p.geom) - ST_UpperLeftX(r.rast))/pw AS xr,
                   (ST_UpperLeftY(r.rast) - ST_Y(p.geom))/ph AS yr
            FROM tmp_xyz AS p,
                 \"%s\" r,
                 pixel_sizes ps
            WHERE %s
          ) AS m1
        ) AS m2
      ) AS m3
    ), tmp_interpolation AS (
      SELECT id,
             rid,
             (1 - xr) * (1 - yr) AS w1, 
             (1 - yr) * xr       AS w2,
             (1 - xr) * yr       AS w3,
             xr * yr             AS w4,
             (xcell + 1 - xdir)  AS x1,
             (xcell + 2 - xdir)  AS x2,
             (ycell + 1 - ydir)  AS y1,
             (ycell + 2 - ydir)  AS y2,
             (xcell + 1)         AS cx,
             (ycell + 1)         AS cy
      FROM tmp_metadata
    ), tmp_rid AS (
      SELECT DISTINCT rid FROM tmp_interpolation
    ), tmp_compute AS (" |> sprintf(
      rastertbl[1],
      rastertbl[1],
      {
        cond <- "ST_Intersects(ST_ConvexHull(r.rast), ST_Expand(p.geom, pw2, ph2))"
        if (!is.null(hull)) {
          cond <- c("ST_Intersects(ST_ConvexHull(r.rast), ST_Expand(ST_GeomFromText('%s', 4326), pw2, ph2))" |> sprintf(hull), cond)
        }
        paste(cond, collapse = " AND ")
      }
    )
    , if (is.null(VAR)) {"
    SELECT m0.id::float AS \"ID\",
           v.nband,
           CASE WHEN AVG(v.valarray[cy][cx]) IS NULL THEN NULL ELSE
           (
             coalesce(AVG(v.valarray[y1][x1]), AVG(v.valarray[cy][cx])) * AVG(m0.w1) +
             coalesce(AVG(v.valarray[y1][x2]), AVG(v.valarray[cy][cx])) * AVG(m0.w2) +
             coalesce(AVG(v.valarray[y2][x1]), AVG(v.valarray[cy][cx])) * AVG(m0.w3) +
             coalesce(AVG(v.valarray[y2][x2]), AVG(v.valarray[cy][cx])) * AVG(m0.w4)
           ) END AS value
    FROM tmp_interpolation AS m0
    JOIN (
      SELECT r0.rid,
             (ST_DumpValues(r0.rast, %s, true)).*
      FROM \"%s\" AS r0
      WHERE r0.rid IN (SELECT * FROM tmp_rid)
    ) AS v
    ON v.rid = m0.rid
    GROUP BY 1,2" |> sprintf(bands, rastertbl[1])
    } else { 
      paste0(collapse = "\n UNION ALL",
             "
    SELECT m0.id::float AS \"ID\",
           v.nband,
           '%s' AS var,
           CASE WHEN AVG(v.valarray[cy][cx]) IS NULL THEN NULL ELSE
           (
             coalesce(AVG(v.valarray[y1][x1]), AVG(v.valarray[cy][cx])) * AVG(m0.w1) +
             coalesce(AVG(v.valarray[y1][x2]), AVG(v.valarray[cy][cx])) * AVG(m0.w2) +
             coalesce(AVG(v.valarray[y2][x1]), AVG(v.valarray[cy][cx])) * AVG(m0.w3) +
             coalesce(AVG(v.valarray[y2][x2]), AVG(v.valarray[cy][cx])) * AVG(m0.w4)
           ) END AS value
    FROM tmp_interpolation AS m0
    JOIN (
      SELECT r0.rid,
             (ST_DumpValues(r0.rast, %s, true)).*
      FROM \"%s\" AS r0
      WHERE r0.rid IN (SELECT * FROM tmp_rid)
    ) AS v
    ON v.rid = m0.rid
    GROUP BY 1,2,3" |> sprintf(VAR, bands, rastertbl)
      )
    }, ")
    SELECT res.*
    FROM tmp_xyz p
    LEFT JOIN tmp_compute res
      ON p.id = res.\"ID\"
    ;"
  )
  
  res <- DBI::dbGetQuery(dbCon, q) |> 
    data.table::setDT()
  
  uniqb <- res[["nband"]] |> unique() |> sort()
  if (!nrow(layers)) {
    layers <- data.table::data.table(
      var_nm = as.character(uniqb),
      laynum = uniqb
    )
    if (!is.null(VAR) && length(VAR)) {
      layers[["var_nm"]] <- paste(VAR[1], layers[["var_nm"]], sep = "_")
    }
  }
  if (nrow(res) && length(misband <- setdiff(layers[["laynum"]], uniqb))) {
    if (length(misband) > 10) {
      misband <- c(utils::head(misband, 10), "...")
    }
    stop(
      "Not all requested bands were in table `%s`. [Bands: %s]" |>
        sprintf(rastertbl[1], paste0(misband, collapse = ","))
    )
  }
  data.table::set(
    res,
    j = "band",
    value = layers[["var_nm"]][match(res[["nband"]], layers[["laynum"]])]
  )
  if (!is.null(VAR) && length(VAR)) {
    res[, band := stringi::stri_replace_all_fixed(
      str = band,
      pattern = VAR[1],
      replacement = var,
      vectorize_all = TRUE
    )]
  }
  if (any(duplicated(res[,list(ID, band)]))) {
    warning("Overlapping raster in table. [%s]" |> sprintf(rastertbl))
  }
  res <- data.table::dcast(res, ID ~ band, value.var = "value")
  data.table::setcolorder(res, colorder)
  
  if (nrow(res) != {nxyz <- DBI::dbGetQuery(dbCon, "SELECT COUNT(1) FROM tmp_xyz")[[1]]}) {
    warning(
      "Extraction from [%s] returned [%s] rows and requested geometries has [%s]. Filled with missing values" |> 
        sprintf(rastertbl[1], nrow(res), nxyz)
    )
    xyzids <- DBI::dbGetQuery(dbCon, "SELECT id \"ID\" FROM tmp_xyz") |> data.table::setDT(key = "ID")
    res <- data.table::rbindlist(list(xyzids, res), use.names = TRUE, fill = TRUE)
  }
  
  return(res |> as.data.frame())
}
