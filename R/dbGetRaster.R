#' Download raster with bounding box from PostGIS (boostao version)
#' @template conn
#' @param name character. Table name in database.
#' @param tile Logical. Retrieve data in tiles to avoid overloading the database?
#' @param rast character. Name of column which stores raster data.
#'   Defaults to "rast"
#' @template bands
#' @template boundary

#' @return A `SpatRaster`
#'
#' @importFrom data.table setDT copy
#' @importFrom terra rast merge sprc
#' @importFrom RPostgres dbQuoteIdentifier dbGetQuery
#' @importFrom DBI dbQuoteIdentifier dbGetQuery
#' @export
dbGetRaster <- function(conn, name, tile, rast = "rast", bands = 37:73,
                       boundary) {

  # nameque <- paste(name1, collapse = ".")
  # namechar <- gsub("'", "''", paste(gsub('^"|"$', "", name1), collapse = "."))

  projID <- db_safe_query("
    select ST_SRID(%s) as srid
    from \"%s\" where rid = 1;
  " |> sprintf(rast, name)
  )$srid[1]
  
  rids <- db_safe_query("
    select rid
    from \"%s\"
    WHERE ST_Intersects(ST_ConvexHull(%s), ST_GeomFromText('POLYGON((%s %s,%s %s,%s %s,%s %s,%s %s))', %s))"
    |> sprintf(name, rast, boundary[4], boundary[1], boundary[4], boundary[2],
               boundary[3], boundary[2], boundary[3], boundary[1], boundary[4], boundary[1],
               projID)
  )

  info <- db_safe_query("
    select
      rid as id,
      st_xmax(st_envelope(rast)) as xmx,
      st_xmin(st_envelope(rast)) as xmn,
      st_ymax(st_envelope(rast)) as ymx,
      st_ymin(st_envelope(rast)) as ymn,
      st_width(rast) as cols,
      st_height(rast) as rows
    from \"%s\"
    WHERE rid IN (%s)" |> sprintf(name, paste(rids$rid, collapse = ","))
  )
  
  bandqs1 <- bands |>
    paste0(collapse = ",") |>
    sprintf(fmt ="UNNEST((ST_Dumpvalues(rast, ARRAY[%s])).valarray::real[][]) as vals")
  
  out_list <- lapply(rids$rid, \(rid) {
    info_tl <- info[info$id == rid,]
    qry2 <- "
      select %s
      from \"%s\"
      where rid = %s
    " |> sprintf(bandqs1, name, rid)
    r_values <- db_safe_query(qry2)[["vals"]]
    if (all(is.na(r_values))) {
      warning("Empty tile - not enough data")
      return(NULL)
    }
    terra::rast(
      nrows = info_tl$rows, ncols = info_tl$cols, xmin = info_tl$xmn,
      xmax = info_tl$xmx, ymin = info_tl$ymn, ymax = info_tl$ymx, nlyrs = length(bands),
      crs = paste0("EPSG:", 4326), vals = r_values
    )
  })
  
  rsrc <- terra::sprc(out_list)
  rast_res <- terra::merge(rsrc)
  return(rast_res)
}

#' Download raster with bounding box from PostGIS (original)
#' @template conn
#' @param name character. Table name in database.
#' @param tile Logical. Retrieve data in tiles to avoid overloading the database?
#' @param rast character. Name of column which stores raster data.
#'   Defaults to "rast"
#' @template bands
#' @template boundary

#' @return A `SpatRaster`
#'
#' @importFrom data.table setDT copy
#' @importFrom terra rast merge sprc
#' @importFrom RPostgres dbQuoteIdentifier dbGetQuery
#' @importFrom DBI dbQuoteIdentifier dbGetQuery
#' @export
pgGetTerra <- function(conn, name, tile, rast = "rast", bands = 1:73,
                       boundary) {
  
  projID <- dbGetQuery(conn, paste0("select ST_SRID(", rast, ") as srid from \"", name, "\" where rid = 1;"))$srid[1]
  
  if (length(bands) > 1664) { ## maximum number of columns
    info <- dbGetQuery(conn, paste0(
      "select
            st_xmax(st_envelope(rast)) as xmx,
            st_xmin(st_envelope(rast)) as xmn,
            st_ymax(st_envelope(rast)) as ymx,
            st_ymin(st_envelope(rast)) as ymn,
            st_width(rast) as cols,
            st_height(rast) as rows
            from
            (select st_union(", rast, ",", 1, ") rast from \"", name, "\" \n
            WHERE ST_Intersects(",
      rast, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
      " ", boundary[1], ",", boundary[4], " ", boundary[2],
      ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
      " ", boundary[1], ",", boundary[4], " ", boundary[1],
      "))'),", projID, "))) as a;"
    ))
    brks <- c(seq(1, length(bands), by = 1663), (length(bands) + 1))
    for (i in 1:(length(brks) - 1)) {
      bands_temp <- bands[brks[i]:(brks[i + 1] - 1)]
      bandqs1 <- paste0("UNNEST(ST_Dumpvalues(rast, ", bands_temp, ")) as vals_", bands_temp)
      bandqs2 <- paste0("ST_Union(rast", rast, ",", bands_temp, ") rast_", bands_temp)
      
      rast_vals_temp <- dbGetQuery(conn, paste0(
        "SELECT ", paste(bandqs1, collapse = ","),
        " from (SELECT ST_Union(rast) rast FROM \"", name, "\" WHERE ST_Intersects(",
        rast, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
        " ", boundary[1], ",", boundary[4], " ", boundary[2],
        ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
        " ", boundary[1], ",", boundary[4], " ", boundary[1],
        "))'),", projID, "))) as a;"
      ))
      setDT(rast_vals_temp)
      if (i == 1) {
        rast_vals <- copy(rast_vals_temp)
      } else {
        rast_vals <- cbind(rast_vals, rast_vals_temp)
      }
    }
    rast_vals <- suppressWarnings(melt(rast_vals, id.vars = c()))
    rout <- rast(
      nrows = info$rows, ncols = info$cols, xmin = info$xmn,
      xmax = info$xmx, ymin = info$ymn, ymax = info$ymx, nlyrs = length(bands),
      crs = paste0("EPSG:", projID), vals = rast_vals$value
    )
  } else {
    if (!tile) {
      rout <- make_raster(boundary, conn, rast, name, projID, bands)
      return(rout)
    }
    max_dist <- 5
    # if(boundary[1] - boundary[2] > max_dist | boundary[3] - boundary[4] > max_dist) {
    x_seq <- unique(c(seq(boundary[2], boundary[1], by = max_dist), boundary[1]))
    y_seq <- unique(c(seq(boundary[4], boundary[3], by = max_dist), boundary[3]))
    
    boundary_ls <- list()
    if (length(x_seq) < 2 | length(y_seq) < 2) {
      boundary_ls[["1_1"]] <- boundary
    } else {
      for (i in 1:(length(x_seq) - 1)) {
        for (j in 1:(length(y_seq) - 1)) {
          boundary_ls[[paste0(i,"_", j)]] <- c(x_seq[i + 1], x_seq[i], y_seq[j + 1], y_seq[j])
        }
      }
    }
    
    
    r_list <- lapply(boundary_ls,
                     FUN = make_raster,
                     conn = conn, rastque = rast,
                     nameque = name, projID = projID,
                     bands = bands
    )
    r_list <- r_list[!sapply(r_list, is.null)]
    if (length(r_list) > 1) {
      rout <- merge(sprc(r_list))
    } else {
      rout <- r_list[[1]]
    }
  }
  return(rout)
}


#' Download individual raster tiles from PostGIS
#' @template conn
#' @param name character. Table name in database.
#' @param pnts data.table of point locations with columns `lon` and `lat`
#' @template bands

#' @return A `SpatRaster`
#'
#' @import data.table
#' @importFrom terra rast merge sprc
#' @importFrom RPostgres dbGetQuery
#' @importFrom DBI dbGetQuery
#' @keywords internal
#' @export
dbGetTiles <- function(conn, name, pnts, bands = 1:73){
  
  #Remove NSE CRAN check warnings
  if (FALSE){ mls <- NULL}
  
  if (inherits(pnts, "SpatRaster")) {
    wkt_str <- terra::ext(ref) |> terra::vect() |> terra::geom(wkt = TRUE)
  } else {
    pnts[,mls := paste0("(",lon," ",lat,")")]
    wkt_str <- paste0("MULTIPOINT(", paste(pnts$mls, collapse = ", "),")") 
  }
  
  qry <- "
    select rid as id,
           st_xmax(st_envelope(rast)) as xmx,
           st_xmin(st_envelope(rast)) as xmn,
           st_ymax(st_envelope(rast)) as ymx,
           st_ymin(st_envelope(rast)) as ymn,
           st_width(rast) as cols,
           st_height(rast) as rows
    FROM \"%s\"
    WHERE ST_Intersects(ST_ConvexHull(rast), ST_GeomFromText('%s', 4326))
  " |> sprintf(name, wkt_str)
  
  info <- db_safe_query(qry)
  
  bandqs1 <- bands |>
    paste0(collapse = ",") |>
    sprintf(fmt ="UNNEST((ST_Dumpvalues(rast, ARRAY[%s], true)).valarray::real[][]) as vals")
  
  qry2 <- "
    select rid,
           %s
    from \"%s\"
    where rid IN (%s)
  " |> sprintf(bandqs1, name, paste(info$id, collapse = ", "))
  r_values <- db_safe_query(qry2)
  r_values <- split(r_values[,-1], r_values$rid)
  
  out_list <- lapply(names(r_values), \(tile) {
    info_tl <- info[info$id == tile,]
    rout <- terra::rast(
      nrows = info_tl$rows, ncols = info_tl$cols, xmin = info_tl$xmn,
      xmax = info_tl$xmx, ymin = info_tl$ymn, ymax = info_tl$ymx, nlyrs = length(bands),
      crs = paste0("EPSG:", 4326), vals = r_values[[tile]]
    )
  })
  
  rsrc <- terra::sprc(out_list)
  rast_res <- terra::merge(rsrc)
  return(rast_res)
}


#' Make raster from a boundary
#'
#' Used internally to access the PostGRS database and
#' create a `SpatRaster` using a given spatial boundary
#'
#' @template boundary
#' @template conn
#' @param rastque character. The "rast" query name obtained with e.g. `dbQuoteIdentifier(conn, "rast")`
#' @param nameque character. The "schema.name"
#' @param projID character. projID in data.base
#' @template bands
#'
#' @return a `SpatRaster`
#'
#' @importFrom DBI dbGetQuery
#' @importFrom terra rast
#' @noRd
make_raster <- function(boundary, conn, rastque, nameque, projID, bands) {
  cat(".")
  info <- db_safe_query(paste0(
    "select
              st_xmax(st_envelope(rast)) as xmx,
              st_xmin(st_envelope(rast)) as xmn,
              st_ymax(st_envelope(rast)) as ymx,
              st_ymin(st_envelope(rast)) as ymn,
              st_width(rast) as cols,
              st_height(rast) as rows
              from
              (select st_union(", rastque, ",", 1, ") rast from \"", nameque, "\" \n
              WHERE ST_Intersects(",
    rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
    " ", boundary[1], ",", boundary[4], " ", boundary[2],
    ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
    " ", boundary[1], ",", boundary[4], " ", boundary[1],
    "))'),", projID, "))) as a;"
  ))
  bandqs1 <- paste0("UNNEST(ST_Dumpvalues(rast, ", bands, ")) as vals_", bands)
  rast_vals <- db_safe_query(paste0(
    "SELECT ", paste(bandqs1, collapse = ","),
    " from (SELECT ST_Union(rast) rast FROM \"", nameque, "\" WHERE ST_Intersects(",
    rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
    " ", boundary[1], ",", boundary[4], " ", boundary[2],
    ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
    " ", boundary[1], ",", boundary[4], " ", boundary[1],
    "))'),", projID, "))) as a;"
  ))
  setDT(rast_vals)
  if (all(is.na(rast_vals[, 1]))) {
    warning("Empty tile - not enough data")
    return(NULL)
  } else {
    rast_vals <- suppressWarnings(melt(rast_vals, id.vars = c()))
    rout <- rast(
      nrows = info$rows, ncols = info$cols, xmin = info$xmn,
      xmax = info$xmx, ymin = info$ymn, ymax = info$ymx, nlyrs = length(bands),
      crs = paste0("EPSG:", projID), vals = rast_vals$value
    )
    rout
  }
}
