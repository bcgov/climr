
#' Retrieval of station data near points or in bounding box
#' 
#' @description
#' This function aims to facilitate the retrieval of station data. The function allows specification 
#' by area, dataset, date range, and completeness. For stations with complete observations (i.e., Tmin, Tmax, PPT),
#' the function calculates the standard suite of derived variables in climr. If `x` is a data.frame of point locations,
#' the function will return the `k` nearest stations to each input point. If `x` is a bounding box (numeric vector or 
#' SpatExtent), the function returns all stations withing the extent.
#' 
#' @param x Either a data.frame containing point locations with columns `id`, `lat`, `lon` OR a numeric vector of length four
#' representing a bounding box to select stations within. This can be created using `get_bb()` or it can be a SpatExtent
#' @param dataset Character. Which station dataset to use? Current options are `AHCCD` or `SCDNA`. Default "AHCCD"
#' @param k Integer. Number of stations to return for each point. Only used if `x` is a data.frame.
#' @param date_range Character vector of length 2. Date range to retrieve data for. Default is `c("2001-01-01", "2005-12-31")`.
#' @template vars
#' @param only_complete Logical. If TRUE, only stations with complete Tmin, Tmax, PPT are returned. Default `FALSE`.
#' 
#' @import data.table
#' @importFrom glue glue glue_collapse
#' 
#' @return data.table containing requested variables for stations near points or within bounding box.
#' If `x` is points, the distance (in meters) and rank of each station relative to each input point will also be returned.
#' 
#' @export
#' 
get_station_data <- function(x, dataset = "AHCCD", k = 1, date_range = c("2001-01-01", "2005-12-31"),
                             vars = sort(sprintf(c("PPT_%02d", "Tmax_%02d", "Tmin_%02d"), sort(rep(1:12, 3)))), 
                             only_complete = FALSE) {
  UseMethod("get_station_data",x)
}

#' @noRd
#' @export
get_station_data.data.frame <- function(xyz, dataset = "AHCCD", k = 1, date_range = c("2001-01-01", "2005-12-31"), ##only run haversine dist on stations within bb around points
                                    vars = sort(sprintf(c("PPT_%02d", "Tmax_%02d", "Tmin_%02d"), sort(rep(1:12, 3)))), 
                                    only_complete = FALSE) {
  if(only_complete){
    qry <- glue("SELECT DISTINCT id
            FROM station_data
            WHERE dataset = '{dataset}' 
            AND period BETWEEN DATE '{date_range[1]}' AND DATE '{date_range[2]}'
            GROUP BY id
            HAVING COUNT(*) = COUNT(tmin)
               AND COUNT(*) = COUNT(tmax)
               AND COUNT(*) = COUNT(ppt);")
    #complete_ids <- dbGetQuery(con,qry)[,1]
    complete_ids <- db_safe_query(qry)[,1]
    c_ids <- glue_collapse(complete_ids, sep = "','")
    #st_meta <- dbGetQuery(con, glue("SELECT id, lon, lat FROM station_meta WHERE dataset = '{dataset}' AND id IN ('{c_ids}')"))
    st_meta <- db_safe_query(glue("SELECT id, lon, lat FROM station_meta WHERE dataset = '{dataset}' AND id IN ('{c_ids}')"))
  } else {
    st_meta <- db_safe_query(glue("SELECT id, lon, lat FROM station_meta WHERE dataset = '{dataset}'"))
  }
  
  st_near <- nearest_haversine_k_cpp(xyz, st_meta, k = k)
  setDT(st_near)
  st_ids <- glue_collapse(unique(st_near$station_id),sep = "','")
  
  st_dat <- db_safe_query(glue("SELECT s.id, m.lat, year, month, tmin, tmax, ppt FROM 
                                        station_data s
                                        JOIN station_meta m USING (id)
                                        WHERE s.id IN ('{st_ids}') AND
                                        period BETWEEN DATE '{date_range[1]}' AND DATE '{date_range[2]}'"))
  setDT(st_dat)
  st_dat[, month := sprintf("%02d",month)]
  setnames(st_dat, old = c("year", "tmin","tmax","ppt"), new = c("PERIOD", "Tmin","Tmax", "PPT"))
  st_dat <- melt(st_dat, id.vars = c("id","PERIOD","month","lat"))
  st_wide <- dcast(st_dat, id + lat + PERIOD ~ variable + month)
  res <- append_clim_vars(copy(st_wide), vars = vars)
  setnames(res, old = "id", new = "station_id")
  out <- res[st_near, on = "station_id", allow.cartesian = TRUE]
  setcolorder(out, c("input_id","station_id","distance","rank","PERIOD"))
  return(out)
}

#' @noRd
#' @export
get_station_data.numeric <- function(bbox, dataset = "AHCCD", k = 1, date_range = c("2001-01-01", "2005-12-31"),
                                 vars = sort(sprintf(c("PPT_%02d", "Tmax_%02d", "Tmin_%02d"), sort(rep(1:12, 3)))), 
                                 only_complete = FALSE) {
  if(only_complete){
    st_dat <- db_safe_query(glue(
      "WITH filtered AS (
    SELECT
        s.id,
        m.lat,
        m.lon,
        s.year,
        s.month,
        s.tmin,
        s.tmax,
        s.ppt
    FROM station_data s
    JOIN station_meta m USING (dataset, id)
    WHERE dataset = '{dataset}'
      AND period
          BETWEEN DATE '{date_range[1]}' AND DATE '{date_range[2]}'
      AND m.lon > {bbox[1]} AND m.lon < {bbox[2]}
      AND m.lat > {bbox[3]} AND m.lat < {bbox[4]}
    ),
    complete_ids AS (
        SELECT id
        FROM filtered
        GROUP BY id
        HAVING COUNT(*) = COUNT(tmin)
           AND COUNT(*) = COUNT(tmax)
           AND COUNT(*) = COUNT(ppt)
    )
    SELECT f.id, f.lat, f.lon, f.year, f.month, f.tmin, f.tmax, f.ppt
    FROM filtered f
    JOIN complete_ids c USING (id)
    ORDER BY f.id, f.year, f.month;"))
  } else {
    st_dat <- db_safe_query(glue("
    SELECT
        s.id,
        m.lat,
        m.lon,
        s.year,
        s.month,
        s.tmin,
        s.tmax,
        s.ppt
    FROM station_data s
    JOIN station_meta m USING (id)
    WHERE dataset = '{dataset}'
      AND period BETWEEN DATE '{date_range[1]}' AND DATE '{date_range[2]}'
      AND m.lon > {bbox[1]} AND m.lon < {bbox[2]}
      AND m.lat > {bbox[3]} AND m.lat < {bbox[4]}"))
  }
  
  setDT(st_dat)
  
  st_dat[, month := sprintf("%02d",month)]
  setnames(st_dat, old = c("year", "tmin","tmax","ppt"), new = c("PERIOD", "Tmin","Tmax", "PPT"))
  st_dat <- melt(st_dat, id.vars = c("id","PERIOD","month","lat","lon"))
  st_wide <- dcast(st_dat, id + lat + lon + PERIOD ~ variable + month)
  res <- append_clim_vars(copy(st_wide), vars = vars)
  setnames(res, old = "id", new = "station_id")
  return(res)
}

#' Retrieve station metadata for given stations
#' 
#' @param station_ids character vector of station ids
#' 
#' @returns data.table containing lon, lat, elev, dataset, and province for each requested station
#' 
#' @importFrom data.table setDT
#' @importFrom glue glue_collapse glue
#' @export
get_station_metadata <- function(station_ids) {
  ids <- glue_collapse(unique(station_ids), sep = "','")
  dat <- db_safe_query(glue("SELECT * FROM station_meta WHERE id IN ('{ids}')"))
  setDT(dat)
  return(dat)
}

