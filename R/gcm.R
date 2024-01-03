#' Create GCM inputs for `downscale` using data on Postgis database.
#' 
#' @return A `list` of SpatRasters, each with possibly multiple layers, that can
#'   be used with `downscale`.
#' 
#' @description
#' `gcm_input` creates GCM climate periods inputs, given chosen GCMs, SSPs, 
#'  periods and runs.
#' 
#' @template dbCon
#' @template bbox
#' @template gcm
#' @template ssp
#' @template period
#' @template max_run
#' @template cache
#' 
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @importFrom uuid UUIDgenerate
#' @import data.table
#' @rdname gcm-input-data
#' @export
gcm_input <- function(dbCon, bbox = NULL, gcm = list_gcm(), ssp = list_ssp(), period = list_gcm_period(), max_run = 0L, cache = TRUE) {
  ## checks
  gcm <- match.arg(gcm, list_gcm(), several.ok = TRUE)
  ssp <- match.arg(ssp, list_ssp(), several.ok = TRUE)
  period <- match.arg(period, list_gcm_period(), several.ok = TRUE)
  
  if (!is(max_run, "numeric")) {
    stop("please pass a numeric value to 'max_runs'")
  }
  
  if (!is(cache, "logical")) {
    stop("please pass a logical value to 'cache'")
  }
  
  # Load each file individually + select layers
  res <- sapply(gcm, process_one_gcm2, ssp = ssp, period = period,
                bbox = bbox, dbnames = dbnames, dbCon = dbCon, 
                max_run = max_run, cache = cache, USE.NAMES = TRUE, simplify = FALSE)
  attr(res, "builder") <- "climr"
  
  # Return a list of SpatRaster, one element for each model
  return(res)
}


#' @description
#' `gcm_hist_input` creates GCM **historic** time series inputs, given chosen GCMs, SSPs, 
#'  years and runs.
#'   
#' @template dbCon
#' @template bbox
#' @template gcm
#' @param years numeric. Vector of desired years. Must be in `1851:2015`.
#'   Can be obtained from `list_gcm_period()`. Default to `list_gcm_period()`.
#' @template max_run
#' @template cache
#' 
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @import uuid
#' @import data.table
#' @rdname gcm-input-data
#' @export
gcm_hist_input <- function(dbCon, bbox = NULL, gcm = list_gcm(), years = 1901:1950, max_run = 0L, cache = TRUE) {
  # Load each file individually + select layers
  res <- sapply(gcm, process_one_gcm3, years = years,
                dbCon = dbCon, bbox = bbox, dbnames = dbnames_hist, 
                max_run = max_run, cache = cache, USE.NAMES = TRUE, simplify = FALSE)
  attr(res, "builder") <- "climr"
  
  # Return a list of SpatRaster, one element for each model
  return(res)
}



# gcm_nm <- "ACCESS-ESM1-5"
# ssp <- c("ssp245","ssp370")
# period <- 2020:2050

#' @description
#' `gcm_ts_input` creates GCM time series inputs, given chosen GCMs, SSPs, 
#'  years and runs.
#' 
#' @template dbCon
#' @template bbox
#' @template gcm
#' @template ssp
#' @param years Numeric or character vector in `2020:2100`. Defaults to `2020:2030`.
#' @template max_run
#' @template cache
#' 
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @import uuid
#' @import data.table
#' @rdname gcm-input-data
#' @export
gcm_ts_input <- function(dbCon, bbox = NULL, gcm = list_gcm(), ssp = list_ssp(), years = 2020:2030, max_run = 0L, cache = TRUE) {
  if (nrow(dbnames_ts) < 1) stop("That isn't a valid GCM")
  # Load each file individually + select layers
  res <- sapply(gcm, process_one_gcm4, ssp = ssp, period = years,
                dbnames = dbnames_ts, bbox = bbox, dbCon = dbCon, 
                max_run = max_run, cache = cache, USE.NAMES = TRUE, simplify = FALSE)
  attr(res, "builder") <- "climr"
  
  # Return a list of SpatRaster, one element for each model
  return(res)
}


#' Read and parse gcm models csv files
#' 
#' @param files character. A vector of file paths.
#' @param col_num integer. Vector of indices of elements to retrieve in label. Label is split
#'   by "_" before processing.
#'   
#' @return A character vector of unique values.
#' 
#' @importFrom data.table fread
list_unique <- function(files, col_num) {
  collection <- character()
  for (file in files) {
    # Read in csv file with headers
    values <- fread(file, header = TRUE)
    # Remove reference lines
    values <- values[which(!grepl("_reference_", values[["x"]], fixed = TRUE)),]
    # Split and extract sub part of x according to col_num
    values <- vapply(strsplit(values[["x"]], "_"), `[`, character(length(col_num)), col_num)
    # In case we have more than one col_num, put them back together
    if (length(col_num) > 1L) {
      values <- apply(values, 2, paste0, collapse = "_")
    }
    # Reassign collection to unique values
    collection <- unique(c(values, collection))
  }
  # Sort and return
  return(sort(collection))
}

#' Process one GCM at a time
#' 
#' @template gcm_nm 
#' @template ssp 
#' @template bbox
#' @template period
#' @template max_run
#' @param dbnames `data.frame` with the list of available GCMs and their 
#'   corresponding names in the PostGIS data base.  See climr:::dbnames
#' @template dbCon
#' @template cache
#'
#' @return SpatRaster
process_one_gcm2 <- function(gcm_nm, ssp, bbox, period, max_run, dbnames = dbnames, dbCon, cache) { ## need to update to all GCMs
  gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
  gcm_nm <- gsub("-", ".", gcm_nm)
  
  rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")
  
  runs <- fread(file.path(rInfoPath, "gcm_period.csv"))
  runs <- sort(unique(runs[mod == gcm_nm & scenario %in% ssp, run]))
  sel_runs <- runs[1:(max_run + 1L)]
  
  if (dir.exists(paste0(cache_path(), "/gcm/", gcmcode))) {
    bnds <- fread(paste0(cache_path(), "/gcm/", gcmcode, "/meta_area.csv"))
    setorder(bnds, -numlay)
    for (i in 1:nrow(bnds)) {
      isin <- is_in_bbox(bbox, matrix(bnds[i, 2:5]))
      if (isin) break
    }
    if (isin) {
      oldid <- bnds$uid[i]
      periods <- fread(paste0(cache_path(), "/gcm/", gcmcode, "/meta_period.csv"))
      ssps <- fread(paste0(cache_path(), "/gcm/", gcmcode, "/meta_ssp.csv"))
      if (all(period %in% periods[uid == oldid, period]) & all(ssp %in% ssps[uid == oldid, ssp]) & max_run <= bnds[uid == oldid, max_run]) {
        message("Retrieving from cache...")
        gcm_rast <- rast(paste0(cache_path(), "/gcm/", gcmcode, "/", oldid, ".tif"))
        layinfo <- data.table(fullnm = names(gcm_rast))
        layinfo[, c("Mod", "Var", "Month", "Scenario", "Run", "Period1", "Period2") := tstrsplit(fullnm, "_")]
        layinfo[, Period := paste(Period1, Period2, sep = "_")]
        layinfo[, laynum := seq_along(fullnm)]
        sel <- layinfo[Scenario %in% ssp & Period %in% period & Run %in% sel_runs, laynum]
        return(gcm_rast[[sel]])
      } else {
        message("Not fully cached :( Will download more")
      }
    }
  }
  
  
  q <- paste0(
    "select fullnm, laynum from esm_layers where mod = '", gcm_nm, "' and scenario in ('", paste(ssp, collapse = "','"),
    "') and period in ('", paste(period, collapse = "','"), "') and run in ('", paste(sel_runs, collapse = "','"), "')"
  )
  # print(q)
  layerinfo <- dbGetQuery(dbCon, q)
  message("Downloading GCM anomalies")
  gcm_rast <- pgGetTerra(dbCon, gcmcode, tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
  names(gcm_rast) <- layerinfo$fullnm
  
  if (cache) {
    message("Caching data...")
    uid <- UUIDgenerate()
    if (!dir.exists(paste0(cache_path(), "/gcm/", gcmcode))) dir.create(paste0(cache_path(), "/gcm/", gcmcode), recursive = TRUE)
    writeRaster(gcm_rast, paste0(cache_path(), "/gcm/", gcmcode, "/", uid, ".tif"))
    rastext <- ext(gcm_rast)
    t1 <- data.table(
      uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1],
      numlay = nlyr(gcm_rast), max_run = max_run
    )
    t2 <- data.table(uid = rep(uid, length(period)), period = period)
    t3 <- data.table(uid = rep(uid, length(ssp)), ssp = ssp)
    fwrite(t1, file = paste0(cache_path(), "/gcm/", gcmcode, "/meta_area.csv"), append = TRUE)
    fwrite(t2, file = paste0(cache_path(), "/gcm/", gcmcode, "/meta_period.csv"), append = TRUE)
    fwrite(t3, file = paste0(cache_path(), "/gcm/", gcmcode, "/meta_ssp.csv"), append = TRUE)
  }
  
  return(gcm_rast)
}

#' Process one historic time series at a time
#'
#' @template gcm_nm 
#' @param years numeric. Vector of desired years. Must be in `1851:2015`.
#'   Can be obtained from `list_gcm_period()`. Default to `list_gcm_period()`.
#' @template dbCon 
#' @template bbox 
#' @template max_run
#' @param dbnames `data.frame` with the list of available GCMs (historical projections) 
#'   and their corresponding names in the PostGIS data base. See climr:::dbnames_hist
#' @template cache 
#'
#' @return SpatRaster
process_one_gcm3 <- function(gcm_nm, years, dbCon, bbox, max_run, dbnames = dbnames_hist, cache) { ## need to update to all GCMs
  gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
  
  rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")
  
  runs <- fread(file.path(rInfoPath, "gcm_hist.csv"))
  runs <- sort(unique(runs[mod == gcm_nm, run]))
  sel_runs <- runs[1:(max_run + 1L)]
  
  if (dir.exists(paste0(cache_path(), "/gcmhist/", gcmcode))) {
    bnds <- fread(paste0(cache_path(), "/gcmhist/", gcmcode, "/meta_area.csv"))
    setorder(bnds, -numlay)
    for (i in 1:nrow(bnds)) {
      isin <- is_in_bbox(bbox, matrix(bnds[i, 2:5]))
      if (isin) break
    }
    if (isin) {
      oldid <- bnds$uid[i]
      yeardat <- fread(paste0(cache_path(), "/gcmhist/", gcmcode, "/meta_years.csv"))
      if (all(years %in% yeardat[uid == oldid, years]) & max_run <= bnds[uid == oldid, max_run]) {
        message("Retrieving from cache...")
        gcm_rast <- rast(paste0(cache_path(), "/gcmhist/", gcmcode, "/", oldid, ".tif"))
        layinfo <- data.table(fullnm = names(gcm_rast))
        layinfo[, c("Mod", "Var", "Month", "Run", "Year") := tstrsplit(fullnm, "_")]
        layinfo[, laynum := seq_along(Year)]
        sel <- layinfo[Year %in% years & Run %in% sel_runs, laynum]
        return(gcm_rast[[sel]])
      } else {
        message("Not fully cached :( Will download more")
      }
    }
  }
  
  q <- paste0("select mod, var, month, run, year, laynum from esm_layers_hist where mod = '", gcm_nm, "' and year in ('", paste(years, collapse = "','"), "') and run in ('", paste(sel_runs, collapse = "','"), "')")
  # print(q)
  layerinfo <- as.data.table(dbGetQuery(dbCon, q))
  layerinfo[, fullnm := paste(mod, var, month, run, year, sep = "_")]
  message("Downloading GCM anomalies")
  gcm_rast <- pgGetTerra(dbCon, gcmcode, tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
  names(gcm_rast) <- layerinfo$fullnm
  
  if (cache) {
    message("Caching data...")
    uid <- UUIDgenerate()
    if (!dir.exists(paste0(cache_path(), "/gcmhist/", gcmcode))) dir.create(paste0(cache_path(), "/gcmhist/", gcmcode), recursive = TRUE)
    writeRaster(gcm_rast, paste0(cache_path(), "/gcmhist/", gcmcode, "/", uid, ".tif"))
    rastext <- ext(gcm_rast)
    t1 <- data.table(
      uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1],
      numlay = nlyr(gcm_rast), max_run = max_run
    )
    t2 <- data.table(uid = rep(uid, length(years)), years = years)
    fwrite(t1, file = paste0(cache_path(), "/gcmhist/", gcmcode, "/meta_area.csv"), append = TRUE)
    fwrite(t2, file = paste0(cache_path(), "/gcmhist/", gcmcode, "/meta_years.csv"), append = TRUE)
  }
  
  return(gcm_rast)
}

#' Process one GCM time series at a time
#'
#' @template gcm_nm
#' @template ssp 
#' @template period 
#' @template max_run
#' @param dbnames `data.frame` with the list of available GCMs (time series projections)
#'   and their corresponding names in the PostGIS data base. See climr:::dbnames_ts
#' @template bbox 
#' @template dbCon 
#' @template cache 
#'
#' @return a SpatRaster
process_one_gcm4 <- function(gcm_nm, ssp, period, max_run, dbnames = dbnames_ts, bbox, dbCon, cache) { ## need to update to all GCMs
  gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
  
  rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")
  
  runs <- fread(file.path(rInfoPath, "gcm_ts.csv"))
  runs <- sort(unique(runs[mod == gcm_nm & scenario %in% ssp, run]))
  if (length(runs) < 1) stop("That GCM isn't in our database yet.")
  sel_runs <- runs[1:(max_run + 1L)]
  
  if (dir.exists(paste0(cache_path(), "/gcmts/", gcmcode))) {
    bnds <- fread(paste0(cache_path(), "/gcmts/", gcmcode, "/meta_area.csv"))
    setorder(bnds, -numlay)
    for (i in 1:nrow(bnds)) {
      isin <- is_in_bbox(bbox, matrix(bnds[i, 2:5]))
      if (isin) break
    }
    if (isin) {
      oldid <- bnds$uid[i]
      periods <- fread(paste0(cache_path(), "/gcmts/", gcmcode, "/meta_period.csv"))
      ssps <- fread(paste0(cache_path(), "/gcmts/", gcmcode, "/meta_ssp.csv"))
      if (all(period %in% periods[uid == oldid, period]) & all(ssp %in% ssps[uid == oldid, ssp]) & max_run <= bnds[uid == oldid, max_run]) {
        message("Retrieving from cache...")
        gcm_rast <- rast(paste0(cache_path(), "/gcmts/", gcmcode, "/", oldid, ".tif"))
        layinfo <- data.table(fullnm = names(gcm_rast))
        layinfo[, c("Mod", "Var", "Month", "Scenario", "Run", "Year") := tstrsplit(fullnm, "_")]
        layinfo[, laynum := seq_along(fullnm)]
        sel <- layinfo[Scenario %in% ssp & Year %in% period & Run %in% sel_runs, laynum]
        return(gcm_rast[[sel]])
      } else {
        message("Not fully cached :( Will download more")
      }
    }
  }
  
  q <- paste0(
    "select fullnm, laynum from esm_layers_ts where mod = '", gcm_nm, "' and scenario in ('", paste(ssp, collapse = "','"),
    "') and period in ('", paste(period, collapse = "','"), "') and run in ('", paste(sel_runs, collapse = "','"), "')"
  )
  # print(q)
  layerinfo <- dbGetQuery(dbCon, q)
  message("Downloading GCM anomalies")
  message("Precip...")
  gcm_rast_ppt <- pgGetTerra(dbCon, paste0(gcmcode, "_ppt"), tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
  names(gcm_rast_ppt) <- gsub("Tmax", "PPT", layerinfo$fullnm)
  message("Tmax...")
  gcm_rast_tmax <- pgGetTerra(dbCon, paste0(gcmcode, "_tmax"), tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
  names(gcm_rast_tmax) <- layerinfo$fullnm
  message("Tmin...")
  gcm_rast_tmin <- pgGetTerra(dbCon, paste0(gcmcode, "_tmin"), tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
  names(gcm_rast_tmin) <- gsub("Tmax", "Tmin", layerinfo$fullnm)
  gcm_rast <- c(gcm_rast_ppt, gcm_rast_tmax, gcm_rast_tmin)
  
  if (cache) {
    message("Caching data...")
    uid <- UUIDgenerate()
    if (!dir.exists(paste0(cache_path(), "/gcmts/", gcmcode))) dir.create(paste0(cache_path(), "/gcmts/", gcmcode), recursive = TRUE)
    writeRaster(gcm_rast, paste0(cache_path(), "/gcmts/", gcmcode, "/", uid, ".tif"))
    rastext <- ext(gcm_rast)
    t1 <- data.table(
      uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1],
      numlay = nlyr(gcm_rast), max_run = max_run
    )
    t2 <- data.table(uid = rep(uid, length(period)), period = period)
    t3 <- data.table(uid = rep(uid, length(ssp)), ssp = ssp)
    fwrite(t1, file = paste0(cache_path(), "/gcmts/", gcmcode, "/meta_area.csv"), append = TRUE)
    fwrite(t2, file = paste0(cache_path(), "/gcmts/", gcmcode, "/meta_period.csv"), append = TRUE)
    fwrite(t3, file = paste0(cache_path(), "/gcmts/", gcmcode, "/meta_ssp.csv"), append = TRUE)
  }
  
  return(gcm_rast)
}