#' Create gcm input for `downscale` using data on Postgis database.
#' @template dbCon
#' @template bbox
#' @template gcm
#' @template ssp
#' @template period
#' @template max_run
#' @template cache
#' @return An object to use with `downscale`. A list of `SpatRaster` with, possibly, multiple layers.
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @importFrom uuid UUIDgenerate
#' @import data.table
#' @export
gcm_input <- function(dbCon, bbox = NULL, gcm = list_gcm(), ssp = list_ssp(), period = list_gcm_period(), max_run = 0L, cache = TRUE) {
  # Load each file individually + select layers
  res <- lapply(gcm, process_one_gcm2, ssp = ssp, period = period,
                bbox = bbox, dbnames = dbnames, dbCon = dbCon, 
                max_run = max_run, cache = cache)
  attr(res, "builder") <- "climr"
  
  # Return a list of SpatRaster, one element for each model
  return(res)
}


#' Create gcm historic timeseries input for `downscale`
#' @template dbCon
#' @template bbox
#' @template gcm
#' @param years Numeric vector of desired years. Must be in `1851:2015`.
#'   Can be obtained from `list_gcm_period()`. Default to `list_gcm_period()`.
#' @template max_run
#' @template cache
#' @return An object to use with `downscale`. A list of `SpatRaster` with, possibly, multiple layers.
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @import uuid
#' @import data.table
#' @export
gcm_hist_input <- function(dbCon, bbox = NULL, gcm = list_gcm(), years = 1901:1950, max_run = 0L, cache = TRUE) {
  # Load each file individually + select layers
  res <- lapply(gcm, process_one_gcm3, years = years,
                dbCon = dbCon, bbox = bbox, dbnames = dbnames_hist, 
                max_run = max_run, cache = cache)
  attr(res, "builder") <- "climr"
  
  # Return a list of SpatRaster, one element for each model
  return(res)
}



# gcm_nm <- "ACCESS-ESM1-5"
# ssp <- c("ssp245","ssp370")
# period <- 2020:2050

#' Create gcm timeseries input for `downscale` using data on Postgis database.
#' 
#' @template dbCon
#' @template bbox
#' @template gcm
#' @template ssp
#' @param years Numeric or character vector in `2020:2100`. Defaults to `2020:2030`.
#' @template max_run
#' @template cache
#' 
#' @return An object to use with `downscale`. A list of `SpatRaster` with, possibly, multiple layers.
#' 
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @import uuid
#' @import data.table
#' @export
gcm_ts_input <- function(dbCon, bbox = NULL, gcm = list_gcm(), ssp = list_ssp(), years = 2020:2030, max_run = 0L, cache = TRUE) {
  if (nrow(dbnames_ts) < 1) stop("That isn't a valid GCM")
  # Load each file individually + select layers
  res <- lapply(gcm, process_one_gcm4, ssp = ssp, period = years,
                dbnames = dbnames_ts, bbox = bbox, dbCon = dbCon, 
                max_run = max_run, cache = cache)
  attr(res, "builder") <- "climr"
  
  # Return a list of SpatRaster, one element for each model
  return(res)
}


#' Read and parse gcm models csv files
#' @param files A character vector. File paths.
#' @param col_num An integer vector. Positions of elements to retrieve in label. Label is split
#'   by "_" before processing.
#' @return A character vector of unique values.
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

#' Read and parse gcm models csv files
#' @param gcm An optional character vector. Limit list to provided global circulation models.
#' @param col_num An integer vector.
#' @return A character vector of unique values.
list_parse <- function(gcm, col_num = 1) {
  # Default pattern csv extension
  pattern <- "\\.csv$"
  
  # In case we need to filter gcm
  if (!missing(gcm)) {
    pattern <- paste0("(", paste0(gcm, collapse = "|"), ").*", pattern)
  }
  files <- list.files(
    file.path(
      data_path(),
      getOption("climr.gcm.path", default = "inputs_pkg/gcm")
    ),
    recursive = TRUE,
    full.names = TRUE,
    pattern = pattern
  )
  
  # Extract all different unique values
  list_unique(files, col_num)
}

#' List available global circulation models
#' @export
list_gcm <- function() {
  c(
    "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "EC-Earth3", "GISS-E2-1-G",
    "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0"
  )
  # sort(dbGetQuery(dbCon, "SELECT DISTINCT mod FROM esm_layers_ts")[,1])
}

#' List available shared socioeconomic pathways
#' @export
list_ssp <- function() {
  # sort(dbGetQuery(dbCon, "SELECT DISTINCT scenario FROM esm_layers")[,1])
  c("ssp126", "ssp245", "ssp370", "ssp585")
}

#' List available period
#' @export
list_gcm_period <- function() {
  # sort(dbGetQuery(dbCon, "SELECT DISTINCT period FROM esm_layers")[,1])
  c("2001_2020", "2021_2040", "2041_2060", "2061_2080", "2081_2100")
}

#' List available runs for given GCM
#' @template dbCon
#' @param gcm Character vector to specify requested GCMs
#' @importFrom RPostgres dbGetQuery
#' @export
list_run <- function(dbCon, gcm) {
  sort(dbGetQuery(dbCon, paste0("SELECT DISTINCT run FROM esm_layers WHERE mod IN ('", paste(gcm, collapse = "','", "')")))[, 1])
}


#' TODO: add documentation here
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
#' @return Spat Raster
process_one_gcm2 <- function(gcm_nm, ssp, bbox, period, max_run, dbnames = dbnames, dbCon, cache) { ## need to update to all GCMs
  gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
  gcm_nm <- gsub("-", ".", gcm_nm)
  
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
        runs <- sort(dbGetQuery(dbCon, paste0("select distinct run from esm_layers where mod = '", gcm_nm, "'"))$run)
        sel_runs <- runs[1:(max_run + 1L)]
        
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
  
  runs <- sort(dbGetQuery(dbCon, paste0("select distinct run from esm_layers where mod = '", gcm_nm, "'"))$run)
  sel_runs <- runs[1:(max_run + 1L)]
  
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

#' TODO: add documentation here
#'
#' @template gcm_nm 
#' @param years Numeric vector of desired years. Must be in `1851:2015`.
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
        runs <- sort(dbGetQuery(dbCon, paste0("select distinct run from esm_layers_hist where mod = '", gcm_nm, "'"))$run)
        sel_runs <- runs[1:(max_run + 1L)]
        
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
  
  runs <- sort(dbGetQuery(dbCon, paste0("select distinct run from esm_layers_hist where mod = '", gcm_nm, "'"))$run)
  sel_runs <- runs[1:(max_run + 1L)]
  
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

#' TODO: add documentation here
#'
#' @template gcm_nm
#' @template ssp 
#' @template period 
#' @template max_run
#' @param dbnames #' @param dbnames `data.frame` with the list of available GCMs (time series)
#'   projections) and their corresponding names in the PostGIS data base. See climr:::dbnames_ts
#' @template bbox 
#' @template dbCon 
#' @template cache 
#'
#' @return
process_one_gcm4 <- function(gcm_nm, ssp, period, max_run, dbnames = dbnames_ts, bbox, dbCon, cache) { ## need to update to all GCMs
  gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
  
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
        runs <- sort(dbGetQuery(dbCon, paste0("select distinct run from esm_layers_ts where mod = '", gcm_nm, "'"))$run)
        sel_runs <- runs[1:(max_run + 1L)]
        
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
  
  runs <- sort(dbGetQuery(dbCon, paste0("select distinct run from esm_layers_ts where mod = '", gcm_nm, "'"))$run)
  if (length(runs) < 1) stop("That GCM isn't in our database yet.")
  sel_runs <- runs[1:(max_run + 1L)]
  
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