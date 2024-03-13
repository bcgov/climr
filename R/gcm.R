#' Retrieve GCM anomalies for `downscale`.
#'
#' @return A `list` of `SpatRasters`, each with possibly multiple layers, that can
#'   be used with [`downscale()`].
#'
#' @description
#' `gcm_input` retrieves anomalies for GCM data, given chosen GCMs, SSPs,
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
#' @details
#' This function returns a list with one slot for each requested GCM. Rasters inside the list contain anomalies for all requested SSPs, runs, and periods.
#' In general this function should only be used in combination with [`downscale()`].
#'
#'
#' @seealso [downscale()]
#'
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @importFrom uuid UUIDgenerate
#' @import data.table
#'
#' @examples
#' library(terra)
#' xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10), id = 1:10)
#'
#' ## get bounding box based on input points
#' thebb <- get_bb(xyz)
#'
#' ## get database connection
#' dbCon <- data_connect()
#' on.exit(try(pool::poolClose(dbCon)))
#'
#' gcm <- gcm_input(dbCon, thebb, list_gcm()[1], list_ssp()[1])
#'
#' ## show ensemble means only
#' lyrs <- grep("ensemble", names(gcm$`ACCESS-ESM1-5`))
#'
#' plot(gcm$`ACCESS-ESM1-5`[[lyrs]])
#'
#' @rdname gcm-input-data
#' @export
gcm_input <- function(dbCon, bbox = NULL, gcm = list_gcm(), ssp = list_ssp(), period = list_gcm_period(), max_run = 0L, cache = TRUE) {
  ## checks
  if (!is.null(bbox)) {
    .check_bb(bbox)
  }
  
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
  res <- sapply(gcm, process_one_gcm2,
                ssp = ssp, period = period,
                bbox = bbox, dbnames = dbnames, dbCon = dbCon,
                max_run = max_run, cache = cache, USE.NAMES = TRUE, simplify = FALSE
  )
  attr(res, "builder") <- "climr"
  
  # Return a list of SpatRasters, one element for each model
  return(res)
}


#' @description
#' `gcm_hist_input` creates GCM **historic** time series inputs, given chosen GCMs,
#'  years and runs.
#'
#' @template dbCon
#' @template bbox
#' @template gcm
#' @param years numeric. Vector of desired years. Default is `1901:1950`.
#'   See [`list_gcm_hist_ts()`] for available years.
#' @template max_run
#' @template cache
#'
#' @seealso [list_gcm_period()], [`list_gcm_period()`]
#'
#' @return A `list` of `SpatRasters`, each with possibly multiple layers, that can
#'   be used with [`downscale()`].
#'
#' @details This function returns a list with one slot for each requested GCM. Rasters inside the list contain anomalies for all runs and years.
#' In general this function should only be used in combination with [`downscale()`].
#'
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @import uuid
#' @import data.table
#'
#' @examples {
#'   library(terra)
#'   xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10), id = 1:10)
#'
#'   ## get bounding box based on input points
#'   thebb <- get_bb(xyz)
#'
#'   ## get database connection
#'   dbCon <- data_connect()
#'   on.exit(try(pool::poolClose(dbCon)))
#'
#'   gcm_hist <- gcm_hist_input(dbCon, thebb, list_gcm()[1])
#'
#'   ## show ensemble means only
#'   lyrs <- grep("ensemble", names(gcm_hist$`ACCESS-ESM1-5`))
#'
#'   plot(gcm_hist$`ACCESS-ESM1-5`[[lyrs]])
#' }
#'
#' @rdname gcm-input-data
#' @export
gcm_hist_input <- function(dbCon, bbox = NULL, gcm = list_gcm(),
                           years = 1901:1950, max_run = 0L, cache = TRUE) {
  ## checks
  if (!is.null(bbox)) {
    .check_bb(bbox)
  }
  
  # Load each file individually + select layers
  res <- sapply(gcm, process_one_gcm3,
                years = years,
                dbCon = dbCon, bbox = bbox, dbnames = dbnames_hist,
                max_run = max_run, cache = cache, USE.NAMES = TRUE, simplify = FALSE
  )
  res <- res[!sapply(res, is.null)] ##remove NULL
  attr(res, "builder") <- "climr"
  
  # Return a list of SpatRasters, one element for each model
  return(res)
}



# gcm_nm <- "ACCESS-ESM1-5"
# ssp <- c("ssp245","ssp370")
# period <- 2020:2050

#' @description
#' `gcm_ts_input` creates future GCM time series inputs, given chosen GCMs, SSPs,
#'  years and runs.
#'
#' @template dbCon
#' @template bbox
#' @template gcm
#' @template ssp
#' @param years Numeric or character vector in `2020:2100`. Defaults to `2020:2030`.
#'   See [`list_gcm_ts()`] for available years.
#' @template max_run
#' @template cache
#'
#' @return A `list` of `SpatRasters`, each with possibly multiple layers, that can
#'   be used with [`downscale()`].
#'
#' @details This function returns a list with one slot for each requested GCM. Rasters inside the list contain anomalies for all SSPs, runs and years.
#' In general this function should only be used in combination with [`downscale()`]. Note that if you request multiple runs, multiple SSPs, and a lot of years,
#' it will take a while to download the data (there's lot of it).
#'
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @import uuid
#' @import data.table
#'
#' @examples
#' library(terra)
#' xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10), id = 1:10)
#'
#' ## get bounding box based on input points
#' thebb <- get_bb(xyz)
#'
#' ## get database connection
#' dbCon <- data_connect()
#' on.exit(try(pool::poolClose(dbCon)))
#'
#' gcm_ts <- gcm_ts_input(dbCon, thebb, list_gcm()[1], list_ssp()[1])
#'
#' ## show ensemble means only
#' lyrs <- grep("ensemble", names(gcm_ts$`ACCESS-ESM1-5`))
#'
#' plot(gcm_ts$`ACCESS-ESM1-5`[[lyrs]])
#'
#' @rdname gcm-input-data
#' @export
gcm_ts_input <- function(dbCon, bbox = NULL, gcm = list_gcm(), ssp = list_ssp(),
                         years = 2020:2030, max_run = 0L, cache = TRUE) {
  ## checks
  if (!is.null(bbox)) {
    .check_bb(bbox)
  }
  
  if (nrow(dbnames_ts) < 1) stop("That isn't a valid GCM")
  # Load each file individually + select layers
  res <- sapply(gcm, process_one_gcm4,
                ssp = ssp, period = years,
                dbnames = dbnames_ts, bbox = bbox, dbCon = dbCon,
                max_run = max_run, cache = cache, USE.NAMES = TRUE, simplify = FALSE
  )
  res <- res[!sapply(res, is.null)] ##remove NULL
  attr(res, "builder") <- "climr"
  
  # Return a list of SpatRasters, one element for each model
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
#' @noRd
list_unique <- function(files, col_num) {
  collection <- character()
  for (file in files) {
    # Read in csv file with headers
    values <- fread(file, header = TRUE)
    # Remove reference lines
    values <- values[which(!grepl("_reference_", values[["x"]], fixed = TRUE)), ]
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
#' @importFrom tools R_user_dir
#' @importFrom data.table fread
#'
#' @return `SpatRaster`
#' @noRd
process_one_gcm2 <- function(gcm_nm, ssp, bbox, period, max_run, dbnames = dbnames, dbCon, cache) { ## need to update to all GCMs
  gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
  # gcm_nm <- gsub("-", ".", gcm_nm)
  
  rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")
  
  runs <- fread(file.path(rInfoPath, "gcm_period.csv"))
  runs <- sort(unique(runs[mod == gcm_nm & scenario %in% ssp, run]))
  sel_runs <- runs[1:(max_run + 1L)]
  
  ## check cached
  needDownload <- TRUE
  
  cPath <- file.path(cache_path(), "gcm", gcmcode)
  
  if (dir.exists(cPath)) {
    bnds <- try(fread(file.path(cPath, "meta_area.csv")), silent = TRUE)
    
    if (is(bnds, "try-error")) {
      ## try to get the data again
      message(
        "Metadata file no longer exists or is unreadable.",
        " Downloading the data again"
      )
    } else {
      needDownload <- FALSE
    }
  }
  
  if (!needDownload) {
    setorder(bnds, -numlay)
    
    spat_match <- lapply(1:nrow(bnds), FUN = \(x){
      if (is_in_bbox(bbox, matrix(bnds[x, 2:5]))) bnds$uid[x]
    })
    spat_match <- spat_match[!sapply(spat_match, is.null)]
    
    if (length(spat_match) > 0) {
      periods <- fread(file.path(cPath, "meta_period.csv"))
      ssps <- fread(file.path(cPath, "meta_ssp.csv"))
      isin <- FALSE
      for (oldid in spat_match) {
        if (all(period %in% periods[uid == oldid, period]) &
            all(ssp %in% ssps[uid == oldid, ssp]) &
            max_run <= bnds[uid == oldid, max_run]) {
          isin <- TRUE
          break
        }
      }
      
      if (isin) {
        message("Retrieving from cache...")
        gcm_rast <- rast(file.path(cPath, paste0(oldid, ".tif")))
        layinfo <- data.table(fullnm = names(gcm_rast))
        layinfo[, c("Mod", "Var", "Month", "Scenario", "Run", "Period1", "Period2") := tstrsplit(fullnm, "_")]
        layinfo[, Period := paste(Period1, Period2, sep = "_")]
        layinfo[, laynum := seq_along(fullnm)]
        sel <- layinfo[Scenario %in% ssp & Period %in% period & Run %in% sel_runs, laynum]
        gcm_rast <- gcm_rast[[sel]]
      } else {
        message("Not fully cached :( Will download more")
        needDownload <- TRUE
      }
    } else {
      message("Not fully cached :( Will download more")
      needDownload <- TRUE
    }
  }
  
  if (needDownload) {
    q <- paste0(
      "select * from esm_layers_period where mod = '", gcm_nm, "' and scenario in ('", paste(ssp, collapse = "','"),
      "') and period in ('", paste(period, collapse = "','"), "') and run in ('", paste(sel_runs, collapse = "','"), "')"
    )
    # print(q)
    layerinfo <- as.data.table(dbGetQuery(dbCon, q))
    message("Downloading GCM anomalies")
    gcm_rast <- pgGetTerra(dbCon, gcmcode, tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
    layerinfo[,fullnm := paste(mod,var,month,scenario,run,period,sep = "_")]
    names(gcm_rast) <- layerinfo$fullnm
    
    if (cache) {
      message("Caching data...")
      uid <- UUIDgenerate()
      dir.create(cPath, recursive = TRUE, showWarnings = FALSE)
      
      writeRaster(gcm_rast, file.path(cPath, paste0(uid, ".tif")))
      rastext <- ext(gcm_rast)
      t1 <- data.table(
        uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1],
        numlay = nlyr(gcm_rast), max_run = max_run
      )
      t2 <- data.table(uid = rep(uid, length(period)), period = period)
      t3 <- data.table(uid = rep(uid, length(ssp)), ssp = ssp)
      fwrite(t1, file = file.path(cPath, "meta_area.csv"), append = TRUE)
      fwrite(t2, file = file.path(cPath, "meta_period.csv"), append = TRUE)
      fwrite(t3, file = file.path(cPath, "meta_ssp.csv"), append = TRUE)
    }
  }
  
  return(gcm_rast)
}

#' Process one historic time series at a time
#'
#' @template gcm_nm
#' @param years numeric. Vector of desired years. Must be in `1851:2015`.
#'   Can be obtained from [`list_gcm_period()`]. Default to [`list_gcm_period()`].
#' @template dbCon
#' @template bbox
#' @template max_run
#' @param dbnames `data.frame` with the list of available GCMs (historical projections)
#'   and their corresponding names in the PostGIS data base. See climr:::dbnames_hist
#' @template cache
#'
#' @importFrom tools R_user_dir
#' @importFrom data.table fread
#'
#' @return `SpatRaster`
#' @noRd
process_one_gcm3 <- function(gcm_nm, years, dbCon, bbox, max_run, dbnames = dbnames_hist, cache) { ## need to update to all GCMs
  if(gcm_nm %in% dbnames$GCM){
    gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
    
    rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")
    
    runs <- fread(file.path(rInfoPath, "gcm_hist.csv"))
    runs <- sort(unique(runs[mod == gcm_nm, run]))
    sel_runs <- runs[1:(max_run + 1L)]
    
    ## check cached
    needDownload <- TRUE
    
    cPath <- file.path(cache_path(), "gcmhist", gcmcode)
    
    if (dir.exists(cPath)) {
      bnds <- try(fread(file.path(cPath, "meta_area.csv")), silent = TRUE)
      
      if (is(bnds, "try-error")) {
        ## try to get the data again
        message(
          "Metadata file no longer exists or is unreadable.",
          " Downloading the data again"
        )
      } else {
        needDownload <- FALSE
      }
    }
    
    if (!needDownload) {
      setorder(bnds, -numlay)
      
      spat_match <- lapply(1:nrow(bnds), FUN = \(x){
        if (is_in_bbox(bbox, matrix(bnds[x, 2:5]))) bnds$uid[x]
      })
      spat_match <- spat_match[!sapply(spat_match, is.null)]
      
      if (length(spat_match) > 0) {
        yeardat <- fread(file.path(cPath, "meta_years.csv"))
        isin <- FALSE
        for (oldid in spat_match) { ## see if any have all required variables
          if (all(years %in% yeardat[uid == oldid, years]) &
              max_run <= bnds[uid == oldid, max_run]) {
            isin <- TRUE
            break
          }
        }
        
        if (isin) {
          message("Retrieving from cache...")
          gcm_rast <- rast(file.path(cPath, paste0(oldid, ".tif")))
          layinfo <- data.table(fullnm = names(gcm_rast))
          layinfo[, c("Mod", "Var", "Month", "Run", "Year") := tstrsplit(fullnm, "_")]
          layinfo[, laynum := seq_along(Year)]
          sel <- layinfo[Year %in% years & Run %in% sel_runs, laynum]
          gcm_rast <- gcm_rast[[sel]]
        } else {
          message("Not fully cached :( Will download more")
          needDownload <- TRUE
        }
      } else {
        message("Not fully cached :( Will download more")
        needDownload <- TRUE
      }
    }
    
    if (needDownload) {
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
        dir.create(cPath, recursive = TRUE, showWarnings = FALSE)
        
        writeRaster(gcm_rast, file.path(cPath, paste0(uid, ".tif")))
        rastext <- ext(gcm_rast)
        t1 <- data.table(
          uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1],
          numlay = nlyr(gcm_rast), max_run = max_run
        )
        t2 <- data.table(uid = rep(uid, length(years)), years = years)
        fwrite(t1, file = file.path(cPath, "meta_area.csv"), append = TRUE)
        fwrite(t2, file = file.path(cPath, "meta_years.csv"), append = TRUE)
      }
    }
    
    return(gcm_rast)
  }
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
#' @importFrom tools R_user_dir
#' @importFrom data.table fread
#'
#' @return a `SpatRaster`
#' @noRd
process_one_gcm4 <- function(gcm_nm, ssp, period, max_run, dbnames = dbnames_ts, bbox, dbCon, cache) { ## need to update to all GCMs
  gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
  
  rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")
  
  runs <- fread(file.path(rInfoPath, "gcm_ts.csv"))
  runs <- sort(unique(runs[mod == gcm_nm & scenario %in% ssp, run]))
  if (length(runs) < 1) {
    warning("That GCM isn't in our database yet.")
  }else{
    sel_runs <- runs[1:(max_run + 1L)]
    
    ## check cached
    needDownload <- TRUE
    
    cPath <- file.path(cache_path(), "gcmts", gcmcode)
    
    if (dir.exists(cPath)) {
      bnds <- try(fread(file.path(cPath, "meta_area.csv")), silent = TRUE)
      
      if (is(bnds, "try-error")) {
        ## try to get the data again
        message(
          "Metadata file no longer exists or is unreadable.",
          " Downloading the data again"
        )
      } else {
        needDownload <- FALSE
      }
    }
    
    
    if (!needDownload) {
      setorder(bnds, -numlay)
      
      spat_match <- lapply(1:nrow(bnds), FUN = \(x){
        if (is_in_bbox(bbox, matrix(bnds[x, 2:5]))) bnds$uid[x]
      })
      spat_match <- spat_match[!sapply(spat_match, is.null)]
      
      if (length(spat_match) > 0) {
        periods <- fread(file.path(cPath, "meta_period.csv"))
        ssps <- fread(file.path(cPath, "meta_ssp.csv"))
        isin <- FALSE
        for (oldid in spat_match) { ## see if any have all required variables
          if (all(period %in% periods[uid == oldid, period]) &
              all(ssp %in% ssps[uid == oldid, ssp]) &
              max_run <= bnds[uid == oldid, max_run]) {
            isin <- TRUE
            break
          }
        }
        
        if (isin) {
          message("Retrieving from cache...")
          gcm_rast <- rast(file.path(cPath, paste0(oldid, ".tif")))
          layinfo <- data.table(fullnm = names(gcm_rast))
          layinfo[, c("Mod", "Var", "Month", "Scenario", "Run", "Year") := tstrsplit(fullnm, "_")]
          layinfo[, laynum := seq_along(fullnm)]
          sel <- layinfo[Scenario %in% ssp & Year %in% period & Run %in% sel_runs, laynum]
          gcm_rast <- gcm_rast[[sel]]
        } else {
          message("Not fully cached :( Will download more")
          needDownload <- TRUE
        }
      } else {
        message("Not fully cached :( Will download more")
        needDownload <- TRUE
      }
    }
    
    if (needDownload) {
      q <- paste0(
        "select fullnm, laynum from esm_layers_ts where mod = '", gcm_nm, "' and scenario in ('", paste(ssp, collapse = "','"),
        "') and period in ('", paste(period, collapse = "','"), "') and run in ('", paste(sel_runs, collapse = "','"), "')"
      )
      # print(q)
      layerinfo <- dbGetQuery(dbCon, q)
      message("Downloading GCM anomalies")
      message("Precip...")
      gcm_rast_ppt <- pgGetTerra(dbCon, gsub("VAR","ppt",gcmcode), tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
      names(gcm_rast_ppt) <- gsub("tasmin", "PPT", layerinfo$fullnm)
      message("Tmax...")
      gcm_rast_tmax <- pgGetTerra(dbCon, gsub("VAR","tmax",gcmcode), tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
      names(gcm_rast_tmax) <- gsub("tasmin", "Tmax", layerinfo$fullnm)
      message("Tmin...")
      gcm_rast_tmin <- pgGetTerra(dbCon, gsub("VAR","tmin",gcmcode), tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
      names(gcm_rast_tmin) <- gsub("tasmin", "Tmin", layerinfo$fullnm)
      gcm_rast <- c(gcm_rast_ppt, gcm_rast_tmax, gcm_rast_tmin)
      
      if (cache) {
        message("Caching data...")
        uid <- UUIDgenerate()
        dir.create(cPath, recursive = TRUE, showWarnings = FALSE)
        
        writeRaster(gcm_rast, file.path(cPath, paste0(uid, ".tif")))
        rastext <- ext(gcm_rast)
        t1 <- data.table(
          uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1],
          numlay = nlyr(gcm_rast), max_run = max_run
        )
        t2 <- data.table(uid = rep(uid, length(period)), period = period)
        t3 <- data.table(uid = rep(uid, length(ssp)), ssp = ssp)
        fwrite(t1, file = file.path(cPath, "meta_area.csv"), append = TRUE)
        fwrite(t2, file = file.path(cPath, "meta_period.csv"), append = TRUE)
        fwrite(t3, file = file.path(cPath, "meta_ssp.csv"), append = TRUE)
      }
    }
    
    return(gcm_rast)
  }
  
}
