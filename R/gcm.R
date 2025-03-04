#' Retrieve global climate model anomalies for `downscale_core`.
#'
#' @return A `list` of `SpatRasters`, each with possibly multiple layers, that can
#'   be used with [`downscale_core()`].
#'
#' @description
#' `input_gcms` retrieves anomalies of 20-year periods for selected GCMs, SSPs,
#'  periods and runs.
#'
#' @template dbCon
#' @template bbox
#' @template gcms
#' @template ssps
#' @template period
#' @template max_run
#' @template cache
#' @template run_nm
#'
#' @details
#' This function returns a list with one slot for each requested GCM. Rasters inside the list contain anomalies for all requested SSPs, runs, and periods.
#' In general this function should only be used in combination with [`downscale_core()`].
#'
#'
#' @seealso [downscale_core()]
#'
#' @importFrom terra rast writeRaster ext nlyr values cells rowColFromCell
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
#'
#' gcms <- input_gcms(dbCon, thebb, list_gcms()[1], list_ssps()[1])
#'
#' ## show ensemble means only
#' lyrs <- grep("ensemble", names(gcms$`ACCESS-ESM1-5`))
#'
#' plot(gcms$`ACCESS-ESM1-5`[[lyrs]])
#' pool::poolClose(dbCon)
#' @rdname gcms-input-data
#' @export
input_gcms <- function(dbCon, bbox = NULL, gcms = list_gcms(), ssps = list_ssps(), period = list_gcm_periods(), max_run = 0L, cache = TRUE, run_nm = NULL) {
  ## checks
  if (!is.null(bbox)) {
    .check_bb(bbox)
  }

  gcms <- match.arg(gcms, list_gcms(), several.ok = TRUE)
  ssps <- match.arg(ssps, list_ssps(), several.ok = TRUE)
  period <- match.arg(period, list_gcm_periods(), several.ok = TRUE)

  if (!is(max_run, "numeric")) {
    stop("please pass a numeric value to 'max_runs'")
  }

  if (!is(cache, "logical")) {
    stop("please pass a logical value to 'cache'")
  }

  # Load each file individually + select layers
  res <- sapply(gcms, process_one_gcm2,
    ssps = ssps, period = period,
    bbox = bbox, dbnames = dbnames, dbCon = dbCon,
    max_run = max_run, cache = cache, run_nm = run_nm, 
    USE.NAMES = TRUE, simplify = FALSE
  )
  attr(res, "builder") <- "climr"

  # Return a list of SpatRasters, one element for each model
  return(res)
}

#' @rdname gcms-input-data
#' @export
input_gcms_db <- function(
  conn,
  gcms = list_gcms(),
  ssps = list_ssps(),
  period = list_gcm_periods(),
  max_run = 0L,run_nm = NULL
) {

  gcms <- match.arg(gcms, list_gcms(), several.ok = TRUE)
  ssps <- match.arg(ssps, list_ssps(), several.ok = TRUE)
  period <- match.arg(period, list_gcm_periods(), several.ok = TRUE)

  if (!is(max_run, "numeric")) {
    stop("please pass a numeric value to 'max_runs'")
  }

  if (!is(cache, "logical")) {
    stop("please pass a logical value to 'cache'")
  }
  
  rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")
  runs <- data.table::fread(file.path(rInfoPath, "gcm_periods.csv"))
  
  res <- lapply(gcms, function(gcm_nm) {
    gcmcode <- dbnames[GCM == gcm_nm, dbname]
    runs <- sort(unique(runs[mod == gcm_nm & scenario %in% ssps, run]))
    if (is.null(run_nm)) {
      sel_runs <- runs[1:(max_run + 1L)]
    } else {
      if (!run_nm %in% runs) {
        stop("Run ", run_nm, "doesn't exist for this GCM.")
      }
      sel_runs <- run_nm
    }

    q <- "select * from esm_layers_period where mod = '%s' and scenario in (%s) and period in (%s) and run in (%s)" |>
      sprintf(
        gcm_nm,
        paste0("'", ssps, "'", collapse = ","),
        paste0("'", period, "'", collapse = ","),
        paste0("'", sel_runs, "'", collapse = ",")
      ) |>
      DBI::SQL()

    layerinfo <- DBI::dbGetQuery(conn, q) |> data.table::setDT()
    layerinfo[, var_nm := paste(mod, var, month, scenario, run, period, sep = "_")]
    list(
      gcm_rast = gcmcode,
      layers = layerinfo[, list(var_nm, laynum)]
    )
  })

  attr(res, "builder") <- "climr"
  return(res)
}


#' @description
#' `input_gcm_hist` creates GCM time series inputs for the historical scenario (1850-2014), given chosen GCMs,
#'  years and runs.
#'
#' @template dbCon
#' @template bbox
#' @template gcms
#' @param years numeric. Vector of desired years. Default is `1901:2014`.
#'   See [`list_gcm_hist_years()`] for available years.
#' @template max_run
#' @template cache
#' @template run_nm
#'
#' @seealso [list_gcm_periods()], [`list_gcm_periods()`]
#'
#' @return A `list` of `SpatRasters`, each with possibly multiple layers, that can
#'   be used with [`downscale_core()`].
#'
#' @details This function returns a list with one slot for each requested GCM. Rasters inside the list contain anomalies for all runs and years.
#' In general this function should only be used in combination with [`downscale_core()`].
#'
#' @importFrom terra rast writeRaster ext nlyr values cells rowColFromCell
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @import uuid
#' @import data.table
#'
#' @rdname gcms-input-data
#' @export
input_gcm_hist <- function(dbCon, bbox = NULL, gcms = list_gcms(),
                           years = 1901:2014, max_run = 0L, cache = TRUE, run_nm = NULL) {
  ## checks
  if (!is.null(bbox)) {
    .check_bb(bbox)
  }

  # Load each file individually + select layers
  res <- sapply(gcms, process_one_gcm3,
    years = years,
    dbCon = dbCon, bbox = bbox, dbnames = dbnames_hist,
    max_run = max_run, cache = cache, run_nm = run_nm, USE.NAMES = TRUE, simplify = FALSE
  )
  res <- res[!sapply(res, is.null)] ## remove NULL
  attr(res, "builder") <- "climr"

  # Return a list of SpatRasters, one element for each model
  return(res)
}

#' @rdname gcms-input-data
#' @export
input_gcm_hist_db <- function(
  conn,
  gcms = list_gcms(),
  years = 1901:2014,
  max_run = 0L,
  run_nm = NULL
) {

  rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")
  runs <- data.table::fread(file.path(rInfoPath, "gcm_hist.csv"))

  res <- lapply(gcms, function(gcm_nm) {
    if (!gcm_nm %in% dbnames_hist$GCM) return(NULL)
    gcmcode <- dbnames_hist[GCM == gcm_nm, dbname]
    runs <- sort(unique(runs[mod == gcm_nm, run]))
    if (is.null(run_nm)) {
      sel_runs <- runs[1:(max_run + 1L)]
    } else {
      if (!run_nm %in% runs) {
        stop("Run ", run_nm, "doesn't exist for this GCM.")
      }
      sel_runs <- run_nm
    }

    q <- "select mod, var, month, run, year, laynum from esm_layers_hist where mod = '%s' and year in (%s) and run in (%s)" |>
      sprintf(
        gcm_nm,
        paste0("'", years, "'", collapse = ","),
        paste0("'", sel_runs, "'", collapse = ",")
      ) |>
      DBI::SQL()

    layerinfo <- DBI::dbGetQuery(conn, q) |> data.table::setDT()
    layerinfo[, var_nm := paste(mod, var, month, run, year, sep = "_")]
    list(
      gcm_rast = gcmcode,
      layers = layerinfo[, list(var_nm, laynum)]
    )
  })

  res <- res[!sapply(res, is.null)] ## remove NULL
  attr(res, "builder") <- "climr"
  return(res)

}

# gcm_nm <- "ACCESS-ESM1-5"
# ssps <- c("ssp245","ssp370")
# period <- 2020:2050

#' @description
#' `input_gcm_ssp` creates future GCM time series inputs, given chosen GCMs, SSPs,
#'  years and runs.
#'
#' @template dbCon
#' @template bbox
#' @template gcms
#' @template ssps
#' @param years Numeric or character vector in `2020:2100`. Defaults to `2020:2030`.
#'   See [`list_gcm_ssp_years()`] for available years.
#' @template max_run
#' @template cache
#' @template run_nm
#' @param fast Logical. Should we use the faster method of downloading data from the database using arrays instead of Postgis rasters?
#'
#' @return A `list` of `SpatRasters`, each with possibly multiple layers, that can
#'   be used with [`downscale_core()`].
#'
#' @details This function returns a list with one slot for each requested GCM. Rasters inside the list contain anomalies for all SSPs, runs and years.
#' In general this function should only be used in combination with [`downscale_core()`]. Note that if you request multiple runs, multiple SSPs, and a lot of years,
#' it will take a while to download the data (there's lot of it).
#'
#' @importFrom terra rast writeRaster ext nlyr values cells rowColFromCell
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @importFrom dplyr tbl sql collect mutate
#' @import uuid
#' @import data.table
#' @import abind
#'
#' @rdname gcms-input-data
#' @export
input_gcm_ssp <- function(dbCon, bbox = NULL, gcms = list_gcms(), ssps = list_ssps(),
                         years = 2020:2030, max_run = 0L, cache = TRUE, run_nm = NULL, fast = TRUE) {

  ## checks
  if (!is.null(bbox)) {
    .check_bb(bbox)
  }

  if (nrow(dbnames_ts) < 1) stop("That isn't a valid GCM")
  # Load each file individually + select layers
  if(fast){
    res <- sapply(gcms, process_one_gcmts_fast,
                  ssps = ssps, period = years,
                  dbnames = dbnames_ts_fast, bbox = bbox, dbCon = dbCon,
                  max_run = max_run, cache = cache, run_nm = run_nm, USE.NAMES = TRUE, simplify = FALSE
    )
  }else{
    res <- sapply(gcms, process_one_gcm4,
                  ssps = ssps, period = years,
                  dbnames = dbnames_ts, bbox = bbox, dbCon = dbCon,
                  max_run = max_run, cache = cache, run_nm = run_nm, USE.NAMES = TRUE, simplify = FALSE
    )
  }
  
  res <- res[!sapply(res, is.null)] ##remove NULL

  attr(res, "builder") <- "climr"

  # Return a list of SpatRasters, one element for each model
  return(res)
}

#' @rdname gcms-input-data
#' @export
input_gcm_ssp_db <- function(
  conn,
  gcms = list_gcms(),
  ssps = list_ssps(),
  years = 2020:2030,
  max_run = 0L,
  run_nm = NULL
) {

  if (nrow(dbnames_ts) < 1) stop("That isn't a valid GCM")
 
  rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")
  runs <- data.table::fread(file.path(rInfoPath, "gcm_ts.csv"))
  vars <- c("PPT", "Tmin", "Tmax")
  gcmsv <- expand.grid(gcm_nm = gcms, var = vars) |>
    apply(1, list) |>
    unlist(recursive = FALSE)

  res <- lapply(gcmsv, function(x) {
    if (!x[["gcm_nm"]] %in% dbnames_ts[["GCM"]]) return()
    gcmcode <- dbnames_ts[GCM == x[["gcm_nm"]], dbname] |>
      gsub("VAR", tolower(x[["var"]]), x = _)
    runs <- sort(unique(runs[mod == x[["gcm_nm"]] & scenario %in% ssps, run]))
    if (length(runs) < 1) {
      warning("That GCM isn't in our database yet.")
      return()
    }
    
    if (is.null(run_nm)) {
      sel_runs <- runs[1:(max_run + 1L)]
    } else {
      if (!run_nm %in% runs) {
        stop("Run ", run_nm, "doesn't exist for this GCM.")
      }
      sel_runs <- run_nm
    }
  
    q <-  "select fullnm as var_nm, laynum from esm_layers_ts where mod = '%s' and scenario in (%s) and period in (%s) and run in (%s)" |>
      sprintf(
        x[["gcm_nm"]],
        paste0("'", ssps, "'", collapse = ","),
        paste0("'", years, "'", collapse = ","),
        paste0("'", sel_runs, "'", collapse = ",")
      ) |>
      DBI::SQL()
  
    layerinfo <- DBI::dbGetQuery(conn, q) |> data.table::setDT()
    layerinfo[, var_nm := gsub("PPT", x[["var"]], var_nm)]
  
    list(
      gcm_rast = gcmcode,
      layers = layerinfo
    )
  })
    
  res <- res[!sapply(res, is.null)] ##remove NULL
  attr(res, "builder") <- "climr"
  return(res)

}


#' Read and parse gcms models csv files
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
#' @template ssps
#' @template bbox
#' @template period
#' @template max_run
#' @param dbnames `data.frame` with the list of available GCMs and their
#'   corresponding names in the PostGIS data base.  See climr:::dbnames
#' @template dbCon
#' @template cache
#' @template run_nm
#'
#' @importFrom tools R_user_dir
#' @importFrom data.table fread
#'
#' @return `SpatRaster`
#' @noRd
process_one_gcm2 <- function(gcm_nm, ssps, bbox, period, max_run, dbnames = dbnames, dbCon, cache, run_nm) { ## need to update to all GCMs
  gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
  # gcm_nm <- gsub("-", ".", gcm_nm)

  rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")
  
  runs <- fread(file.path(rInfoPath, "gcm_periods.csv"))
  runs <- sort(unique(runs[mod == gcm_nm & scenario %in% ssps, run]))
  if(is.null(run_nm)){
    sel_runs <- runs[1:(max_run + 1L)]
  }else{
    if(!run_nm %in% runs){
      stop("Run ", run_nm, "doesn't exist for this GCM.")
    }
    sel_runs <- run_nm
  }
  
  
  ## check cached
  needDownload <- TRUE

  cPath <- file.path(cache_path(), "gcms", gcmcode)

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

    spat_match <- lapply(1:nrow(bnds), FUN = function(x){
      if (is_in_bbox(bbox, matrix(bnds[x, 2:5]))) bnds$uid[x]
    })
    spat_match <- spat_match[!sapply(spat_match, is.null)]

    if (length(spat_match) > 0) {
      periods <- fread(file.path(cPath, "meta_period.csv"))
      ssps_cached <- fread(file.path(cPath, "meta_ssp.csv"))
      isin <- FALSE
      for (oldid in spat_match) {
        if (all(period %in% periods[uid == oldid, period]) &
          all(ssps %in% ssps_cached[uid == oldid, ssps]) &
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
        sel <- layinfo[Scenario %in% ssps & Period %in% period & Run %in% sel_runs, laynum]
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
      "select * from esm_layers_period where mod = '", gcm_nm, "' and scenario in ('", paste(ssps, collapse = "','"),
      "') and period in ('", paste(period, collapse = "','"), "') and run in ('", paste(sel_runs, collapse = "','"), "')"
    )
    # print(q)
    layerinfo <- as.data.table(dbGetQuery(dbCon, q))
    message("Downloading GCM anomalies")
    gcm_rast <- pgGetTerra(dbCon, gcmcode, tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
    layerinfo[, fullnm := paste(mod, var, month, scenario, run, period, sep = "_")]
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
      t3 <- data.table(uid = rep(uid, length(ssps)), ssps = ssps)
      fwrite(t1, file = file.path(cPath, "meta_area.csv"), append = TRUE)
      fwrite(t2, file = file.path(cPath, "meta_period.csv"), append = TRUE)
      fwrite(t3, file = file.path(cPath, "meta_ssp.csv"), append = TRUE)
    }
  }

  return(gcm_rast)
}

#' Process one gcm historic time series at a time
#'
#' @template gcm_nm
#' @param years numeric. Vector of desired years. Must be in `1851:2015`.
#'   Can be obtained from [`list_gcm_periods()`]. Default to [`list_gcm_periods()`].
#' @template dbCon
#' @template bbox
#' @template max_run
#' @param dbnames `data.frame` with the list of available GCMs (historical projections)
#'   and their corresponding names in the PostGIS data base. See climr:::dbnames_hist
#' @template cache
#' @template run_nm
#'
#' @importFrom tools R_user_dir
#' @importFrom data.table fread
#'
#' @return `SpatRaster`
#' @noRd
process_one_gcm3 <- function(gcm_nm, years, dbCon, bbox, max_run, dbnames = dbnames_hist, cache, run_nm) { ## need to update to all GCMs
  if (gcm_nm %in% dbnames$GCM) {
    gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]

    rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")

    runs <- fread(file.path(rInfoPath, "gcm_hist.csv"))
    runs <- sort(unique(runs[mod == gcm_nm, run]))
    if(is.null(run_nm)){
      sel_runs <- runs[1:(max_run + 1L)]
    }else{
      if(!run_nm %in% runs){
        stop("Run ", run_nm, "doesn't exist for this GCM.")
      }
      sel_runs <- run_nm
    }
    

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

      spat_match <- lapply(1:nrow(bnds), FUN = function(x){
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
#' @template ssps
#' @template period
#' @template max_run
#' @param dbnames `data.frame` with the list of available GCMs (time series projections)
#'   and their corresponding names in the PostGIS data base. See climr:::dbnames_ts
#' @template bbox
#' @template dbCon
#' @template cache
#' @template run_nm
#'
#' @importFrom tools R_user_dir
#' @importFrom data.table fread
#'
#' @return a `SpatRaster`
#' @noRd
process_one_gcm4 <- function(gcm_nm, ssps, period, max_run, dbnames = dbnames_ts, bbox, dbCon, cache, run_nm) { ## need to update to all GCMs
  if (gcm_nm %in% dbnames$GCM) {
    gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]

    rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")

    runs <- fread(file.path(rInfoPath, "gcm_ts.csv"))
    runs <- sort(unique(runs[mod == gcm_nm & scenario %in% ssps, run]))
    if (length(runs) < 1) {
      warning("That GCM isn't in our database yet.")
    } else {
      if(is.null(run_nm)){
        sel_runs <- runs[1:(max_run + 1L)]
      }else{
        if(!run_nm %in% runs){
          stop("Run ", run_nm, "doesn't exist for this GCM.")
        }
        sel_runs <- run_nm
      }

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

        spat_match <- lapply(1:nrow(bnds), FUN = function(x){
          if (is_in_bbox(bbox, matrix(bnds[x, 2:5]))) bnds$uid[x]
        })
        spat_match <- spat_match[!sapply(spat_match, is.null)]

        if (length(spat_match) > 0) {
          periods <- fread(file.path(cPath, "meta_period.csv"))
          ssps_cache <- fread(file.path(cPath, "meta_ssp.csv"))
          isin <- FALSE
          for (oldid in spat_match) { ## see if any have all required variables
            if (all(period %in% periods[uid == oldid, period]) &
              all(ssps %in% ssps_cache[uid == oldid, ssps]) &
              max_run <= bnds[uid == oldid, max_run]) {
              isin <- TRUE
              break
            }
          }

          if (isin) {
            message("Retrieving from cache...")
            gcm_rast <- tryCatch(
              suppressWarnings(rast(file.path(cPath, paste0(oldid, ".tif")))),
              error = function(e) {
                rast(file.path(cPath, paste0(oldid, ".grd")))
              }
            )
            layinfo <- data.table(fullnm = names(gcm_rast))
            layinfo[, c("Mod", "Var", "Month", "Scenario", "Run", "Year") := tstrsplit(fullnm, "_")]
            layinfo[, laynum := seq_along(fullnm)]
            sel <- layinfo[Scenario %in% ssps & Year %in% period & Run %in% sel_runs, laynum]
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
          "select fullnm, laynum from esm_layers_ts where mod = '", gcm_nm, "' and scenario in ('", paste(ssps, collapse = "','"),
          "') and period in ('", paste(period, collapse = "','"), "') and run in ('", paste(sel_runs, collapse = "','"), "')"
        )
        # print(q)
        layerinfo <- dbGetQuery(dbCon, q)
        message("Downloading GCM anomalies")
        message("Precip...")
        gcm_rast_ppt <- pgGetTerra(dbCon, gsub("VAR", "ppt", gcmcode), tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
        names(gcm_rast_ppt) <- gsub("PPT", "PPT", layerinfo$fullnm)
        message("Tmax...")
        gcm_rast_tmax <- pgGetTerra(dbCon, gsub("VAR", "tmax", gcmcode), tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
        names(gcm_rast_tmax) <- gsub("PPT", "Tmax", layerinfo$fullnm)
        message("Tmin...")
        gcm_rast_tmin <- pgGetTerra(dbCon, gsub("VAR", "tmin", gcmcode), tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
        names(gcm_rast_tmin) <- gsub("PPT", "Tmin", layerinfo$fullnm)
        gcm_rast <- c(gcm_rast_ppt, gcm_rast_tmax, gcm_rast_tmin)

        if (cache) {
          message("Caching data...")
          uid <- UUIDgenerate()
          dir.create(cPath, recursive = TRUE, showWarnings = FALSE)
          if (nlyr(gcm_rast) > 65500) { ## geotifs are limited to 65535 layers
            writeRaster(gcm_rast, file.path(cPath, paste0(uid, ".grd")), filetype = "ENVI")
          } else {
            writeRaster(gcm_rast, file.path(cPath, paste0(uid, ".tif")))
          }
          rastext <- ext(gcm_rast)
          t1 <- data.table(
            uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1],
            numlay = nlyr(gcm_rast), max_run = max_run
          )
          t2 <- data.table(uid = rep(uid, length(period)), period = period)
          t3 <- data.table(uid = rep(uid, length(ssps)), ssps = ssps)
          fwrite(t1, file = file.path(cPath, "meta_area.csv"), append = TRUE)
          fwrite(t2, file = file.path(cPath, "meta_period.csv"), append = TRUE)
          fwrite(t3, file = file.path(cPath, "meta_ssp.csv"), append = TRUE)
        }
      }

      return(gcm_rast)
    }
  }
}


#' Process one GCM time series at a time (faster version)
#'
#' @template gcm_nm
#' @template ssps
#' @template period
#' @template max_run
#' @param dbnames `data.frame` with the list of available GCMs (time series projections)
#'   and their corresponding names in the PostGIS data base. See climr:::dbnames_ts
#' @template bbox
#' @template dbCon
#' @template cache
#' @template run_nm
#'
#' @importFrom tools R_user_dir
#'
#' @return a `SpatRaster`
#' @noRd
process_one_gcmts_fast <- function(gcm_nm, ssps, period, max_run, dbnames = dbnames_ts, bbox, dbCon, cache, run_nm) { 
  if(gcm_nm %in% dbnames$GCM){
    gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
    gcmarray <- dbnames$dbarray[dbnames$GCM == gcm_nm]
    rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")
    
    runs <- fread(file.path(rInfoPath, "gcm_ts.csv"))
    runs <- sort(unique(runs[mod == gcm_nm & scenario %in% ssps, run]))
    if (length(runs) < 1) {
      warning("That GCM isn't in our database yet.")
    }else{
      if(is.null(run_nm)){
        sel_runs <- runs[1:(max_run + 1L)]
      }else{
        if(!run_nm %in% runs){
          stop("Run ", run_nm, "doesn't exist for this GCM.")
        }
        sel_runs <- run_nm
      }
      
      
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
          ssps_cache <- fread(file.path(cPath, "meta_ssp.csv"))
          isin <- FALSE
          for (oldid in spat_match) { ## see if any have all required variables
            if (all(period %in% periods[uid == oldid, period]) &
                all(ssps %in% ssps_cache[uid == oldid, ssps]) &
                max_run <= bnds[uid == oldid, max_run]) {
              isin <- TRUE
              break
            }
          }
          
          if (isin) {
            message("Retrieving from cache...")
            gcm_rast <- tryCatch(
              suppressWarnings(rast(file.path(cPath, paste0(oldid, ".tif")))),
              error = function(e) {
                rast(file.path(cPath, paste0(oldid, ".grd")))
              }
            )
            layinfo <- data.table(fullnm = names(gcm_rast))
            layinfo[, c("Mod", "Var", "Month", "Scenario", "Run", "Year") := tstrsplit(fullnm, "_")]
            layinfo[, laynum := seq_along(fullnm)]
            sel <- layinfo[Scenario %in% ssps & Year %in% period & Run %in% sel_runs, laynum]
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
        template <- pgGetTerra(dbCon, name = gcmcode, tile = F, bands = 1, boundary = bbox) 
        
        if(length(period) >= 79){ ##faster if almost all years are selected
          results <- tbl(dbCon, sql(paste0("select cellid, ssp, year, run, vals from ",gcmarray," where cellid in (",paste0(values(template)[,1], collapse = ','),") 
                                  and ssp in ('",paste(ssps, collapse = "','"),"') and run in ('", paste(sel_runs, collapse = "','"), "')")))
        }else{
          results <- tbl(dbCon, sql(paste0("select cellid, ssp, year, run, vals from ",gcmarray," where cellid in (",paste0(values(template)[,1], collapse = ','),") 
                                  and year in ('",paste(period, collapse = "','"),"') and ssp in ('",paste(ssps, collapse = "','"),"') and run in ('", paste(sel_runs, collapse = "','"), "')")))
        }
        
        # dat <-
        #   results %>% 
        #   mutate(vals = unnest(vals)) %>%
        #   collect()
        dat <- collect(mutate(results,vals = unnest(vals)))
        setDT(dat)
        setorder(dat, cellid, year, ssp, run)
        dat[, month := rep(sort(sprintf(c("PPT_%02d", "Tmax_%02d", "Tmin_%02d"), sort(rep(1:12, 3)))), 
                           nrow(dat)/(12*3))]
        
        dat[, fullnm := paste(month, ssp, run, year, sep = "_")]
        cell_nums = cells(template)
        coords <- rowColFromCell(template, cell_nums)
        cellcoords <- data.table(coords, values(template))
        setnames(cellcoords, c("row","col","cellid"))
        dat[cellcoords, `:=`(row = i.row, col = i.col), on = "cellid"]
        temp <- dat[,.(row,col,fullnm,vals)]
        t2 <- dcast(temp, fullnm + row ~ col, value.var = "vals")
        
        t_array = split(as.data.frame(t2[,!c("fullnm","row")]), t2$fullnm)
        t3 <- abind(t_array, along = 3)
        gcm_rast <- rast(t3)
        terra::ext(gcm_rast) <- ext(template)
        names(gcm_rast) <- paste0(gcm_nm,"_", names(t_array))
        
        if (cache) {
          message("Caching data...")
          uid <- UUIDgenerate()
          dir.create(cPath, recursive = TRUE, showWarnings = FALSE)
          if(nlyr(gcm_rast) > 65500){ ##geotifs are limited to 65535 layers
            writeRaster(gcm_rast, file.path(cPath, paste0(uid, ".grd")), filetype = "ENVI")
          }else{
            writeRaster(gcm_rast, file.path(cPath, paste0(uid, ".tif")))
          }
          rastext <- ext(gcm_rast)
          t1 <- data.table(
            uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1],
            numlay = nlyr(gcm_rast), max_run = max_run
          )
          t2 <- data.table(uid = rep(uid, length(period)), period = period)
          t3 <- data.table(uid = rep(uid, length(ssps)), ssps = ssps)
          fwrite(t1, file = file.path(cPath, "meta_area.csv"), append = TRUE)
          fwrite(t2, file = file.path(cPath, "meta_period.csv"), append = TRUE)
          fwrite(t3, file = file.path(cPath, "meta_ssp.csv"), append = TRUE)
        }
      }
      
      return(gcm_rast)
    }
  }
}

