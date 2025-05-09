#' Retrieve observational anomalies.
#'
#' @description
#' `input_obs` produces anomalies of the average observed climate for a given **period**,
#' relative to the 1961-1990 reference period. The anomalies are calculated from the `"cru.gpcc"` dataset
#' which is the Climatic Research Unit TS dataset (for temperature) and Global Precipitation Climatology Centre dataset
#' (for precipitation).
#'
#' @return A `list` of `SpatRasters`, each with possibly multiple layers, that can
#'   be used with [`downscale_core()`]. Each element of the list corresponds to a particular period, and the
#'   values of the `SpatRaster` are anomalies of the obs period compare to the reference period.
#'
#' @template bbox
#' @param period character. Vector of labels of the periods to use.
#'   Can be obtained from [`list_obs_periods()`]. Default to "2001_2020".
#' @template cache
#'
#' @seealso [downscale_core()], [`list_obs_periods()`]
#' @details
#' Generally, this function should only be used in combination with [`downscale_core()`] as the values
#' returned in the rasters are anomalies compared to the 1961-1990 reference period,
#' and are usually not meaningful without the whole downscale process.
#'
#'
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @importFrom data.table fread setorder data.table fwrite
#' @importFrom uuid UUIDgenerate
#' @rdname hist-input-data
#' @export
input_obs <- function(bbox = NULL, period = list_obs_periods(), cache = TRUE) {
  ## checks
  if (!is.null(bbox)) {
    .check_bb(bbox)
  }

  dbnames2 <- structure(list(
    PERIOD = c("2001_2020"),
    dbname = c("historic_periods")
  ), class = "data.frame", row.names = c(NA, -13L))


  dbcode <- dbnames2$dbname[dbnames2$PERIOD %in% period]

  ## check cached
  needDownload <- TRUE

  cPath <- file.path(cache_path(), "obs", dbcode)

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
    for (i in 1:nrow(bnds)) {
      isin <- is_in_bbox(bbox, matrix(bnds[i, .(xmin,xmax,ymin,ymax)]))
      if (isin) break
    }
    if (isin) {
      oldid <- bnds$uid[i]
      periods <- fread(file.path(cPath, "meta_period.csv"))
      if (all(period %in% periods[uid == oldid, period])) {
        message("Retrieving from cache...")
        hist_rast <- rast(file.path(cPath, paste0(oldid, ".tif")))
        hist_rast <- list(hist_rast)
        attr(hist_rast, "builder") <- "climr"
        names(hist_rast) <- period
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
    q <- paste0("select var_nm, laynum from historic_layers where period in ('", paste(period, collapse = "','"), "')")
    # print(q)
    layerinfo <- db_safe_query(q)
    message("Downloading observed period anomalies")
    hist_rast <- pgGetTerra(dbcode, tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
    names(hist_rast) <- layerinfo$var_nm

    if (cache) {
      message("Caching data...")
      uid <- UUIDgenerate()
      dir.create(cPath, recursive = TRUE, showWarnings = FALSE)

      writeRaster(hist_rast, file.path(cPath, paste0(uid, ".tif")))
      rastext <- ext(hist_rast)
      t1 <- data.table(
        uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1],
        numlay = nlyr(hist_rast)
      )
      t2 <- data.table(uid = rep(uid, length(period)), period = period)
      fwrite(t1, file = file.path(cPath, "meta_area.csv"), append = TRUE)
      fwrite(t2, file = file.path(cPath, "meta_period.csv"), append = TRUE)
    }

    hist_rast <- list(hist_rast)
    attr(hist_rast, "builder") <- "climr"
    names(hist_rast) <- period
  }

  return(hist_rast)
}

#' @rdname hist-input-data
#' @export
input_obs_db <- function(period = list_obs_periods()) {
  
  #Remove NSE CRAN check warnings
  if (FALSE){ var_nm <- NULL}

  dbnames2 <- structure(list(
    PERIOD = c("2001_2020"),
    dbname = c("historic_periods")
  ), class = "data.frame", row.names = c(NA, -1L))

  dbcode <- dbnames2$dbname[dbnames2$PERIOD %in% period]

  q <- "select var_nm, laynum from historic_layers where period in (%s)" |>
    sprintf(
      paste0("'", period, "'", collapse = ",")
    ) 
  layerinfo <- db_safe_query(q) |>
    data.table::setDT()
  res <- list(
    list(
      tbl = dbcode,
      layers = layerinfo[, list(var_nm, laynum)]
    )
  )
  attr(res, "builder") <- "climr"
  return(res)

}


#' Retrieve observational anomalies for specified years
#' @description
#' `input_obs_ts` produces anomalies of observed climate for a given **time series** of individual years.
#'
#' @param dataset Character. Which observational dataset to use? Options are `"climatena"` for the
#' ClimateNA gridded time series or `"cru.gpcc"` for the combined Climatic Research Unit TS dataset
#' (for temperature) and Global Precipitation Climatology Centre dataset (for precipitation).
#' @template bbox
#' @template cache
#' @param years numeric. Years to retrieve obs anomalies for. Defaults to `2010:2022`.
#'   See [`list_obs_years()`] for available years.
#' @return List of length 1 containing a `SpatRaster`
#' @details
#' The returned raster contains anomalies for each year specified in `years`. In general this
#' function should only be used in conjunction with [`downscale_core()`].
#'
#'
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head tail
#' @importFrom RPostgres dbGetQuery
#' @importFrom data.table fread setorder data.table fwrite
#' @importFrom uuid UUIDgenerate
#' @rdname hist-input-data
#' @export
input_obs_ts <- function(dataset = c("cru.gpcc", "climatena"), bbox = NULL, years = 2010:2022, cache = TRUE) {
  ## checks
  if (!is.null(bbox)) {
    .check_bb(bbox)
  }

  res <- sapply(dataset, process_one_historicts,
    years = years,
    dbnames = dbnames_hist_obs, bbox = bbox,
    cache = cache, USE.NAMES = TRUE, simplify = FALSE
  )
  res <- res[!sapply(res, is.null)] ## remove NULL
  attr(res, "builder") <- "climr"

  # Return a list of SpatRasters, one element for each model
  return(res)

  # hist_rast <- list(hist_rast)
  # attr(hist_rast, "builder") <- "climr"
  # names(hist_rast) <- paste(years[1], tail(years, 1), sep = ":")
  # return(hist_rast)
}

#' @rdname hist-input-data
#' @export
input_obs_ts_db <- function(dataset = c("cru.gpcc", "climatena"), years = 2010:2022) {
  
  #Remove NSE CRAN check warnings
  if (FALSE){ var_nm <- NULL}

  res <- lapply(dataset, function(d) {
    if (is.na(m <- match(d, dbnames_hist_obs$dataset))) return(NULL)
    q <- "select var_nm, period, laynum from %s_layers where period in (%s)" |>
      sprintf(
        dbnames_hist_obs[["dbname"]][m],
        paste0("'", years, "'", collapse = ",")
      )
    layerinfo <- db_safe_query(q) |> 
      data.table::setDT()
    layerinfo[, var_nm := paste(d, var_nm, period, sep = "_")] 
    list(
      tbl = dbnames_hist_obs[["dbname"]][m],
      layers = layerinfo[, list(var_nm, laynum)]
    )
  })

  res <- res[!sapply(res, is.null)] ## remove NULL
  attr(res, "builder") <- "climr"
  return(res)

}

process_one_historicts <- function(dataset, years, bbox, dbnames = dbnames_hist_obs, cache) {
  if (dataset %in% dbnames$dataset) {
    ts_name <- dataset
    dbcode <- dbnames$dbname[dbnames$dataset == dataset]

    ## check cached
    needDownload <- TRUE

    cPath <- file.path(cache_path(), "obs_ts", ts_name)

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
        if (is_in_bbox(bbox, matrix(bnds[x, .(xmin,xmax,ymin,ymax)]))) bnds$uid[x]
      })
      spat_match <- spat_match[!sapply(spat_match, is.null)]
      if (length(spat_match) > 0) {
        periods <- fread(file.path(cPath, "meta_period.csv"))
        isin <- FALSE
        for (oldid in spat_match) {
          if (all(years %in% periods[uid == oldid, period])) {
            isin <- TRUE
            break
          }
        }

        if (isin) {
          message("Retrieving from cache...")
          hist_rast <- rast(file.path(cPath, paste0(oldid, ".tif")))
          hist_rast <- hist_rast[[grep(paste(years, collapse = "|"), names(hist_rast))]]
          return(hist_rast)
          # hist_rast <- list(hist_rast)
          # attr(hist_rast, "builder") <- "climr"
          # names(hist_rast) <- paste(years[1], tail(years, 1), sep = ":")
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
      q <- paste0("select var_nm, period, laynum from ", dbcode, "_layers where period in ('", paste(years, collapse = "','"), "')")
      # print(q)
      layerinfo <- db_safe_query(q)
      message("Downloading obs anomalies")
      hist_rast <- pgGetTerra(dbcode, tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
      names(hist_rast) <- paste(ts_name, layerinfo$var_nm, layerinfo$period, sep = "_")

      if (cache) {
        message("Caching data...")
        uid <- UUIDgenerate()
        dir.create(cPath, recursive = TRUE, showWarnings = FALSE)

        writeRaster(hist_rast, file.path(cPath, paste0(uid, ".tif")))
        rastext <- ext(hist_rast)
        t1 <- data.table(
          uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1],
          numlay = nlyr(hist_rast)
        )
        t2 <- data.table(uid = rep(uid, length(years)), period = years)
        fwrite(t1, file = file.path(cPath, "meta_area.csv"), append = TRUE)
        fwrite(t2, file = file.path(cPath, "meta_period.csv"), append = TRUE)
      }
      return(hist_rast)
    }
  }
}



# input_obs <- function(period = list_obs_periods()[1]) {
#
#   # Check if we have data, if not download some.
#   data_check()
#
#   # Get relevant files
#   get_rel_files <- function(pattern) {
#     res <- lapply(
#       file.path(
#         data_path(),
#         getOption("climr.obs.path", default = "inputs_pkg/obs"),
#         period
#       ),
#       list.files, recursive = TRUE, full.names = TRUE, pattern = pattern
#     )
#     res
#   }
#   files_tif <- get_rel_files("\\.tif$")
#
#   # Load each file individually + select layers
#   process_one_historic <- function(file_tif) {
#
#     # Initiate raster
#     r <- terra::rast(file_tif)
#     #nm <- names(r)
#     return(r)
#
#   }
#
#   res <- lapply(files_tif, process_one_historic)
#   attr(res, "builder") <- "climr"
#
#   # Return a list of SpatRaster, one element for each model
#   return(res)
#
# }

# dat <- rast("../climR-pnw-data/inputs_pkg/obs/Historic_2001_2020/anom_2001_2020.nc")
# nm <- fread("../climR-pnw-data/inputs_pkg/obs/Historic_2001_2020/anom_2001_2020.csv",header = T)[['x']]
# r <- terra::rast(list.files(dir_gcm, full.names = TRUE, pattern = "\\.nc"))
# names(dat) <- nm
#
# message(
#   "Saving uncompressed obs deltas to: ",
#   file.path(dir_hist, sprintf("gcmData.%s.deltas.tif", h))
# )
#
# # Actual writing
# terra::writeRaster(
#   dat,
#   "../climR-pnw-data/inputs_pkg/obs/Historic_2001_2020/2001_2020.tif",
#   overwrite = TRUE,
#   gdal="COMPRESS=NONE"
# )
