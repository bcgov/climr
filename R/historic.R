#' Create historic inputs for `downscale` from PostGIS database.
#' 
#' @description
#' `historic_input` creates inputs from a given historic **period**.
#' 
#' @return A `list` of `SpatRasters`, each with possibly multiple layers, that can
#'   be used with `downscale`.
#' 
#' @template dbCon
#' @template bbox
#' @param period character. Vector of labels of the periods to use. 
#'   Can be obtained from `list_historic()`. Default to "2001_2020".
#' @template cache
#'
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @importFrom data.table fread setorder data.table fwrite
#' @importFrom uuid UUIDgenerate
#' @rdname hist-input-data
#' @export
historic_input <- function(dbCon, bbox = NULL, period = list_historic(), cache = TRUE) {
  dbnames2 <- structure(list(
    PERIOD = c("2001_2020"),
    dbname = c("historic_periods")
  ), class = "data.frame", row.names = c(NA, -13L))


  dbcode <- dbnames2$dbname[dbnames2$PERIOD %in% period]

  if (dir.exists(paste0(cache_path(), "/historic/", dbcode))) {
    bnds <- fread(paste0(cache_path(), "/historic/", dbcode, "/meta_area.csv"))
    setorder(bnds, -numlay)
    for (i in 1:nrow(bnds)) {
      isin <- is_in_bbox(bbox, matrix(bnds[i, 2:5]))
      if (isin) break
    }
    if (isin) {
      oldid <- bnds$uid[i]
      periods <- fread(paste0(cache_path(), "/historic/", dbcode, "/meta_period.csv"))
      if (all(period %in% periods[uid == oldid, period])) {
        message("Retrieving from cache...")
        hist_rast <- rast(paste0(cache_path(), "/historic/", dbcode, "/", oldid, ".tif"))
        attr(hist_rast, "builder") <- "climr"
        hist_rast <- list(hist_rast)
        names(hist_rast) <- period
        return(hist_rast)
      } else {
        message("Not fully cached :( Will download more")
      }
    }
  }

  q <- paste0("select fullnm, laynum from historic_layers where period in ('", paste(period, collapse = "','"), "')")
  # print(q)
  layerinfo <- dbGetQuery(dbCon, q)
  message("Downloading historic anomalies")
  hist_rast <- pgGetTerra(dbCon, dbcode, tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
  names(hist_rast) <- layerinfo$fullnm

  if (cache) {
    message("Caching data...")
    uid <- UUIDgenerate()
    if (!dir.exists(paste0(cache_path(), "/historic/", dbcode))) dir.create(paste0(cache_path(), "/historic/", dbcode), recursive = TRUE)
    writeRaster(hist_rast, paste0(cache_path(), "/historic/", dbcode, "/", uid, ".tif"))
    rastext <- ext(hist_rast)
    t1 <- data.table(
      uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1],
      numlay = nlyr(hist_rast)
    )
    t2 <- data.table(uid = rep(uid, length(period)), period = period)
    fwrite(t1, file = paste0(cache_path(), "/historic/", dbcode, "/meta_area.csv"), append = TRUE)
    fwrite(t2, file = paste0(cache_path(), "/historic/", dbcode, "/meta_period.csv"), append = TRUE)
  }

  attr(hist_rast, "builder") <- "climr"
  hist_rast <- list(hist_rast)
  names(hist_rast) <- period
  return(hist_rast)
}


#' @description
#' `historic_input` creates inputs from a given historic **time series**.
#' 
#' @template dbCon
#' @template bbox
#' @template cache
#' @param years numeric. Years to retrieve timeseries for, in `1902:2022`. Default `2010:2022`
#' 
#' @importFrom terra rast writeRaster ext nlyr
#' @importFrom utils head tail
#' @importFrom RPostgres dbGetQuery
#' @importFrom data.table fread setorder data.table fwrite
#' @importFrom uuid UUIDgenerate
#' @rdname hist-input-data
#' @export
historic_input_ts <- function(dbCon, bbox = NULL, years = 2010:2022, cache = TRUE) {
  dbcode <- "historic_ts"
  ts_name <- "climatebc"

  if (dir.exists(paste0(cache_path(), "/historic_ts/", ts_name))) {
    bnds <- fread(paste0(cache_path(), "/historic_ts/", ts_name, "/meta_area.csv"))
    setorder(bnds, -numlay)
    for (i in 1:nrow(bnds)) {
      isin <- is_in_bbox(bbox, matrix(bnds[i, 2:5]))
      if (isin) break
    }
    if (isin) {
      oldid <- bnds$uid[i]
      periods <- fread(paste0(cache_path(), "/historic_ts/", ts_name, "/meta_period.csv"))
      if (all(years %in% periods[uid == oldid, period])) {
        message("Retrieving from cache...")
        hist_rast <- rast(paste0(cache_path(), "/historic_ts/", ts_name, "/", oldid, ".tif"))
        attr(hist_rast, "builder") <- "climr"
        hist_rast <- list(hist_rast)
        names(hist_rast) <- paste(years[1], tail(years, 1), sep = ":")
        return(hist_rast)
      } else {
        message("Not fully cached :( Will download more")
      }
    }
  }

  q <- paste0("select fullnm, laynum from historic_ts_layers where period in ('", paste(years, collapse = "','"), "')")
  # print(q)
  layerinfo <- dbGetQuery(dbCon, q)
  message("Downloading historic anomalies")
  hist_rast <- pgGetTerra(dbCon, dbcode, tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
  names(hist_rast) <- layerinfo$fullnm

  if (cache) {
    message("Caching data...")
    uid <- UUIDgenerate()
    if (!dir.exists(paste0(cache_path(), "/historic_ts/", ts_name))) dir.create(paste0(cache_path(), "/historic_ts/", ts_name), recursive = TRUE)
    writeRaster(hist_rast, paste0(cache_path(), "/historic_ts/", ts_name, "/", uid, ".tif"))
    rastext <- ext(hist_rast)
    t1 <- data.table(
      uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1],
      numlay = nlyr(hist_rast)
    )
    t2 <- data.table(uid = rep(uid, length(years)), period = years)
    fwrite(t1, file = paste0(cache_path(), "/historic_ts/", ts_name, "/meta_area.csv"), append = TRUE)
    fwrite(t2, file = paste0(cache_path(), "/historic_ts/", ts_name, "/meta_period.csv"), append = TRUE)
  }

  attr(hist_rast, "builder") <- "climr"
  hist_rast <- list(hist_rast)
  names(hist_rast) <- paste(years[1], tail(years, 1), sep = ":")
  return(hist_rast)
}





# historic_input <- function(period = list_historic()[1]) {
#
#   # Check if we have data, if not download some.
#   data_check()
#
#   # Get relevant files
#   get_rel_files <- function(pattern) {
#     res <- lapply(
#       file.path(
#         data_path(),
#         getOption("climr.historic.path", default = "inputs_pkg/historic"),
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

# dat <- rast("../climR-pnw-data/inputs_pkg/historic/Historic_2001_2020/anom_2001_2020.nc")
# nm <- fread("../climR-pnw-data/inputs_pkg/historic/Historic_2001_2020/anom_2001_2020.csv",header = T)[['x']]
# r <- terra::rast(list.files(dir_gcm, full.names = TRUE, pattern = "\\.nc"))
# names(dat) <- nm
#
# message(
#   "Saving uncompressed historic deltas to: ",
#   file.path(dir_hist, sprintf("gcmData.%s.deltas.tif", h))
# )
#
# # Actual writing
# terra::writeRaster(
#   dat,
#   "../climR-pnw-data/inputs_pkg/historic/Historic_2001_2020/2001_2020.tif",
#   overwrite = TRUE,
#   gdal="COMPRESS=NONE"
# )
