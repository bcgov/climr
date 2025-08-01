#' Pre cache data for presets or custom areas
#' @description
#' This function allows users to efficiently pre-cache data for a specific study region, meaning you then don't have to wait for data to download during analysis. Currently, the function only supports caching of reference maps, and future gcm periods. 
#' @param region character. Preset region; options are one of "BC", "WNA", and "NA"
#' @param bbox optional custom bounding box (or SpatExtent) for region of interest. If bbox is specified, region will be ignored.
#' @template gcms
#' @template ssps
#' @param gcm_periods character. 20-year reference periods for GCM simulations.
#'   Options are [`list_gcm_periods()`]. 
#' @template max_run
#' 
#' @details
#' Since the reference climatologies are stored at a high resolution, downloading a large region from the database can be slow due to the tile stitching required. Thus, to speed up this portion, we built an API on the database server which clips a full GeoTiff using gdalwarp. This allows the download to happen all at once instead of by tiles, and is substantially faster. Since GCM anomalies are at a much lower resolution, downloading them from the database is sufficiently fast.
#' 
#' 
#' @importFrom terra rast writeRaster ext
#' @importFrom data.table data.table
#' @importFrom uuid UUIDgenerate
#' @importFrom httr GET content
#' @importFrom curl curl_download
#' @importFrom uuid UUIDgenerate
#' @export
#' 
pre_cache <- function(region = c("BC","WNA","NA"), bbox = NULL, 
                      gcms = NULL, ssps = NULL, 
                      gcm_periods = NULL, max_run = 0) {
  
  if(!is.null(gcms)){
    if(is.null(ssps) | is.null(gcm_periods)) stop("Error: if `gcms` is not NULL, `ssps` and `gcm_periods` must be specified.")
  }
  
  region <- match.arg(region)
  reg_box <- data.table(rname = c("BC","WNA","NA"),
                        xmin = c(-139.1, -139.17, -179.054166666518),
                        xmax = c(-114.0, -89.9, -52.0041666680861),
                        ymin = c(48.3, 29.92, 13.9958333334196),
                        ymax = c(60.3, 60.3, 83.4958333325618))
  if(!is.null(bbox)){
    message("Custom bounding box provided. Region will be ignored.")
    bb_use <- as.vector(bbox)
  } else {
    temp <- reg_box[rname == region,]
    bb_use <- c(temp$xmin,temp$xmax,temp$ymin,temp$ymax)
  }
  
  message("Clipping region...")
  res <- httr::GET("http://146.190.244.244:8000/clipr", 
             query = list(rname = "climr_mosaic_clamped.tif", 
                          xmin = bb_use[1], xmax = bb_use[2], 
                          ymin = bb_use[3], ymax = bb_use[4]))
  url <- httr::content(res)$url[[1]]
  tmp <- tempfile()
  message("Downloading refmap...")
  curl::curl_download(content(res)$url[[1]], tmp)
  res <- terra::rast(tmp)
  names(res)[73] <- "dem2_WNA"
  
  message("Caching refmap...")
  cPath <- file.path(cache_path(), "reference", "refmap_climr")
  uid <- UUIDgenerate()
  dir.create(cPath, recursive = TRUE, showWarnings = FALSE)
  writeRaster(res, file.path(cPath, paste0(uid, ".tif")))
  rastext <- ext(res)
  temp <- data.table(uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1])
  fwrite(temp, file = file.path(cPath, "meta_data.csv"), append = TRUE)
  
  if(!is.null(gcms)){
    lapply(gcms, cache_one_gcm, ssps = ssps, period = gcm_periods, max_run = max_run, bbox = bb_use)
  }
  
  return(TRUE)
}


#' Cache one GCM at a time
#'
#' @importFrom tools R_user_dir
#' @importFrom data.table fread fwrite
#' @noRd
cache_one_gcm <- function(gcm_nm, ssps, period, max_run, bbox) {
  ###gcm periods
  gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
  cPath <- file.path(cache_path(), "gcms", gcmcode)
  
  runs <- .globals[["gcm_period_runs"]]
  runs <- sort(unique(runs[mod == gcm_nm & scenario %in% ssps, run]))
  sel_runs <- runs[1:(max_run + 1L)]

  q <- paste0(
    "select * from esm_layers_period where mod = '", gcm_nm, "' and scenario in ('", paste(ssps, collapse = "','"),
    "') and period in ('", paste(period, collapse = "','"), "') and run in ('", paste(sel_runs, collapse = "','"), "')"
  )
  # print(q)
  layerinfo <- as.data.table(db_safe_query(q))
  message("Downloading ",gcm_nm,"...")
  gcm_rast <- pgGetTerra(gcmcode, tile = FALSE, bands = layerinfo$laynum, boundary = bbox)
  layerinfo[, fullnm := paste(mod, var, month, scenario, run, period, sep = "_")]
  names(gcm_rast) <- layerinfo$fullnm
  
  message("Caching gcm...")
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