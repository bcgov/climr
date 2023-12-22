#' Create normal baseline inputs for `downscale` from PostGIS database.
#' 
#' @template dbCon
#' @template bbox
#' @template normal
#' @template cache
#'
#' @return A `SpatRaster` containing normals, lapse rates
#'   and digital elevation model layers, that can be used with `downscale`. 
#'
#' @importFrom terra rast writeRaster ext
#' @importFrom data.table fread fwrite data.table
#' @importFrom uuid UUIDgenerate
#' @rdname normal-input-data
#' @export
normal_input <- function(dbCon, bbox = NULL, normal = "normal_na", cache = TRUE) {
  ## checks
  if (is(normal, "character")) {
    match.arg(normal, list_normal())
  } else {
    if (!is(normal, "SpatRaster")) {
      stop("'normal' must be one of 'list_normal()' or a SpatRaster with 36 layers",
           " of normal climate variables")
    }
  }
  
  if (!is(cache, "logical")) {
    stop("please pass a logical value to 'cache'")
  }
  
  ## check cached
  normalPath <- file.path(cache_path(), "/normal/", normal)
  if (dir.exists(normalPath)) {
    bnds <- fread(file.path(normalPath, "meta_data.csv"))

    for (i in 1:nrow(bnds)) {
      isin <- is_in_bbox(bbox, matrix(bnds[i, 2:5]))
      if (isin) break
    }
    if (isin) {
      message("Retrieving from cache...")
      oldid <- bnds$uid[i]
      res <- rast(file.path(normalPath, paste0(oldid, ".tif")))
      attr(res, "builder") <- "climr"
      return(res)
    }
  }
  
  message("Downloading new data...")
  res <- pgGetTerra(dbCon, normal, tile = TRUE, boundary = bbox, bands = 1:73)
  names(res) <- c(
    "PPT01", "PPT02", "PPT03", "PPT04", "PPT05", "PPT06", "PPT07",
    "PPT08", "PPT09", "PPT10", "PPT11", "PPT12", "Tmax01", "Tmax02",
    "Tmax03", "Tmax04", "Tmax05", "Tmax06", "Tmax07", "Tmax08", "Tmax09",
    "Tmax10", "Tmax11", "Tmax12", "Tmin01", "Tmin02", "Tmin03", "Tmin04",
    "Tmin05", "Tmin06", "Tmin07", "Tmin08", "Tmin09", "Tmin10", "Tmin11",
    "Tmin12", "lr_PPT01", "lr_PPT02", "lr_PPT03", "lr_PPT04", "lr_PPT05",
    "lr_PPT06", "lr_PPT07", "lr_PPT08", "lr_PPT09", "lr_PPT10", "lr_PPT11",
    "lr_PPT12", "lr_Tmax01", "lr_Tmax02", "lr_Tmax03", "lr_Tmax04",
    "lr_Tmax05", "lr_Tmax06", "lr_Tmax07", "lr_Tmax08", "lr_Tmax09",
    "lr_Tmax10", "lr_Tmax11", "lr_Tmax12", "lr_Tmin01", "lr_Tmin02",
    "lr_Tmin03", "lr_Tmin04", "lr_Tmin05", "lr_Tmin06", "lr_Tmin07",
    "lr_Tmin08", "lr_Tmin09", "lr_Tmin10", "lr_Tmin11", "lr_Tmin12",
    "dem2_WNA"
  )
  attr(res, "builder") <- "climr"
  if (cache) {
    message("Caching data...")
    uid <- UUIDgenerate()
    if (!dir.exists(paste0(cache_path(), "/normal/", normal))) dir.create(paste0(cache_path(), "/normal/", normal), recursive = TRUE)
    writeRaster(res, paste0(cache_path(), "/normal/", normal, "/", uid, ".tif"))
    rastext <- ext(res)
    temp <- data.table(uid = uid, ymax = rastext[4] + 0.1, ymin = rastext[3] - 0.1, xmax = rastext[2] + 0.1, xmin = rastext[1] - 0.1)
    fwrite(temp, file = paste0(cache_path(), "/normal/", normal, "/meta_data.csv"), append = TRUE)
  }
  # Return preprocessed raster
  return(res)
}
