#' Retrieve climatologies for normal period
#' @description
#' This function downloads (or retrieves from cache) monthly Tmin, Tmax, and PPT variables
#' for the specified climatology and for the specified bounding box. It is intended for use with [`downscale()`],
#' but can also be used as a stand-alone climatology.
#'
#'
#' @template dbCon
#' @template bbox
#' @template normal
#' @template cache
#'
#' @return A `SpatRaster` containing normals, lapse rates
#'   and digital elevation model layers, that can be used with [`downscale()`].
#'
#' @details
#' The first 36 layers of the output raster correspond with the actual climate variables. The raster also contains
#' lapse rates for each variable, and a corresponding digital elevation model.
#'
#'
#' @seealso [downscale()]
#'
#' @importFrom terra rast writeRaster ext
#' @importFrom data.table fread fwrite data.table
#' @importFrom uuid UUIDgenerate
#' @rdname normal-input-data
#' @export
normal_input <- function(dbCon, bbox, normal = "normal_na", cache = TRUE) {
  ## checks
  if (is(normal, "character")) {
    match.arg(normal, list_normal())
  } else {
    if (!is(normal, "SpatRaster")) {
      stop(
        "'normal' must be one of 'list_normal()' or a SpatRaster with 36 layers",
        " of normal climate variables"
      )
    }
  }

  if (!is(cache, "logical")) {
    stop("please pass a logical value to 'cache'")
  }
  
  if (!is.null(bbox)) {
    .check_bb(bbox)
  }

  ## check cached
  ## check cached
  needDownload <- TRUE

  cPath <- file.path(cache_path(), "normal", normal)

  if (dir.exists(cPath)) {
    bnds <- try(fread(file.path(cPath, "meta_data.csv")), silent = TRUE)

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
    for (i in 1:nrow(bnds)) {
      isin <- is_in_bbox(bbox, matrix(bnds[i, 2:5]))
      if (isin) break
    }
    if (isin) {
      message("Retrieving from cache...")
      oldid <- bnds$uid[i]
      res <- rast(file.path(cPath, paste0(oldid, ".tif")))
      attr(res, "builder") <- "climr"
    } else {
      message("Not fully cached :( Will download more")
      needDownload <- TRUE
    }
  }

  if (needDownload) {
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
      dir.create(cPath, recursive = TRUE, showWarnings = FALSE)
      writeRaster(res, file.path(cPath, paste0(uid, ".tif")))
      rastext <- ext(res)
      temp <- data.table(uid = uid, ymax = rastext[4] + 0.1, ymin = rastext[3] - 0.1, xmax = rastext[2] + 0.1, xmin = rastext[1] - 0.1)
      fwrite(temp, file = file.path(cPath, "meta_data.csv"), append = TRUE)
    }
  }

  # Return preprocessed raster
  return(res)
}
