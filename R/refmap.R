#' Retrieve reference period climate maps
#' @description
#' This function downloads (or retrieves from cache) monthly Tmin, Tmax, and PPT climatologies (maps of long-term average climate)
#' from a specified data source for the specified bounding box.
#' It is intended for use with [`downscale_core()`], but can also be used as stand-alone raster data.
#'
#' @template dbCon
#' @template bbox
#' @template reference
#' @template cache
#'
#' @return A `SpatRaster` containing reference period climatologies, lapse rates
#'   and digital elevation model layers, that can be used with [`downscale_core()`].
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
#' @rdname input_refmap
#' @export
input_refmap <- function(dbCon, bbox, reference = "refmap_climatena", cache = TRUE, indiv_tiles = FALSE, xyz = NULL) {
  ## checks
  if (is(reference, "character")) {
    # match.arg(reference, list_refmaps()) ## temporarily disable
  } else {
    if (!is(reference, "SpatRaster")) {
      stop(
        "'reference' must be one of 'list_refmaps()' or a SpatRaster with 36 layers",
        " of reference climate variables"
      )
    }
  }

  if (!is(cache, "logical")) {
    stop("please pass a logical value to 'cache'")
  }

  if (!is.null(bbox)) {
    .check_bb(bbox)
  }

  needDownload <- TRUE
  if (!grepl("normal", reference)) {
    rmap_nm <- switch(reference,
                      refmap_prism = "normal_bc",
                      refmap_climr = "normal_composite",
                      refmap_climatena = "normal_na",
                      auto = "normal_composite"
    )
  } else {
    rmap_nm <- reference
  }
  

  cPath <- file.path(cache_path(), "reference", rmap_nm)

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
    if(indiv_tiles){
      cache <- FALSE
      res <- dbGetTiles(dbCon, rmap_nm, pnts = xyz, bands = 1:73)
    }else{
      res <- pgGetTerra(dbCon, rmap_nm, tile = TRUE, boundary = bbox, bands = 1:73)
    }
    names(res) <- c(
      "PPT_01", "PPT_02", "PPT_03", "PPT_04", "PPT_05", "PPT_06", "PPT_07",
      "PPT_08", "PPT_09", "PPT_10", "PPT_11", "PPT_12", "Tmax_01", "Tmax_02",
      "Tmax_03", "Tmax_04", "Tmax_05", "Tmax_06", "Tmax_07", "Tmax_08", "Tmax_09",
      "Tmax_10", "Tmax_11", "Tmax_12", "Tmin_01", "Tmin_02", "Tmin_03", "Tmin_04",
      "Tmin_05", "Tmin_06", "Tmin_07", "Tmin_08", "Tmin_09", "Tmin_10", "Tmin_11",
      "Tmin_12", "lr_PPT_01", "lr_PPT_02", "lr_PPT_03", "lr_PPT_04", "lr_PPT_05",
      "lr_PPT_06", "lr_PPT_07", "lr_PPT_08", "lr_PPT_09", "lr_PPT_10", "lr_PPT_11",
      "lr_PPT_12", "lr_Tmax_01", "lr_Tmax_02", "lr_Tmax_03", "lr_Tmax_04",
      "lr_Tmax_05", "lr_Tmax_06", "lr_Tmax_07", "lr_Tmax_08", "lr_Tmax_09",
      "lr_Tmax_10", "lr_Tmax_11", "lr_Tmax_12", "lr_Tmin_01", "lr_Tmin_02",
      "lr_Tmin_03", "lr_Tmin_04", "lr_Tmin_05", "lr_Tmin_06", "lr_Tmin_07",
      "lr_Tmin_08", "lr_Tmin_09", "lr_Tmin_10", "lr_Tmin_11", "lr_Tmin_12",
      "dem2_WNA"
    )
    attr(res, "builder") <- "climr"
    if (cache) {
      message("Caching data...")
      uid <- UUIDgenerate()
      dir.create(cPath, recursive = TRUE, showWarnings = FALSE)
      writeRaster(res, file.path(cPath, paste0(uid, ".tif")))
      rastext <- ext(res)
      temp <- data.table(uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1])
      fwrite(temp, file = file.path(cPath, "meta_data.csv"), append = TRUE)
    }
  }

  # Return preprocessed raster
  return(res)
}
