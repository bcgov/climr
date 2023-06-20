#' Update external package data
#' @param gcm A character. Relative path from the source root to global circulation models files folder.
#' Default to option value "climRpnw.gcm.path" if set, or "inputs_pkg/gcmData".
#' @param normal A character. Relative path from the source root to base normal files folder.
#' @param quiet A logical. If `TRUE`, suppress status messages (if any), and the progress bar.
#' @param ... Others parameters such as `source` or `repo` for content getting functions.
#' @details This package uses data that are too big to be included with sources.
#' Instead, data is downloaded, optionally cached, when you need to run functions.
#' @export
data_update <- function(
  ...,
  gcm = getOption("climRpnw.gcm.path", default = "inputs_pkg/gcm"),
  historic = getOption("climRpnw.historic.path", default = "inputs_pkg/historic"),
  normal = getOption("climRpnw.normal.path", default = "inputs_pkg/normal"),
  quiet = !interactive()) {
  
  # Reset options value if provided by user. They will be used to retrieve data by other functions.
  options("climRpnw.gcm.path" = gcm)
  options("climRpnw.normal.path" = normal)
  options("climRpnw.historic.path" = historic)
  
  # Retrieve gcm file list
  gcm_files <- content_get(path = gcm, ...)
  
  # Retrieve normal file list
  normal_files <- content_get(path = normal, ...)
  
  # Retrieve historic file list
  historic_files <- content_get(path = historic, ...)
  
  # Do the actual download of files
  data_get(
    files = data.table::rbindlist(
      list(
        gcm_files,
        normal_files,
        historic_files
      )
    ),
    quiet = quiet
  )
  
  return(invisible(TRUE))
  
}

#' Process a list of files.
#' @param files A list of files.
#' @param quiet A logical. If `TRUE`, suppress status messages (if any), and the progress bar.
#' @details Each element must have a `url`, a `path` relative
#' to data path and a unique identifier `uid`.
data_get <- function(files, quiet = !interactive()) {
  data_download(
    url = files[["url"]],
    path = files[["path"]],
    uid = files[["uid"]],
  )
}

#' Download file to data path
#' @param url A character. Remote file location
#' @param path A character. Path where to save file relative to data path
#' @param uid A character. A unique identifier used to determine if file has been
#' updated or not.
#' @param quiet A logical. If `TRUE`, suppress status messages (if any), and the progress bar.
#' @importFrom utils download.file
data_download <- function(url, path, uid, quiet = !interactive()) {
  
  # If file uid  on local disk is the same, skip download
  existing <- uid_check(path, uid)
  if (all(existing)) {
    return(invisible(TRUE))
  }
  
  # Determine where to save the downloaded file
  dest <- file.path(data_path(), path[!existing])
  
  # Create directory if it does not already exist
  lapply(unique(dirname(dest)), dir.create, recursive = TRUE, showWarnings = FALSE)
  
  # Download files
  download.file(
    url = url[!existing],
    destfile = dest,
    method = "libcurl",
    quiet = quiet,
    mode = "wb"
  )
  
  # Update uid db
  uid_update(path[!existing], uid[!existing])
  
  # Decompress, precompute and cache data
  data_prepare()
  
  return(invisible(TRUE))
}

#' Return current package data path
#' @param ... Other parameters for `cache_ask`.
#' @details If cache is used, return the cache path. Otherwise, it returns
#' a temporary folder that will be used for this session only.
#' @export
data_path <- function(...) {
  use_cache <- getOption("climRpnw.session.cache.ask.response", default = cache_ask(...))
  if (use_cache) {
    return(cache_path())
  } else {
    # Create temp path and return it subsequently if it is already set for this session.
    path <- getOption("climRpnw.session.tmp.path", default = file.path(tempdir(), "climRpnw"))
    options("climRpnw.session.tmp.path" = path)
    return(path)
  }
}

#' Delete package local cache
#' @param ask A boolean. Ask before deleting files. Default to `interactive()`.
#' @export
data_delete <- function(ask = interactive()) {
  
  if (isTRUE(ask)) {
    response <- utils::askYesNo(
      paste0("The following files will be deleted :\n", paste0(list_data(), collapse = "\n")),
      prompts = c("Yes", "No", "Cancel")
    )
    if (is.na(response)) {
      stop("Cancelled by user.", call. = FALSE)
    } else if (!isTRUE(response)) {
      return(invisible(FALSE))
    }
  }
  
  # Remove cache directory
  unlink(data_path(ask = FALSE), recursive = TRUE)
  # Reset files list uid database
  uid_delete()
  # Unset options
  options(
    "climRpnw.session.tmp.path" = NULL,
    "climRpnw.gcm.path" = NULL,
    "climRpnw.normal.path" = NULL,
    "climRpnw.historic.path" = NULL,
    "climRpnw.session.cache.ask.response" = NULL
  )
  
  return(invisible(TRUE))
}

#' @noRd
data_check <- function() {
  if (!length(c(list_gcm(), list_normal()))) {
    response <- utils::askYesNo(
      "climRpnw could not find data to use for this package. Do you want to download it now?",
      prompts = c("Yes", "No", "Cancel")
    )
    if (!isTRUE(response)) {
      message("You can call `data_update()` whenever you want to obtain data.")
    } else {
      data_update()
    }
  }
}



#' List package local cache files
#' @param subdirectory A character. A subdirectory of `data_path()`. Restrict listing to only
#' this particular subdirectory. Use `getOption("climRpnw.gcm.path")` or
#' `getOption("climRpnw.normal.path")`.
#' @export
list_data <- function(subdirectory) {
  dir <- data_path()
  if (!missing(subdirectory)) {
    dir <- file.path(dir, subdirectory)
  }
  list.files(dir, recursive = TRUE, full.names = TRUE)
}

#' Prepare downloaded data for package use
#' @importFrom terra writeRaster rast
#' @export
data_prepare <- function() {

  normals <- list_normal()
  
  # Loop for each normal
  for (n in normals) {
    
    # Load normal files
    dir_normal <- file.path(
      data_path(),
      getOption("climRpnw.normal.path", default = "inputs_pkg/normal"),
      n
    )
    dir_dem <- file.path(dir_normal, "dem")
    
    nm <- data.table::fread(list.files(dir_normal, full.names = TRUE, pattern = "\\.csv"), header = TRUE)[["x"]]
    r <- terra::rast(list.files(dir_normal, full.names = TRUE, pattern = "\\.nc"))
    names(r) <- nm
    # Assuming Tmin and Tmax are in integer precision after applying a times 10 mod
    # to reduce storage size. This removes the mod to get original values back.
    r <- r / c(1L, 10L)[startsWith(names(r), "T") + 1L]
    
    nm <- data.table::fread(list.files(dir_dem, full.names = TRUE, pattern = "\\.csv"), header = TRUE)[["x"]]
    d <- terra::rast(list.files(dir_dem, full.names = TRUE, pattern = "\\.nc"))
    names(d) <- nm
    
    message("Computing lapse rates for normal: ", n)
    lr <- lapse_rate(
      normal = r,
      dem = d,
      NA_replace = TRUE,
      nthread = 2,
      rasterize = TRUE
    )
    
    message(
      "Saving uncompresseed normal + lapse rates + dem to: ",
      file.path(dir_normal, sprintf("%s.wlrdem.tif", n))
    )
    
    # Actual writing
    terra::writeRaster(
      c(r, lr, d),
      file.path(dir_normal, sprintf("%s.wlrdem.tif", n)),
      overwrite = TRUE,
      gdal="COMPRESS=NONE"
    )
    
  }
  
  gcms <- list_gcm()
  
  # Loop for each gcm
  for (g in gcms) {
    
    # Load normal files
    dir_gcm <- file.path(
      data_path(),
      getOption("climRpnw.gcm.path", default = "inputs_pkg/gcm"),
      g
    )
    
    nm <- data.table::rbindlist(
      lapply(
        list.files(dir_gcm, full.names = TRUE, pattern = "\\.csv"),
        data.table::fread,
        header = TRUE
      )
    )[["x"]]
    # Replace GCM climate variables names with official labels
    nm <- gsub("_pr_", "_PPT_", nm, fixed = TRUE)
    nm <- gsub("_tasmax_", "_Tmax_", nm, fixed = TRUE)
    nm <- gsub("_tasmin_", "_Tmin_", nm, fixed = TRUE)
        
    r <- terra::rast(list.files(dir_gcm, full.names = TRUE, pattern = "\\.nc"))
    names(r) <- nm
    
    ppt_layers <- grep("_PPT_",nm,fixed = TRUE)
    r_ppt <- r[[ppt_layers]]
    r_temp <- r[[-ppt_layers]]
    # Substract reference layers, only keep deltas, plus load in memory instead of disk
    message("Computing deltas for precipitation")
    # Find matching reference layer for each layer
    # Reference layers will match with themselves
    nm_ptt <- nm[ppt_layers]
    ref_layers <- grep("_reference_", nm_ptt, fixed = TRUE)
    names(ref_layers) <- gsub("^([^_]+_[^_]+_[^_]+_).*$", "\\1", nm_ptt[ref_layers])
    matching_ref <- ref_layers[gsub("^([^_]+_[^_]+_[^_]+_).*$", "\\1", nm_ptt)]
    
    # Reference layers positions
    # They will be used to avoid computing deltas of
    # reference layers with themselves
    uniq_ref <- sort(unique(matching_ref))
    
    # Substract reference layer, this takes a few seconds as all
    # data have to be loaded in memory from disk
    r_ppt <- r_ppt[[-uniq_ref]] / r_ppt[[matching_ref[-uniq_ref]]]
    
    message("Computing deltas for tmin and tmax")
    # Find matching reference layer for each layer
    # Reference layers will match with themselves
    nm_temp <- nm[-ppt_layers]
    ref_layers <- grep("_reference_", nm_temp, fixed = TRUE)
    names(ref_layers) <- gsub("^([^_]+_[^_]+_[^_]+_).*$", "\\1", nm_temp[ref_layers])
    matching_ref <- ref_layers[gsub("^([^_]+_[^_]+_[^_]+_).*$", "\\1", nm_temp)]
    
    # Reference layers positions
    # They will be used to avoid computing deltas of
    # reference layers with themselves
    uniq_ref <- sort(unique(matching_ref))
    
    # Substract reference layer, this takes a few seconds as all
    # data have to be loaded in memory from disk
    r_temp <- r_temp[[-uniq_ref]] - r_temp[[matching_ref[-uniq_ref]]]
    r <- c(r_ppt, r_temp)
    
    message(
      "Saving uncompressed gcm deltas to: ",
      file.path(dir_gcm, sprintf("gcmData.%s.deltas.tif", g))
    )
    
    # Actual writing
    terra::writeRaster(
      r,
      file.path(dir_gcm, sprintf("gcmData.%s.deltas.tif", g)),
      overwrite = TRUE,
      gdal="COMPRESS=NONE"
    )
    
  }
  
  histper <- list_historic()
  
  # Loop for each gcm
  for (h in histper) {
    
    # Load normal files
    dir_hist <- file.path(
      data_path(),
      getOption("climRpnw.historic.path", default = "inputs_pkg/historic"),
      h
    )
    
    # nm <- data.table::rbindlist(
    #   lapply(
    #     list.files(dir_hist, full.names = TRUE, pattern = "\\.csv"),
    #     data.table::fread,
    #     header = TRUE
    #   )
    # )[["x"]]
    
    # nm <- gsub("PPT","PPT_",nm)
    # nm <- gsub("Tmax","Tmax_",nm)
    # nm <- gsub("Tmin","Tmin_",nm)
    # r <- terra::rast(list.files(dir_hist, full.names = TRUE, pattern = "\\.tif"))
    # names(r) <- nm
    
    message(
      "Saving uncompressed historic deltas to: ",
      file.path(dir_hist, sprintf("gcmData.%s.deltas.tif", h))
    )
    
    # Actual writing
    # terra::writeRaster(
    #   r,
    #   file.path(dir_hist, sprintf("historicData.%s.deltas.tif", h)),
    #   overwrite = TRUE,
    #   gdal="COMPRESS=NONE"
    # )
    
  }
  
  message("Done")
  
  return(invisible(TRUE))
  
}
