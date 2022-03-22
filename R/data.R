#' Update external package data
#' @param dem A character. Relative path from the source root to digital elevation model files folder.
#' Default to option value "climRpnw.dem.path" if set, or "inputs/digitalElevationModel".
#' @param gcm A character. Relative path from the source root to global circulation models files folder.
#' Default to option value "climRpnw.gcm.path" if set, or "inputs/gcmData".
#' @param normal A character. Relative path from the source root to base normal files folder.
#' Default to option value "climRpnw.normal.path" if set, or "inputs/Normal_1961_1990MP".
#' @param quiet A logical. If `TRUE`, suppress status messages (if any), and the progress bar.
#' @param ... Others parameters such as `source` or `repo` for content getting functions.
#' @details This package uses data that are too big to be included with sources.
#' Instead, data is downloaded, optionally cached, when you need to run functions.
#' @export
data_update <- function(
  dem = getOption("climRpnw.dem.path", default = "inputs/dem"),
  gcm = getOption("climRpnw.gcm.path", default = "inputs/gcm"),
  normal = getOption("climRpnw.normal.path", default = "inputs/normal"),
  quiet = !interactive(),
  ...) {
  
  # Reset options value if provided by user. They will be used to retrieve data by other functions.
  options("climRpnw.dem.path" = dem)
  options("climRpnw.gcm.path" = gcm)
  options("climRpnw.normal.path" = normal)
  
  # Retrieve digital elevation models file list
  dem_files <- content_get(path = dem, ...)
  
  # Retrieve gcm file list
  gcm_files <- content_get(path = gcm, ...)
  
  # Retrieve normal file list
  normal_files <- content_get(path = normal, ...)
  
  # Do the actual download of files
  data_get(files = c(dem_files, gcm_files, normal_files), quiet = quiet)
  
  return(invisible(TRUE))
  
}

#' Process a list of files.
#' @param files A list of files.
#' @param quiet A logical. If `TRUE`, suppress status messages (if any), and the progress bar.
#' @details Each element must have a `url`, a `path` relative
#' to data path and a unique identifier `uid`.
data_get <- function(files, quiet = !interactive()) {
  invisible(
    vapply(
      files, 
      function(file) {
        data_download(
          url = file[["url"]],
          path = file[["path"]],
          uid = file[["uid"]],
          quiet = quiet
        )
      },
      logical(1)
    )
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
  if (!uid_check(path, uid)) {
    # Determine where to save the downloaded file
    outfile <- file.path(data_path(), path)
    # Create directory if it does not already exist
    dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
    # Download file
    download.file(url = url, destfile = outfile, quiet = quiet)
    # Update uid db
    uid_update(path, uid)
  }
  return(invisible(TRUE))
}

#' Return current package data path
#' @details If cache is used, return the cache path. Otherwise, it returns
#' a temporary folder that will be used for this session only.
#' @export
data_path <- function() {
  use_cache <- getOption("climRpnw.session.cache.ask.response", default = cache_ask())
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
    )
    if (is.na(response)) {
      stop("Cancelled by user.", call. = FALSE)
    } else if (!isTRUE(response)) {
      return(invisible(FALSE))
    }
  }
  
  # Remove cache directory
  unlink(data_path(), recursive = TRUE)
  # Reset files list uid database
  uid_delete()
  # Unset options
  options(
    "climRpnw.session.tmp.path" = NULL,
    "climRpnw.dem.path" = NULL,
    "climRpnw.gcm.path" = NULL,
    "climRpnw.normal.path" = NULL
  )
  
  return(invisible(TRUE))
}

#' List package local cache files
#' @param subdirectory A character. A subdirectory of `data_path()`. Restrict listing to only
#' this particular subdirectory. Use `getOption("climRpnw.dem.path")`,
#' `getOption("climRpnw.gcm.path")` or `getOption("climRpnw.normal.path")`.
#' @export
list_data <- function(subdirectory) {
  dir <- data_path()
  if (!missing(subdirectory)) {
    dir <- file.path(dir, subdirectory)
  }
  list.files(dir, recursive = TRUE, full.names = TRUE)
}
