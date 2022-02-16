data_path <- function() {
  use_cache <- getOption("climRpnw.session.cache.ask.response", default = cache_ask())
  if (use_cache) {
    return(cache_path())
  } else {
    path <- getOption("climRpnw.session.tmp.path")
    if (is.null(path)) {
      # Create temp path and return it subsequently
      path <- file.path(tempdir(), "climRpnw")
      options("climRpnw.session.tmp.path" = path)
    }
    return(path)
  }
}

data_gh <- function(files, quiet = interactive()) {
  invisible(
    vapply(
      files, 
      function(res) {
        data_download(
          url = res[["download_url"]],
          path = res[["path"]],
          sha = res[["sha"]],
          quiet = quiet
        )
      },
      logical(1)
    )
  )
}

data_download <- function(url, path, sha, quiet = interactive()) {
  # If file sha on local disk is the same, skip download
  if (!sha_check(path, sha)) {
    # Determine where to save the downloaded file
    outfile <- file.path(raster_path(), path)
    # Create directory if it does not already exist
    dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
    # Download file
    download.file(url = url, destfile = outfile, quiet = quiet)
    # Update sha db
    sha_update(path, sha)
  }
  return(invisible(TRUE))
}

data_update <- function(quiet = interactive()) {
  
  # Using original folder names
  org <- getOption("climRpnw.org", default = "bcgov")
  repo <- getOption("climRpnw.repo", default = "climR-pnw")
  dem <- getOption("climRpnw.dem.path", default = "inputs/digitalElevationModel")
  gcm <- getOption("climRpnw.gcm.path", default = "inputs/gcmData")
  normal <- getOption("climRpnw.normal.path", default = "inputs/Normal_1961_1990MP")
  
  # Retrieve digital elevation models file list
  dem_files <- content_get(org, repo, dem)
  
  # Retrieve gcm file list
  gcm_files <- content_get(org, repo, gcm)
  
  # Retrieve normal file list
  normal_files <- content_get(org, repo, normal)
  
  # Do the actual download
  data_gh(files = c(dem_files, gcm_files, normal_files), quiet = quiet)
  
  return(TRUE)
  
}

