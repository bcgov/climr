data_update <- function(
  repo = getOption("climRpnw.org", default = "bcgov/climR-pnw"),
  dem = getOption("climRpnw.dem.path", default = "inputs/digitalElevationModel"),
  gcm = getOption("climRpnw.gcm.path", default = "inputs/gcmData"),
  normal = getOption("climRpnw.normal.path", default = "inputs/Normal_1961_1990MP"),
  quiet = interactive()) {
  
  # Reset options value if provided by user. They will be used to retrieve data by other functions.
  options("climRpnw.dem.path" = dem)
  options("climRpnw.gcm.path" = gcm)
  options("climRpnw.normal.path" = normal)
  
  # Retrieve digital elevation models file list
  dem_files <- content_get(repo = repo, path = dem)
  
  # Retrieve gcm file list
  gcm_files <- content_get(repo = repo, path = gcm)
  
  # Retrieve normal file list
  normal_files <- content_get(repo = repo, path = normal)
  
  # Do the actual download of files
  data_gh(files = c(dem_files, gcm_files, normal_files), quiet = quiet)
  
  return(TRUE)
  
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
    outfile <- file.path(data_path(), path)
    # Create directory if it does not already exist
    dir.create(dirname(outfile), recursive = TRUE, showWarnings = FALSE)
    # Download file
    download.file(url = url, destfile = outfile, quiet = quiet)
    # Update sha db
    sha_update(path, sha)
  }
  return(invisible(TRUE))
}

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
