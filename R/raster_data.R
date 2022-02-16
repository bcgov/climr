raster_path <- function() {
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




cache_gh <- function(gh_res) {
  
  cache_file(file, sha, quiet)
}

cache_file <- function(file, sha1, path) {
  
}

cache_local <- function(quiet = FALSE) {
  
  # Using original folder names
  org <- getOption("climRpnw.org", default = "bcgov")
  repo <- getOption("climRpnw.repo", default = "climR-pnw")
  dem <- getOption("climRpnw.dem.path", default = "inputs/digitalElevationModel")
  gcm <- getOption("climRpnw.gcm.path", default = "inputs/gcmData")
  normal <- getOption("climRpnw.normal.path", default = "inputs/Normal_1961_1990MP")
  
  # Cache digital elevation models
  content_get(org, repo, dem)
  
  # Cache gcm Data
  
  # Cache normal
  
  return(TRUE)
}


# Retrieve from github
# Local cache management of github files
# In memory cache management with memoise?

