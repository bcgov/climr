# A sha value is a unique identifier that git uses to distinguish files and the content.
# It is the results of applying a sha1 algorithm to the file content + metadata.
# It is returned by the GitHub API and will be used in this package to manage files update.

# Environment to store package files sha database while the package is in use.
#' @noRd
.climRpnw <- new.env()

# On load, instantiate either as new or from cache
#' @noRd
.onLoad <- function(libname, pkgname) {
  .climRpnw[["files_sha_db"]] <- character()
  if (file.exists(sha_db())) {
    .climRpnw[["files_sha_db"]] <- readRDS(sha_db())
  }
}

# Return path to cached files sha database
#' @noRd
sha_db <- function() {
  file.path(cache_path(), ".files_sha_db")
}

# Check if the provided sha is the same as the one stored in the files sha database object.
#' @noRd
sha_check <- function(path, sha) {
  sha %in% .climRpnw[["files_sha_db"]][path]
}

# Update the sha value of a file in the files sha database object.
#' @noRd
sha_update <- function(path, sha) {
  .climRpnw[["files_sha_db"]][path] <- sha
  if (cache_exists()) {
    saveRDS(climRpnw[["files_sha_db"]], sha_db())
  }
}

# Reset and delete the current files sha database object.
sha_delete <- function() {
  .climRpnw[["files_sha_db"]] <- character()
  unlink(sha_db())
}