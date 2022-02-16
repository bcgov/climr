.climRpnw <- new.env()

.onLoad <- function(libname, pkgname) {
  .climRpnw[["files_sha_db"]] <- character()
  if (file.exists(sha_db())) {
    .climRpnw[["files_sha_db"]] <- readRDS(sha_db())
  }
}

sha_db <- function() {
  file.path(cache_path(), ".files_sha_db")
}

sha_check <- function(path, sha) {
  sha %in% .climRpnw[["files_sha_db"]][path]
}

sha_update <- function(path, sha) {
  .climRpnw[["files_sha_db"]][path] <- sha
  if (cache_exists()) {
    saveRDS(climRpnw[["files_sha_db"]], sha_db())
  }
}