# On load, instantiate either as new or from cache
#' @noRd
.onLoad <- function(libname, pkgname) {
  .climRpnw[["files_uid_db"]] <- character()
  if (file.exists(uid_db())) {
    .climRpnw[["files_uid_db"]] <- readRDS(uid_db())
  }
}

#' Climate variables definition
"variables"