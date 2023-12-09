#' Environment to store package files uid database while the package is in use.
#' @noRd
.climr <- new.env()

#' Return path to cached files uid database
#' @noRd
uid_db <- function() {
  file.path(cache_path(), ".files_uid_db")
}

#' Check if the provided uid is the same as the one stored in the files uid database object.
#' @noRd
uid_check <- function(path, uid) {
  uid %in% .climr[["files_uid_db"]][path]
}

#' Update the uid value of a file in the files uid database object.
#' @noRd
uid_update <- function(path, uid) {
  .climr[["files_uid_db"]][path] <- uid
  if (cache_exists()) {
    saveRDS(.climr[["files_uid_db"]], uid_db())
  }
}

#' Reset and delete the current files uid database object.
#' @noRd
uid_delete <- function() {
  .climr[["files_uid_db"]] <- character()
  unlink(uid_db())
}
