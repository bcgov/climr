# Cache utility

#' Return package local cache path
#' @importFrom tools R_user_dir
#' @rdname cache
#' @details By default, it uses `tools::R_user_dir`. The cache location can be
#' set using the `climRpnw.cache.path` option with `options("climRpnw.cache.path" = "your_path")`.
#' @return The full path of the package local cache.
#' @export
cache_path <- function() {
  getOption("climRpnw.cache.path", default = tools::R_user_dir("climRpnw", "cache"))
}

#' Delete package local cache
#' @rdname cache
#' @export
cache_delete <- function() {
  unlink(cache_dir(), recursive = TRUE)
}

#' Check if package local cache exists
#' @rdname cache
#' @export
cache_exists <- function() {
  file.exists(cache_path())
}

#' List package local cache files
#' @rdname cache
#' @export
cache_list <- function() {
  list.files(cache_path(), recursive = TRUE, full.names = TRUE)
}

#' Ask if user want to use local cache, otherwise use a temporary directory.
#' @importFrom utils askYesNo
#' @noRd
cache_ask <- function() {
  # Only ask if cache does not already exists
  if (!cache_exists()) {
    response <- utils::askYesNo("Is it ok to cache climRpnw raster files locally?")
    if (is.na(response)) {
      stop("Cancelled by user.", call. = FALSE)
    } else {
      # To avoid asking again in the same session
      options("climRpnw.session.cache.ask.response" = response)
      return(response)
    }
  } else {
    return(TRUE)
  }
}
