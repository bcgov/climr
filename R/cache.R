# Cache utility

#' Return package local cache path
#' @details By default, it uses [tools::R_user_dir()]. The cache location can be
#' set using the `climr.cache.path` option with `options("climr.cache.path" = "your_path")`.
#' @return The full path of the package local cache.
#' @importFrom tools R_user_dir
#' @export
cache_path <- function() {
  getOption("climr.cache.path", default = R_user_dir("climr", "cache"))
}

#' Check if package local cache exists
cache_exists <- function() {
  file.exists(cache_path())
}

#' Ask if user want to use local cache, otherwise use a temporary directory.
#' @param ask A boolean. Ask before deleting files. Default to `interactive()`.
#' @importFrom utils askYesNo
#' @noRd
cache_ask <- function(ask = interactive()) {
  # Only ask if cache does not already exists
  if (!cache_exists() && isTRUE(ask)) {
    response <- askYesNo(
      "Is it ok to cache climr raster files locally?",
      prompts = c("Yes", "No", "Cancel")
    )
    if (is.na(response)) {
      stop("Cancelled by user.", call. = FALSE)
    } else {
      # To avoid asking again in the same session
      options("climr.session.cache.ask.response" = response)
      return(response)
    }
  } else {
    return(TRUE)
  }
}
