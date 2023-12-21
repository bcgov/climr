# Cache utilities

#' Return package local cache path
#' @details By default, it uses [tools::R_user_dir()]. The cache location can be
#' set using the `climr.cache.path` option with `options("climr.cache.path" = "your_path")`.
#' 
#' @export
#' 
#' @return character. The full path of the package local cache.
#' 
#' @importFrom tools R_user_dir
cache_path <- function() {
  getOption("climr.cache.path", default = R_user_dir("climr", "cache"))
}

#' Check if package local cache exists
#' 
#' @return logical
cache_exists <- function() {
  file.exists(cache_path())
}

#' Ask if user want to use local cache, otherwise use a temporary directory.
#' 
#' @noRd
#' @param ask A boolean. Ask before deleting files. Default to `interactive()`.
#' 
#' @return logical
#' 
#' @importFrom utils askYesNo
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


#' Clear the package's local cache path
#' 
#' Attempts to delete all folder/files in `cache_path()`.
#' 
#' @param what character. Which data folders should be cleared?
#'    Accepts "normal", "gcm" or both.
#' 
#' @details
#'   It may fail if R has no permission to delete files/folders
#'   in the `cache_path()` directory
#' 
#' @return NULL
#' @export
cache_clear <- function(what = c("gcm", "normal", "historic")) {
  browser()  ## TODO: check historic folder
  match.arg(what, several.ok = TRUE)
  
  fileList <- list.files(cache_path())
  fileList <- fileList[fileList %in% what]
  fileList <- unlist(sapply(file.path(cache_path(), fileList), list.files, 
                            recursive = TRUE, full.names = TRUE,
                            simplify = FALSE, USE.NAMES = FALSE))
  unlink(fileList, recursive = TRUE, force = TRUE)

  fileList2 <- list.files(cache_path(), recursive = TRUE, full.names = TRUE)
  
  if (any(fileList %in% fileList2)) {
    warning("Unable to fully clear the cache. This may be due to restricted permissions.")
  }
}
