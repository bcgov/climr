# Cache utilities

#' Return package local cache path
#'
#' @details By default, it uses [tools::R_user_dir()].
#' The cache location can be set using the `CLIMR_CACHE_PATH` environment variable,
#' or using the `climr.cache.path` option. The value set via the option takes precedence.
#'
#' @return character. The full path of the package local cache.
#' 
#' @examples
#' \dontshow{
#'   ## record previous settings for post-example cleanup
#'   prev_envvar <- Sys.getenv("CLIMR_CACHE_PATH")
#'   prev_opts <- getOption("climr.cache.path")
#' }
#' cache_path() ## the current cache path
#' 
#' ## set via environment variable
#' Sys.setenv(CLIMR_CACHE_PATH = file.path(tempdir(), "climr-cache"))
#' cache_path() ## e.g., /tmp/Rtmpbo9VC7/climr-cache
#' 
#' ## set via option (takes precedence over envvar)
#' options(climr.cache.path = file.path(tempdir(), "climr-cache-2"))
#' cache_path() ## e.g., /tmp/Rtmpbo9VC7/climr-cache-2
#' 
#' \dontshow{
#'   ## unset/cleanup
#'   if (nzchar(prev_envvar)) {
#'     Sys.unsetenv("CLIMR_CACHE_PATH")
#'   } else {
#'     Sys.setenv(CLIMR_CACHE_PATH = prev_envvar)
#'   }
#'   options(prev_opts)
#' }
#'
#' @export
#' @rdname Caching
#' @importFrom tools R_user_dir
cache_path <- function() {
  opt_name <- "climr.cache.path"
  envvar_name <- toupper(opt_name) %>% gsub("[.]", "_", x = .)
  envvar_val <- Sys.getenv(envvar_name, unset = "")
  
  if (nzchar(envvar_val)) {
    opt_val <- envvar_val
  } else {
    opt_val <- R_user_dir("climr", "cache") ## default
  }
  getOption(opt_name, default = opt_val)
}

#' Check if package local cache exists
#'
#' @return logical.
#' @noRd
cache_exists <- function() {
  file.exists(cache_path())
}

#' Ask if user want to use local cache, otherwise use a temporary directory.
#'
#' @noRd
#' @param ask logical. Ask before deleting files. Default to `interactive()`.
#'
#' @return logical.
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
#' Attempts to delete all folder/files in `cache_path()`.
#'
#' @param what character. Which data folders should be cleared?
#'    Accepts "reference", "gcms" or both.
#'
#' @details
#'   It may fail if R has no permission to delete files/folders
#'   in the `cache_path()` directory
#' @seealso [cache_path()]
#'
#' @return TRUE or FALSE depending on whether cache was cleared successfully
#'   or not.
#' @rdname Caching
#' @export
cache_clear <- function(what = c("gcms", "gcmts", "gcmhist", "reference", "obs", "obs_ts")) {
  what <- match.arg(what, several.ok = TRUE)

  fileList <- list.files(cache_path())
  fileList <- fileList[fileList %in% what]
  fileList <- unlist(sapply(file.path(cache_path(), fileList),
    FUN = function(p) {
      fileList <- list.files(p, recursive = TRUE, full.names = TRUE)
      folderList <- list.dirs(p, recursive = TRUE, full.names = TRUE)
      c(fileList, folderList)
    },
    simplify = FALSE, USE.NAMES = FALSE
  ))
  unlink(fileList, recursive = TRUE, force = TRUE)

  fileList2 <- list.files(cache_path(), recursive = TRUE, full.names = TRUE)

  if (any(fileList %in% fileList2)) {
    warning("Unable to fully clear the cache. This may be due to restricted permissions.")
    return(FALSE)
  } else {
    return(TRUE)
  }
}
