#' Get content from source
#' @param path File path relative to repo root without starting `/`.
#' @param source A character. Name of the content source. Only supports
#' "github" as of now.
#' @param ... Extra arguments for `content_get` source dependent functions.
#' @return A list of elements, each with a `url`, a `path` relative
#' to data path and a unique identifier `uid`.
#' @rdname content_get
content_get <- function(path, source = c("github"), ...) {
  source <- match.arg(source)
  if (source == "github") {
    return(content_get_gh(path, ...))
  } else {
    return(content_get_void(path, ...))
  }
}

#' @param repo Repository name prefixed with organisation name and separated by a `/`.
#' Default to option "climRpnw.gh.repo" value if set, or "bcgov/climR-pnw". This is
#' the source root.
#' @param ref The name of the commit/branch/tag. Default NULL means it uses the repository’s default branch.
#' @return A list of metadata about files in a repository directory or the content of a file. See 
#' https://docs.github.com/en/rest/reference/repos#get-repository-content for details.
#' @importFrom gh gh
#' @rdname content_get
content_get_gh <- function(path,
                           repo = getOption("climRpnw.gh.repo", default = "bcgov/climR-pnw"),
                           ref = NULL,
                           ...) {
  
  res <- gh::gh(
    "GET /repos/{repo}/contents/{path}{ref}",
    repo = repo, path = path, ref = if (!is.null(ref)) {paste0("?ref=", ref)} else {""},
    .accept = "application/vnd.github.v3+json", .token = gh::gh_token()
  )
  
  # When file is a subdirectory, get content inside, otherwise extract download_url, path and sha.
  #
  # A sha value is a unique identifier that git uses to distinguish files and the content.
  # It is the results of applying a sha1 algorithm to the file content + metadata.
  # It is returned by the GitHub API and will be used in this package to manage files update.
  # This package use it as a uid for files.
  flag_for_removal <- integer()
  for (i in seq_len(length(res))) {
    file <- res[[i]]
    if (file[["type"]] == "dir") {
      res <- c(res, content_get_gh(file[["path"]], repo = repo, ref = ref))
      flag_for_removal <- c(flag_for_removal, i)
    } else {
      res[[i]] <- c(
        url = file[["download_url"]],
        path = file[["path"]],
        uid = file[["sha"]]
      )
    }
  }
  res[flag_for_removal] <- NULL
  
  return(res)
  
}

#' @rdname content_get
content_get_void <- function(...) {
  list(
    url = character(),
    uid = character(),
    path = character()
  )
}
