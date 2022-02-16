#' Get file content from Github
#'
#' @param org Organization name 
#' @param repo Repo name
#' @param file File path relative to repo root without starting /
#' @param ref The name of the commit/branch/tag. Default: the repository’s default branch
#' @return File content as character
#' @importFrom gh gh
#' @export
content_get <- function(org, repo, file,ref = NULL) {
  gh::gh(
    "GET /repos/{org}/{repo}/contents/{file}{ref}",
    org = org, repo = repo, file = file, ref = if (!is.null(ref)) {paste0("?ref=", ref)} else {""},
    .accept = "application/vnd.github.v3+json", .token = gh::gh_token()
  )
}
