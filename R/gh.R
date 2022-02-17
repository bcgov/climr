#' Get file content from Github
#'
#' @param repo Repository name prefixed with organisation name and separated by a `/`.
#' @param file File path relative to repo root without starting `/`.
#' @param ref The name of the commit/branch/tag. Default: the repository’s default branch.
#' @return A list of metadata about files in a repository directory or the content of a file. See 
#' https://docs.github.com/en/rest/reference/repos#get-repository-content for details.
#' @importFrom gh gh
#' @export
content_get <- function(repo, path, ref = NULL) {
  gh::gh(
    "GET /repos/{repo}/contents/{file}{ref}",
    org = org, repo = repo, file = file, ref = if (!is.null(ref)) {paste0("?ref=", ref)} else {""},
    .accept = "application/vnd.github.v3+json", .token = gh::gh_token()
  )
}
