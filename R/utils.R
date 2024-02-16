#' Be quiet
#'
#' Suppresses messaging.
#'
#' @param expr expression to evaluate in quite mode.
#'
#' @noRd
shush <- function(expr) {
  suppressMessages(
    try(expr, silent = TRUE),
    classes = c("message", "condition")
  )
}


#' Check that new BB is in old BB
#'
#' @param newbb TODO
#' @param oldbb TODO
#'
#' @return logical
#' @noRd
is_in_bbox <- function(newbb, oldbb) {
  if (newbb[1] < oldbb[1] & newbb[2] > oldbb[2] & newbb[3] < oldbb[3] & newbb[4] > oldbb[4]) {
    TRUE
  } else {
    FALSE
  }
}
