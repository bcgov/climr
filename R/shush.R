#' Be quiet
#' Suppress messaging
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
