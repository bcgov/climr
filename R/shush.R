#' Be quiet
#' @noRd
shush <- function(expr) {
  suppressMessages(
    try(expr, silent = TRUE),
    classes = c("message", "condition")
  )
}
