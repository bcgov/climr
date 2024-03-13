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

#' Find bounding box of data
#'
#' @param in_xyz `data.table` (or `data.frame`) of points to downscale
#'  with columns "lon", "lat", "elev" and "id"
#' @return numeric vector. Bounding box coordinates with order ymax,ymin,xmax,xmin (e.g. `c(51, 50, -121, -122)`).
#' @export
get_bb <- function(in_xyz) {
  .checkXYZ(copy(in_xyz))
  thebb <- c(max(in_xyz[, "lat"]), min(in_xyz[, "lat"]), max(in_xyz[, "lon"]), min(in_xyz[, "lon"]))
  
  if (any(is.na(thebb))) {
    stop("Couldn't guess bounding box. Are there NA's in 'xyz'?")
  }
  
  .check_bb(thebb)
  
  return(thebb)
}


