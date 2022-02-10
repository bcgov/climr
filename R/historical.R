historical(target) <- function() {
  
}

historical_build <- function(objects, dem) {
  #
  if (!all(vapply(target, compare_, logical(1), dem))) {
    stop("Targets do not share the same extent, number of rows and columns, projection, resolution and origin as the referenced digital elevation model.")
  }
  attr(objects, "dem") <- dem
  objects
}

historical_available <- function() {
  
}