# Preload all Suggests so that examples don't take tons of time
requireNamespace("parallel", quietly = TRUE)

## utility function
isInRange <- function(x, y) {
  if (!inherits(x, c("numeric", "integer"))) {
    stop("x must be numeric/integer")
  }
  if (!inherits(y, c("numeric", "integer"))) {
    stop("y must be numeric/integer")
  }
  
  (min(x) >= min(y)) & (max(x) <= max(y))
}