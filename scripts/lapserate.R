library(raster)

# This is going to be ~slow, optimization will be done in C/C++ once we validate

# lapse rate ----

dem <- raster("./inputs/digitalElevationModel/dem2_WNA.asc")

# do it for one of the 36 variables
tmax01 <- raster("./inputs/Normal_1961_1990MP/Tmax01.asc")

# Recycle the borders of matrix to expand the matrix by one cell in each directions
recycle_borders <- function(mat, nr, nc) {
  
  res <- matrix(nrow = nr + 2L, ncol = nc + 2L)
  
  # Fill the representation starting with the original data in the center
  res[2L:(nr + 1L), 2L:(nc + 1L)] <- mat
  
  # Recycle the borders
  # North
  res[1L, 2L:(nc + 1L)] <- mat[1L,]
  # South
  res[(nr + 2L), 2L:(nc + 1L)] <- mat[nr,]
  # West
  res[2L:(nr + 1L), 1L] <- mat[,1L]
  # East
  res[2L:(nr + 1L), nc + 2L] <- mat[,nc]
  
  # Recycle the corners
  # North-West
  res[1L, 1L] <- mat[1L, 1L]
  # North-East
  res[1L, (nc + 2L)] <- mat[1L, nc]
  # South-West
  res[(nr + 2L), 1L] <- mat[nr, 1L]
  # South-East
  res[(nr + 2L), (nc + 2L)] <- mat[nr, nc]
  
  return(res)
  
}

Σ <- function(x) {
  Reduce(`+`, x)
}

Π <- function(x, y) {
  mapply(`*`, x, y, SIMPLIFY = FALSE)
}

Δ <- function(x, y) {
  mapply(`-`, x, y, SIMPLIFY = FALSE)
}

sup <- function(x, exp) {
  lapply(x, `^`, exp)
}

fitted <- function(x, β) {
  lapply(x, function(x) x*β)
}

# Compute list of cells delta matrix values from reference matrix
# Offset approach until we cover all surrounding cells
deltas <- function(mat, nr, nc) {
  ref <- mat[c(-1L,-(nr+2L)), c(-1L,-(nc+2L))]
  return(
    list(
      northwest = mat[-(nr + 1L):-(nr + 2L), -(nc + 1L):-(nc + 2L)] - ref,
      north = mat[-(nr + 1L):-(nr + 2L), c(-1L,-(nc + 2L))] - ref,
      northeast = mat[-(nr + 1L):-(nr + 2L), -1L:-2L] - ref,
      east = mat[c(-1L,-(nr + 2L)), -1L:-2L] - ref,
      southeast = mat[-1L:-2L, -1L:-2L] - ref,
      south = mat[-1L:-2L, c(-1L,-(nc + 2L))] - ref,
      southwest = mat[-1L:-2L, -(nc + 1L):-(nc + 2L)] - ref,
      west = mat[c(-1L,-(nr + 2L)), -(nc + 1L):-(nc + 2L)] - ref
    )
  )
}

lapse_rate <- function(dem, target, as_raster = FALSE) {
  if (!raster::compareRaster(dem, target)) {
    stop("Rasters do not share the same extent, rowcol, crs and rotation.")
  }
  x <- as.matrix(dem)
  y <- as.matrix(target)
  
  # For the lapse rate, x is the elevation, and y is the target
  nr <- nrow(x)
  nc <- ncol(x)
  
  # Create a representation to account for raster borders. Recycle.
  x <- recycle_borders(x, nr, nc)
  y <- recycle_borders(y, nr, nc)
  
  # Formulas
  # Simple linear regression without the intercept term
  
  # β = Σxy / Σx²
  # mss = Σ(xβ)², sum of squared fitted values
  # rss = Σε², sum of squared (y minus fitted), sum of absolute errors
  # R² = mss / (mss + rss)
  # Lapse rate = βR²
  
  # Regression is done on the difference between
  # reference cells and surrounding cells
  x <- deltas(x, nr, nc)
  y <- deltas(y, nr, nc)
  # Number of surrounding cells, should be 8  
  n <- length(x)
  # This is the regression coefficient matrix
  β <- Σ(Π(x,y)) / Σ(Π(x,x))
  # We need the fitted values to compute the
  # coefficient of determination
  f <- fitted(x, β)
  # We use the same approach as stats::summary.lm
  # applied to a list matrices
  mss <- Σ(sup(f,2))
  rss <- Σ(sup(Δ(y,f),2))
  # We can combine the resulting matrices to 
  # get the coefficient of determination and multiply
  # by β
  lapse_rate <- β * mss / (mss + rss)
  
  # ASK : Colin, some regressions produce NaN lapse rate
  # An example
  # lapse_rate[100,1856:1864]
  # matrix(dem[99:101, 1855:1865], nrow=3)
  # matrix(target[99:101, 1855:1865], nrow=3)
  # Seems to be because it is regressing on 0 (all surrounding cells deltas are 0s)
  # I will be replacing them with 0s
  lapse_rate[is.nan(lapse_rate)] <- 0L
  
  # And we can return the lapse rate as a raster
  if (isTRUE(as_raster)) {
    lapse_rate <- raster::raster(lapse_rate)
    attr(lapse_rate, "extent") <- attr(dem, "extent")
    # plot(lapse_rate)
  }
  
  return(lapse_rate)
  
}