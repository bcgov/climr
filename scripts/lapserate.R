if (!requireNamespace("raster", quietly = TRUE)) {
  install.packages("raster")
}

if (!requireNamespace("parallel", quietly = TRUE)) {
  install.packages("parallel")
}

library(raster)
library(parallel)

# This is going to be ~slow, optimizations will be done in C/C++
# once we validate and if required. Although there isn't much to
# be optimized beside multithreading it.

# lapse rate ----

dem <- raster::raster("./inputs/digitalElevationModel/dem2_WNA.asc")

# do it for all 36 variables
targets <- list(
  Tmax01 = raster::raster("./inputs/Normal_1961_1990MP/Tmax01.asc"),
  Tmax02 = raster::raster("./inputs/Normal_1961_1990MP/Tmax02.asc"),
  Tmax03 = raster::raster("./inputs/Normal_1961_1990MP/Tmax03.asc"),
  Tmax04 = raster::raster("./inputs/Normal_1961_1990MP/Tmax04.asc"),
  Tmax05 = raster::raster("./inputs/Normal_1961_1990MP/Tmax05.asc"),
  Tmax06 = raster::raster("./inputs/Normal_1961_1990MP/Tmax06.asc"),
  Tmax07 = raster::raster("./inputs/Normal_1961_1990MP/Tmax07.asc"),
  Tmax08 = raster::raster("./inputs/Normal_1961_1990MP/Tmax08.asc"),
  Tmax09 = raster::raster("./inputs/Normal_1961_1990MP/Tmax09.asc"),
  Tmax10 = raster::raster("./inputs/Normal_1961_1990MP/Tmax10.asc"),
  Tmax11 = raster::raster("./inputs/Normal_1961_1990MP/Tmax11.asc"),
  Tmax12 = raster::raster("./inputs/Normal_1961_1990MP/Tmax12.asc"),
  Tmin01 = raster::raster("./inputs/Normal_1961_1990MP/Tmin01.asc"),
  Tmin02 = raster::raster("./inputs/Normal_1961_1990MP/Tmin02.asc"),
  Tmin03 = raster::raster("./inputs/Normal_1961_1990MP/Tmin03.asc"),
  Tmin04 = raster::raster("./inputs/Normal_1961_1990MP/Tmin04.asc"),
  Tmin05 = raster::raster("./inputs/Normal_1961_1990MP/Tmin05.asc"),
  Tmin06 = raster::raster("./inputs/Normal_1961_1990MP/Tmin06.asc"),
  Tmin07 = raster::raster("./inputs/Normal_1961_1990MP/Tmin07.asc"),
  Tmin08 = raster::raster("./inputs/Normal_1961_1990MP/Tmin08.asc"),
  Tmin09 = raster::raster("./inputs/Normal_1961_1990MP/Tmin09.asc"),
  Tmin10 = raster::raster("./inputs/Normal_1961_1990MP/Tmin10.asc"),
  Tmin11 = raster::raster("./inputs/Normal_1961_1990MP/Tmin11.asc"),
  Tmin12 = raster::raster("./inputs/Normal_1961_1990MP/Tmin12.asc"),
  PPT01  = raster::raster("./inputs/Normal_1961_1990MP/PPT01.asc"),
  PPT02  = raster::raster("./inputs/Normal_1961_1990MP/PPT02.asc"),
  PPT03  = raster::raster("./inputs/Normal_1961_1990MP/PPT03.asc"),
  PPT04  = raster::raster("./inputs/Normal_1961_1990MP/PPT04.asc"),
  PPT05  = raster::raster("./inputs/Normal_1961_1990MP/PPT05.asc"),
  PPT06  = raster::raster("./inputs/Normal_1961_1990MP/PPT06.asc"),
  PPT07  = raster::raster("./inputs/Normal_1961_1990MP/PPT07.asc"),
  PPT08  = raster::raster("./inputs/Normal_1961_1990MP/PPT08.asc"),
  PPT09  = raster::raster("./inputs/Normal_1961_1990MP/PPT09.asc"),
  PPT10  = raster::raster("./inputs/Normal_1961_1990MP/PPT10.asc"),
  PPT11  = raster::raster("./inputs/Normal_1961_1990MP/PPT11.asc"),
  PPT12  = raster::raster("./inputs/Normal_1961_1990MP/PPT12.asc")
)

# Recycle the borders of matrix to expand the matrix by one cell in each directions
# It will help later when we use the offset method.
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

# This returns a single matrix containing the sums of matching row, cell
# in the list of matrices.
#' @param x A list of matrices of the same dimensions
Σ <- function(x) {
  Reduce(`+`, x)
}

# This returns a list of matrices corresponding to the products of
# individual matching row, cell from each x matrices and y matrices
#' @param x A list of matrices of the same dimensions.
#' @param y A list of matrices of the same dimensions as x and the same
#' length as x.
Π <- function(x, y) {
  mapply(`*`, x, y, SIMPLIFY = FALSE)
}

# This returns a list of matrices corresponding to the differences of
# individual matching row, cell from each x matrices and y matrices
#' @param x A list of matrices of the same dimensions.
#' @param y A list of matrices of the same dimensions as x and the same
#' length as x.
Δ <- function(x, y) {
  mapply(`-`, x, y, SIMPLIFY = FALSE)
}

# This returns a list of matrices after applying exponent `exp` to each
# matrix, row, cell
#' @param x A list of matrices.
#' @param exp A numeric vector of length 1.
sup <- function(x, exp) {
  lapply(x, `^`, exp)
}

# This returns the fitted simple linear regression values using 
# a pre-calculated β matrix.
#' @param x A list of matrices of the same dimensions.
#' @param β A matrix of the same dimensions as a matrix in x.
fitted <- function(x, β) {
  lapply(x, function(x) x*β)
}

# This compute a list of matrices. Each matrix represents differences between
# surrounding cells and the reference cells.
# It uses an offset approach to compute surrounding cells in each directions.
#' @param mat A matrix previously ran through `recycle_borders`
#' @param nr The number of rows in the original matrix used by `recycle_borders`
#' @param nc The number of columns in the original matrix used by `recycle_borders`
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

# Lapse rate computation
#' @param dem A digital elevation model rasterLayer.
#' @param targets A list of target raster Layers to compute lapse rates for.
#' @param replace_NaN A boolean. Should NaN lapse rates be replaced by zeros. Default to TRUE.
#' @param as_raster A boolean. Should the results be returned as a list of raster. Default to FALSE.
#' @param use_parallel A boolean. Should parallel::mclapply be used. Default to FALSE.
#' with the same extend as the digital elevation model.
#' @details Formulas
#' Simple linear regression without the intercept term
#' β = Σxy / Σx²
#' mss = Σ(xβ)², sum of squared fitted values
#' rss = Σε², sum of squared (y minus fitted), sum of absolute errors
#' R² = mss / (mss + rss)
#' Lapse rate = βR²
#' @return Lapse rate values.
lapse_rate <- function(dem, targets, replace_NaN = TRUE, as_raster = FALSE, use_parallel = FALSE) {
  
  if (!all(vapply(targets, raster::compareRaster, logical(1), dem))) {
    stop("Target rasters do not share the same extent, rowcol, crs and rotation as the digital elevation model raster.")
  }
  
  # Compute everything related to the dem and independant of targets
  x <- as.matrix(dem)
  nr <- nrow(x)
  nc <- ncol(x)
  # Expand and recycle borders
  x <- recycle_borders(x, nr, nc)
  # Compute surrounding cells deltas
  x <- deltas(x, nr, nc)
  # Number of surrounding cells
  n <- length(x)
  # Sums of x squared
  sum_xx <- Σ(Π(x,x))
  
  # For the lapse rate, x is the elevation, and y is the target
  lapse_rate_redux <- function(target, x, nr, nc, n, sum_xx, replace_NaN, as_raster) {
    
    y <- as.matrix(target)
    # Expand and recycle borders
    y <- recycle_borders(y, nr, nc)
    # Compute surrounding cells deltas
    y <- deltas(y, nr, nc)
    # This is the regression coefficient matrix
    β <- Σ(Π(x,y)) / sum_xx
    # We need the fitted values to compute the
    # coefficient of determination
    f <- fitted(x, β)
    # We use the same approach as stats::summary.lm
    # applied to a list matrices
    mss <- Σ(sup(f,2))
    rss <- Σ(sup(Δ(y,f),2))
    # We can combine the resulting matrices to get the
    # coefficient of determination and multiply by β
    lapse_rate <- β * mss / (mss + rss)
  
    if (isTRUE(replace_NaN)) {
      lapse_rate[is.nan(lapse_rate)] <- 0L  
    }
    
    # And we can return the lapse rate as a raster
    if (isTRUE(as_raster)) {
      lapse_rate <- raster::raster(lapse_rate)
      attr(lapse_rate, "extent") <- attr(dem, "extent")
      # plot(lapse_rate)
    }
  
    return(lapse_rate)
    
  }
  
  if (isTRUE(use_parallel)) {
    options("mc.cores" = min(4L, parallel::detectCores(logical = FALSE) - 1L))
    func <- parallel::mclapply
  } else {
    func <- lapply
  }
  
  return(func(targets, lapse_rate_redux, x, nr, nc, n, sum_xx, replace_NaN, as_raster))
  
}

lapse_rates <- lapse_rate(dem, targets, use_parallel = TRUE, as_raster = TRUE)

for (r in seq_len(length(lapse_rates))) {
  raster::writeRaster(
    x = as.integer(lapse_rates[[r]] * 1000L),
    filename = sprintf("./inputs/lapseRates/LRx1000_%s.asc", names(lapse_rates)[r]),
    overwrite = TRUE,
    format="ascii",
    options=c("INTERLEAVE=BAND","COMPRESS=LZW")
  )
}
