# Recycle the borders of matrix to expand the matrix by one cell in each directions
# It will help later when we use the offset method.
#' @param mat A matrix to recycle borders from.
#' @param nr The number of rows in the matrix.
#' @param nc The number of columns in the matrix.
#' @noRd
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
#' @noRd
Σ <- function(x) {
  Reduce(`+`, x)
}

# This returns a list of matrices corresponding to the products of
# individual matching row, cell from each x matrices and y matrices
#' @param x A list of matrices of the same dimensions.
#' @param y A list of matrices of the same dimensions as x and the same
#' length as x.
#' @noRd
Π <- function(x, y) {
  mapply(`*`, x, y, SIMPLIFY = FALSE)
}

# This returns a list of matrices corresponding to the differences of
# individual matching row, cell from each x matrices and y matrices
#' @param x A list of matrices of the same dimensions.
#' @param y A list of matrices of the same dimensions as x and the same
#' length as x.
#' @noRd
Δ <- function(x, y) {
  mapply(`-`, x, y, SIMPLIFY = FALSE)
}

# This returns a list of matrices after applying exponent `exp` to each
# matrix, row, cell
#' @param x A list of matrices.
#' @param exp A numeric vector of length 1.
#' @noRd
sup <- function(x, exp) {
  lapply(x, `^`, exp)
}

# This returns the fitted simple linear regression values using 
# a pre-calculated β matrix.
#' @param x A list of matrices of the same dimensions.
#' @param β A matrix of the same dimensions as a matrix in x.
#' @noRd
fitted <- function(x, β) {
  lapply(x, function(x) x*β)
}

# This compute a list of matrices. Each matrix represents differences between
# surrounding cells and the reference cells.
# It uses an offset approach to compute surrounding cells in each directions.
#' @param mat A matrix previously ran through `recycle_borders`
#' @param nr The number of rows in the original matrix used by `recycle_borders`
#' @param nc The number of columns in the original matrix used by `recycle_borders`
#' @noRd
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

#' Lapse rate computation
#' @param targets A list of target SpatRaster object to compute lapse rates for.
#' @param dem A digital elevation model SpatRaster.
#' @param replace_NaN A boolean. Should NaN lapse rates be replaced by zeros. Default to TRUE.
#' @param use_parallel A boolean. Should parallel::mclapply be used. Default to FALSE.
#' @param rasterize Return an object of the same class as targets and dem.
#' with the same extend as the digital elevation model.
#' @details Formulas
#' Simple linear regression without the intercept term
#' β = Σxy / Σx²
#' mss = Σ(xβ)², sum of squared fitted values
#' rss = Σε², sum of squared (y minus fitted), sum of absolute errors
#' R² = mss / (mss + rss)
#' Lapse rate = βR²
#' @return Lapse rate values.
#' @import terra
#' @importFrom parallel detectCores mclapply
#' @export
setGeneric("lapse_rate",
  def = function(targets, dem, replace_NaN = TRUE, use_parallel = FALSE, rasterize = TRUE) standardGeneric("lapse_rate")
)

#' @noRd
#' @usage NULL
lapse_rate_terra <- function(targets, dem, replace_NaN = TRUE, use_parallel = FALSE, rasterize = TRUE) {
  
  if (!all(vapply(targets, terra::compareGeom, logical(1), dem))) {
    stop("Target SpatRaster do not share the same extent, number of rows and columns, projection, resolution and origin as the digital elevation model SpatRaster.")
  }
  
  # Compute everything related to the dem and independant of targets
  x <- matrix(as.numeric(dem), byrow = TRUE, nrow = terra::nrow(dem))
  nr <- terra::nrow(dem)
  nc <- terra::ncol(dem)
  # Expand and recycle borders
  x <- recycle_borders(x, nr, nc)
  # Compute surrounding cells deltas
  x <- deltas(x, nr, nc)
  # Number of surrounding cells
  n <- length(x)
  # Sums of x squared
  sum_xx <- Σ(sup(x,2))
  
  # For the lapse rate, x is the elevation, and y is the target
  lapse_rate_redux <- function(target, x, nr, nc, n, sum_xx, replace_NaN) {
    
    y <- matrix(as.numeric(target), byrow = TRUE, nrow = nr)
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
    
    # And we can return the lapse rate
    return(lapse_rate)
    
  }
  
  if (isTRUE(use_parallel)) {
    options("mc.cores" = min(4L, parallel::detectCores(logical = FALSE) - 1L))
    func <- parallel::mclapply
  } else {
    func <- lapply
  }
  
  res <- func(targets, lapse_rate_redux, x, nr, nc, n, sum_xx, replace_NaN)
  
  if (isTRUE(rasterize)) {
    res <- terra::rast(lapply(res, terra::rast, extent = terra::ext(dem)))
  }
  
  return(res)
  
}

#' @rdname lapse_rate
#' @export
setMethod("lapse_rate", signature("list", "SpatRaster"), lapse_rate_terra)

#' @noRd
#' @usage NULL
lapse_rate_raster <- function(targets, dem, replace_NaN = TRUE, use_parallel = FALSE, rasterize = TRUE) {
  
  if (!all(vapply(targets, raster::compareRaster, logical(1), dem))) {
    stop("Target SpatRaster do not share the same extent, number of rows and columns, projection, resolution and origin as the digital elevation model SpatRaster.")
  }
  
  # Compute everything related to the dem and independant of targets
  x <- as.matrix(dem)
  nr <- nrow(dem)
  nc <- ncol(dem)
  # Expand and recycle borders
  x <- recycle_borders(x, nr, nc)
  # Compute surrounding cells deltas
  x <- deltas(x, nr, nc)
  # Number of surrounding cells
  n <- length(x)
  # Sums of x squared
  sum_xx <- Σ(sup(x,2))
  
  # For the lapse rate, x is the elevation, and y is the target
  lapse_rate_redux <- function(target, x, nr, nc, n, sum_xx, replace_NaN) {
    
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
    
    # And we can return the lapse rate
    return(lapse_rate)
    
  }
  
  if (isTRUE(use_parallel)) {
    options("mc.cores" = min(4L, parallel::detectCores(logical = FALSE) - 1L))
    func <- parallel::mclapply
  } else {
    func <- lapply
  }
  
  res <- func(targets, lapse_rate_redux, x, nr, nc, n, sum_xx, replace_NaN)
  
  if (isTRUE(rasterize)) {
    res <- lapply(res, raster::raster)
    raster::extent(res) <- raster::extent(res)
    res <- raster::brick(res)
  }
  
  return(res)
  
}

#' @rdname lapse_rate
#' @export
setMethod("lapse_rate", signature("list", "RasterLayer"), lapse_rate_raster)
