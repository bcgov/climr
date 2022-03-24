# Recycle the borders of matrix to expand the matrix by one cell in each directions
# It will help later with the offset approach.
#' @param mat A matrix to recycle borders from.
#' @param nr The number of rows in the matrix.
#' @param nc The number of columns in the matrix.
#' @noRd
recycle_borders <- function(mat, nr, nc) {
  
  # Instantiate an extended border representation
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
sum_matrix <- function(x) {
  Reduce(`+`, x)
}

# This returns a list of matrices corresponding to the products of
# individual corresponding cells from each x matrices and y matrices
#' @param x A list of matrices of the same dimensions.
#' @param y A list of matrices of the same dimensions as x and the same
#' length as x.
#' @noRd
prod_matrix <- function(x, y) {
  mapply(`*`, x, y, SIMPLIFY = FALSE)
}

# This returns a list of matrices corresponding to the differences of
# individual corresponding cells from each x matrices and y matrices
#' @param x A list of matrices of the same dimensions.
#' @param y A list of matrices of the same dimensions as x and the same
#' length as x.
#' @noRd
delta_matrix <- function(x, y) {
  mapply(`-`, x, y, SIMPLIFY = FALSE)
}

# This returns a list of matrices after applying exponent `exp` to each
# matrix cells
#' @param x A list of matrices.
#' @param exp A numeric vector of length 1.
#' @noRd
sup <- function(x, exp) {
  lapply(x, `^`, exp)
}

# This returns the fitted simple linear regression values using 
# a pre-calculated beta coefficient matrix.
#' @param x A list of matrices of the same dimensions.
#' @param beta_coef A matrix of the same dimensions as a matrix in x.
#' @noRd
fitted <- function(x, beta_coef) {
  lapply(x, function(x) x*beta_coef)
}

# This compute a list of matrices. Each matrix represents differences between
# surrounding cells and the reference cells.
# It uses an offset approach to compute surrounding cells in each directions.
#' @param mat A matrix previously ran through `recycle_borders`
#' @param nr The number of rows in the original matrix used by `recycle_borders`
#' @param nc The number of columns in the original matrix used by `recycle_borders`
#' @param NA_replace A boolean. Should NA delta results be replaced by zeros. Default to TRUE.
#' @noRd
deltas <- function(mat, nr, nc, NA_replace = TRUE) {
  
  # Reference values
  ref <- mat[c(-1L,-(nr+2L)), c(-1L,-(nc+2L))]
  
  # Surrounding values
  res <- list(
    northwest = mat[-(nr + 1L):-(nr + 2L), -(nc + 1L):-(nc + 2L)] - ref,
    north     = mat[-(nr + 1L):-(nr + 2L), c(-1L,-(nc + 2L))] - ref,
    northeast = mat[-(nr + 1L):-(nr + 2L), -1L:-2L] - ref,
    east      = mat[c(-1L,-(nr + 2L)), -1L:-2L] - ref,
    southeast = mat[-1L:-2L, -1L:-2L] - ref,
    south     = mat[-1L:-2L, c(-1L,-(nc + 2L))] - ref,
    southwest = mat[-1L:-2L, -(nc + 1L):-(nc + 2L)] - ref,
    west      = mat[c(-1L,-(nr + 2L)), -(nc + 1L):-(nc + 2L)] - ref
  )
  
  # Replace NA/NaN by 0
  # Only the deltas are replaced by 0.
  NArep <- function(x) {
    x[is.na(x)] <- 0L
    return(x)
  }
  if (isTRUE(NA_replace)) {
    res <- lapply(res, NArep)
  }
  
  return(res)
}

#' Lapse rate computation
#' @param target Target rasters to compute lapse rates for. Build with this package functions.
#' @param dem A digital elevation model with matching target extent.
#' @param NA_replace A boolean. Should NA lapse rate results be replaced by zeros. Default to TRUE.
#' @param use_parallel A boolean. Should parallel::mclapply be used. Default to TRUE.
#' @param rasterize Return an object of the same class category as target with the same extend.
#' @details Formulas
#' Simple linear regression without the intercept term
#' beta_coef = sum(xy) / sum(x²)
#' mss = sum(x * beta_coef)², sum of squared fitted values
#' rss = sum(ε²), sum of squared (y minus fitted), sum of absolute errors
#' R² = mss / (mss + rss)
#' Lapse rate = beta_coef * R²
#' @return Lapse rate values.
#' @importFrom terra as.list as.matrix ext
#' @importFrom parallel detectCores mclapply
#' @export
lapse_rate <- function(target, dem, NA_replace = TRUE, use_parallel = TRUE, rasterize = TRUE) {
  
  # Transform target to list, capture names before
  target_names <- names(target)
  target <- shush(terra::as.list(target))
  
  # Compute everything related to the dem and independant of target
  nr <- nrow(dem)
  nc <- ncol(dem)
  x <- shush(terra::as.matrix(dem, wide = TRUE))
  # Expand and recycle borders
  x <- recycle_borders(x, nr, nc)
  # Compute surrounding cells deltas
  x <- deltas(x, nr, nc, NA_replace)
  # Number of surrounding cells
  n <- length(x)
  # Sums of x squared
  sum_xx <- sum_matrix(sup(x,2))
  
  # For the lapse rate, x is the elevation, and y is the target
  lapse_rate_redux <- function(r, x, nr, nc, n, sum_xx, NA_replace) {
    
    y <- shush(terra::as.matrix(r, wide = TRUE))
    # Expand and recycle borders
    y <- recycle_borders(y, nr, nc)
    # Compute surrounding cells deltas
    y <- deltas(y, nr, nc)
    # This is the regression coefficient matrix
    beta_coef <- sum_matrix(prod_matrix(x,y)) / sum_xx
    # We need the fitted values to compute the
    # coefficient of determination
    f <- fitted(x, beta_coef)
    # We use the same approach as stats::summary.lm
    # applied to a list matrices
    mss <- sum_matrix(sup(f,2))
    rss <- sum_matrix(sup(delta_matrix(y,f),2))
    # We can combine the resulting matrices to get the
    # coefficient of determination and multiply by beta coefficient
    lapse_rate <- beta_coef * mss / (mss + rss)
    
    if (isTRUE(NA_replace)) {
      lapse_rate[is.na(lapse_rate)] <- 0L  
    }
    
    # And we can return the lapse rate
    return(lapse_rate)
    
  }
  
  if (isTRUE(use_parallel)) {
    # Store current option value
    cur_value <- getOption("mc.cores")
    # Restore value on function exit
    on.exit(options("mc.cores" = cur_value), add = TRUE)
    # Set option value for the rest of the function execution
    options("mc.cores" = parallel::detectCores()/2L)
    # Use parallel lapply
    func <- parallel::mclapply
  } else {
    # Use regular lapply
    func <- lapply
  }
  
  res <- func(target, lapse_rate_redux, x, nr, nc, n, sum_xx, NA_replace)
  
  # Transform back into raster layers stack
  if (isTRUE(rasterize)) {
    res <- shush(
      terra::rast(
        lapply(
          res,
          terra::rast,
          extent = terra::ext(dem)
        )
      )
    )
  }
  
  # Set names of lapse rates to match target
  names(res) <- target_names
  
  return(res)
  
}
