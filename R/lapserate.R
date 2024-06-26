#' Expand matrix using recycled borders
#'
#' Recycle the borders of matrix to expand the matrix by one cell in each directions
#' It will help later with the offset approach.
#'
#' @param mat A `matrix` to recycle borders from.
#' @param nr integer. The number of rows in the matrix.
#' @param nc integer. The number of columns in the matrix.
#'
#' @return a `matrix`. The expanded `mat`.
#'
#' @noRd
recycle_borders <- function(mat, nr, nc) {
  # Instantiate an extended border representation
  res <- matrix(nrow = nr + 2L, ncol = nc + 2L)

  # Fill the representation starting with the original data in the center
  res[2L:(nr + 1L), 2L:(nc + 1L)] <- mat

  # Recycle the borders
  # North
  res[1L, 2L:(nc + 1L)] <- mat[1L, ]
  # South
  res[(nr + 2L), 2L:(nc + 1L)] <- mat[nr, ]
  # West
  res[2L:(nr + 1L), 1L] <- mat[, 1L]
  # East
  res[2L:(nr + 1L), nc + 2L] <- mat[, nc]

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

#' Sum matrices in a list
#'
#' Returns a single matrix containing the sums of matching row, cell
#' in the list of matrices.
#' @param x A `list` of matrices of the same dimensions
#'
#' @return a `matrix`.
#' @noRd
sum_matrix <- function(x) {
  ## TODO: test that matrix dimensions are always the same.
  Reduce(`+`, x)
}

#' Cell-by-cell multiplication of matrices in a list
#'
#' Returns a list of matrices corresponding to the products of
#'   individual corresponding cells from each \eqn(x_{i}) matrix and \eqn(y_{i}) matrix,
#'   where *i* is the ith matrix in each list.
#'
#' @param x A `list` of matrices of the same dimensions.
#' @param y A `list` of matrices of the same dimensions as x and the same
#' length as x.
#'
#' @return a `list` of matrices.
#'
#' @noRd
prod_matrix <- function(x, y) {
  ## TODO: check that list lengths are the same
  ## TODO: check that matrix dimensions are the same
  mapply(`*`, x, y, SIMPLIFY = FALSE)
}

#' Cell-by-cell subtraction of matrices in a list
#'
#' Returns a list of matrices corresponding to the differences of
#'   individual corresponding cells from each \eqn(x_{i}) matrix and \eqn(y_{i}) matrix,
#'   where *i* is the ith matrix in each list.
#'
#' @param x A `list` of matrices of the same dimensions.
#' @param y A `list` of matrices of the same dimensions as x and the same
#' length as x.
#'
#' @return a `list` of matrices.
#'
#' @noRd
delta_matrix <- function(x, y) {
  ## TODO: check that list lengths are the same
  ## TODO: check that matrix dimensions are the same
  mapply(`-`, x, y, SIMPLIFY = FALSE)
}

#' Matrix exponentiation to *e* (`exp`)
#'  This returns a list of matrices after applying exponent `exp` to each
#'  matrix cell
#'
#' @param x A `list` of matrices.
#' @param exp numeric. The exponent value.
#'
#' @return a `list` of matrices.
#'
#' @noRd
sup <- function(x, exp) {
  if (length(exp) > 1) stop("'exp' should have a single value")
  lapply(x, `^`, exp)
}

#' Return LM predictions
#'
#' Returns the predicted simple linear regression values using
#'   a pre-calculated beta coefficient matrix.
#'
#' @param x A `list` of matrices of the same dimensions.
#' @param beta_coef A `matrix` of beta coefficients, with the same dimensions as
#'   the matrices in x.
#'
#' @return a `list` of predicted values.
#'
#' @noRd
fitted <- function(x, beta_coef) {
  ## TODO: no intercept?
  ## TODO: test matrix dimensions match
  lapply(x, function(x) x * beta_coef)
}

#' Differences between reference and neighbouring cells
#'
#' Computes a list of matrices, where each represents the differences between
#'   each cell and its immediate neighbours.
#'   It uses an offset approach to compute surrounding cells in each directions.
#'
#' @param mat the output `matrix` of `[recycle_borders()]`
#' @param nr integer. The number of rows in the original matrix used by `[recycle_borders()]`
#' @param nc integer. The number of columns in the original matrix used by `[recycle_borders()]`
#' @param NA_replace logical. Should NA delta results be replaced by zeros.
#'   Defaults to TRUE.
#'
#' @return a `list` of matrices.
#'
#' @noRd
deltas <- function(mat, nr, nc, NA_replace = TRUE) {
  # Reference values
  ref <- mat[c(-1L, -(nr + 2L)), c(-1L, -(nc + 2L))]

  # Surrounding values
  res <- list(
    northwest = mat[-(nr + 1L):-(nr + 2L), -(nc + 1L):-(nc + 2L)] - ref,
    north     = mat[-(nr + 1L):-(nr + 2L), c(-1L, -(nc + 2L))] - ref,
    northeast = mat[-(nr + 1L):-(nr + 2L), -1L:-2L] - ref,
    east      = mat[c(-1L, -(nr + 2L)), -1L:-2L] - ref,
    southeast = mat[-1L:-2L, -1L:-2L] - ref,
    south     = mat[-1L:-2L, c(-1L, -(nc + 2L))] - ref,
    southwest = mat[-1L:-2L, -(nc + 1L):-(nc + 2L)] - ref,
    west      = mat[c(-1L, -(nr + 2L)), -(nc + 1L):-(nc + 2L)] - ref
  )

  # Only the deltas are replaced by 0.
  if (isTRUE(NA_replace)) {
    res <- lapply(res, NArep)
  }

  return(res)
}

#' Calculation of gridded lapse rates
#'
#' @description
#' This function computes lapse rates (a linear relationship of a climate variable to elevation) across user-provided rasters.
#' Lapse rates are calculated using simple linear regression (without the intercept) using the focal cell and its eight neighbors as observations.
#' These gridded lapse rates are used in [`downscale()`] and [`downscale_core()`] for elevation adjustment of climate values during downscaling to user-specified locations.
#' The method is illustrated in the vignette `vignette("lapse_rates")`
#'
#' @param reference a `SpatRaster` stack. climate rasters to compute lapse rates for. Build with this package functions.
#' @template dem
#' @param NA_replace logical. Should NA lapse rate results be replaced by zeros. Default to TRUE.
#' @param nthread integer. Number of parallel threads to use to compute lapse rates.
#' @param rasterize logical. Return an object of the same class category as reference with the same extent?
#'    Default to `TRUE`.
#'
#' @details Formulas \cr
#'   beta_coef = sum(xy) / sum(x²) \cr
#'   mss = sum(x * beta_coef)², sum of squared fitted values \cr
#'   rss = sum(ε²), sum of squared (y minus fitted), sum of absolute errors \cr
#'   R² = mss / (mss + rss) \cr
#'   Lapse rate = beta_coef * R²

#' @return `SpatRaster` of lapse rate values.
#'
#' @importFrom terra as.list as.matrix ext nlyr compareGeom resample rast crs crs<-
#' @export
lapse_rate <- function(reference, dem, NA_replace = TRUE, nthread = 1L, rasterize = TRUE) {
  # Transform reference to list, capture names before
  normal_names <- names(reference)

  # Validation
  if (nlyr(reference) != 36L || !all(sprintf(c("PPT%02d", "Tmax%02d", "Tmin%02d"), sort(rep(1:12, 3))) %in% names(reference))) {
    stop(
      "Normal raster does not have the required 36 layers. Required layers are ",
      paste(sort(sprintf(c("PPT%02d", "Tmax%02d", "Tmin%02d"), sort(rep(1:12, 3)))), collapse = ", "),
      "."
    )
  }
  if (nlyr(dem) != 1L) {
    stop(
      "Digital elevation model raster has to have one layer only."
    )
  }
  # Matching geometries check
  if (!compareGeom(reference, dem)) {
    warning("Normal and Digital elevation model rasters have different extents. They must be the same. Resampling dem to match.")
    dem <- resample(dem, reference, method = "bilinear")
  }

  # Compute everything related to the dem and independant of reference
  n_r <- nrow(dem)
  n_c <- ncol(dem)
  x_i <- shush(as.matrix(dem, wide = TRUE))
  # Expand and recycle borders
  x_i <- recycle_borders(x_i, n_r, n_c)
  # Compute surrounding cells deltas
  x_i <- deltas(x_i, n_r, n_c, NA_replace)
  # Number of surrounding cells
  n_sc <- length(x_i)
  # Sums of x squared
  sum_xx <- sum_matrix(sup(x_i, 2))

  if (isTRUE(nthread > 1L)) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      message("nthreads is >1, but 'parallel' package is not available.")
      message("Setting nthreads to 1 and running computations in sequential mode.")
      message("If you wish to parallelise please run install.packages('parallel')")
      nthread <- 1L
    }
  }

  if (isTRUE(nthread > 1L)) {
    message("Parallelising lapse rate computations across ", nthread, " threads")

    # initiate cluster
    if (Sys.info()["sysname"] != "Windows") {
      cl <- parallel::makeForkCluster(nthread)
    } else {
      cl <- parallel::makePSOCKcluster(nthread)
    }

    # destroy cluster on exit
    on.exit(parallel::stopCluster(cl), add = TRUE)

    res <- parallel::parLapply(
      cl = cl,
      X = shush(lapply(as.list(reference), as.matrix, wide = TRUE)),
      fun = lapse_rate_redux,
      x_i = x_i,
      n_r = n_r,
      n_c = n_c,
      n_sc = n_sc,
      sum_xx = sum_xx,
      NA_replace = NA_replace
    )
  } else {
    # Use regular lapply
    res <- lapply(
      X = shush(lapply(as.list(reference), as.matrix, wide = TRUE)),
      FUN = lapse_rate_redux,
      x_i = x_i,
      n_r = n_r,
      n_c = n_c,
      n_sc = n_sc,
      sum_xx = sum_xx,
      NA_replace = NA_replace
    )
  }

  # Transform back into SpatRaster
  if (isTRUE(rasterize)) {
    res <- shush(
      rast(
        lapply(
          res,
          rast,
          extent = ext(reference)
        )
      )
    )
    crs(res) <- crs(reference)
  }

  # Set names of lapse rates to match reference
  names(res) <- normal_names

  return(res)
}

#' Replace NA/NaN by 0
#'
#' @param x a vector of values.
#'
#' @return `x` with NAs replaced by 0s
#' @noRd
NArep <- function(x) {
  x[is.na(x)] <- 0L
  return(x)
}


#' TODO: add documentation here

#' For the lapse rate, x is the elevation, and y is the reference
#'
#' @param y_i TODO
#' @param x_i TODO
#' @param n_r TODO
#' @param n_c TODO
#' @param n_sc TODO
#' @param sum_xx TODO
#' @param NA_replace TODO
#'
#' @return `data.table`
#' @noRd
lapse_rate_redux <- function(y_i, x_i, n_r, n_c, n_sc, sum_xx, NA_replace) {
  # Expand and recycle borders
  y_i <- recycle_borders(y_i, n_r, n_c)
  # Compute surrounding cells deltas
  y_i <- deltas(y_i, n_r, n_c)
  # This is the regression coefficient matrix
  beta_coef <- sum_matrix(prod_matrix(x_i, y_i)) / sum_xx
  # We need the fitted values to compute the
  # coefficient of determination
  f <- fitted(x_i, beta_coef)
  # We use the same approach as stats::summary.lm
  # applied to a list matrices
  mss <- sum_matrix(sup(f, 2))
  rss <- sum_matrix(sup(delta_matrix(y_i, f), 2))
  # We can combine the resulting matrices to get the
  # coefficient of determination and multiply by beta coefficient
  lapse_rate <- beta_coef * mss / (mss + rss)

  if (isTRUE(NA_replace)) {
    lapse_rate[is.na(lapse_rate)] <- 0L
  }

  # And we can return the lapse rate
  return(lapse_rate)
}
