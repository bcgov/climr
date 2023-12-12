#' @noRd
calc_DD_m_above <- function(tm, k, a, b, t0, beta, c) {
  DD_m <- numeric(length(tm))

  # when k is missing Tm will always be above the negative temperatures used for k
  # Sign reversed in paper
  # Reverse parameters b and t0 from paper
  i <- which(tm < k)
  DD_m[i] <- a / (1 + exp(-(tm[i] - t0) / b))
  i <- which(tm >= k)
  DD_m[i] <- c + beta * tm[i]

  return(DD_m)
}

#' @noRd
calc_DD_m_below <- function(tm, k, a, b, t0, beta, c) {
  DD_m <- numeric(length(tm))

  # when k is missing Tm will always be above the negative temperatures used for k
  # Sign reversed in paper
  # Reverse parameters b and t0 from paper
  i <- which(tm > k)
  DD_m[i] <- a / (1 + exp(-(tm[i] - t0) / b))
  i <- which(tm <= k)
  DD_m[i] <- c + beta * tm[i]

  return(DD_m)
}


#' Calculate Degree-Day Below 0 (DD<0)
#'
#' @param m month of the year
#' @param tm monthly mean temperature for the `m` month
#'
#' @return Degree-Day Below 0
#'
#' @examples
#' \dontrun{
#' climr:::calc_DD_below_0(2, -14)
#' }
calc_DD_below_0 <- function(m, tm) {
  if (FALSE) {
    Month <- k <- a <- b <- T0 <- beta <- c <- NULL
  }

  param[["DD_lt_0"]][m, calc_DD_m_below(
    tm = tm,
    k = k,
    a = a,
    b = b,
    t0 = T0,
    beta = beta,
    c = c
  )]
}

#' Calculate Degree-Day Above 5 (DD>5)
#'
#' @param m month of the year
#' @param tm monthly mean temperature for the `m` month
#' @param region one of either "All", "West", "East"
#'
#' @return Degree-Day Above 5
#'
#' @examples
#' \dontrun{
#' climr:::calc_DD_above_5(2, -14, "All")
#' }
calc_DD_above_5 <- function(m, tm, region) {
  if (FALSE) {
    Month <- Region <- k <- a <- b <- T0 <- beta <- c <- NULL
  }

  if (m %in% 5:10) {
    region <- "All"
  }

  param[["DD_gt_5"]][Month == m & Region == region, calc_DD_m_above(
    tm = tm,
    k = k,
    a = a,
    b = b,
    t0 = T0,
    beta = beta,
    c = c
  )]
}

#' Calculate Degree-Day Below 18 (DD<18)
#'
#' @param m month of the year
#' @param tm monthly mean temperature for the `m` month
#'
#' @return Degree-Day Below 18
#'
#' @examples
#' \dontrun{
#' climr:::calc_DD_below_18(2, -14)
#' }
calc_DD_below_18 <- function(m, tm) {
  if (FALSE) {
    Month <- k <- a <- b <- T0 <- beta <- c <- NULL
  }

  param[["DD_lt_18"]][m, calc_DD_m_below(
    tm = tm,
    k = k,
    a = a,
    b = b,
    t0 = T0,
    beta = beta,
    c = c
  )]
}


#' Calculate Degree-Day Above 18 (DD>18)
#'
#' @param m month of the year
#' @param tm monthly mean temperature for the `m` month
#' @param region one of either "All", "South west", "The rest"
#'
#' @return Degree-Day Above 18
#'
#' @examples
#' \dontrun{
#' climr:::calc_DD_above_18(2, -14, "All")
#' }
calc_DD_above_18 <- function(m, tm, region) {
  if (FALSE) {
    Month <- Region <- k <- a <- b <- T0 <- beta <- c <- NULL
  }

  if (m %in% 6:9) {
    region <- "All"
  }

  param[["DD_gt_18"]][Month == m & Region == region, calc_DD_m_above(
    tm = tm,
    k = k,
    a = a,
    b = b,
    t0 = T0,
    beta = beta,
    c = c
  )]
}
