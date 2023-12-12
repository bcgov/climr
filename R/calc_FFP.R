#' Calculate bFFP
#'
#' bFFP : Day of the year on which the Frost-Free Period begins
#' @param td difference between the mean warmest monthly temperature and the mean coldest monthly temperature
#' @param NFFD Number of Frost-Free Days 
#' @param t_min_list named list of monthly minimum temperature for each month
#'
#' @return Day of the year on which the Frost-Free Period begins
#'
#' @examples
#' \dontrun{
#' t_min_list <- list("1" = -35, "2" = -32, "3" = -25, "4" = -10, 
#' "5" = -5, "6" = 3,  "7" = 15, "8" = 17, "9" = 10, "10" = -5,
#' "11" = -20, "12" = -30)
#' 
#' climr:::calc_bFFP(td = 30, NFFD= 10, t_min_list = t_min_list)
#' }
calc_bFFP <- function(td, NFFD, t_min_list) {
  
  tmin4 <- t_min_list[[4]]
  tmin6 <- t_min_list[[6]]
  
  352.1358994 + -0.021715653 * tmin4^2 + -3.542187618 * tmin6 +
    0.020359471 * tmin6^2 - 4.897998097 * td + 0.033521327 * td^2 -
    2.164862277 * NFFD + 0.006767633 * NFFD^2 - 0.00000929 * NFFD^3 +
    0.043516586 * (td * NFFD) - 0.00000253 * (td * NFFD)^2
  
}

#' Calculate eFFP
#'
#' bFFP : Day of the year on which the Frost-Free Period ends
#' 
#' @param NFFD Number of Frost-Free Days  
#' @param t_min_list named list of monthly minimum temperature for each month 
#'
#' @return Day of the year on which the Frost-Free Period ends
#'
#' @examples
#' \dontrun{
#' t_min_list <- list("1" = -35, "2" = -32, "3" = -25, "4" = -10, 
#' "5" = -5, "6" = 3,  "7" = 15, "8" = 17, "9" = 10, "10" = -5,
#' "11" = -20, "12" = -30)
#' 
#' climr:::calc_eFFP(NFFD = 10, t_min_list = t_min_list)
#' }
calc_eFFP <- function(NFFD, t_min_list) {
  
  tmin9 <- t_min_list[[9]]
  tmin10 <- t_min_list[[10]]
  tmin11 <- t_min_list[[11]]
  
  243.7752209 + 4.134210825 * tmin9 - 0.162876448 * tmin9^2 +
    1.248649021 * tmin10 + 0.145073612 * tmin10^2 + 0.004319892 * tmin10 +
    -0.005753127 * tmin10^2 - 0.06296471 * NFFD + 0.000399177 * NFFD^2
  
}

#' Calculate Frost-Free Period
#'
#' @param bFFP Day of the year on which the Frost-Free Period begins
#' @param eFFP Day of the year on which the Frost-Free Period ends
#'
#' @return Frost-Free Period
#'
#' @examples
#' \dontrun{
#' climr:::calc_FFP(bFFP = 214.5964, eFFP = 265.4581)
#' }
calc_FFP <-function(bFFP, eFFP) {
  eFFP - bFFP
}
