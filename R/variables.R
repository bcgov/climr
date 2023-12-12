#' Available climate variables
#'
#' List of available climate variables in ClimateBC.
#'
#' @format A `data.table` with columns:
#' \describe{
#'   \item{Code}{variable code name}
#'   \item{Variable}{variable description (or full name)}
#'   \item{Time}{time of year the variable corresponds to (e.g, summer, August, year)}
#'   \item{Category}{time resolution (e.g., annual, monthly, seasonal, etc.)}
#'   \item{Scale}{whether the variable has been scaled, or is in raw values}
#' }
#' @source todo add/describe source
"variables"
