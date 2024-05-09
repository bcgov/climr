#' Available climate variables
#'
#' List of available climate variables in climr
#'
#' @format A `data.table` with columns:
#' \describe{
#'   \item{Code}{variable code name}
#'   \item{Variable}{variable description (or full name)}
#'   \item{Code_Element}{code of the climate element (e.g. PPT)}
#'   \item{Element}{name of the climate element (e.g. precipitation)}
#'   \item{Code_Time}{code for the time of year. Blank for annual variables}
#'   \item{Time}{time of year the variable corresponds to (e.g, summer, August)}
#'   \item{Category}{time resolution (e.g., annual, monthly, seasonal, etc.)}
#'   \item{Type}{type of variable. Differentiates interval and ratio variables}
#'   \item{Unit}{unit of measurement. Blank for functionally unitless variables or where the unit is contained in the variable name}
#'   \item{Code_ClimateNA}{variable code as used in ClimateNA}
#' }
"variables"
