#' Add extra climate variables to a data.table
#' @param dt A data.table with TminXX, TmaxXX, PPTXX for XX in 01 to 12.
#' @param extra_variables A character vector of extra variables to compute.
#' @param grouping A character vector of variables grouping. Can be `m` (monthly), `s` (seasonal) or `a` (annual).
append_calc <- function(dt, extra_variables, grouping) {
  
  # Suffixes
  s <- c("wt", "sp", "sm", "at")
  m <- sprintf("%02d", 1:12)
  
  #Deal with default variables first
  # if ("s" %in% grouping) {
  #   set(dt, j = paste0("")
  # }
  if ("DD_0" %in% extra_variables) {
    set(dt, j = paste0("DD_0_",m), value = {
      lapply(1:12, function(x) {
        calc_DD_below_0(x, dt[, rowMeans(.SD), .SDcols = sprintf("%s%02d", c("Tmin", "Tmax"), x)])
      })
    })
  }
}

#' List climate variables
#' @param include_default A boolean. Should Tmin, Tmax and PPT be included? Default to TRUE.
#' @export
list_variables <- function(include_default = TRUE) {
  if (FALSE) { variables <- NULL; Category <- NULL }
  res <- variables[Category %in% c("Any", "Annual")][["Code"]]
  if (!isTRUE(include_default)) {
    res <- setdiff(res, c("PPT", "Tmax", "Tmin"))
  }
  return(sort(unique(res)))
}
