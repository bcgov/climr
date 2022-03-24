#' Add extra climate variables to a data.table
#' @param dt A data.table with TminXX, TmaxXX, PPTXX for XX in 01 to 12.
#' @param extra A character vector of extra variables to compute.
append_extra <- function(dt, extra) {
  
  # Suffixes
  s <- c("wt", "sp", "sm", "at")
  m <- sprintf("%02d", 1:12)
  
  #Deal with default variables first
  # if ("s" %in% grouping) {
  #   set(dt, j = paste0("")
  # }
  if ("DD_0" %in% extra) {
    set(dt, j = paste0("DD_0_",m), value = {
      lapply(1:12, function(x) {
        calc_DD_below_0(x, dt[, rowMeans(.SD), .SDcols = sprintf("%s%02d", c("Tmin", "Tmax"), x)])
      })
    })
  }
}

#' List climate variables
#' @param only_extra A boolean. Should Tmin, Tmax and PPT be excluded? Default to TRUE.
#' @export
list_variables <- function(only_extra = TRUE) {
  if (FALSE) { variables <- NULL }
  res <- variables[["Code"]]
  if (isTRUE(only_extra)) {
    res <- res[!grepl("(^PPT|^Tmax|^Tmin)", res)]
  }
  return(sort(unique(res)))
}
