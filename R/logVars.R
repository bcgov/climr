#' Log-transform climate variables
#'
#' @description
#' This function performs logarithmic transformation on climr output to a user-specified base. 
#' This function currently only works with climr output in data.table format (i.e., not with raster output).
#' 
#' @param dat a `data.table` with columns of climate variables including
#'   the selected climate `elements`.
#' @param elements character. Climate elements to log-transform.
#' @param base numeric. Logarithmic base.
#' @param add.fields logical. If `TRUE`, the new logged variables are added to `dat` 
#'   (TRUE). Otherwise, original column values are replaced with the logs (FALSE).
#' @param zero_adjust logical. If `TRUE` adjusts zeroes in raw data as:
#'   \eqn{base^{\log_base{x_min} - 1}}.
#'   where \eqn{x_min} is the minimum non-zero, non-NA value.
#'
#' @details
#' In climate analysis it is often preferable to log-transform ratio variables, i.e., variables such as precipitation and 
#' degree-days that have a lower limit of zero and for which proportions are meaningful. 
#' 
#' Climate elements include all related time variables, hence all column names that 
#' partially match strings in `elements` will be log-transformed. For example, specifying 
#' `elements = "PPT"` will #'   result in log-transformation of `c("PPT", "PPT_01", "PPT_02", ... , "PPT_at")`. 
#'  
#' By default, zeroes are assigned a log value that is one order of magnitude less than the minimum positive value
#' of the untransformed variable. 
#' 
#' @return `data.table` 
#' @export
logVars <- function(dat,
                    elements = c("AHM", "DD", "Eref", "FFP", "NFFD", "PAS", "PPT", "SHM", "CMD"),
                    base = exp(1),
                    add.fields = FALSE,
                    zero_adjust = TRUE) {
  
  dat <- copy(dat)
  
  # Fields to operate on (generally these should be ratio (zero-limited) variables)
  logFields <- grep(paste(elements, collapse = "|"), names(dat), value = TRUE)
  dat.log <- dat[, .SD, .SDcols = logFields]
  
  # If specified by the user, give zero values a positive value that is one order of magnitude less than the minimum positive value
  if (zero_adjust) {
    dat.log <- dat.log[, lapply(.SD, function(x) {
      x[x <= 0] <- base^(log(min(x[x > 0], na.rm = TRUE), base = base) - 1)
      return(x)
    })]
  }
  
  # Perform log transformation
  dat.log <- dat.log[, lapply(.SD, function(x) log(x, base = base))]
  
  # Add 
  if(add.fields){
    setnames(dat.log, logFields, paste0(logFields, "_log"))
    dat <- cbind(dat, dat.log)
  } else {
    dat[, (logFields) := Map(x =.SD, xname = logFields, f = function(x, xname) {
      x <- dat.log[[xname]]
      return(x)
    }), .SDcols = logFields]
  }
  return(dat)
}
