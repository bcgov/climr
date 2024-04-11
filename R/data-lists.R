#' List available runs, global circulation models, periods and climate scenarios
#'
#' @return a character vector.
#'
#' @description
#' `list_gcm` lists available global circulation models.
#'
#' @rdname data-option-lists
#' @export
list_gcm <- function() {
  c(
    "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3",
    "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6",
    "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
  )
  # sort(dbGetQuery(dbCon, "SELECT DISTINCT mod FROM esm_layers_ts")[,1])
}

#' @description
#' `list_ssp` lists available shared socioeconomic pathways.
#'
#' @rdname data-option-lists
#' @export
list_ssp <- function() {
  # sort(dbGetQuery(dbCon, "SELECT DISTINCT scenario FROM esm_layers")[,1])
  c("ssp126", "ssp245", "ssp370", "ssp585")
}

#' @description
#' `list_gcm_period` lists available periods.
#'
#' @rdname data-option-lists
#' @export
list_gcm_period <- function() {
  # sort(dbGetQuery(dbCon, "SELECT DISTINCT period FROM esm_layers")[,1])
  c("2001_2020", "2021_2040", "2041_2060", "2061_2080", "2081_2100")
}

#' @description
#' `list_run` lists available runs for a given GCM.
#'
#' @template dbCon
#' @param gcm Character vector to specify requested GCMs
#' @importFrom RPostgres dbGetQuery
#'
#' @rdname data-option-lists
#' @export
list_run <- function(dbCon, gcm) {
  sort(dbGetQuery(dbCon, paste0("SELECT DISTINCT run FROM esm_layers_period WHERE mod IN ('", paste(gcm, collapse = "','", "')")))[, 1])
}

#' @description
#' `list_normal` lists available normals.
#'
#' @details
#' Currently available normals (`list_normal()`) are:
#'   * "normal_na" for Climate NA derived normals
#'   * "normal_bc" for British Columbia PRISM climatologies derived normals
#'   * "normal_composite" for a composite of BC PRISM, adjusted US PRISM and
#'     DAYMET (Alberta and Saskatchewan), covering western Canada and western
#'     US.
#'
#' @rdname data-option-lists
#' @export
list_normal <- function() {
  c("normal_na", "normal_bc", "normal_composite")
}


#' @description
#' `list_historic` lists available historic periods
#'
#' @rdname data-option-lists
#' @export
list_historic <- function() {
  return("2001_2020")
}

#' @description
#' `list_variables` lists climate variables
#'
#' @param set character. One of All, Monthly, Seasonal, Annual, or any combination thereof. Defaults to "All".
#' @param only_extra logical. Should Tmin, Tmax and PPT be excluded? Defaults to FALSE.
#'
#' @rdname data-option-lists
#' @export
list_variables <- function(set = c("All", "Monthly", "Seasonal", "Annual"), only_extra = FALSE) {
  if (FALSE) {
    variables <- NULL
  }
  set <- match.arg(set, several.ok = TRUE)
  if ("All" %in% set) {
    res <- variables[["Code"]]
  } else {
    res <- variables[["Code"]][variables[["Category"]] %in% set]
  }
  if (isTRUE(only_extra)) {
    res <- res[!grepl("(^PPT|^Tmax|^Tmin)", res)]
  }
  return(sort(unique(res)))
}


#' @description
#' `list_historic_ts` lists available years for historic time series
#'
#' @rdname data-option-lists
#' @export
list_historic_ts <- function() {
  1902:2022
}

#' @description
#' `list_gcm_ts` lists available years for future projections' time series
#'
#' @rdname data-option-lists
#' @export
list_gcm_ts <- function() {
  2015:2100
}

#' @description
#' `list_gcm_hist_ts` lists available years for historic projections' time series
#'
#' @rdname data-option-lists
#' @export
list_gcm_hist_ts <- function() {
  1851:2015
}
