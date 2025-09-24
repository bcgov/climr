#' List available runs, global climate models (GCMs), time periods and scenarios (SSPs)
#'
#' @return a character vector.
#'
#' @description
#' `list_gcms` lists available global climate models.
#' 
#' @details
#' The eight-gcm ensemble recommended by Mahony et al. (2022) can be specified as `gcms = list_gcm()[c(1,4,5,6,7,10,11,12)]`. 
#' 
#'
#' @rdname data-option-lists
#' @export
list_gcms <- function() {
  c(
    "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3",
    "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6",
    "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
  )
  # sort(dbGetQuery(dbCon, "SELECT DISTINCT mod FROM esm_layers_ts")[,1])
}

#' @description
#' `list_ssps` lists available greenhouse gas concentration scenarios (SSP-RCPs).
#'
#' @rdname data-option-lists
#' @export
list_ssps <- function() {
  # sort(dbGetQuery(dbCon, "SELECT DISTINCT scenario FROM esm_layers")[,1])
  c("ssp126", "ssp245", "ssp370", "ssp585")
}

#' @description
#' `list_gcm_periods` lists available 20-year normal periods for GCM simulations.
#'
#' @rdname data-option-lists
#' @export
list_gcm_periods <- function() {
  # sort(dbGetQuery(dbCon, "SELECT DISTINCT period FROM esm_layers")[,1])
  c("2001_2020", "2021_2040", "2041_2060", "2061_2080", "2081_2100")
}


#' @description
#' `list_runs_ssp` lists available runs for a given GCM/ssp.
#' @param gcm Name of GCM. Must be one of the elements in list_gcms().
#' @param ssp Name of scenario Must be one of the elements in list_ssps().
#' @importFrom data.table fread
#' @importFrom tools R_user_dir
#' 
#' @rdname data-option-lists
#' @export
list_runs_ssp <- function(gcm, ssp){
  .globals[["gcm_period_runs"]][mod %in% gcm & scenario %in% ssp, run]
}

#' @description
#' `list_runs_historic` lists available runs from the historical simulation (1851-2014) for a specified GCM.
#' @param gcm Name of GCM
#' @importFrom data.table fread
#' @importFrom tools R_user_dir
#' 
#' @rdname data-option-lists
#' @export
list_runs_historic <- function(gcm){
  .globals[["gcm_hist_runs"]][mod %in% gcm, run]
}

#' @description
#' `list_refmaps` lists available reference maps of gridded climate normals
#'
#' @details
#' Currently available reference maps of gridded climate normals (`list_refmaps()`) are:
#'   * "refmap_climatena" for Climate NA derived normals
#'   * "refmap_climr" for a composite of BC PRISM, adjusted US PRISM and
#'     DAYMET (Alberta and Saskatchewan), covering North America
#'
#' @rdname data-option-lists
#' @export
list_refmaps <- function() {
  c("refmap_climr", "refmap_climatena")
}

#' @description
#' `list_obs_ts_datasets` lists available observational time series datasets
#'
#' @details
#' Three historical observational time series are available: 
#'   * "cru.gpcc"---The 1901-2024 combined Climatic Research Unit TS dataset (for temperature) and Global Precipitation Climatology Centre dataset (for precipitation); 
#'   * "climatena"---The 1901-2023 ClimateNA time series (Wang et al., 2024); and 
#'   * "mswx.blend"---the 1981-2024 Multi-Source Weather (MSWX; for temperature) and Multi-Source Weighted-Ensemble Precipitation (MSWEP; for precipitation), extended back to 1901 using the CRU/GPCC dataset. 
#'
#' @rdname data-option-lists
#' @export
list_obs_ts_datasets <- function() {
  c("mswx.blend", "cru.gpcc", "climatena") 
}

#' @description
#' `list_obs_periods` lists available normal periods for observational climate data
#'
#' @rdname data-option-lists
#' @export
list_obs_periods <- function() {
  return("2001_2020")
}

#' @description
#' `list_vars` lists available climate variables
#' 
#' @details
#' A lookup table of variable names, units, types, and other attributes is available by calling `data(variables)`.
#' 
#' @param set character. One of `c("All", "Monthly", "Seasonal", "Annual")` or any combination thereof. Defaults to "All".
#' @param only_extra logical. Should Tmin, Tmax and PPT be excluded? Defaults to FALSE.
#'
#' @rdname data-option-lists
#' @export
list_vars <- function(set = c("All", "Monthly", "Seasonal", "Annual"), only_extra = FALSE) {
  if (FALSE) {
    variables <- NULL
  }
  set <- match.arg(set, several.ok = TRUE)
  if ("All" %in% set) {
    res <- climr::variables[["Code"]]
  } else {
    res <- climr::variables[["Code"]][climr::variables[["Category"]] %in% set]
  }
  if (isTRUE(only_extra)) {
    res <- res[!grepl("(^PPT|^Tmax|^Tmin)", res)]
  }
  return(sort(unique(res)))
}


#' @description
#' `list_obs_years` lists available years for time series of observational climate data
#'
#' @rdname data-option-lists
#' @export
list_obs_years <- function() {
  1901:2024
}

#' @description
#' `list_gcm_ssp_years` lists available years for time series of global climate model future simulations 
#'
#' @rdname data-option-lists
#' @export
list_gcm_ssp_years <- function() {
  2015:2100
}

#' @description
#' `list_gcm_hist_years` lists available years for time series of global climate model historical simulations 
#'
#' @rdname data-option-lists
#' @export
list_gcm_hist_years <- function() {
  1851:2015
}
