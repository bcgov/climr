# On load, instantiate either as new or from cache
#' @importFrom data.table fwrite
#' @importFrom RPostgres dbGetQuery
#' @noRd
.onLoad <- function(libname, pkgname) {
  if(!dir.exists(paste0(cache_path(), "/run_info/"))){
    message("Downloading and Caching ESM run info :)")
    dbCon <- data_connect()
    if(is.null(dbCon)){
      warning("Could not connect to server. Only cached normal periods will be available.")
    }else{
      dir.create(paste0(cache_path(), "/run_info/"), recursive = TRUE)
      gcm_period_runs <- dbGetQuery(dbCon, "select distinct mod, scenario, run from esm_layers order by mod, scenario, run;")
      gcm_ts_runs <- dbGetQuery(dbCon, "select distinct mod, scenario, run from esm_layers_ts order by mod, scenario, run;")
      gcm_hist_runs <- dbGetQuery(dbCon, "select distinct mod, run from esm_layers_hist order by mod, run;")
      fwrite(gcm_period_runs, paste0(cache_path(), "/run_info/gcm_period.csv"))
      fwrite(gcm_ts_runs, paste0(cache_path(), "/run_info/gcm_ts.csv"))
      fwrite(gcm_hist_runs, paste0(cache_path(), "/run_info/gcm_hist.csv"))
    }

  }
}

# .onLoad <- function(libname, pkgname) {
#   .climr[["files_uid_db"]] <- character()
#   if (file.exists(uid_db())) {
#     .climr[["files_uid_db"]] <- readRDS(uid_db())
#   }
# }

#' Climate variables definition
"variables"
