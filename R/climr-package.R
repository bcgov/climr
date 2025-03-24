#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    do.call(
      sprintf,
      c(
        list(fmt = "Welcome to climr %s.%s.%s!"),
        utils::packageVersion("climr") |> unlist() |> head(3) |> as.list()
      )
    )
  )
}

# On load, instantiate either as new or from cache
#' @importFrom data.table fwrite
#' @importFrom RPostgres dbGetQuery
#' @importFrom tools R_user_dir
#' @importFrom utils packageVersion
#' @noRd
.onLoad <- function(libname, pkgname) {

  .globals[["sesscon"]] <- session_connections()
  dbCon <- tryCatch(data_con(), error = \(e) NULL)

  if (is.null(dbCon)) {
    
    warning("Could not connect to server. Only cached reference periods will be available.")
    
    rInfoPath <- file.path(R_user_dir("climr", "data"), "run_info")
    if (dir.exists(rInfoPath)) {
      if (file.exists(f <- file.path(rInfoPath, "gcm_periods.csv"))) {
        .globals[["gcm_period_runs"]] <- data.table::fread(f)
      }
      if (file.exists(f <- file.path(rInfoPath, "gcm_ts.csv"))) {
        .globals[["gcm_ts_runs"]] <- data.table::fread(f)
      }
      if (file.exists(f <- file.path(rInfoPath, "gcm_hist.csv"))) {
        .globals[["gcm_hist_runs"]] <- data.table::fread(f)
      }
    }
    
  } else {
    
    .globals[["gcm_period_runs"]] <- RPostgres::dbGetQuery(
      dbCon, "
      select distinct mod, scenario, run
      from esm_layers_period order by mod, scenario, run;
      ") |> data.table::setDT()
    
    .globals[["gcm_ts_runs"]] <- RPostgres::dbGetQuery(
      dbCon,"
      select distinct mod, scenario, run
      from esm_layers_ts order by mod, scenario, run;
      ") |> data.table::setDT()
    
    .globals[["gcm_hist_runs"]] <- RPostgres::dbGetQuery(
      dbCon, "
      select distinct mod, run
      from esm_layers_hist order by mod, run;
      ") |> data.table::setDT()
    
  }
  
}

# .onLoad <- function(libname, pkgname) {
#   .climr[["files_uid_db"]] <- character()
#   if (file.exists(uid_db())) {
#     .climr[["files_uid_db"]] <- readRDS(uid_db())
#   }
# }
