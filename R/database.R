#' Connect to PostGIS database
#'
#' @return pool object of database connection
#' @param local A logical. Use a local database. Default `FALSE`.
#' @importFrom pool dbPool
#' @importFrom RPostgres Postgres
#'
#' @export
data_connect <- function(local = FALSE) {
  if(local){
    pool <- dbPool(
      drv = Postgres(),
      dbname = "climr",
      host = "localhost",
      port = 5432,
      user = "postgres",
      password = "climrserver"
    )
  }else{
    pool <- tryCatch(
      {
        dbPool(
          drv = Postgres(),
          dbname = "climr",
          host = "146.190.244.244",
          port = 5432,
          user = "climr_client",
          password = "PowerOfBEC2023"
        )
      },
      error = function(e) {
        tryCatch(
          {
            dbPool(
              drv = Postgres(),
              dbname = "climr",
              host = "146.190.244.244",
              port = 5432,
              user = "climr_client",
              password = "PowerOfBEC2023"
            )
          },
          error = function(f) {
            warning("Could not connect to database. Will try using cached data.")
            NULL
          }
        )
      }
    )
  }
  
  return(pool)
}

#' List connections in cache
#' @param profile Either `climr-db-user` or `local`.
#' @rdname data_con
#' @importFrom utils modifyList
#' @export
data_con <- function(profile = c("climr-db-user", "local")) {
  profile <- match.arg(profile)
  con <- .globals[["sesscon"]]$get(profile)
  if (is.null(con)) {
    default_args <- list(
      drv = RPostgres::Postgres(),
      dbname = "climr",
      port = 5432
    )
    args <- utils::modifyList(default_args, connection_creds(profile))
    con <- do.call(RPostgres::dbConnect, args)
  }
  .globals[["sesscon"]]$set(profile, con)
  return(con)
}

#' @noRd
session_connections <- function(nm) {
  connections <- list()
  active <- TRUE
  return(
    list(
      set = function(nm, con) { 
        if (active) connections[[nm]] <<- con 
      },
      get = function(nm) {
        if (!active || !length(nm)) return(NULL)
        con <- connections[[nm]]
        if (inherits(try(RPostgres::dbGetQuery(con ,"SELECT 1"), silent = TRUE), "try-error")) {
          connections[[nm]] <<- NULL
          return(NULL)
        }
        if (interactive() && getOption("climr.recycle.warn", TRUE)) {
          message("Recycling connection from cache [%s]. Run {.run climr::connections_clear()} if you want to clear cached connections." |> sprintf(nm))
          options("climr.recycle.warn" = FALSE)
        }
        return(con)
      },
      clear = function() { lapply(connections, RPostgres::dbDisconnect); connections <<- list(); return(invisible()) },
      list = function() {if (length(names(connections))) names(connections) else return(invisible())},
      enable = function() { active <<- TRUE},
      disable = function() { active <<- FALSE}
    )
  )
}

#' List connections in cache
#' @rdname data_con
#' @export
connections <- function() {
  .globals[["sesscon"]]$list()
}

#' Clear connections in cache
#' @rdname data_con
#' @export
connections_clear <- function() {
  .globals[["sesscon"]]$clear()
}

#' @noRd
connection_creds <- function(profile = c("climr-db-user", "local")) {
  profile <- match.arg(profile)
  switch(
    profile,
    `climr-db-user` = list(
      host = "146.190.244.244",
      user = "climr_client",
      password = "PowerOfBEC2023"
    ),
    local = list(
      host = "localhost",
      user = "postgres",
      password = "climrserver"
    )
  )
}

#' @noRd
.globals <- new.env()

### climr_client
### PowerOfBEC2023
