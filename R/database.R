#' List connections in cache
#' @param profile Either `climr-db-user` or `local`.
#' @rdname data_connect
#' @importFrom RPostgres dbConnect
#' @export
data_connect <- function(profile = .globals[["sessprof"]]$list()) {
  profile <- match.arg(profile)
  con <- .globals[["sesscon"]]$get(profile)
  if (is.null(con)) {
    con <- tryCatch({
      do.call(RPostgres::dbConnect, .globals[["sessprof"]]$get(profile))
    }, error = \(e) {
      warning("Could not establish connection to database: ", conditionMessage(e))
      return(NULL) 
    })
    if (!is.null(.globals[["last_xyz"]])) {
      write_xyz(.globals[["last_xyz"]])
    }
  }
  .globals[["sesscon"]]$set(profile, con)
  return(con)
}

#' @noRd
write_xyz <- function(xyz) {
  db_safe_write("tmp_xyz", xyz, temporary = TRUE, overwrite = TRUE)
  db_safe_exec("ALTER TABLE tmp_xyz ADD COLUMN IF NOT EXISTS geom GEOMETRY(Point, 4326)")
  db_safe_exec("UPDATE tmp_xyz SET geom = ST_SetSRID(ST_MakePoint(lon, lat), 4326)")
  db_safe_exec("CREATE INDEX idx_tmp_xyz_id ON tmp_xyz (id);")
  return(xyz)
}

#' @noRd
#' @importFrom RPostgres dbGetQuery dbDisconnect
session_connections <- function() {
  connections <- list()
  active <- TRUE
  return(
    list(
      set = function(nm, con) { 
        if (active) {
          connections[[nm]] <<- con
          .globals[["sessprof"]]$last(nm)
        }
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

#' @noRd
#' @importFrom RPostgres Postgres
#' @importFrom utils modifyList
session_profiles <- function() {
  profiles <- list()
  last <- NULL
  active <- TRUE
  default_args <- list(
    drv = RPostgres::Postgres(),
    dbname = "climr",
    port = 5432
  )
  return(
    list(
      set = function(nm, args) { 
        if (active) {
          profiles[[nm]] <<- args
        }
      },
      get = function(nm = c(last, .globals[["sessprof"]]$list())) {
        nm <- match.arg(nm)
        if (!active || !length(nm)) return(NULL)
        if (is.null(profiles[[nm]])) return(NULL)
        utils::modifyList(default_args, profiles[[nm]])
      },
      clear = function() { profiles <<- list() },
      last = function(nm) { last <<- nm },
      list = function() {if (length(names(profiles))) c(last, names(profiles)) else return(invisible())},
      enable = function() { active <<- TRUE},
      disable = function() { active <<- FALSE}
    )
  )
}

#' List connections in cache
#' @rdname data_connect
#' @export
connections <- function() {
  .globals[["sesscon"]]$list()
}

#' Clear connections in cache
#' @rdname data_connect
#' @export
connections_clear <- function() {
  .globals[["sesscon"]]$clear()
}

#' @noRd
.globals <- new.env()

#' @noRd
init_globals <- function() {
  .globals[["sesscon"]] <- session_connections()
  .globals[["sessprof"]] <- session_profiles()
  .globals[["sessprof"]]$set(nm = "climr-db-use", args = list(host = "146.190.244.244", user = "climr_client", password = "PowerOfBEC2023"))
  .globals[["sessprof"]]$set(nm = "local", args = list(host = "localhost", user = "postgres", password = "climrserver"))
  .globals[["cache"]] <- list()
}

#' @importFrom RPostgres dbGetQuery
#' @noRd
db_safe_query <- function(statement, ...) {
  db_safe(RPostgres::dbGetQuery, statement, ...)
}

#' @importFrom RPostgres dbExecute
#' @noRd
db_safe_exec <- function(statement, ...) {
  db_safe(RPostgres::dbExecute, statement, ...)
}

#' @importFrom RPostgres dbWriteTable
#' @noRd
db_safe_write <- function(name, value, ...) {
  if (name %in% "tmp_xyz") {
    .globals[["last_xyz"]] <- value
  }
  db_safe(RPostgres::dbWriteTable, name, value, ...)
}

#' @noRd
db_safe <- function(f, ...) {
  with_retries(
    f(conn = data_connect(), ...)
  )
}


#' @noRd
with_retries <- function(expr, retries = 3L, xyz = NULL) {
  attempt <- 1L
  while (attempt <= retries) {
    no_error <- TRUE
    msg <- character()
    res <- tryCatch(
      eval.parent(substitute(expr)),
      error = function(e) {
        msg <<- conditionMessage(e)
        no_error <<- FALSE
      }
    )
    if (no_error) break
    message("Caught error: ", msg)
    message(paste("Retrying attempt", attempt, "in", 3^attempt, "seconds."))
    Sys.sleep(3^attempt)
    attempt <- attempt + 1L
    if (attempt > retries) {
      stop("Database operation could not be completed.")
    }
  }
  return(res)
}

### climr_client
### Powe