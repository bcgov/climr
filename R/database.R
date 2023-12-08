#' Connect to postgis database
#' @return pool object of database connection
#' @importFrom pool dbPool
#' @importFrom RPostgres Postgres
#' @export

data_connect <- function(){
  pool <- tryCatch({
    pool::dbPool(
      drv = RPostgres::Postgres(),
      dbname = "climr",
      host = '146.190.244.244',
      port = 5432,
      user = 'climr_client',
      password = 'PowerOfBEC2023'
    )
  },
   error = function(e) {
     tryCatch({
       pool::dbPool(
         drv = RPostgres::Postgres(),
         dbname = "climr",
         host = '146.190.244.244',
         port = 5432,
         user = 'climr_client',
         password = 'PowerOfBEC2023'
       )
     },
     error = function(f) {
       warning(f,"Could not connect to database. Will try using cached data.")
       NULL
     })
   })
  return(pool)
}

###climr_client
###PowerOfBEC2023