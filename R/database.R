#' Connect to postgis database
#' @return pool object of database connection
#' @import pool
#' @import RPostgres
#' @export

data_connect <- function(){
  pool <- dbPool(
    drv = RPostgres::Postgres(),
    dbname = "climr",
    host = '146.190.244.244',
    port = 5432, 
    user = 'climr_client',
    password = 'PowerOfBEC2023'
  )
  return(pool)
}

###climr_client
###PowerOfBEC2023