# sudo apt install postgresql postgis
# sudo -u postgres psql
# CREATE DATABASE climrpnw;
# \q
# sudo -u postgres psql -d climrpnw
# CREATE USER climrpnw PASSWORD 'climrpnw'

library(RPostgres)
conn <- dbConnect(
  drv = Postgres(),
  dbname = "climrpnw",
  host = "localhost",
  port = 5432, 
  user = "climrpnw",
  password = "climrpnw"
)
