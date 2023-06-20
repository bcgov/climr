remotes::install_github("bcgov/climR-pnw")
library(climRpnw)
library(data.table)
library(terra)
# Retrieve package data
data_delete() ##changed data, so this makes sure it gets redownloaded
data_update()

# Create a normal baseline
list_normal()
normal <- normal_input()

# # Select GCM
list_gcm()
gcm <- gcm_input(
  gcm = c("BCC-CSM2-MR"),
  ssp = c("ssp245"),
  period = "2001_2020",
  max_run = 0
)

## Select historic period
list_historic()
historic <- historic_input()

# Provide or create a points dataframe (lon, lat, elev)
# n <- 100
# xyz <- data.frame(lon = runif(n, -125, -120), lat = runif(n, 51, 53), elev = numeric(n))
# xyz$elev <- terra::extract(dem1, xyz[,1:2], method = "bilinear")[["dem2_BC"]]

in_locations <- fread("../AMAT_site_locations.csv")
in_xyz <- as.data.frame(in_locations[,.(LONG_S,LAT_S,ELEV_S)]) ##currently needs to be a data.frame or matrix, not data.table

list_variables()
# Downscale!
results <- downscale(
  xyz = in_xyz,
  normal = normal,
  gcm = gcm,
  historic = historic,
  vars = c("CMD_sp","Tave_wt","PPT_sp")
)

# Details about available data
list_data()
list_gcm()
list_normal()
list_period()
list_run()
list_ssp()
list_variables()