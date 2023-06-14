remotes::install_github("bcgov/climR-pnw")
library(climRpnw)
library(data.table)
library(terra)
# Retrieve package data
data_update()

# Create a normal baseline
list_period()
normal <- normal_input()
dem1 <- terra::rast("C:/Users/kirid/AppData/Local/R/cache/R/climRpnw/inputs_pkg/normal/Normal_1961_1990MP_BC/Normal_1961_1990MP_BC.wlrdem.tif")
# # Select GCM
gcm <- gcm_input(
  gcm = c("BCC-CSM2-MR"),
  ssp = c("ssp245"),
  period = "2001_2020",
  max_run = 0
)

test1 <- rast("C:/Users/kirid/AppData/Local/R/cache/R/climRpnw/inputs_pkg/gcm/ACCESS-ESM1-5/gcmData.ACCESS-ESM1-5.deltas.tif")
plot(test1[[1000]])

list_historic()
historic <- historic_input()

# Provide or create a points dataframe (lon, lat, elev)
n <- 100
xyz <- data.frame(lon = runif(n, -125, -120), lat = runif(n, 51, 53), elev = numeric(n))
xyz$elev <- terra::extract(dem1, xyz[,1:2], method = "bilinear")[["dem2_BC"]]

# Use downscale
results <- downscale(
  xyz = xyz,
  normal = normal,
  gcm = gcm,
  historic = historic,
  vars = c("CMD_sp","Tave_wt")
)

# Details about available data
list_data()
list_gcm()
list_normal()
list_period()
list_run()
list_ssp()
list_variables()