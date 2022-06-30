remotes::install_github("bcgov/climR-pnw")
library(climRpnw)
# Retrieve package data
data_update()

# Create a normal baseline
normal <- normal_input()
dem1 <- terra::rast("C:/Users/kirid/AppData/Local/R/cache/R/climRpnw/inputs_pkg/normal/Normal_1961_1990MP/Normal_1961_1990MP.wlrdem.tif")
# Select GCM
gcm <- gcm_input(
  gcm = c("BCC-CSM2-MR"),
  ssp = c("ssp245"),
  period = "2041_2060",
  max_run = 0
)

# Provide or create a points dataframe (lon, lat, elev)
n <- 100000
xyz <- data.frame(lon = runif(n, -125, -120), lat = runif(n, 51, 53), elev = numeric(n))
xyz[,3] <- terra::extract(dem1, xyz[,1:2], method = "bilinear")[,-1L]

# Use downscale
results <- downscale(
  xyz = xyz,
  normal = normal,
  gcm = gcm,
  var = c("Tmax01", "DD5_01")
)

# Details about available data
list_data()
list_gcm()
list_normal()
list_period()
list_run()
list_ssp()
list_variables()