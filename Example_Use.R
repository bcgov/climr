library(climRdev)
library(data.table)
library(terra)
library(pool)

in_locations <- fread("Test_Locations_vsmall.csv")
in_xyz <- as.data.frame(in_locations[,.(Long,Lat,Elev)]) ##currently needs to be a data.frame or matrix, not data.table

thebb <- get_bb(in_xyz) ##get bounding box based on input points
dbCon <- data_connect() ##connect to database
normal <- normal_input_postgis(dbCon = dbCon, bbox = thebb, cache = TRUE) ##get normal data and lapse rates
plot(normal[[42]])

##get GCM anomolies
gcm <- gcm_input_postgis(dbCon, bbox = thebb, gcm = c("BCC-CSM2-MR","UKESM1-0-LL"), ssp = "ssp245", period = c("2021_2040","2041_2060","2061_2080"))
plot(gcm[[1]][[1]])

# Downscale!
results <- downscale(
  xyz = in_xyz,
  normal = normal,
  gcm = gcm,
  vars = c("CMD_sp","Tave_wt","PPT_sp","DD5")
)

poolClose(dbCon)
