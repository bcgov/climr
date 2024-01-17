## ---------------------------------------------
## REFERENCE OUTPUTS FOR TESTING
## ---------------------------------------------

## this script creates reference outputs for testing using the `main` version of
## the package. For now it is run manually and saves outputs to tests/ 

remotes::install_github("bcgov/climr@main")
library(terra)
library(climr)

dbCon <- data_connect()
on.exit(try(pool::poolClose(dbCon)), add = TRUE)

xyz <- data.frame(lon = c(-127.70), 
                  lat = c(55.35), 
                  elev = c(291L),
                  id = LETTERS[1])

cache_clear()

## get bounding box based on input points
thebb <- get_bb(xyz)

# Create a normal baseline
normal <- normal_input(dbCon = dbCon, bbox = thebb, normal = "composite_normal",
                       cache = TRUE)

# Select GCM
gcms <- c("BCC-CSM2-MR", "INM-CM5-0")

gcm <- gcm_input(
  dbCon, 
  thebb,
  gcm = gcms,
)

gcm_hist <- gcm_hist_input(
  dbCon, 
  thebb,
  gcm = gcms,
)

gcm_ts <- gcm_ts_input(
  dbCon, 
  thebb,
  gcm = gcms
)

historic <- historic_input(
  dbCon, 
  thebb
)

historic_ts <- historic_input_ts(
  dbCon, 
  thebb
)

## make tiny area
dem <- normal$dem2_WNA
xyz <- data.frame(lon = c(-127.7300, -127.7957, -127.5695, -127.5778, -127.7719), 
                  lat = c(55.34114, 55.24666, 55.39660, 55.08937, 55.15381), 
                  elev = NA,
                  id = 1:5)
xyz[, 3] <- extract(dem, xyz[, 1:2], method = "bilinear")[, -1L]

downscaleout_gcm <- downscale(
  xyz = xyz,
  normal = normal,
  gcm = gcm,
  var = list_variables()
)

downscaleout_gcm_hist <- downscale(
  xyz = xyz,
  normal = normal,
  gcm_hist = gcm_hist,
  var = list_variables()
)

downscaleout_gcm_ts <- downscale(
  xyz = xyz,
  normal = normal,
  gcm_ts = gcm_ts,
  var = list_variables()
)

downscaleout_historic <- downscale(
  xyz = xyz,
  normal = normal,
  historic = historic,
  var = list_variables()
)

downscaleout_historic_ts <- downscale(
  xyz = xyz,
  normal = normal,
  historic_ts = historic_ts,
  var = list_variables()
)

saveRDS(xyz, "tests/points_downscale_ref.rds")
saveRDS(downscaleout_gcm, "tests/downscaleout_gcm_ref.rds")
saveRDS(downscaleout_gcm_hist, "tests/downscaleout_gcm_hist_ref.rds")
saveRDS(downscaleout_gcm_ts, "tests/downscaleout_gcm_ts_ref.rds")
saveRDS(downscaleout_historic, "tests/downscaleout_historic_ref.rds")
saveRDS(downscaleout_historic_ts, "tests/downscaleout_historic_ts_ref.rds")
