library(data.table)
library(terra)
library(climr)

library(rworldmap)
library(climr)
library(pool)
data("countriesCoarse")


data("xyzDT")
temp <- xyzDT[1:4,]
dbCon <- data_connect()
test <- gcm_ts_input(dbCon, bbox = get_bb(temp),
                     gcm = list_gcm()[3], 
                     ssp = list_ssp()[c(1,2,4)], 
                     max_run = 3,
                     years = 2015:2100)
t1 <- test$CanESM5
writeRaster(t1, "Test.grd", filetype = "ENVI", overwrite = T)
t2 <- rast("Test.grd")
plot(t1)


projected <- climr_downscale(temp, gcm_models = list_gcm()[3], 
                             ssp = list_ssp()[c(1,2,4)], 
                             max_run = 10,
                             gcm_hist_years = 1851:2014, 
                             gcm_ts_years = 2015:2100, 
                             vars = "Tmax07")

par(mar=c(1,1,1,1), mfrow=c(3,5))
dbCon <- data_connect()
gcm <- gcm_input(
  dbCon = dbCon,
  bbox = c(80, 20, -50, -170),
  gcm = list_gcm(),
  ssp = list_ssp()[2],
  period = list_gcm_period()[2],
)

for(i in 1:length(list_gcm())){
  plot(gcm[[i]][[7]], legend=F, main=list_gcm()[i])
  plot(countriesCoarse, add = T, xlim=c(-170, -50), ylim=c(20,80))
}
poolClose(dbCon)

list_variables("Monthly")
## A data.table of point coordinates, IDs and elevation
data("xyzDT")
temp <- xyzDT[1:4,]
mods <- c(c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3", 
            "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", 
            "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"))

mods <- c("ACCESS-ESM1-5", "IPSL-CM6A-LR", "MIROC6", 
            "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")

gcmres <- gcm_input(
  dbCon = data_connect(),
  bbox = c(80, 20, -50, -170),
  gcm = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5"),
  ssp = list_ssp()[2],
  period = list_gcm_period()[2])

plot(gcmres$`ACCESS-ESM1-5`[[6]])

library(rworldmap)
data("countriesCoarse")
plot(countriesCoarse, xlim=c(-170, -50), ylim=c(20,80))
plot(gcmres[[1]][[7]], legend=F)
plot(countriesCoarse, xlim=c(-170, -50), ylim=c(20,80), add = T)

plot(gcmres[[1]])
## if you just want to downscale points and not think about what happening behind the scenes, use this function

ds_out <- climr_downscale(
  xyz = temp, 
  which_normal = "auto",
  gcm_models = mods,
  gcm_period = list_gcm_period()[2],
  gcm_ts_years = 2020:2050,
  ssp = list_ssp()[4],
  max_run = 3, # we want 3 individual runs for each model
  vars = c("PPT", "CMI04","CMI06","CMI07", "CMI")
)



ds_out <- climr_downscale(
  xyz = temp, 
  which_normal = "auto",
  gcm_models = mods,
  historic_ts = 1950:1965,
  gcm_hist_years = 1950:1965,
  max_run = 0, # we want 3 individual runs for each model
  vars = c("PPT", "CMI04","CMI06","CMI07", "CMI")
)

ds_out_ts <- climr_downscale(
  xyz = temp,
  which_normal = "auto",
  gcm_ts_years = 2015:2040,     ## currently starting at 2021
  gcm_models = list_gcm()[1:5],
  gcm_hist_years = 2001:2014,
  ssp = "ssp245",
  max_run = 1,
  return_normal = TRUE, ## to return the 1961-1990 normals period
  vars = c("MAT", "PPT", "CMI07")
)

tout <- climr_downscale(xyz = temp,
                        which_normal = "auto",
                        gcm_models = list_gcm(),
                        gcm_ts_years = 2080:2100,
                        ssp = "ssp245")

coords <- fread("../../../Downloads/coords.csv")
get_bb(coords)

bb <- c(57.1763589075024, 52.7889907334224, -124.01035407693, -130.046934182713)
dbCon <- data_connect()
clim_vars <- normal_input(dbCon, bb, normal = "normal_composite")

tic()
clim_vars <- climr_downscale(coords, which_normal = "normal_composite", 
                                              vars = list_variables(), return_normal = T)
toc()
