### Create 2001 - 2020 anomolies
library(data.table)
library(terra)

t1 <- rast("inputs_raw/dem/westnorthamerica/Decade_2001_2010MP/PPT01.asc")
plot(t1)

normal <- rast("inputs_pkg/normal/Normal_1961_1990MP/Normal_1961_1990MP.nc")
plot(normal$Normal_1961_1990MP_1)

tgcm <- rast("inputs_pkg/gcm/ACCESS-ESM1-5/gcmData.ACCESS-ESM1-5.pr.nc")
plot(tgcm[[2000]])

#################################
datdir_2001 <- "inputs_raw/dem/westnorthamerica/Decade_2001_2010MP/"
datdir_2011 <- "inputs_raw/dem/westnorthamerica/Decade_2011_2020MP/"
all_files <- list.files(datdir_2001)
all_ppt <- all_files[grep("PPT",all_files)]

d1 <- rast(paste0(datdir_2001,all_ppt[1]))
d1 <- terra::crop(d1,normal)
d2 <- rast(paste0(datdir_2011,all_ppt[1]))
d2 <- terra::crop(d2,normal)
ppt <- (d1 + d2)/2

for(file in all_ppt[-1]){
  d1 <- rast(paste0(datdir_2001,file))
  d1 <- terra::crop(d1,normal)
  d2 <- rast(paste0(datdir_2011,file))
  d2 <- terra::crop(d2,normal)
  davg <- (d1 + d2)/2
  add(ppt) <- davg
}

plot(ppt$PPT01)

ppt2 <- ppt - normal[[1:12]]
plot(ppt2$PPT01)

all_temp <- all_files[!grepl("PPT",all_files)]

d1 <- rast(paste0(datdir_2001,all_temp[1]))
d1 <- terra::crop(d1,normal)
d2 <- rast(paste0(datdir_2011,all_temp[1]))
d2 <- terra::crop(d2,normal)
temp <- (d1 + d2)/2

for(file in all_temp[-1]){
  d1 <- rast(paste0(datdir_2001,file))
  d1 <- terra::crop(d1,normal)
  d2 <- rast(paste0(datdir_2011,file))
  d2 <- terra::crop(d2,normal)
  davg <- (d1 + d2)/2
  add(temp) <- davg
}

temp2 <- temp - normal[[13:36]]
plot(temp2$Tmax07)
temp2 <- temp2/10
plot(temp2$Tmax01)

all_dat <- c(ppt2,temp2)
names(all_dat)
plot(all_dat$PPT01)
plot(all_dat$Tmax07)


# tgcm <- rast("inputs_pkg/gcm/CanESM5/gcmData.CanESM5.pr.nc")
# plot(tgcm[[13]])
# ppt <- resample(ppt, normal, method = "bilinear")
# plot(ppt$PPT01)
# ppt_norm <- normal[[1:12]]
# anom <- ppt/ppt_norm
# plot(anom$PPT01)

#######Temperature####################

#t_norm[t_norm < 1000] <- NA

ref_rast <- rast("inputs_pkg/gcm/ACCESS-ESM1-5/gcmData.ACCESS-ESM1-5.pr.nc")
plot(ref_rast[[1]])
all_ref <- resample(all_dat,ref_rast, method = "bilinear")
plot(all_ref$PPT01)
plot(all_ref$Tmax08)

writeRaster(all_ref, "inputs_pkg/historic/Historic_2001_2020/2001_2020.tif", overwrite = T)
all_ref <- rast("inputs_pkg/historic/Historic_2001_2020/2001_2020.tif")
terra::writeCDF(
  all_ref,
  file.path("inputs_pkg/historic/Historic_2001_2020/2001_2020.nc"),
  overwrite = TRUE,
  zname = "index",
  missval = NA
)
write.csv(names(all_ref),"inputs_pkg/historic/Historic_2001_2020/2001_2020.csv")


plot(refsize[[1]])
plot(temp_anom$Tmin01)

plot(anom$PPT01)
plot(d1)
plot(d2)
