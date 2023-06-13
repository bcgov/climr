### Create 2001 - 2020 anomolies
library(data.table)
library(terra)

t1 <- rast("inputs_raw/dem/westnorthamerica/dem2_WNA_2001/Decade_2001_2010MP/PPT01.asc")
plot(t1)

normal <- rast("inputs_pkg/normal/Normal_1961_1990MP_BC/Normal_1961_1990MP.nc")
plot(normal$Normal_1961_1990MP_1)

tgcm <- rast("inputs_pkg/gcm/ACCESS-ESM1-5/gcmData.ACCESS-ESM1-5.pr.nc")
plot(tgcm[[2000]])

#################################
datdir_2001 <- "inputs_raw/dem/westnorthamerica/dem2_WNA_2001/Decade_2001_2010MP/"
datdir_2011 <- "inputs_raw/dem/westnorthamerica/dem2_WNA_2010/Decade_2011_2020MP/"
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

ppt <- resample(ppt, normal, method = "bilinear")
plot(ppt$PPT01)
ppt_norm <- normal[[1:12]]
anom <- ppt/ppt_norm
plot(anom$PPT01)

#######Temperature####################
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

temp <- resample(temp, normal, method = "bilinear")
plot(temp$Tmax01)
t_norm <- normal[[-c(1:12)]]
#t_norm[t_norm < 1000] <- NA
temp_anom <- temp - t_norm
temp_anom[temp_anom > 10000] <- NA

all_anom <- c(anom, temp_anom)

terra::writeCDF(
  all_anom,
  "inputs_raw/raw_anom_2001_2020.nc",
  overwrite = TRUE,
  prec = "integer",
  compression = 9,
  shuffle = TRUE,
  missval = 99999
)

refsize <- rast("inputs_pkg/gcm/ACCESS-ESM1-5/gcmData.ACCESS-ESM1-5.pr.nc")
all_ref <- resample(all_anom, refsize, method = "bilinear")
plot(all_ref$Tmin01)

terra::writeCDF(
  all_ref,
  "inputs_raw/anom_2001_2020.nc",
  overwrite = TRUE,
  prec = "integer",
  compression = 9,
  shuffle = TRUE,
  missval = 99999
)
write.csv(names(all_ref),"inputs_raw/anom_2001_2020.csv")


plot(refsize[[1]])
plot(temp_anom$Tmin01)

plot(anom$PPT01)
plot(d1)
plot(d2)
