##historic observed timeseries
library(terra)
library(data.table)
library(climr)

normal <- rast("C:\\Users\\kirid\\AppData\\Local/R/cache/R/climr/normal/normal_na/4da15456-9ae2-462c-a375-35913dde8c7d.tif")
rna_dem <- rna[[73]]
writeRaster(rna_dem, 'noram_dem.asc',NAflag=-9999,overwrite=TRUE)

temp <- readLines('noram_dem.asc')

write.table(temp,'noram_dem2.asc', row.names=F,col.names=F,quote=F)

ppt <- rast("../Common_Files/TimeSeries_gridded_monthly/GPCC/precip.comb.v2020to2019-v2020monitorafter.total.nc")
plot(ppt$precip_1)

bbna <- c(83.125,	18.545,	-51.5625,	-175.2875)

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09","10","11","12")
con <- data_connect()
normal <- normal_input(con, bbox = bbna, "normal_na")
template <- historic_input(con, bbox = bbna, period = "2001_2020")
plot(template[[1]])
template <- template[[1]]
ppt <- project(ppt, template)

ppt_tm <- time(ppt)
ppt <- crop(ppt, template)
ppt_nrm <- ppt[[ppt_tm >= as.Date("1961-01-01") & ppt_tm <= as.Date("1990-01-01")]]
nrm <- tapp(ppt_nrm, index = "months", fun = mean)
ppt_srt <- ppt[[ppt_tm >= as.Date("1901-01-01") & ppt_tm <= as.Date("2022-12-31")]]
ppt_delta <- ppt_srt/nrm

delta_nrm <- ppt_delta[[time(ppt_delta) >= as.Date("1961-01-01") & time(ppt_delta) <= as.Date("1990-01-01")]]
avg2 <- tapp(delta_nrm, index = "months", fun = mean)

plot(avg2)
plot(ppt_delta[[1460]])

tm <- time(ppt_delta)
yr <- year(tm)
mn <- month(tm)
nms <- paste0("PPT",monthcodes[mn],"_",yr)
names(ppt_delta) <- nms

##tmin
tmin <- rast("../Common_Files/TimeSeries_gridded_monthly/cru_ts4.07/cru_ts4.07.1901.2022.tmn.dat.nc")
nms <- names(tmin)
tmin <- tmin[[grep("tmn",nms)]]
plot(tmin[[1]])
tmin <- crop(tmin,template)
plot(tmin[[1]])
tmin_tm <- time(tmin)
tmin_nrm <- tmin[[(tmin_tm > as.Date("1961-01-01")) & (tmin_tm < as.Date("1990-01-01"))]]
nrm <- tapp(tmin_nrm, index = "months", fun = mean)
plot(nrm[[7]])
plot(normal[[31]])
tmin <- tmin[[tmin_tm > as.Date("1901-01-01") & tmin_tm < as.Date("2022-12-31")]]
tmin_delta <- tmin - nrm

##test
delta_nrm <- tmin_delta[[(tmin_tm > as.Date("1961-01-01")) & (tmin_tm < as.Date("1990-01-01"))]]
avg2 <- tapp(delta_nrm, index = "months", fun = mean)

tmin_delta <- resample(tmin_delta, ppt_delta)
plot(tmin_delta[[18]])

tm <- time(tmin_delta)
yr <- year(tm)
mn <- month(tm)
nms <- paste0("Tmin",monthcodes[mn],"_",yr)
names(tmin_delta) <- nms

##tmax
tmin <- rast("../Common_Files/TimeSeries_gridded_monthly/cru_ts4.07/cru_ts4.07.1901.2022.tmx.dat.nc")
nms <- names(tmin)
tmin <- tmin[[grep("tmx",nms)]]
plot(tmin[[1]])
tmin <- crop(tmin,template)
plot(tmin[[7]])
tmin_tm <- time(tmin)
tmin_nrm <- tmin[[tmin_tm >= as.Date("1960-01-01") & tmin_tm <= as.Date("1990-01-01")]]
nrm <- tapp(tmin_nrm, index = "months", fun = mean)
plot(nrm[[7]])
tmin <- tmin[[tmin_tm > as.Date("1901-01-01") & tmin_tm < as.Date("2022-12-31")]]
tmax_delta <- tmin - nrm
plot(tmax_delta[[19]])
tmax_delta <- resample(tmax_delta, ppt_delta)
plot(tmin_delta[[12]])

delta_nrm <- tmax_delta[[(time(tmax_delta) > as.Date("1961-01-01")) & (time(tmax_delta) < as.Date("1990-01-01"))]]
avg2 <- tapp(delta_nrm, index = "months", fun = mean)

#tmax_delta <- tmax_delta[[tmin_tm > as.Date("1901-01-01")]]

tm <- time(tmax_delta)
yr <- year(tm)
mn <- month(tm)
nms <- paste0("Tmax",monthcodes[mn],"_",yr)
names(tmax_delta) <- nms

gpcc_all <- c(ppt_delta, tmin_delta, tmax_delta)
writeRaster(gpcc_all,"../Common_Files/cru_gpcc_anom.tif", gdal="COMPRESS=NONE", overwrite = TRUE)
temp <- rast("../Common_Files/cru_gpcc_anom.tif")

##check
delta_nrm <- ppt_delta[[(time(ppt_delta) > as.Date("1961-01-01")) & (time(ppt_delta) < as.Date("1990-01-01"))]]
avg2 <- tapp(delta_nrm, index = "months", fun = mean)
plot(avg2)

##metadata
nms <- names(gpcc_all)

metadt <- data.table(fullnm = nms)
metadt[,c("var","period") := tstrsplit(fullnm, "_")]
metadt[,laynum := seq_along(nms)]

library(RPostgres)
conn <- dbConnect(RPostgres::Postgres(),dbname = 'climr',
                  host = '146.190.244.244',
                  port = 5432,
                  user = 'postgres',
                  password = '')
dbWriteTable(conn, "historic_cru_gpcc_layers", metadt, row.names = F, append = TRUE)
dbExecute(conn, "create index on historic_cru_gpcc_layers(period)")
dbExecute(conn, "drop table historic_cru_layers")
# test <- ppt[[ppt_tm == as.Date("1900-07-01")]]
# plot(test)
# plot(test/nrm[[7]])
# plot(ppt_delta[[ppt_tm == as.Date("1900-07-01")]])


###climate NA
files <- list.files("noram_dem2/", full.names = T, recursive = T)
files <- files[!grepl("Rad|Tave",files)]

yr <- 1902
years <- 1902:2023

ref.years <- 1961:1990

for(yr in years){
  cat(".")
  f_yr <- files[grep(yr, files)]
  r <- rast(f_yr)
  r <- terra::aggregate(r, fact = 8, fun = "mean")
  tms <- as.Date(paste0(yr,"-",gsub("\\D", "", names(r)),"-","01"))
  time(r) <- tms
  names(r) <- paste0(names(r),yr)
  if(yr == years[1]){
    r_all <- r
  }else{
    add(r_all) <- r
  }
}

writeRaster(r_all, "../Common_Files/climate_na_lr.tif")

curr_r <- r_all[[grep("PPT", names(r_all))]]
r_nrm <- curr_r[[time(curr_r) >= as.Date("1961-01-01") & time(curr_r) <= as.Date("1990-01-01")]]
nrm <- tapp(r_nrm, index = "months", fun = mean)
delta_ppt <- curr_r/nrm
nms <- sub("([A-Za-z]+)([0-9]+)(.{4})$", "\\1_\\2_\\3", names(delta_ppt))
names(delta_ppt) <- nms

curr_r <- r_all[[grep("Tmin", names(r_all))]]
r_nrm <- curr_r[[time(curr_r) >= as.Date("1961-01-01") & time(curr_r) <= as.Date("1990-01-01")]]
nrm <- tapp(r_nrm, index = "months", fun = mean)
delta_tmin <- curr_r - nrm
nms <- sub("([A-Za-z]+)([0-9]+)(.{4})$", "\\1_\\2_\\3", names(delta_tmin))
names(delta_tmin) <- nms
plot(delta_tmin[[7]])

curr_r <- r_all[[grep("Tmax", names(r_all))]]
r_nrm <- curr_r[[time(curr_r) >= as.Date("1961-01-01") & time(curr_r) <= as.Date("1990-01-01")]]
nrm <- tapp(r_nrm, index = "months", fun = mean)
delta_tmax <- curr_r - nrm
nms <- sub("([A-Za-z]+)([0-9]+)(.{4})$", "\\1_\\2_\\3", names(delta_tmax))
names(delta_tmax) <- nms
plot(delta_tmax[[7]])

deltas_all <- c(delta_ppt,delta_tmin,delta_tmax)
deltas_all <- aggregate(deltas_all, fact = 3, fun = "mean")
writeRaster(deltas_all,"../Common_Files/climatena_anom.tif", gdal="COMPRESS=NONE", overwrite = TRUE)

nms <- names(deltas_all)

metadt <- data.table(fullnm = nms)
metadt[,c("var","month","period") := tstrsplit(fullnm, "_")]
metadt[,var_nm := paste(var,month, sep = "_")]
metadt[,laynum := seq_along(nms)]
metadt[,c("var","month") := NULL]

library(RPostgres)
conn <- dbConnect(RPostgres::Postgres(),dbname = 'climr',
                  host = '146.190.244.244',
                  port = 5432,
                  user = 'postgres',
                  password = '')
dbWriteTable(conn, "historic_climatena_layers", metadt, row.names = F, append = TRUE)
dbExecute(conn, "create index on historic_climatena_layers(period)")

for(yr in years) {
  cat(yr,"...\n")
  files <- list.files(paste0("C:/DataFiles/wna_normal/Year_",yr,"MP"), full.names = T)
  files <- files[!grepl("Rad|Tave",files)]
  hist_dat <- rast(files)
  ##calculate deltas
  nms <- names(hist_dat)
  ppt <- grep("PPT",nms)
  
  pptdelta <- hist_dat[[ppt]]/normal[[1:12]]
  tempdelta <- hist_dat[[-ppt]] - normal[[13:36]]
  alldelta <- c(pptdelta,tempdelta)
  alldelta <- resample(alldelta,template, method = "bilinear")
  #plot(alldelta[[1]])
  names(alldelta) <- paste0(names(alldelta),"_",yr)
  
  if(yr == years[1]) {
    all_hist <- copy(alldelta)
  }else{
    add(all_hist) <- alldelta
  }
}
writeRaster(all_hist,"Historic_Anomolies.tif")
