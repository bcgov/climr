##historic observed timeseries
library(terra)
library(data.table)
library(climr)

curr_lay <- climr:::db_safe_query("select * from historic_cru_gpcc_layers")
req_yr <- unique(yr[!yr %in% curr_lay$period])
req_lay <- tmax[[yr %in% req_yr]]

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09","10","11","12")
table_name <- "historic_cru_gpcc"
ext_sql <- paste0("SELECT
    ST_XMin(extent) AS xmin,
    ST_YMin(extent) AS ymin,
    ST_XMax(extent) AS xmax,
    ST_YMax(extent) AS ymax
FROM (
    SELECT ST_Extent(ST_Envelope(rast)) AS extent
    FROM ",table_name,"
) sub;")

r_ext <- climr:::db_safe_query(ext_sql)
r_ext <- ext(r_ext$xmin, r_ext$xmax, r_ext$ymin, r_ext$ymax)

## cru.gpcc update

process_deltas <- function(fpath, r_ext, sel_name, precip = FALSE){
  tmax <- rast(fpath)
  tmax <- tmax[[grep(sel_name,names(tmax))]]
  tmax <- crop(tmax,r_ext)
  tm <- time(tmax)
  nrm_per <- tmax[[tm >= as.Date("1961-01-01") & tm <= as.Date("1990-12-31")]]
  nrm <- tapp(nrm_per, index = "months", fun = mean)
  if(precip){
    delta <- tmax/nrm
  } else {
    delta <- tmax - nrm
  }
   return(delta)
}

tmax_delta <- process_deltas("../Common_Files/cru_gpcc/cru_ts4.09.1901.2024.tmx.dat.nc", r_ext, "tmx")
tmin_delta <- process_deltas("../Common_Files/cru_gpcc/cru_ts4.09.1901.2024.tmn.dat.nc", r_ext, "tmn")

tmp <- rast("../Common_Files/cru_gpcc/cru_ts4.09.1901.2024.tmx.dat.nc")

ppt <- rast("../Common_Files/cru_gpcc/precip.comb.v2020to2019-v2020monitorafter.total.nc")
tm <- time(ppt)
tm_ym <- format(tm, "%Y-%m")
tm_tmax <- format(time(tmax_delta), "%Y-%m")
ppt <- ppt[[tm_ym %in% tm_tmax]]
ppt <- project(ppt, tmax_delta)
ppt <- crop(ppt, r_ext)
plot(ppt[[1484]])
tm <- time(ppt)
nrm_per <- ppt[[tm >= as.Date("1961-01-01") & tm <= as.Date("1990-12-31")]]
nrm <- tapp(nrm_per, index = "months", fun = mean)
ppt_delta <- ppt/nrm
plot(ppt_delta[[1488]])

cru_gpcc <- c(ppt_delta, tmin_delta, tmax_delta)

var_nms <- rep(c("PPT","Tmin","Tmax"), each = nlyr(tmax_delta))
dir.create("cru_gpcc")
tm <- time(cru_gpcc)
yr <- year(tm)
mn <- month(tm)

cru_sub <- cru_gpcc[[yr %in% c(2023,2024)]]
# for(i in 1:nlyr(cru_gpcc)){
#   cat(".")
#   nm <- paste0(var_nms[i],"_",monthcodes[mn[i]],"_",yr[i],".tif")
#   writeRaster(cru_gpcc[[i]], file.path("cru_gpcc",nm))
# }


qry <- "UPDATE historic_cru_gpcc t
SET rast = ST_AddBand(t.rast, r2.rast)
FROM cru_temp r2
WHERE t.rid = r2.rid;"

q1 <- "SELECT (ST_MetaData(rast)).width  AS tile_width,
       (ST_MetaData(rast)).height AS tile_height
FROM historic_cru_gpcc
LIMIT 1;"

q2 <- "SELECT (ST_MetaData(rast)).scalex AS pixel_width,
       (ST_MetaData(rast)).scaley AS pixel_height,
       (ST_MetaData(rast)).upperleftx AS origin_x,
       (ST_MetaData(rast)).upperlefty AS origin_y,
       (ST_SRID(rast)) AS srid,
       (ST_BandMetaData(rast)).nodatavalue as ndv
FROM historic_cru_gpcc
LIMIT 1;"

q3 <- paste0("SELECT
    ST_XMin(extent) AS xmin,
    ST_YMin(extent) AS ymin,
    ST_XMax(extent) AS xmax,
    ST_YMax(extent) AS ymax
FROM (
    SELECT ST_Extent(ST_Envelope(rast)) AS extent
    FROM ",table_name,"
) sub;")

tilesize <- dbGetQuery(conn, q1)
resalign <- dbGetQuery(conn, q2)
extent <- dbGetQuery(conn, q3)
r_ext <- ext(extent$xmin, extent$xmax, extent$ymin, extent$ymax)

template <- rast(extent = r_ext, resolution = resalign$pixel_width, crs = "epsg:4326")
cru_resamp <- resample(cru_sub, template)
plot(cru_resamp[[5]])

writeRaster(cru_resamp, "temp_raster.tif", overwrite = T)
r2p <- "C:/Program Files/PostgreSQL/17/bin/raster2pgsql.exe"
psql <- "C:/Program Files/PostgreSQL/17/bin/psql.exe"

sqlfile <- tempfile(fileext = ".sql")

system2(
  r2p,
  args = c("-s", "4326", "-M", "-t", "10x10",
           "temp_raster.tif", "cru_temp"),
  stdout = sqlfile
)

dbExecute(conn, "drop table cru_temp")
system2(
  psql,
  args = c("-h", "146.190.244.244",
           "-p", "5432",
           "-U", "postgres",
           "-d", "climr",
           "-f", sqlfile)
)

qry <- "UPDATE historic_cru_gpcc t
SET rast = ST_AddBand(t.rast, r2.rast)
FROM cru_temp r2
WHERE t.rid = r2.rid;"

dbExecute(conn, qry)

nms <- names(cru_sub)
metadt <- data.table(var = nms, period = year(time(cru_sub)), month = monthcodes[month(time(cru_sub))])
metadt[,var := gsub("precip.*","PPT",var)]
metadt[,var := gsub("tmn.*","Tmin",var)]
metadt[,var := gsub("tmx.*","Tmax",var)]
old_laymax <- climr:::db_safe_query("select * from historic_cru_gpcc_layers where laynum = (select max(laynum) from historic_cru_gpcc_layers)")

metadt[,laynum := (old_laymax$laynum+1):(old_laymax$laynum + nrow(metadt))]
metadt[,var_nm := paste0(var, "_", month)]
metadt[,month := NULL]
metadt[,fullnm := paste0(var_nm,"_",period)]
setcolorder(metadt, names(old_laymax))
dbWriteTable(conn, "historic_cru_gpcc_layers", metadt, append = TRUE)
###########################################################################


library(RPostgres)
conn <- dbConnect(RPostgres::Postgres(),dbname = 'climr',
                  host = '146.190.244.244',
                  port = 5432,
                  user = 'postgres',
                  password = '')

dbExecute(conn, qry)

##aseem blended ts

##PPT
fnames <- list.files("../Common_Files/clmr_blend_ts_1901_2024/", pattern = "prcp.*.nc$", full.names = TRUE)
ppt <- rast(fnames)
ppt_tm <- time(ppt)
ord <- order(ppt_tm)
ppt <- ppt[[ord]]
ppt_tm <- time(ppt)
ppt_nrm <- ppt[[ppt_tm >= as.Date("1961-01-01") & ppt_tm <= as.Date("1990-12-31")]]
nrm <- tapp(ppt_nrm, index = "months", fun = mean)
ppt_delta <- (ppt)/(nrm)
plot(ppt_delta[[7]])

# delta_nrm <- ppt_delta[[time(ppt_delta) >= as.Date("1961-01-01") & time(ppt_delta) <= as.Date("1990-01-01")]]
# avg2 <- tapp(delta_nrm, index = "months", fun = mean)
# 
# plot(avg2)
# plot(ppt_delta[[1460]])
# 
# tm <- time(ppt_delta)
# yr <- year(tm)
# mn <- month(tm)
# nms <- paste0("PPT",monthcodes[mn],"_",yr)
# names(ppt_delta) <- nms

##tmin
fnames <- list.files("../Common_Files/clmr_blend_ts_1901_2024/", pattern = "tmin.*.nc$", full.names = TRUE)
tmin <- rast(fnames)
tmin_tm <- time(tmin)
ord <- order(tmin_tm)
tmin <- tmin[[ord]]
tmin_tm <- time(tmin)
tmin_nrm <- tmin[[(tmin_tm > as.Date("1961-01-01")) & (tmin_tm < as.Date("1990-12-31"))]]
nrm <- tapp(tmin_nrm, index = "months", fun = mean)
#plot(nrm[[7]])
#tmin <- tmin[[tmin_tm > as.Date("1901-01-01") & tmin_tm < as.Date("2022-12-31")]]
tmin_delta <- tmin - nrm

##tmax'
fnames <- list.files("../Common_Files/clmr_blend_ts_1901_2024/", pattern = "tmax.*.nc$", full.names = TRUE)
tmin <- rast(fnames)
tmin_tm <- time(tmin)
ord <- order(tmin_tm)
tmin <- tmin[[ord]]
tmin_tm <- time(tmin)
tmin_nrm <- tmin[[tmin_tm >= as.Date("1960-01-01") & tmin_tm <= as.Date("1990-12-31")]]
nrm <- tapp(tmin_nrm, index = "months", fun = mean)
plot(nrm[[7]])
#tmin <- tmin[[tmin_tm > as.Date("1901-01-01") & tmin_tm < as.Date("2022-12-31")]]
tmax_delta <- tmin - nrm
plot(tmax_delta[[19]])

mswx_all <- c(ppt_delta, tmin_delta, tmax_delta)
writeRaster(mswx_all,"../Common_Files/climr_blend_anom_ts.tif", gdal="COMPRESS=NONE", overwrite = TRUE)

library(analogsea)


temp <- rast("../Common_Files/cru_gpcc_anom.tif")
mswx_all <- rast("../Common_Files/climr_blend_anom_ts.tif")
##check
delta_nrm <- ppt_delta[[(time(ppt_delta) > as.Date("1961-01-01")) & (time(ppt_delta) < as.Date("1990-01-01"))]]
avg2 <- tapp(delta_nrm, index = "months", fun = mean)
plot(avg2)

##metadata
nms <- names(mswx_all)
metadt <- data.table(var = nms, period = year(time(mswx_all)), month = monthcodes[month(time(mswx_all))])
metadt[,var := gsub("prcp.*","PPT",var)]
metadt[,var := gsub("tmin.*","Tmin",var)]
metadt[,var := gsub("tmax.*","Tmax",var)]
metadt[,laynum := seq_along(var)]
metadt[,var_nm := paste0(var, "_", month)]


###testing
bb <- get_bb(in_xyz)
climrdat <- input_obs_ts(dataset = "mswx.blend", bbox = c(-127.7162, -126.9495, 54.61025, 55.38847), years = 2016)
climrdat_new <- input_obs_ts(dataset = "mswx.blend", bbox = c(-127.7162, -126.9495, 54.61025, 55.38847), years = 2016)

plot(climrdat_new$mswx.blend[[6]])

climrdat <- climrdat$mswx.blend
localdat <- mswx_all[[year(time(mswx_all)) == 2014]]
directdat <- crop(ppt_delta[[year(time(ppt_delta)) == 2014]], climrdat)
obj_strg <- rast("Y:/data_climr_blend_monthly_anomalies/clmr_blend_ts_1901_2024/prcp_climrblend_ano_dt_NA_mon_6_1901_2024.nc")
objdat <- crop(obj_strg[[year(time(obj_strg)) == 2014]], climrdat)
localdat <- crop(localdat, climrdat)

laynum <- 6
plot(localdat[[laynum]])
plot(climrdat[[laynum]])
plot(directdat[[laynum]])
plot(objdat)



# metadt <- data.table(fullnm = nms)
# metadt[,c("var","period") := tstrsplit(fullnm, "_")]
# metadt[,laynum := seq_along(nms)]

library(RPostgres)
conn <- dbConnect(RPostgres::Postgres(),dbname = 'climr',
                  host = '146.190.244.244',
                  port = 5432,
                  user = 'postgres',
                  password = '')
dbWriteTable(conn, "historic_mswx_ts_layers", metadt, row.names = F)
dbExecute(conn, "create index on historic_mswx_ts_layers(period)")
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
