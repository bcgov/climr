###Prep historic modelled timeseries
library(terra)
library(data.table)
library(climr)
library(RPostgres)

con <- data_connect()
r <- input_obs(con, bbox = c(83,15,-52.5,-170))

ppt <- rast("Y:\\TransferAnomalies/delta.from.1961_1990.to.2001_2020.Pr.tif")
tmax <- rast("Y:\\TransferAnomalies/delta.from.1961_1990.to.2001_2020.Tmax.tif")
tmin <- rast("Y:\\TransferAnomalies/delta.from.1961_1990.to.2001_2020.Tmin.tif")

plot(r[[1]][[12]])

dbCon <- data_connect()
nms <- dbGetQuery(con, "select * from historic_layers")
allr <- c(ppt,tmax,tmin)
names(allr) <- nms$fullnm
writeRaster(allr, "Aseem_2001_2020.tif", gdal="COMPRESS=NONE", overwrite = T)
r2 <- rast("Colin_2001_2020.tif")
plot(r2[[13]])

allgcms <- list.files("C:/Users/kdaust/LocalFiles/ProcessedGCMs/gcm/historic/")
gcm_nm <- allgcms[1]
ref_rast <- rast("C:/DataFiles/ProcessedRasters/Normal_NA.wlrdem.tif")
ref_rast <- ref_rast[[1]]

library(RPostgres)
conn <- dbConnect(RPostgres::Postgres(),dbname = 'climr',
                  host = '146.190.244.244',
                  port = 5432,
                  user = 'postgres',
                  password = '')



for(gcm_nm in allgcms[-c(1:6)]) {
  cat(gcm_nm,"\n")
  mod.files <- list.files(paste0("C:/Users/kdaust/LocalFiles/ProcessedGCMs/gcm/historic/",gcm_nm,"/"), full.names = TRUE, pattern = "\\.deltas.tif", all.files = TRUE)
  rt <- rast(mod.files)
  nms <- names(rt)
  nms2 <- gsub("ensembleMean","ensemble_Mean",nms)
  
  metadt <- data.table(Orig = nms2)
  metadt[,c("mod","var","month","type","run","year") := tstrsplit(Orig, "_")]
  metadt[run == "Mean", run := "ensembleMean"]
  metadt[,c("Orig","type") := NULL]
  metadt[,laynum := seq_along(nms)]
  
  dbWriteTable(conn, "esm_layers_hist", metadt, row.names = F, append = TRUE)
  
}
dbExecute(conn,"create index on esm_layers_hist(mod,run,year)")

for(gcm_nm in allgcms[-c(1:4)]) {
  ###gcm time series
  cat(gcm_nm)
  mod.files <- list.files(paste0("C:/Users/kdaust/LocalFiles/ProcessedGCMs/gcm/historic/",gcm_nm,"/"), full.names = TRUE, pattern = "\\.tif", all.files = TRUE)
  
  r <- rast(mod.files)
  #r <- crop(r, ref_rast)
  plot(r[[1]])
  # 
  # t1 <- r[[1]]
  # plot(t1)
  # t2 <- crop(t1,temp)
  nm <- names(r)
  nm <- gsub("_pr_", "_PPT_", nm, fixed = TRUE)
  nm <- gsub("_tasmax_", "_Tmax_", nm, fixed = TRUE)
  nm <- gsub("_tasmin_", "_Tmin_", nm, fixed = TRUE)
  
  names(r) <- nm
  ppt_layers <- grep("_PPT_",nm,fixed = TRUE)
  r_ppt <- r[[ppt_layers]]
  r_temp <- r[[-ppt_layers]]
  # Substract reference layers, only keep deltas, plus load in memory instead of disk
  message("Computing deltas for precipitation")
  # Find matching reference layer for each layer
  # Reference layers will match with themselves
  nm_ptt <- nm[ppt_layers]
  ref_layers <- grep("_reference_", nm_ptt, fixed = TRUE)
  names(ref_layers) <- gsub("^([^_]+_[^_]+_[^_]+_).*$", "\\1", nm_ptt[ref_layers])
  matching_ref <- ref_layers[gsub("^([^_]+_[^_]+_[^_]+_).*$", "\\1", nm_ptt)]
  
  # Reference layers positions
  # They will be used to avoid computing deltas of
  # reference layers with themselves
  uniq_ref <- sort(unique(matching_ref))
  
  # Substract reference layer, this takes a few seconds as all
  # data have to be loaded in memory from disk
  r_ppt <- r_ppt[[-uniq_ref]] / r_ppt[[matching_ref[-uniq_ref]]]
  
  message("Computing deltas for tmin and tmax")
  # Find matching reference layer for each layer
  # Reference layers will match with themselves
  nm_temp <- nm[-ppt_layers]
  ref_layers <- grep("_reference_", nm_temp, fixed = TRUE)
  names(ref_layers) <- gsub("^([^_]+_[^_]+_[^_]+_).*$", "\\1", nm_temp[ref_layers])
  matching_ref <- ref_layers[gsub("^([^_]+_[^_]+_[^_]+_).*$", "\\1", nm_temp)]
  
  # Reference layers positions
  # They will be used to avoid computing deltas of
  # reference layers with themselves
  uniq_ref <- sort(unique(matching_ref))
  
  # Substract reference layer, this takes a few seconds as all
  # data have to be loaded in memory from disk
  r_temp <- r_temp[[-uniq_ref]] - r_temp[[matching_ref[-uniq_ref]]]
  #r <- c(r_ppt, r_temp)
  r_all <- c(r_ppt,r_temp)
  
  terra::writeRaster(
    r_all,
    file.path(paste0("C:/Users/kdaust/LocalFiles/ProcessedGCMs/gcm/historic/",gcm_nm), sprintf("gcmData.%s.deltas.tif", gcm_nm)),
    overwrite = TRUE,
    gdal="COMPRESS=NONE"
  )
}
##upload
library(ssh)

session <- ssh_connect("root@146.190.244.244")

for(gcm_nm in allgcms[-c(1,2,3,4)]) {
  cat(gcm_nm)
  scp_upload(session, file.path(paste0("C:/Users/kdaust/LocalFiles/ProcessedGCMs/gcm/historic/",gcm_nm), sprintf("gcmData.%s.deltas.tif", gcm_nm)), 
             to = "/share/gcm_historic")
}

ssh_exec_wait(session, command = c("cd /share",
                                   "ls",
                                   "raster2pgsql -s 4326 -I -C -M Normal_1961_1990MP.wlrdem.tif -t 50x50 normal_wna > normal_wna.sql"))




### Prep historic timeseries
library(terra)
library(data.table)

files <- list.files("C:/DataFiles/wna_normal/Year_1902MP", full.names = T)
dat <- rast(files)
plot(dat)

normal <- rast("C:/Users/kdaust/AppData/Local/R/cache/R/climr/inputs_pkg/normal/Normal_1961_1990MP/Normal_1961_1990MP.wlrdem.tif")
test <- dat - normal$Tmax07
plot(test)

template <- rast("C:/Users/kdaust/AppData/Local/R/cache/R/climr/inputs_pkg/historic/Historic_2001_2020/2001_2020.tif")

yr <- 1902
years <- 1902:2022

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
