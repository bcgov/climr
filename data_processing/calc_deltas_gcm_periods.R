library(terra)
library(data.table)
library(climr)
library(sf)

in_dir <- "../Common_Files/GCM_Periods/"
out_dir <- "../Common_Files/gcm_period_deltas/"
temp <- rast("../Common_Files/colin_climatology/composite_WNA_1961_1990_Pr01.tif")
gcms <- list.dirs(in_dir, full.names = FALSE, recursive = FALSE)
na_ext <- ext(c(-179.4375, -52.3125, 13.455323687013, 82.9744960699135))
na_ext <- ext(temp)
gcm_nm <- gcms[1]

for(gcm_nm in gcms) {
  ###gcm time series
  cat(gcm_nm)
  mod.files <- list.files(paste0(in_dir,gcm_nm,"/"), full.names = TRUE, pattern = "\\.tif", all.files = TRUE)
  
  r_ppt <- rast(mod.files[grep("pr.tif",mod.files)])
  r_tmin <- rast(mod.files[grep("tasmin.tif",mod.files)])
  r_tmax <- rast(mod.files[grep("tasmax.tif",mod.files)])
  
  # r_ppt <- crop(rast(mod.files[grep("pr.tif",mod.files)]),na_ext)
  # r_tmin <- crop(rast(mod.files[grep("tasmin.tif",mod.files)]),na_ext)
  # r_tmax <- crop(rast(mod.files[grep("tasmax.tif",mod.files)]),na_ext)
  
  
  plot(r_ppt[[1]])
  names(r_ppt) <- gsub("_pr_", "_PPT_", names(r_ppt), fixed = TRUE)
  names(r_tmax) <- gsub("_tasmax_", "_Tmax_", names(r_tmax), fixed = TRUE)
  names(r_tmin) <- gsub("_tasmin_", "_Tmin_", names(r_tmin), fixed = TRUE)
  
  # Substract reference layers, only keep deltas, plus load in memory instead of disk
  message("Computing deltas for precipitation")
  # Find matching reference layer for each layer
  # Reference layers will match with themselves
  nm_ptt <- names(r_ppt)
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
  nm_tmin <- names(r_tmin)
  ref_layers <- grep("_reference_", nm_tmin, fixed = TRUE)
  names(ref_layers) <- gsub("^([^_]+_[^_]+_[^_]+_).*$", "\\1", nm_tmin[ref_layers])
  matching_ref <- ref_layers[gsub("^([^_]+_[^_]+_[^_]+_).*$", "\\1", nm_tmin)]
  uniq_ref <- sort(unique(matching_ref))
  r_tmin <- r_tmin[[-uniq_ref]] - r_tmin[[matching_ref[-uniq_ref]]]
  
  nm_tmax <- names(r_tmax)
  ref_layers <- grep("_reference_", nm_tmax, fixed = TRUE)
  names(ref_layers) <- gsub("^([^_]+_[^_]+_[^_]+_).*$", "\\1", nm_tmax[ref_layers])
  matching_ref <- ref_layers[gsub("^([^_]+_[^_]+_[^_]+_).*$", "\\1", nm_tmax)]
  uniq_ref <- sort(unique(matching_ref))
  r_tmax <- r_tmax[[-uniq_ref]] - r_tmax[[matching_ref[-uniq_ref]]]
  
  terra::writeRaster(
    c(r_tmin, r_tmax, r_ppt),
    file.path(paste0(out_dir, sprintf("gcm.%s.deltas.tif", gcm_nm))),
    overwrite = TRUE,
    gdal="COMPRESS=NONE"
  )
  
}


library(RPostgres)
conn <- dbConnect(RPostgres::Postgres(),dbname = 'climr',
                  host = '',
                  port = 5432,
                  user = 'postgres',
                  password = '')

in_dir <- "../Common_Files/gcm_period_deltas/"

for(i in 1:13) {
  gcm <- gcms[i]
  cat(gcm,"\n")
  temp <- rast(paste0(in_dir,"/gcm.",gcm,".deltas.tif"))
  metadat <- names(temp)
  metadat <- gsub("ensembleMean","none_ensembleMean",metadat)
  metadt <- data.table(Orig = metadat)
  metadt[,c("Mod","Var","Month","Scenario","Sc2", "Run","DtStart","DtEnd") := tstrsplit(Orig, "_")]
  metadt[,Period := paste(DtStart,DtEnd,sep = "_")]
  metadt[,c("DtStart","DtEnd","Sc2","Orig") := NULL]
  setnames(metadt,c("mod","var","month","scenario","run","period"))
  metadt[,laynum := seq_along(metadt$mod)]
  dbWriteTable(conn, name = "esm_layers_period", metadt, row.names = FALSE, append = TRUE)
}

dbExecute(conn,"create index on esm_layers_period(mod,scenario)")



for(gcm_nm in gcms) {
  cat(gcm_nm,"\n")
  mod.files <- list.files(paste0(in_dir,gcm_nm,"/"), full.names = TRUE, pattern = "\\.tif", all.files = TRUE)
  rtmin <- rast(mod.files[grep("pr.tif",mod.files)])
  nms <- names(rt)
  nms2 <- gsub("1961_1990","1961",nms)
  
  metadt <- data.table(fullnm = nms2)
  metadt[,c("mod","var","month","scenario","run","period") := tstrsplit(fullnm, "_")]
  #metadt[run == "Mean", run := "ensembleMean"]
  metadt[,var := NULL]
  metadt[,laynum := seq_along(period)]
  
  dbWriteTable(conn, "esm_layers_ts", metadt, row.names = F, append = TRUE)
  
}
dbExecute(conn,"create index on esm_layers_ts(mod,run,period)")
