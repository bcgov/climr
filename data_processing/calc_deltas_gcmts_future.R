library(terra)
library(data.table)
library(climr)
library(lidR)
library(sf)

# in_dir <- "../../list_csv_test/"
# dirlist <- list.dirs(in_dir)
# use_dirs <- dirlist[grep("ccc",dirlist)]
# use_files <- sapply(use_dirs, FUN = \(x) {list.files(x, full.names = T)})
# flist <- lapply(use_files, FUN = \(x) {fread(x)})

in_dir <- "../Common_Files/gcmts_future/"
out_dir <- "../Common_Files/gcmts_deltas/"
temp <- rast("../Common_Files/colin_climatology/composite_WNA_1961_1990_Pr01.tif")
gcms <- list.dirs(in_dir, full.names = FALSE, recursive = FALSE)
na_ext <- ext(c(-179.4375, -52.3125, 13.455323687013, 82.9744960699135))
na_ext <- ext(temp)
gcm_nm <- gcms[5]
for(gcm_nm in gcms[-c(1:5)]) {
  ###gcm time series
  cat(gcm_nm)
  mod.files <- list.files(paste0(in_dir,gcm_nm,"/"), full.names = TRUE, pattern = "\\.tif", all.files = TRUE)
  
  r_ppt <- rast(mod.files[grep("pr.tif",mod.files)])
  r_tmin <- rast(mod.files[grep("tasmin.tif",mod.files)])
  r_tmax <- rast(mod.files[grep("tasmax.tif",mod.files)])
  
  r_ppt <- crop(rast(mod.files[grep("pr.tif",mod.files)]),na_ext)
  r_tmin <- crop(rast(mod.files[grep("tasmin.tif",mod.files)]),na_ext)
  r_tmax <- crop(rast(mod.files[grep("tasmax.tif",mod.files)]),na_ext)
  

  plot(r_ppt[[1]])
  # 
  # t1 <- r[[1]]
  # plot(t1)
  # t2 <- crop(t1,temp)
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
    r_ppt,
    file.path(paste0(out_dir, sprintf("gcmts.ppt.%s.deltas.tif", gcm_nm))),
    overwrite = TRUE,
    gdal="COMPRESS=NONE"
  )
  terra::writeRaster(
    r_tmax,
    file.path(paste0(out_dir, sprintf("gcmts.tmax.%s.deltas.tif", gcm_nm))),
    overwrite = TRUE,
    gdal="COMPRESS=NONE"
  )
  terra::writeRaster(
    r_tmin,
    file.path(paste0(out_dir, sprintf("gcmts.tmin.%s.deltas.tif", gcm_nm))),
    overwrite = TRUE,
    gdal="COMPRESS=NONE"
  )
}


library(RPostgres)
conn <- dbConnect(RPostgres::Postgres(),dbname = 'climr',
                  host = '146.190.244.244',
                  port = 5432,
                  user = 'postgres',
                  password = 'climr2022')


for(gcm_nm in gcms) {
  cat(gcm_nm,"\n")
  rtmin <- rast(paste0(out_dir,sprintf("gcmts.ppt.%s.deltas.tif", gcm_nm)))
  nms <- names(rtmin)

  metadt <- data.table(fullnm = nms)
  metadt[,c("mod","var","month","scenario","run","period") := tstrsplit(fullnm, "_")]
  #metadt[run == "Mean", run := "ensembleMean"]
  metadt[,var := NULL]
  metadt[,laynum := seq_along(period)]
  
  dbWriteTable(conn, "esm_layers_ts", metadt, row.names = F, append = TRUE)
  
}
dbExecute(conn,"create index on esm_layers_ts(mod,run,period)")
