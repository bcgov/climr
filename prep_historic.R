### Prep historic timeseries
library(terra)
library(data.table)

files <- list.files("C:/DataFiles/wna_normal/Year_1902MP", full.names = T)
dat <- rast(files)
plot(dat)

normal <- rast("C:/Users/kdaust/AppData/Local/R/cache/R/climRpnw/inputs_pkg/normal/Normal_1961_1990MP/Normal_1961_1990MP.wlrdem.tif")
test <- dat - normal$Tmax07
plot(test)

template <- rast("C:/Users/kdaust/AppData/Local/R/cache/R/climRpnw/inputs_pkg/historic/Historic_2001_2020/2001_2020.tif")

yr <- 1902
years <- 1902:2022

for(yr in years){
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
  
  if(yr == years[1]){
    all_hist <- copy(alldelta)
  }else{
    add(all_hist) <- alldelta
  }
}
writeRaster(all_hist,"Historic_Anomolies.tif")
