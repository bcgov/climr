library(terra)
library(data.table)
library(climr)

climr_mosaic <- rast("../Common_Files/climr_mosaic.tif")
climr_lrd <- climr_mosaic[[-c(1:36)]]
climr_mosaic <- climr_mosaic[[1:36]]


##tmax
delta <- rast("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1981_2010.to.1961_1990.Tmax.tif")
clim <- climr_mosaic[[grep("Tmax",names(climr_mosaic))]]
delta_r <- resample(delta, clim, method = "bilinear")
clim_tmax <- clim + delta_r

##tmin
delta <- rast("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1981_2010.to.1961_1990.Tmin.tif")
clim <- climr_mosaic[[grep("Tmin",names(climr_mosaic))]]
delta_r <- resample(delta, clim, method = "bilinear")
clim_tmin <- clim + delta_r

##ppt
delta <- rast("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1981_2010.to.1961_1990.Pr.tif")
clim <- climr_mosaic[[grep("PPT",names(climr_mosaic))]]
delta_r <- resample(delta, clim, method = "bilinear")
clim_ppt <- clim + delta_r

clim_all <- c(clim_ppt,clim_res,clim_tmin)
writeRaster(clim_all, "../Common_Files/climr_mosaic_1961_1990.tif")

names(clim_all) <- gsub("_","",names(clim_all))
clim_wlrdem <- lapse_rate(clim_all, dem, nthread = 4L)

names(clim_wlrdem) <- paste0("lr_",names(clim_wlrdem))

temp <- c(clim_all$Tmax07,dem)
lrt <- focalReg(temp)

# Actual writing
terra::writeRaster(
  c(clim_all, climr_lrd),
  file.path("../Common_Files/climr_mosiac_wlrdem_compressed.tif"),
  datatype = "FLT4S",
  overwrite = TRUE)
tr <- rast("../Common_Files/transfer_anomalies/delta.from.1981_2010.to.1961_1990.Tmax.tif")
