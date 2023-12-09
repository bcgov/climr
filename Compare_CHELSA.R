##compare chelsa climatologies to current cimr

library(terra)
library(data.table)


ref_rast <- rast("BC_Template.tif")
ch_rast <- copy(ref_rast)
clr_rast <- copy(ref_rast)

##read in climr and chelsa data
climr_vars <- fread("climr_climatologies_BC.csv")
chelsa_vars <- fread("CHELSA_Climatology_BC.csv")

varnm <- "Tmax07"
ch_rast[chelsa_vars$ID] <- chelsa_vars[[varnm]]
clr_rast[climr_vars$ID] <- climr_vars[[varnm]]
plot(ch_rast)
plot(clr_rast)

diff <- abs(clr_rast - ch_rast)
plot(diff)

##calculate pixelwise gradients using the slope function
ch_grad <- terrain(ch_rast, v="slope")
plot(ch_grad)

clr_grad <- terrain(clr_rast, v = "slope")
plot(clr_grad)

grad_diff <- ch_grad - clr_grad
plot(grad_diff)
writeRaster(grad_diff,"Chelsa_test.tif")


##code to extract data from chelsa files

# files <- list.files("C:/Users/kdaust/LocalFiles/CHELSA/tasmax/",full.names = TRUE)
# 
# cell_nums <- cells(ch2)
# tmaxls <- list()
# nms <- sprintf("Tmax%02d", 1:12)
# for(i in 1:length(files)){
#   cat(".")
#   ch <- rast(files[i])
#   ch2 <- crop(ch, bc2)
#   ch2 <- mask(ch2,bc2)
#   temp <- extract(ch2,cell_nums)
#   tmaxls[[nms[i]]] <- temp
# }
# 
# ch_tmax <- as.data.table(tmaxls)
# 
# ################################################
# files <- list.files("C:/Users/kdaust/LocalFiles/CHELSA/tasmin/",full.names = TRUE)
# 
# tminls <- list()
# nms <- sprintf("Tmin%02d", 1:12)
# for(i in 1:length(files)){
#   cat(".")
#   ch <- rast(files[i])
#   ch2 <- crop(ch, bc2)
#   ch2 <- mask(ch2,bc2)
#   temp <- extract(ch2,cell_nums)
#   tminls[[nms[i]]] <- temp
# }
# 
# ch_tmin <- as.data.table(tminls)
# 
# ################################################
# files <- list.files("C:/Users/kdaust/LocalFiles/CHELSA/pr/",full.names = TRUE)
# files <- files[-1]
# pptls <- list()
# nms <- sprintf("PPT%02d", 2:12)
# for(i in 1:length(files)){
#   cat(".")
#   ch <- rast(files[i])
#   ch2 <- crop(ch, bc2)
#   ch2 <- mask(ch2,bc2)
#   temp <- extract(ch2,cell_nums)
#   pptls[[nms[i]]] <- temp
# }
# 
# ch_ppt <- as.data.table(pptls)
# 
# ch_all <- cbind(ID = cell_nums,ch_tmin,ch_tmax,ch_ppt)
# fwrite(ch_all, "CHELSA_Climatology_BC.csv")
# ##############################################3
# 
# 
# 
# ch_tmin <- rast("C:/Users/kdaust/LocalFiles/CHELSA/pr/CHELSA_pr_04_1981-2010_V.2.1.tif")
# plot(ch_tmin)
# 
# bc_outline <- terra::rast(system.file("extdata", "bc_outline.tif", package="climRdev"))
# plot(bc_outline)
# bc2 <- resample(bc_outline, ch_tmin)
# plot(bc2)
# bc2 <- trim(bc2)
# 
# ch2 <- crop(ch_tmin, bc2)
# plot(ch2)
# ch2 <- mask(ch2,bc2)
# 
# library(climr)
# dem <- rast("../WNA_BGC/WNA_DEM_SRT_30m_cropped.tif")
# t1 <- crds(ch2, na.rm = F)
# elev <- extract(dem,t1)
# point_dat <- as.data.frame(cbind(t1,elev))
# colnames(point_dat) <- c("x","y","elev")
# point_dat$id <- seq_along(point_dat$x)
# cellNums <- cells(ch2)
# point_dat <- point_dat[!is.na(values(ch2)),]
# point_dat <- point_dat[!is.na(point_dat$elev),]
# 
# clim_dat <- climr_downscale(point_dat, which_normal = "BC", return_normal = TRUE)
# fwrite(clim_dat, "climr_climatologies_BC.csv")
# 
# clmr <- copy(ch2)
# 
# values(clmr) <- NA
# writeRaster(clmr, "BC_Template.tif")
# clmr[ch_all$ID] <- ch_all$Tmin01
# plot(clmr)
# plot(ch2)
# 
# 
# writeRaster(grad_diff, "Tmin07_Ch_climr_grad.tif")
