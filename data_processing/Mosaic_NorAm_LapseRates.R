# calculate lapse rate layers for the climr mosaics, and write to file. 

library(terra)

e=3
m=1

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
elements <- c("Tmin", "Tmax", "Pr")


# dir <- "//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/"
dir <- "C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/" # option to use a local copy for faster processing
files <- list.files(dir, pattern=paste(".*.tif", sep=""))
dem <- rast(paste(dir, "climr_mosaic_dem.tif", sep=""))
clim <- rast(paste(dir, files[1:36], sep=""))
names(clim) <- paste0(rep(c("PPT", "Tmax", "Tmin"), each=12), rep(monthcodes, times=3))

lapse <- lapse_rate(clim, dem)

names(clim) <- paste0(rep(c("PPT_", "Tmax_", "Tmin_"), each=12), rep(monthcodes, times=3))
names(lapse) <- paste0("lr_", names(clim))
combined <- c(clim, lapse, dem)

writeRaster(combined, "//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic.tif", overwrite=T)

# -----------------------------------
# evaluate
dem.NA <- rast("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic_dem.tif")
dem <- crop(dem.NA, studyarea)
studyarea <- ext(c(-124, -118, 47, 50))
grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
colnames(grid) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects

mosaic.NA <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/climr_mosaic.tif")
mosaic <- crop(mosaic.NA, studyarea)

climatena <- input_refmap(dbCon, get_bb(grid), reference = "refmap_climatena")
climatena <- crop(climatena, studyarea)

plot(mosaic[[13]])
plot(climatena[[13]])

dem.800m <- rast("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic_dem_800m.tif")
dem.800m <- crop(dem.800m, studyarea)



