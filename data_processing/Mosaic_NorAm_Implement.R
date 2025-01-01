
# Assemble a composite climatology for western North America from multiple sources
# Colin Mahony 
# Jan 1, 2025

library(terra)
library(data.table)
library(leaflet)
library(RColorBrewer)
library(rnaturalearth)

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
elements <- c("Tmin", "Tmax", "Pr")

# function for creating a weight surface for blending climatologies together
weightSurface <- function(lat, lon, ext){ # ext is c(lon.start, lon.end, lat.start, lat.end)
  weights.lat <- (lat-ext[4])/(ext[3]-ext[4])
  weights.lat[weights.lat>1] <- 1
  weights.lat[weights.lat<0] <- 0
  weights.lon <- (lon-ext[1])/(ext[2]-ext[1])
  weights.lon[weights.lon>1] <- 1
  weights.lon[weights.lon<0] <- 0
  weights <- weights.lat-weights.lon
  weights[weights>1] <- 1
  weights[weights<0] <- 0
  return(weights)
}

studyarea <- ext(c(-170, -52, 14, 83.5))

# study area DEM (create, write, and read)
# dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/", sep="")
# dem.bc <- rast(paste(dir, "PRISM_dem/PRISM_dem.asc", sep=""))
# dem <- aggregate(dem.bc, 3)
# dem <- extend(dem, studyarea) # start with the BC DEM and extend it to the full study area range
# dem <- crop(dem, studyarea) # if the dem had an extent already outside the study area
# dem.noram <- rast("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/NA_Elevation/data/northamerica/northamerica_elevation_cec_2023.tif") #250m dem downloaded from http://www.cec.org/north-american-environmental-atlas/elevation-2023/
# dem.noram <- project(dem.noram, dem, method="near") #project 250m source dem to the study area grid. method="near" to preserve elevation variance 
# dem <- dem.noram  
# dem[is.na(dem)] <- 0
# writeRaster(dem, paste("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic_dem.tif", sep=""), overwrite=T)
land <- dem.noram
values(land)[!is.finite(values(land))] <- NA
values(land)[is.finite(values(land))] <- 1
writeRaster(land, paste("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic_land.tif", sep=""), overwrite=T)
dem <- rast("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic_dem.tif")
land <- rast("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic_land.tif")

# rm(dem.noram)
plot(dem)

# lat and lon rasters
lon <- init(dem, 'x')
lat <- init(dem, 'y')

#-----------------
# Create a buffered land mask for trimming the coastline after mosaicing

# Get coastline data at high resolution (10m) from rNaturalEarth
coastline <- vect(ne_download(scale = 10, type = "coastline", category = "physical", returnclass = "sf"))
coastline <- crop(coastline, studyarea)

# buffer mask. have to build it up because of a bug in terra that leaves holes during rasterize. 
buffered_coastline <- buffer(coastline, width = 5000)  
buffer <- rasterize(buffered_coastline, land, touches = TRUE)
land_buffered <- merge(land, buffer)
buffered_coastline <- buffer(coastline, width = 10000)  
buffer <- rasterize(buffered_coastline, land, touches = TRUE)
land_buffered <- merge(land_buffered, buffer)
buffered_coastline <- buffer(coastline, width = 15000)  
buffer <- rasterize(buffered_coastline, land, touches = TRUE)
land_buffered <- merge(land_buffered, buffer)
buffered_coastline <- buffer(coastline, width = 20000)  
buffer <- rasterize(buffered_coastline, land, touches = TRUE)
land_buffered <- merge(land_buffered, buffer)
#fill a hole created by a bug in terra
land_buffered[lat>25 & lat<25.6 & lon > (-98) & lon < (-97.4)] <- 1

#-----------------
# blending blocks
#-----------------

#-----------------
# Alaska
dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_AK/", sep="")
prism.ak <- rast(paste(dir, list.files(dir, pattern=paste(".*.", c("tmin", "tmax", "ppt")[1],".*.", monthcodes[1], ".asc", sep="")), sep=""))
ext.ak <- ext(prism.ak)
ext.ak[1:2] <- ext.ak[1:2] - 360
ext(prism.ak) <- ext.ak
ext.ak[1] <- -179
prism.ak <- crop(prism.ak, ext.ak)

blend.north <- ext(prism.ak)[3+0.5]
blend.south <- ext(prism.ak)[3]
blend.east <- -141
blend.west <- blend.east-0.5
ext.blend <- c(blend.west, blend.east, blend.north, blend.south)
weights.ak <- weightSurface(lat, lon, ext.blend)

#-----------------
# bc

# start with BC PRISM area
dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/", sep="")
temp <- rast(paste(dir, list.files(dir, pattern=paste(c("tmin", "tmax", "pr")[1],".*._", 1, ".tif", sep="")), sep=""))
temp <- project(temp, dem)
values(temp)[!is.na(values(temp))] <- 1
values(temp)[is.na(values(temp))] <- 0

# blend at 60th parallel
weights1 <- weightSurface(lat, lon, c(-100, -100, 60, 59.75))

# blend at 120th meridion
weights2 <- weightSurface(lat, lon, c(-120, -120.5, 53.81, 53.81))

# blend at 49th parallel
weights3 <- weightSurface(lat, lon, c(-100, -100, 49, 49.25))

weights.bc <- temp - weights1 - weights2 - weights3
weights.bc[weights.bc>1] <- 1
weights.bc[weights.bc<0] <- 0
plot(weights.bc, xlim=c(-142, -110), ylim=c(48, 61))

#-----------------
# CONUS
# Main blending block for 49th parallel
ext.blend1 <- c(-106, -106, 49, 48.5)
weights1 <- weightSurface(lat, lon, ext.blend1)
# plot(weights1, col=rev(heat.colors(99)), ext=pilotarea)

# Main blending block for vancouver island
ext.blend2 <- c(-122.95, -122.7625, 48.15, 48)
weights2 <- weightSurface(lat, lon, ext.blend2)

weights.us <- weights1 + weights2
weights.us[weights.us>1] <- 1
weights.us[weights.us<0] <- 0
# plot(weights.us, col=rev(heat.colors(99)), ext=pilotarea)

#-----------------
# Northern Mexico
ext.blend <- c(-100, -100, 33, 34)
weights.nmex <- weightSurface(lat, lon, ext.blend)
# plot(weights.nmex)

#-----------------
# baja mexico
ext.blend <- c(-111, -109, 33, 34)
weights.baja <- weightSurface(lat, lon, ext.blend)
plot(weights.baja)

#-----------------
# Arctic Coast
studyarea <- ext(c(-155, -100, 65, 72))

ext.blend <- c(-100, -100, 67, 65)
weights.arcticCoast <- weightSurface(lat, lon, ext.blend)
weights.arcticCoast[values(lon <= (-149))] <- 0
weights.arcticCoast[values(lon >= (-106))] <- 0
# plot(weights.arcticCoast)

#-----------------
# Eastern North America

ext.blend0 <- c(-127.1, -127.2, 83.5, 71)
ext.blend1 <- c(-126, -127.1, 71, 70.5)
ext.blend2 <- c(-119.5, -126, 70.5, 70)
ext.blend3 <- c(-115, -119.5, 70, 52)
ext.blend4 <- c(-113, -115, 52, 49)
ext.blend5 <- c(-110, -113, 49, 46)
ext.blend6 <- c(-107, -110, 46, 44)
ext.blend7 <- c(-105, -107, 44, 42)
ext.blend8 <- c(-102, -105, 42, 25)
ext.blend9 <- c(-100, -102, 25, 13)
# plot(ext(ext.blend0[c(2,1,4,3)]), add=T)
# plot(ext(ext.blend1[c(2,1,4,3)]), add=T)
# plot(ext(ext.blend2[c(2,1,4,3)]), add=T)
# plot(ext(ext.blend3[c(2,1,4,3)]), add=T)
# plot(ext(ext.blend4[c(2,1,4,3)]), add=T)
# plot(ext(ext.blend5[c(2,1,4,3)]), add=T)
# plot(ext(ext.blend6[c(2,1,4,3)]), add=T)
# plot(ext(ext.blend7[c(2,1,4,3)]), add=T)
# plot(ext(ext.blend8[c(2,1,4,3)]), add=T)
# plot(ext(ext.blend9[c(2,1,4,3)]), add=T)
weights0 <- weightSurface(lat, lon, ext.blend0)
weights1 <- weightSurface(lat, lon, ext.blend1)
weights2 <- weightSurface(lat, lon, ext.blend2)
weights3 <- weightSurface(lat, lon, ext.blend3)
weights4 <- weightSurface(lat, lon, ext.blend4)
weights5 <- weightSurface(lat, lon, ext.blend5)
weights6 <- weightSurface(lat, lon, ext.blend6)
weights7 <- weightSurface(lat, lon, ext.blend7)
weights8 <- weightSurface(lat, lon, ext.blend8)
weights9 <- weightSurface(lat, lon, ext.blend9)

weights.daymet <- weights0 + weights1 + weights2 + weights3 + weights4 + weights5 + weights6 + weights7 + weights8 + weights9
weights.daymet[weights.daymet>1] <- 1
weights.daymet[weights.daymet<0] <- 0
weights.daymet[values(lat<=25)] <- 1
weights.daymet[values(lat<=25)] <- 1

#-----------------
# function for extending coastal values out into the ocean
#-----------------

extend.coastal <- function(x){
  x <- focal(x, w=3, fun="mean", na.policy="only") 
  x <- focal(x, w=5, fun="mean", na.policy="only") 
  x <- focal(x, w=7, fun="mean", na.policy="only") 
  x <- focal(x, w=9, fun="mean", na.policy="only") 
  values(x)[!is.finite(values(x))] <- NA
  return(x)
}

#-----------------
# compile mosaic
#-----------------

e <- 3
m <- 1
for(e in 1:3){
  for(m in 1:12){
    
    #template raster
    comp <- dem
    values(comp) <- NA
    
    #-----------------------
    # initiate with the Yukon/alberta/BC GAN prediction
    dir <- paste0("//objectstore2.nrs.bcgov/ffec/Mosaic_Yukon/operational/WorldClim/", c("tmin", "tmax", "prec")[e], "/", tolower(month.abb[m]), "/Predictions/")
    gan <- rast(paste(dir, list.files(dir, pattern=".*.nc"), sep=""))
    
    # crop out the fringe
    # plot(gan)
    # plot(ext(gan)+c(-0.5,-1,0,-1), add=T)
    gan <- crop(gan, ext(gan)+c(-0.5,-1,0,-1))
    
    gan.project <- project(gan, comp)
    gan.project <- extend.coastal(gan.project)
    comp <- merge(comp, gan.project)
    # plot(comp)
    
    #-----------------------
    # for precipitation, sub in the random forest prediction for the arctic coast (the GAN failed here for an unknown reason)
    # if(e==3){
    dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/composite_arcticCoast/", sep="")
    file <- paste("composite_arcticCoast_1981_2010_", elements[e], monthcodes[m], ".tif", sep="")
    arcticCoast <- rast(paste(dir, file, sep=""))
    arcticCoast <- project(arcticCoast, comp)
    arcticCoast <- extend.coastal(arcticCoast)
    # plot(arcticCoast)
    
    comp <- mosaic(comp*(1-weights.arcticCoast), arcticCoast*weights.arcticCoast, fun="sum")
    # plot(comp)
    # }
    
    #-----------------------
    #blend in the BC PRISM 
    
    dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/", sep="")
    prism.bc <- rast(paste(dir, list.files(dir, pattern=paste(c("tmin", "tmax", "pr")[e],".*._", m, ".tif", sep="")), sep=""))
    values(prism.bc)[!is.finite(values(prism.bc))] <- NA
    prism.bc <- project(prism.bc, comp)
    comp <- mosaic(comp*(1-weights.bc), prism.bc*weights.bc, fun="sum")
    
    #-----------------------
    # blend in alaska prism
    dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_AK/", sep="")
    prism.ak <- rast(paste(dir, list.files(dir, pattern=paste(".*.", c("tmin", "tmax", "ppt")[e],".*.", monthcodes[m], ".asc", sep="")), sep=""))
    ext.ak <- ext(prism.ak)
    ext.ak[1:2] <- ext.ak[1:2] - 360
    ext(prism.ak) <- ext.ak
    ext.ak[1] <- -179
    prism.ak <- crop(prism.ak, ext.ak)
    
    prism.ak <- project(prism.ak, comp)
    prism.ak <- extend.coastal(prism.ak)
    
    comp <- mosaic(prism.ak*weights.ak, comp*(1-weights.ak), fun="sum")
    # plot(comp)
    
    #-----------------------
    #blend in the US PRISM 
    
    dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_US/PRISM_",c("tmin", "tmax", "ppt")[e] ,"_30yr_normal_1981_2010_800m_all_files_asc/", sep="")
    file <- paste("PRISM_", c("tmin", "tmax", "ppt")[e], "_30yr_normal_1981_2010_800m_", monthcodes[m], "_asc.asc", sep="")
    prism.us <- rast(paste(dir, file, sep=""))
    
    # blend the modified US PRISM to the BC PRISM
    prism.us <- project(prism.us, comp)
    # prism.us <- crop(prism.us, comp)
    # prism.us.fill <- focal(crop(prism.us, ext(c(-123.5, -122.5, 48, 49.25))), w=7, fun="mean", na.policy="only") # add a buffer of approximate values around the puget sound border to allow a wider blend region near bellingham. 
    # prism.us <- merge(prism.us, prism.us.fill)
    prism.us <- extend.coastal(prism.us)
    comp <- mosaic(comp*weights.us, prism.us*(1-weights.us), fun="sum")
    # plot(comp, xlim=c(-126, -120), ylim=c(47, 50))
    # plot(comp)
    
    #-----------------------
    # add in the Machine learning composite for Northern Mexico
    
    dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/composite_nMex/", sep="")
    file <- paste("composite_nMex_1981_2010_", elements[e], monthcodes[m], ".tif", sep="")
    nmex <- rast(paste(dir, file, sep=""))
    nmex <- project(nmex, comp)
    nmex <- extend.coastal(nmex)
    
    comp <- mosaic(comp*(1-weights.nmex), nmex*weights.nmex, fun="sum")
    # plot(comp)
    
    #-----------------------
    # blend Daymet into eastern north america. 
    
    # read in daymet
    dir <- "//objectstore2.nrs.bcgov/ffec/data_daymet/daymet_climatology_1981_2010/"
    daymet <- rast(paste(dir, list.files(dir, pattern=paste(".*.", month.abb[m], "_",  c("tmin", "tmax", "prcp")[e],".*.nc", sep="")), sep=""))
    values(daymet)[!is.finite(values(daymet))] <- NA
    daymet <- project(daymet, comp)
    # plot(daymet)
    
    # add a buffer of mean values (daymet too tight to shore. )
    daymet <- extend.coastal(daymet)
    
    # map <- leaflet() %>%
    #   addTiles(group = "basemap") %>%
    #   addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
    #   addRasterImage(daymet, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "daymet") %>%
    #   addLayersControl(
    #     baseGroups = c("basemap", "sat photo"),
    #     overlayGroups = c("daymet"),
    #     options = layersControlOptions(collapsed = FALSE)
    #   )
    # map
    
    # clear daymet values where they won't be used (would create artefacts along coastline)
    daymet[values(lat)>25 & values(lat)<=40 & values(lon) < c(-105)] <- NA
    daymet[values(lat)>40 & values(lat)<=71 & values(lon) < c(-120)] <- NA
    daymet[values(lat)>71 & values(lat)<=72 & values(lon) < c(-140)] <- NA
    
    # clear comp values where they won't be used (would create artefacts along coastline)
    comp[values(lon) > c(-99)] <- NA
    comp[values(lat)>60 & values(lon) > c(-110)] <- NA

    # plot(weights.daymet, col=rev(heat.colors(99)))
    # plot(comp)
    
    comp <- mosaic(comp*(1-weights.daymet), daymet*weights.daymet, fun="sum")
    # plot(test)
    
    #-----------------------
    # blend in the Machine learning composite for the Baja peninsula
    
    # dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/composite_baja/", sep="")
    dir <- paste("C:/Users/CMAHONY/OneDrive - Government of BC/Projects/2024_intercomparison/outputs/", sep="")
    file <- paste("composite_baja_1981_2010_", elements[e], monthcodes[m], ".tif", sep="")
    baja <- rast(paste(dir, file, sep=""))
    baja <- project(baja, comp)
    baja <- extend.coastal(baja)
    
    comp <- mosaic(comp*(1-weights.baja), baja*weights.baja, fun="sum")
    # plot(comp)
    
    #-----------------------
    # trim the coastal buffer to remove blending artefacts
    comp <- mask(comp, land_buffered)    

    #-----------------------
    # write
    writeRaster(comp, paste("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic_1981_2010_", elements[e], monthcodes[m], ".tif", sep=""), overwrite=T)
    
    print(monthcodes[m])  
  }
  print(elements[e])
}

#-----------------------------
# Map showing the data sources
#-----------------------------

plot(gan.project, col="gray")
comp <- dem
values(comp) <- NA

values(gan.project)[!is.na(values(gan.project))] <- 2
comp <- merge(comp, gan.project)
plot(comp)

values(nmex)[!is.na(values(nmex))] <- 3
comp <- merge(comp, nmex)
plot(comp)

values(prism.ak)[!is.na(values(prism.ak))] <- 4
comp <- mosaic(prism.ak*weights.ak, comp*(1-weights.ak), fun="sum")
plot(comp)

values(prism.us)[!is.na(values(prism.us))] <- 5
comp <- mosaic(comp*weights.us, prism.us*(1-weights.us), fun="sum")
plot(comp)

values(daymet)[!is.na(values(daymet))] <- 6
comp <- mosaic(comp*(1-weights.daymet), daymet*weights.daymet, fun="sum")
values(comp)[!is.na(values(comp)) & values(lat) <= 28] <- 6
plot(comp)

comp <- mask(comp, land_buffered)    
plot(comp)

ocean <- mask(land_buffered, land, inverse=T)
ocean[lat>47 & lat<49 & lon > (-123.5) & lon < (-122)] <- NA

comp.final <- cover(ocean, comp)
comp.final <- mask(comp.final, comp)
plot(comp.final)

dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/", sep="")
prism.bc <- rast(paste(dir, list.files(dir, pattern=paste(c("tmin", "tmax", "pr")[e],".*._", m, ".tif", sep="")), sep=""))
values(prism.bc)[!is.finite(values(prism.bc))] <- NA
prism.bc <- project(prism.bc, comp)
values(prism.bc)[!is.na(values(prism.bc))] <- 7
comp.final <- cover(prism.bc, comp.final)
plot(comp.final)

png(filename=paste("results/climr_mosaic_dataSources.png", sep=""), 
    type="cairo", units="in", width=9.5, height=8.5, pointsize=12, res=300)
par(mar=c(0,0,0,0))
plot(comp.final, col = hcl.colors(99, "Grays"), legend=F, axes=F)
text(-90, 45, "Daymet", font=2)
text(-115, 40, "US\nPRISM", font=2)
text(-126, 57, "BC\nPRISM", font=2)
text(-150, 65, "AK\nPRISM", font=2)
text(-130, 65, "GAN\nPrediction", font=2, col="grey95")
text(-115.5, 29.5, "Random Forest\nPrediction", font=2, pos=2)
text(-75, 35, "20km coastal\nbuffer", font=2, pos=4)
dev.off()

#-----------------------------
# Leaflet maps for QA
#-----------------------------

# rasters for the variable of interest
dm <- daymet
us <- prism.us
cp <- comp

# Color Scheme
combined <- c(values(cp))
combined <- combined[is.finite(combined)]
if(elements[e]=="Pr") combined <- log2(combined)
combined <- combined[is.finite(combined)]
inc=diff(range(combined[is.finite(combined)], na.rm=T))/500
breaks=seq(quantile(combined, 0.005, na.rm=T)-inc, quantile(combined, 0.995, na.rm=T)+inc, inc)
ColScheme <- colorRampPalette(if(elements[e]=="Pr") brewer.pal(9, "YlGnBu") else rev(brewer.pal(11, "RdYlBu")))(length(breaks)-1)
ColPal <- colorBin(ColScheme, bins=breaks, na.color = "white")
ColPal.raster <- colorBin(ColScheme, bins=breaks, na.color = "transparent")

if(elements[e]=="Pr"){
  values(us) <- log2(values(us))
  values(cp) <- log2(values(cp))
  values(dm) <- log2(values(dm))
}

# leaflet map
map <- leaflet() %>%
  addTiles(group = "basemap") %>%
  addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
  # addRasterImage(dem, colors =terrain.colors(99), opacity = 1, maxBytes = 6 * 1024 * 1024, group = "elevation") %>%
  addRasterImage(cp, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "comp") %>%
  addRasterImage(dm, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "daymet") %>%
  addRasterImage(us, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "US Prism") %>%
  addLayersControl(
    baseGroups = c("basemap", "sat photo"),
    overlayGroups = c("comp", "daymet", "US Prism"),
    # overlayGroups = c("comp"),
    options = layersControlOptions(collapsed = FALSE)
  )
map

# coastline buffering QA
map <- leaflet() %>%
  addTiles(group = "basemap") %>%
  addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
  addRasterImage(comp, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "comp") %>%
  addPolylines(data = coastline, color = "blue", weight = 1, opacity = 1, group = "coastline") %>%
  addPolylines(data = buffered_coastline, color = "dodgerblue", weight = 1, opacity = 1, group = "buffered_coastline") %>%
  addRasterImage(dem, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "dem") %>%
  addRasterImage(land, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "land") %>%
  addRasterImage(buffer, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "buffer") %>%
  addRasterImage(land_buffered, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "land_buffered") %>%
  addLayersControl(
    baseGroups = c("basemap", "sat photo"),
    overlayGroups = c("comp", "coastline", "buffered_coastline", "dem", "land", "buffer", "land_buffered"),
    options = layersControlOptions(collapsed = FALSE)
  )
map
