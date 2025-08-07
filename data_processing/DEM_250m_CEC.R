# create a 250m latlon DEM from the 250m CEC Lambert DEM
# NB we didn't end up using this dem and went with another 150m dem created by @kdaust

library(sf)
library(data.table)
library(terra)
library(bcmaps)
library(RColorBrewer)
library(rnaturalearth)


# dem_lambert <- rast("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/NA_Elevation/data/northamerica/northamerica_elevation_cec_2023.tif") #250m dem downloaded from http://www.cec.org/north-american-environmental-atlas/elevation-2023/
dem_lambert <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Data/DEM/northamerica_elevation_cec_2023.tif") # use a local copy for faster processing.
studyarea <- ext(c(-179.0625, -52, 14, 83.5))

res_deg <- 250 / 111500 # target resolution in decimal degrees (111500 meters = 1 degree of latitude)
dem_template <- rast(studyarea, resolution = res_deg, crs = "EPSG:4326")
dem_latlon <- project(dem_lambert, dem_template, method = "near")

# Assign a value of zero to ocean cells (this doesn't increase the file size)
dem_latlon[is.na(dem_latlon)] <- 0

# writeRaster(dem_latlon, paste("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/dem_noram_5arcsec.tif", sep=""), overwrite=T)
writeRaster(dem_latlon, paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/DEM/dem_noram_5arcsec.tif", sep=""), overwrite=T)

dem_latlon <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Data/DEM/dem_noram_5arcsec.tif")

# ------------------------------
# Test for the nimpkish study area
# ------------------------------

# dir <- "//objectstore2.nrs.bcgov/ffec/Data_Sharing/"
dir <- "C:/Users/CMAHONY/OneDrive - Government of BC/Data/bdy/"
bnd <- vect(paste0(dir, "NimpkishTest.gpkg")) #boundary file

##make study area dem from generalized 25m DEM
dem.source <- bcmaps::cded_terra(st_as_sf(bnd))
dem <- aggregate(dem.source, 11) #aggregate to 250m
res(dem)*111 # ~250m
dem <- project(dem,"epsg:4326") # 
dem <- mask(dem,bnd)

# climr query
climr_out <- downscale(
  xyz = dem, 
  vars = "Tmax_07"
)

##make study area dem from reprojected CEC 250m DEM
dem.cec <- crop(dem_latlon, dem) 
dem.cec <- mask(dem.cec,bnd)

# climr query
climr_cec <- downscale(
  xyz = dem.cec, 
  vars = "Tmax_07"
)

## climr-app export data
app_out <- rast(paste0(dir, "NimpkishTest_Tmax_07.tif"))[[1]]
app_out <- crop(app_out, climr_out)


# ------------------------------
# comparison plots
# ------------------------------
studyarea <- ext(c(-126.4, -126.2, 50.06, 50.2))

lower <- min(values(climr_out), na.rm=T)
upper <- max(values(climr_out), na.rm=T)
inc=0.01
breaks=seq(lower-inc, upper+inc, inc)
ColScheme <- colorRampPalette(c(brewer.pal(5, "GnBu"), rev(brewer.pal(11, "RdYlBu")), rev(brewer.pal(5, "Greys"))))(length(breaks)-1)

par(mfrow=c(2,3), mar=c(0,0,0,0))
plot(climr_out, col=ColScheme, breaks=breaks, mar = NA, axes = FALSE, legend=F, frame = FALSE, xaxt="n", yaxt="n", maxcell = ncell(climr_out))
mtext("True 250m DEM", line=-1.5, adj=0.5, side=3, cex=1, font=2)
plot(studyarea, add=T, lwd=2)

plot(climr_cec, col=ColScheme, breaks=breaks, mar = NA, axes = FALSE, legend=F, frame = FALSE, xaxt="n", yaxt="n", maxcell = ncell(climr_cec))
mtext("Reprojected CEC 250m DEM", line=-1.5, adj=0.5, side=3, cex=1, font=2)
plot(studyarea, add=T, lwd=2)

plot(app_out, col=ColScheme, breaks=breaks, mar = NA, axes = FALSE, legend=F, frame = FALSE, xaxt="n", yaxt="n", maxcell = ncell(app_out))
mtext("climr-app 250m output", line=-1.5, adj=0.5, side=3, cex=1, font=2)
plot(studyarea, add=T, lwd=2)

# ------------------------------
# zoomed in
# ------------------------------

par(mar=c(0.5, 0.5, 1.5, 0.5))

X <- climr_out
X <- crop(X, studyarea)
plot(X, col=ColScheme, breaks=breaks, mar = NA, axes = FALSE, legend=F, frame = FALSE, xaxt="n", yaxt="n", maxcell = ncell(X),
     main="True 250m DEM")
box(lwd=2)

X <- climr_cec
X <- crop(X, studyarea)
plot(X, col=ColScheme, breaks=breaks, mar = NA, axes = FALSE, legend=F, frame = FALSE, xaxt="n", yaxt="n", maxcell = ncell(X),
     main="Reprojected CEC 250m DEM")
box(lwd=2)

X <- app_out
X <- crop(X, studyarea)
plot(X, col=ColScheme, breaks=breaks, mar = NA, axes = FALSE, legend=F, frame = FALSE, xaxt="n", yaxt="n", maxcell = ncell(X),
     main="climr-app 250m output")
box(lwd=2)


