
library(sf)
library(data.table)
library(terra)
library(bcmaps)
library(RColorBrewer)


dir <- "//objectstore2.nrs.bcgov/ffec/Data_Sharing/"
# dir <- "C:/Users/CMAHONY/OneDrive - Government of BC/Data/bdy/"
bnd <- vect(paste0(dir, "NimpkishTest.gpkg")) #boundary file

##make study area dem from generalized 25m DEM
dem.source <- bcmaps::cded_terra(st_as_sf(bnd))
dem <- aggregate(dem.source, 11) #aggregate to 250m
res(dem)*111 # ~250m
dem <- project(dem,"epsg:4326") # 
dem <- mask(dem,bnd)
plot(dem)

# climr query
climr_out <- downscale(
  xyz = dem, 
  vars = "Tmax_07"
)
plot(climr_out)

## climr-app export data
app_out <- rast(paste0(dir, "NimpkishTest_Tmax_07.tif"))
app_out <- crop(app_out, climr_out)
plot(app_out, )

##make study area dem from reprojected CEC 250m DEM
# dem.cec <- rast("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/NA_Elevation/data/northamerica/northamerica_elevation_cec_2023.tif")
dem.cec <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Data/DEM/northamerica_elevation_cec_2023.tif") #local copy for speed
dem.cec <- project(dem.cec, dem) 
dem.cec <- mask(dem.cec,bnd)
plot(dem.cec)

# climr query
climr_cec <- downscale(
  xyz = dem.cec, 
  vars = "Tmax_07"
)
plot(climr_cec)

##make study area dem from CEC 250m DEM reprojected to 25m and then aggregated to 250m
# dem.cec <- rast("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/NA_Elevation/data/northamerica/northamerica_elevation_cec_2023.tif")
dem.cec25 <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Data/DEM/northamerica_elevation_cec_2023.tif") #local copy for speed
dem.cec25 <- project(dem.cec25, dem.source) 
dem.cec25 <- project(dem.cec25,"epsg:4326") # 
dem.cec25 <- aggregate(dem.cec25, 11) #aggregate to 250m
dem.cec25 <- mask(dem.cec25,bnd)
plot(dem.cec25)

# ------------------------------
# comparison plots
# ------------------------------
studyarea <- ext(c(-126.3, -126.2, 50.13, 50.2))

lower <- min(values(climr_out), na.rm=T)
upper <- max(values(climr_out), na.rm=T)
inc=0.01
breaks=seq(lower-inc, upper+inc, inc)
ColScheme <- colorRampPalette(c(brewer.pal(5, "GnBu"), rev(brewer.pal(11, "RdYlBu")), rev(brewer.pal(5, "Greys"))))(length(breaks)-1)

par(mfrow=c(2,3), mar=c(0,0,0,0))
plot(climr_out, col=ColScheme, breaks=breaks, mar = NA, axes = FALSE, legend=F, frame = FALSE, xaxt="n", yaxt="n", maxcell = ncell(climr_out))
mtext("True 250m DEM", line=-1.5, adj=0.5, side=3, cex=1, font=2)
plot(studyarea, add=T, lwd=2)

plot(app_out, col=ColScheme, breaks=breaks, mar = NA, axes = FALSE, legend=F, frame = FALSE, xaxt="n", yaxt="n", maxcell = ncell(app_out))
mtext("climr-app 250m output", line=-1.5, adj=0.5, side=3, cex=1, font=2)
plot(studyarea, add=T, lwd=2)

plot(climr_cec, col=ColScheme, breaks=breaks, mar = NA, axes = FALSE, legend=F, frame = FALSE, xaxt="n", yaxt="n", maxcell = ncell(climr_cec))
mtext("Reprojected CEC 250m DEM", line=-1.5, adj=0.5, side=3, cex=1, font=2)
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

X <- app_out
X <- crop(X, studyarea)
plot(X, col=ColScheme, breaks=breaks, mar = NA, axes = FALSE, legend=F, frame = FALSE, xaxt="n", yaxt="n", maxcell = ncell(X),
     main="climr-app 250m output")
box(lwd=2)

X <- climr_cec
X <- crop(X, studyarea)
plot(X, col=ColScheme, breaks=breaks, mar = NA, axes = FALSE, legend=F, frame = FALSE, xaxt="n", yaxt="n", maxcell = ncell(X),
     main="Reprojected CEC 250m DEM")
box(lwd=2)

                 





