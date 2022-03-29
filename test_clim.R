library(raster)
library(climRpnw)

dem <- raster(paste("dem_test.tif", sep=""))
land <- which(!is.na(values(dem)))

dem.pts <- rasterToPoints(dem, spatial=T) 
dem.pts <- as.data.frame(dem.pts)

X <- dem
values(X) <- NA

xyz <- data.frame(
  lon=dem.pts$x,
  lat=dem.pts$y,
  elev=dem.pts[,1]
)

# Select GCM
gcm <- gcm_input(
  gcm = "EC-Earth3",
  ssp = c("ssp245"),
  period = "2041_2060",
  max_run = 0
)

normal <- normal_input()

# projected normals
projnormals <- downscale(
  xyz = xyz,
  normal = normal,
  gcm = NULL,
  var = list_variables()
)

var="Tave"
run.0 <- projnormals[
  GCM=="EC-Earth3" &
  SSP=="ssp245" &
  RUN=="ensembleMean" &
  PERIOD=="2041_2060"
][[var]]

run.0 <- projnormals[
][[var]]

a <- terra::extract(normal, xyz[,1:2], method = "bilinear")

values(X)[land] <- rowMeans(a[,-1L:-13L])
plot(X)
mtext(var, cex=1.5, line=-1.5, adj=0.05)


