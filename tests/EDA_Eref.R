## messy code used to troubleshoot the Eref modification required to the Hargreaves equation. 
## colin mahony


day_month <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
day_julian <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)

calc_Eref <- function(m, tmmin, tmmax, latitude) {
  Eref <- numeric(length(tmmax))
  tmean <- (tmmax + tmmin) / 2
  i <- which(tmean >= 0)
  day_month <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  day_julian <- c(15, 45, 74, 105, 135, 166, 196, 227, 258, 288, 319, 349)
  # Paper unclear, 1.18 - 0.0065 in Appendix, 1.18 - 0.0067 in paper
  # Wangetal2012_ClimateWNA_JAMC-D-11-043.pdf
  # Probably missing
  Eref[i] <- 0.0023 * day_month[m] *
    calc_S0_I(day_julian[m], tmean[i], latitude[i]) *
    (tmean[i] + 17.8) * sqrt(tmmax[i] - tmmin[i]) *
    (1.18 - 0.0065 * latitude[i])
  
  Eref[is.na(tmmax)] <- tmmax[is.na(tmmax)] ## use tmmax[is.na(tmmax)] to respect NA type
  
  return(Eref)
}

# Eref by latitude and month at Tmean=13C
x <- seq(40, 80, 0.1)
plot(x, rep(0, length(x)), ylim=c(0,80), col="white", xlab="Latitude", ylab="Eref", main="Eref by latitude and month at Tmean=13C")
latitude=x
tmmin=rep(11, length(x))
tmmax=rep(15, length(x))
for(m in 1:12){
  y <- calc_Eref(m, tmmin, tmmax, latitude)
  points(x,y, col=m, pch=m, cex=0.1)
  text(40, y[1], m, col=m)
  text(80, y[length(x)], m, col=m)
}

# Eref by latitude and Tmean in june
x <- seq(40, 80, 0.1)
plot(x, rep(0, length(x)), ylim=c(0,80), col="white", xlab="Latitude", ylab="Eref", main="Eref by latitude and Tmean in june")
latitude=x
m=6
for(t in seq(-4, 16, 2)){
  tmmin=rep(t-4, length(x))
  tmmax=rep(t+4, length(x))
  y <- calc_Eref(m, tmmin, tmmax, latitude)
  points(x,y, col=m, pch=m, cex=0.1)
  text(40, y[1], t, col=m)
  text(80, y[length(x)], t, col=m)
}

calc_S0_I <- function(d, tm, latitude) {
  # BASIC COMPUTER PROGRAM FOR ESTIMATING DAILY RA VALUES
  # D=JULIAN DAY (JANUARY 1=1)
  # DEC=DECLINATION OF THE SUN IN RADIANS
  # ES=MEAN MONTHLY DISTANCE OF THE SUN TO THE EARTH DIVIDED BY THE MEAN ANNUAL DISTANCE
  # LD=LATITUDES IN DEGREES
  # LDM=MINUTES OF LATITUDES
  # RA=MEAN MONTHLY EXTRATERRESTRIAL RADIATION IN MM/DAY
  # RAL=MEAN MONTHLY EXTRATERRESTRIAL RADIATION IN LANGLEYS/DAY
  # TC=MEAN DAILY TEMPERATURE IN DEGREE CELSIUS
  Y <- cos(0.0172142 * (d + 192L))
  DEC <- 0.40876 * Y
  ES <- 1.0028 + 0.03269 * Y
  XLR <- latitude / 57.2958
  Z <- -tan(XLR) * tan(DEC)
  OM <- -atan(Z / sqrt(-Z * Z + 1)) + pi / 2
  OM[!is.finite(OM)] <- if(m %in% 4:9) 3.1 else 0
  # CALCULATE THE DAILY EXTRATERRESTRIAL RADIATION IN LANGLEYS/DAY
  DL <- OM / 0.1309
  RAL <- 120 * (DL * sin(XLR) * sin(DEC) + 7.639 * cos(XLR) * cos(DEC) * sin(OM)) / ES
  # CALCULATE THE EXTRATERRESTRIAL RADIATION IN MM/DAY
  RA <- RAL * 10 / (595.9 - 0.55 * tm)
  
  return(RA)
}

#plot of OM by month and latitude (the only two variables it depends on )
lat.min <- 65
x <- seq(lat.min, 90, 0.01)
latitude=x
plot(x, rep(0, length(x)), ylim=c(0,3.2), col="white", xlab="Latitude", ylab="OM", main="OM value by latitude and month")
for(m in 1:12){
d=day_julian[m]
Y <- cos(0.0172142 * (d + 192L))
DEC <- 0.40876 * Y
ES <- 1.0028 + 0.03269 * Y
XLR <- latitude / 57.2958
Z <- -tan(XLR) * tan(DEC)
OM <- -atan(Z / sqrt(-Z * Z + 1)) + pi / 2
OM[!is.finite(OM)] <- if(m %in% 4:9) 3.14 else 0 # NB this is a modification of Hargreaves program to provide finite values above the arctic circle. 
points(x,OM, col=m, pch=m, cex=0.1)
text(lat.min, OM[1], m)
# print(min(OM, na.rm=T))
print(max(OM, na.rm=T))
}

library(terra)
library(climr)
library(data.table)

#test for noram dem after loading the changes
dem <- rast("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/dem_noram_lowres.tif")

my_grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
colnames(my_grid) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects

## A simple climr query. This will return the observed 1961-1990 and 2001-2020 mean annual temperature (MAT) for the raster grid points. 
climr <- downscale(xyz = my_grid, which_refmap = "refmap_climatena", vars = list_vars())

par(mfrow=c(1,3))

eref_climr <- climr$Eref
## populate the raster grid with the downscaled climate values
X <- rast(dem) # use the DEM as a template raster
X[climr[, id]] <- eref_climr # populate the raster cells with the 2001-2020 mean annual temperature (MAT) values, using the `id` field as the link. 
plot(X, main="climr Eref")

climna <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noram_Normal_1961_1990MSY.csv")
eref_climna <- climna$Eref
eref_climna[eref_climna<0] <- 0
## populate the raster grid with the downscaled climate values
X <- rast(dem) # use the DEM as a template raster
X[climna[, id1]] <- eref_climna # populate the raster cells with the 2001-2020 mean annual temperature (MAT) values, using the `id` field as the link. 
plot(X, main="ClimateNA Eref")

eref_climna[!is.finite(eref_climr)] <- NA

X[climna[, id1]] <- eref_climna-eref_climr 
plot(X, main="Eref Difference (ClimateNA - climr)")

X[climr[, id]] <- climr$Eref_06 # populate the raster cells with the 2001-2020 mean annual temperature (MAT) values, using the `id` field as the link. 
plot(X, main="climr Eref")

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
par(mfrow=c(3,4))
for(m in 1:12){
  X[climr[, id]] <- climr[[grep("Eref", names(climr))[m+1]]] # populate the raster cells with the 2001-2020 mean annual temperature (MAT) values, using the `id` field as the link. 
  plot(X, main=paste(month.name[m], "Eref"))
}

slice <- climr[which(my_grid$lon==my_grid$lon[which.min(my_grid$lon > -120)]),]
grid_slice <- my_grid[which(my_grid$lon==my_grid$lon[which.min(my_grid$lon > -120)]),]
climr_slice <- downscale(xyz = grid_slice, which_refmap = "refmap_climatena", vars = list_vars())

latitude=my_grid$lat[which(my_grid$lon==my_grid$lon[which.min(my_grid$lon > -120)])]
tmmin=slice$Tmin_06
tmmax=slice$Tmax_06
m=6
x <- latitude
y <- calc_Eref(m, tmmin, tmmax, latitude)
plot(latitude, slice$Eref_06, xlab="Latitude", ylab="Eref", main=paste(month.name[m], "Eref from slice of climr query on raster"))
plot(latitude, y, xlab="Latitude", ylab="Eref", main=paste(month.name[m], "Eref from climr function using climr temperatures"))
plot(latitude, climr_slice$Eref_06, xlab="Latitude", ylab="Eref", main=paste(month.name[m], "Eref from climr query of slice using climr temperatures"))
plot(x, tmmax, xlab="Latitude", ylab="Tmax", main=paste(month.name[m], "climr tmax"))

