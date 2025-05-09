## comparison of climr to ClimateNA for north america
## Colin Mahony

library(terra)
library(climr)
library(data.table)

#low res dem for north america
dem <- rast("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/dem_noram_lowres.tif")

my_grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
colnames(my_grid) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects

## climr data
climr <- downscale(xyz = my_grid, vars = list_vars())

## climateNA data 
climna_grid <- my_grid[,c(1,1,2,3,4)]
colnames(climna_grid) <- c("id1", "id2", "lon", "lat", "el") # rename column names to what climr expects
write.csv(climna_grid, "C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noram.csv")
## Run climateNA and then return to script. 
climna <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noram_Normal_1961_1990MSY.csv")

## comparison plot
X <- rast(dem) # use the DEM as a template raster
data("variables")
figDir <- tempdir()

vars <- list_vars()
vars <- vars[-which(vars%in%c("Tave", "Tmin", "Tmax"))]

# for(var in vars[grep("NFFD", vars)]){
var="PPT_07"
for(var in vars){
  png(filename=paste(figDir, "/climrVclimna", var, "png",sep="."), type="cairo", units="in", width=6.5, height=2, pointsize=10, res=600)
  
  par(mfrow=c(1,3), mar=c(0,0,2,2))
  values(X) <- NA
  
  data_climr <-   climr[,get(var)]
  X[climr[, id]] <- data_climr
  plot(X, main=paste("climr", var), axes=F)
  
  s <- which(data_climr>1000)
  points(my_grid$lon[s], my_grid$lat[s])
  
  var_climna <- variables$Code_ClimateNA[which(variables$Code==var)]
  data_climna <- climna[,get(var_climna)]
  data_climna[data_climna == -9999] <- NA
  X[climr[, id]] <- data_climna
  plot(X, main=paste("ClimateNA", var_climna), axes=F)
  
  # var_type <- if(length(grep("Eref|CMD|PPT|PAS|DD|AHM|MSP|MAP", var))>0) "ratio" else "interval"
  var_type <- "interval"
  diff <- if(var_type=="interval") data_climna-data_climr else data_climna/data_climr 
  plotrange <- quantile(diff, c(0.001, 0.999), na.rm=T)
  ceiling <- max(abs(plotrange))
  pal <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(99)
  X[climna[, id1]] <- diff
  plot(X, axes=F, range= if(is.finite(ceiling)) c(-ceiling, ceiling) else NULL, main= if(var_type=="interval") "Difference (ClimateNA - climr)" else "Difference (ClimateNA/climr)", col=pal)
  
  # library(scales)
  # par(mar=c(3,3,2,2))
  # hist_climr <- hist(data_climr, xlab=var, main="")
  # hist(data_climna, add=T, col=alpha("dodgerblue", 0.5), breaks=hist_climr$breaks)
  # legend("topright", legend=c("climr", "ClimateNA"), fill=c("gray", alpha("dodgerblue", 0.5)), bty="n")
  
  print(var)
  dev.off()
}

#----------------------------
## comparison of refmap_climatena to ClimateNA for north america
#----------------------------

## climr data
climr.climatena <- downscale(xyz = my_grid, which_refmap = "refmap_climatena", vars = list_vars())

## comparison plot
X <- rast(dem) # use the DEM as a template raster
data("variables")
figDir <- tempdir()

vars <- list_vars()
vars <- vars[-which(vars%in%c("Tave", "Tmin", "Tmax"))]

# for(var in vars[grep("NFFD", vars)]){
var="CMD"
for(var in vars){
  png(filename=paste(figDir, "/refmapclimatenaVsClimateNA", var, "png",sep="."), type="cairo", units="in", width=6.5, height=2, pointsize=10, res=600)
  
  par(mfrow=c(1,3), mar=c(0,0,2,2))
  values(X) <- NA
  
  data_climr <-   climr.climatena[,get(var)]
  X[climr[, id]] <- data_climr
  plot(X, main=paste("climr (refmap_climatena)", var), axes=F)
  
  var_climna <- variables$Code_ClimateNA[which(variables$Code==var)]
  data_climna <- climna[,get(var_climna)]
  data_climna[data_climna == -9999] <- NA
  X[climr[, id]] <- data_climna
  plot(X, main=paste("ClimateNA", var_climna), axes=F)
  
  # var_type <- if(length(grep("Eref|CMD|PPT|PAS|DD|AHM|MSP|MAP", var))>0) "ratio" else "interval"
  var_type <- "interval"
  diff <- if(var_type=="interval") data_climna-data_climr else data_climna/data_climr 
  plotrange <- quantile(diff, c(0.001, 0.999), na.rm=T)
  ceiling <- max(abs(plotrange))
  pal <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(99)
  X[climna[, id1]] <- diff
  plot(X, axes=F, range= if(is.finite(ceiling)) c(-ceiling, ceiling) else NULL, main= if(var_type=="interval") "Difference (ClimateNA - climr)" else "Difference (ClimateNA/climr)", col=pal)
  
  # library(scales)
  # par(mar=c(3,3,2,2))
  # hist_climr <- hist(data_climr, xlab=var, main="")
  # hist(data_climna, add=T, col=alpha("dodgerblue", 0.5), breaks=hist_climr$breaks)
  # legend("topright", legend=c("climr", "ClimateNA"), fill=c("gray", alpha("dodgerblue", 0.5)), bty="n")
  
  print(var)
  dev.off()
}

#----------------------------
## comparison of refmap_climatena to ClimateNA for north america (using raster)
#----------------------------

## climr data
climr.climatena <- downscale(xyz = crop(dem, ext(-179, -51, 15, 83)), which_refmap = "refmap_climatena", vars = list_vars())

## comparison plot
X <- rast(dem) # use the DEM as a template raster
data("variables")
figDir <- tempdir()

vars <- list_vars()
vars <- vars[-which(vars%in%c("Tave", "Tmin", "Tmax"))]

# for(var in vars[grep("NFFD", vars)]){
var="Eref"
for(var in vars){
  png(filename=paste(figDir, "/refmapclimatenaVsClimateNA", var, "png",sep="."), type="cairo", units="in", width=6.5, height=2, pointsize=10, res=600)
  
  par(mfrow=c(1,3), mar=c(0,0,2,2))
  values(X) <- NA
  
  test <- climr.climatena[[paste0("REFPERIOD_", var, "_1961_1990")]]
  test <- project(test, dem)
  plot(test, main=paste("climr (raster) (refmap_climatena)", var), axes=F)
  
  var_climna <- variables$Code_ClimateNA[which(variables$Code==var)]
  data_climna <- climna[,get(var_climna)]
  data_climna[data_climna == -9999] <- NA
  X[climr[, id]] <- data_climna
  plot(X, main=paste("ClimateNA", var_climna), axes=F)
  
  # var_type <- if(length(grep("Eref|CMD|PPT|PAS|DD|AHM|MSP|MAP", var))>0) "ratio" else "interval"
  var_type <- "interval"
  diff <- if(var_type=="interval") X-test else X/test 
  plotrange <- quantile(values(diff), c(0.001, 0.999), na.rm=T)
  ceiling <- max(abs(plotrange))
  pal <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(99)
  plot(diff, axes=F, range= if(is.finite(ceiling)) c(-ceiling, ceiling) else NULL, main= if(var_type=="interval") "Difference (ClimateNA - climr)" else "Difference (ClimateNA/climr)", col=pal)
  
  # library(scales)
  # par(mar=c(3,3,2,2))
  # hist_climr <- hist(data_climr, xlab=var, main="")
  # hist(data_climna, add=T, col=alpha("dodgerblue", 0.5), breaks=hist_climr$breaks)
  # legend("topright", legend=c("climr", "ClimateNA"), fill=c("gray", alpha("dodgerblue", 0.5)), bty="n")
  
  print(var)
  dev.off()
}

#----------------------------
# Everything below here is EDA/troubleshooting, not part of the test. 
#----------------------------

#----------------------------
# Investigate speckling
#----------------------------

#----------------------------
# compare refmap to downscaled

## get bounding box based on input points
thebb <- get_bb(my_grid)

## get database connection
dbCon <- data_connect()

# obtain the climatena 1961-1990 normals for the study area. 
refmap <- input_refmap(dbCon, thebb, reference = "refmap_climr")

var="Tmin_01"
par(mfrow=c(1,2), mar=c(0,0,2,2))
values(X) <- NA

plot(refmap[[names(refmap)==var]], main=paste("climr refmap", var), axes=F)

data_climr <-   climr[,get(var)]
X[climr[, id]] <- data_climr
plot(X, main=paste("climr downscaled", var), axes=F)
s <- which(data_climr>100)
points(my_grid$lon[s], my_grid$lat[s])


#----------------------------
# Zoom in and investigate lapse rates

var="Tmin_01"
par(mfrow=c(1,3), mar=c(0,0,2,2))
values(X) <- NA

studyarea <- ext(c(-121, -118, 65, 66.5))

plot(refmap[[names(refmap)==var]], main=paste("climr refmap", var), xlim=studyarea[1:2], ylim=studyarea[3:4])

data_climr <-   climr[,get(var)]
X[climr[, id]] <- data_climr
plot(X, main=paste("climr downscaled", var), xlim=studyarea[1:2], ylim=studyarea[3:4])
s <- which(data_climr>100)
points(my_grid$lon[s], my_grid$lat[s])

plot(refmap[[grep(var, names(refmap))[2]]], main=paste("climr lapse rate", var), xlim=studyarea[1:2], ylim=studyarea[3:4])
s <- which(data_climr > 100)
points(my_grid$lon[s], my_grid$lat[s])

#----------------------------
# log transformation to better understand lapse rates

var="Tmin_01"
par(mfrow=c(1,3), mar=c(0,0,2,2))
values(X) <- NA

studyarea <- ext(c(-121, -118, 65, 66.5))

plot(log10(refmap[[names(refmap)==var]]), main=paste("climr refmap", var), xlim=studyarea[1:2], ylim=studyarea[3:4])

data_climr <-   climr[,get(var)]
X[climr[, id]] <- log10(data_climr)
plot(X, main=paste("climr downscaled (log10)", var), xlim=studyarea[1:2], ylim=studyarea[3:4])
s <- which(data_climr>100)
points(my_grid$lon[s], my_grid$lat[s])

plot(log10(abs(refmap[[grep(var, names(refmap))[2]]]*1000)), main=paste("climr lapse rate", var), xlim=studyarea[1:2], ylim=studyarea[3:4])
s <- which(data_climr > 100)
points(my_grid$lon[s], my_grid$lat[s])

#----------------------------
# aggregate lapse rates to downscaled resolution

var="Tmin_01"
par(mfrow=c(1,3), mar=c(0,0,2,2))
values(X) <- NA

plot(refmap[[names(refmap)==var]], main=paste("climr refmap", var), xlim=studyarea[1:2], ylim=studyarea[3:4])

data_climr <-   climr[,get(var)]
X[climr[, id]] <- data_climr
plot(X, main=paste("climr downscaled", var), xlim=studyarea[1:2], ylim=studyarea[3:4])
s <- which(data_climr>100)
points(my_grid$lon[s], my_grid$lat[s])

lr <- crop(refmap[[grep(var, names(refmap))[2]]], studyarea)
lr <- project(lr, dem)
plot(lr, main=paste("climr lapse rate", var), xlim=studyarea[1:2], ylim=studyarea[3:4])
s <- which(data_climr > 100)
points(my_grid$lon[s], my_grid$lat[s])


#----------------------------
# log transformation to better understand lapse rates

var="Tmin_01"
par(mfrow=c(1,3), mar=c(0,0,2,2))
values(X) <- NA

plot(refmap[[names(refmap)==var]], main=paste("climr refmap", var), xlim=studyarea[1:2], ylim=studyarea[3:4])

lr <- crop(refmap[[grep(var, names(refmap))[2]]], studyarea)
lr_abslog <- log10(abs(lr*1000))
plot(lr_abslog, main=paste("abs climr lapse rate log10(1000*K/m)", var), xlim=studyarea[1:2], ylim=studyarea[3:4])
s <- which(data_climr > 0)
points(my_grid$lon[s], my_grid$lat[s])
s <- which(data_climr < -100)
points(my_grid$lon[s], my_grid$lat[s])

dem.crop <- crop(refmap[[grep("dem", names(refmap))]], studyarea)
plot(log10(dem.crop), main=paste("climr dem (log10)", var), xlim=studyarea[1:2], ylim=studyarea[3:4])


#----------------------------
# histogram of lapse rates

par(mfrow=c(1,2), mar=c(3,3,2,0))
hist(lr_abslog, main="absolute climr lapse rate, log10(1000*K/m)")
hist((lr*1000)^0.05, main="climr lapse rate, (1000*K/m)^0.05")

#----------------------------
# lapse rates in extreme pixels

data_climr <-   climr[,get(var)]
X[climr[, id]] <- data_climr

s <- which(data_climr > 100)
my_grid[s,]
lr <- refmap[[grep(var, names(refmap))[2]]]
pt.lr <- extract(lr, my_grid[s,2:3])
pt.climr <- extract(X, my_grid[s,2:3]) 
pt.refmap <- extract(refmap[[names(refmap)==var]], my_grid[s,2:3]) 

pt.lr*1000
# Note that the lapse rates are not all high. some of them are even zero, and some are negative, which should not result in high temperatures of ~200C. 
# suggests that the lapse rates may be getting interpolated. 

#----------------------------
# remove implausible lapse rates and downscale from scratch

# remove implausible lapse rates (>|20K/km|)
refmap.mod <- refmap
lim <- 20/1000 # set maximum absolute lapse rate to ±20 K/km (but data is in K/m, so limit is 0.01 K/m)
i <- grep("lr_T", names(refmap.mod)) # identify temperature lapse rate layers
refmap.mod[[i]] <- clamp(refmap.mod[[i]], lower = -lim, upper = lim) # apply lapse rate limit

climr.scratch <- downscale_core(xyz = my_grid, refmap = refmap.mod, vars = list_vars())

var="Tmin_01"
par(mfrow=c(1,3), mar=c(0,0,2,2))
values(X) <- NA

X_refmap <- refmap[[names(refmap)==var]]
X_refmap <- project(X_refmap, dem)
plot(X_refmap, main=paste("climr refmap", var), axes=F)

data_climr <-   climr.scratch[,get(var)]
X[climr[, id]] <- data_climr
plot(X, main=paste("clipped lapse rates", var), axes=F)
s <- which(data_climr>100)
points(my_grid$lon[s], my_grid$lat[s])

var_type <- "interval"
diff <- if(var_type=="interval") X_refmap-X else X_refmap/X 
plotrange <- quantile(values(diff), c(0.001, 0.999), na.rm=T)
ceiling <- max(abs(plotrange))
pal <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(99)
plot(diff, axes=F, range= if(is.finite(ceiling)) c(-ceiling, ceiling) else NULL, main= if(var_type=="interval") "Difference (refmap - clipped)" else "Difference (refmap/clipped)", col=pal)

#----------------------------
# compare custom lapse rate downscaling to standard

var="Tmin_01"
par(mfrow=c(1,3), mar=c(0,0,2,2))
values(X) <- NA

data_custom <-   climr.scratch[,get(var)]
X[climr[, id]] <- data_custom
plot(X, main=paste(var, "with clipped lapse rates"), axes=F)
s <- which(data_climr>100)
points(my_grid$lon[s], my_grid$lat[s])

data_climr <-   climr[,get(var)]
X[climr[, id]] <- data_climr
plot(X, main=paste(var, "with climr lapse rates"), axes=F)
s <- which(data_climr>100)
points(my_grid$lon[s], my_grid$lat[s])

var_type <- "interval"
diff <- if(var_type=="interval") data_custom-data_climr else data_custom/data_climr 
plotrange <- quantile(diff, c(0.001, 0.999), na.rm=T)
ceiling <- max(abs(plotrange))
pal <- colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"))(99)
X[climr[, id]] <- diff
plot(X, axes=F, range= if(is.finite(ceiling)) c(-ceiling, ceiling) else NULL, main= if(var_type=="interval") "Difference (clipped - climr)" else "Difference (clipped/climr)", col=pal)



