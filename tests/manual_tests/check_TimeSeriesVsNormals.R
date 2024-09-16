## Manual test to determine whether historical anomalies calculated from time series and normals are consistent
## Colin Mahony, Sept 12, 2024

library(climr)
library(data.table)

## Data
points <- data.frame(
  lon = c(-127.7052),
  lat = c(55.3557),
  elev = c(291),
  id = 1
)

clim <- downscale(points[1,], 
                  which_refmap = "refmap_climatena", #this doesn't change anything
                  obs_periods = "2001_2020",
                  obs_years = 1901:2020,
                  obs_ts_dataset = "climatena"
)

par(mar = c(4,4,0.5,0.5), mgp = c(1.75, 0.25, 0))
## check consistency of period averages and time series
ts_avg <- apply(clim[clim$PERIOD %in% 2001:2020,-c(1,2,3)], 2, "mean")  
pd_avg <- as.vector(unlist(clim[clim$PERIOD=="2001_2020",-c(1,2,3)]))

comp <- data.frame(var=names(ts_avg), ts_avg=as.vector(ts_avg), pd_avg=pd_avg)
diff <- comp$ts_avg[-grep("PPT", comp$var)]-comp$pd_avg[-grep("PPT", comp$var)]
var <- comp$var[-grep("PPT", comp$var)]
plot(diff, xaxt="n", xlab="", ylab="diff in 2001-2020 value (time series - normal)")
axis(1, at=1:length(var), labels = var, las=2, tck=0)
lines(c(-99,99), c(0,0), lty=2)
# Not consistent! differences of up to 3 degrees. 


## check consistency of change based on period averages vs time series
comp$ts_chg <- ts_avg - apply(clim[clim$PERIOD %in% 1961:1990,-c(1,2,3)], 2, "mean")  
comp$pd_chg <- pd_avg - as.vector(unlist(clim[clim$PERIOD=="1961_1990",-c(1,2,3)]))

diff.chg <- comp$ts_chg[-grep("PPT", comp$var)]-comp$pd_chg[-grep("PPT", comp$var)]
plot(diff.chg, xaxt="n", xlab="", ylab="diff in 2001-2020 change (time series - normal)")
axis(1, at=1:length(var), labels = var, las=2, tck=0)
lines(c(-99,99), c(0,0), lty=2)
# the inconsistencies carry over. This means the problem is in the 2001-2020 normals


## check consistency of reference period normals based on period averages vs time series
comp$ts_ref <- apply(clim[clim$PERIOD %in% 1961:1990,-c(1,2,3)], 2, "mean")  
comp$pd_ref <- as.vector(unlist(clim[clim$PERIOD=="1961_1990",-c(1,2,3)]))

diff.ref <- comp$ts_ref[-grep("PPT", comp$var)]-comp$pd_ref[-grep("PPT", comp$var)]
plot(diff.ref, xaxt="n", xlab="", ylab="diff in 1961-1990 values (time series - normal)")
axis(1, at=1:length(var), labels = var, las=2, tck=0)
lines(c(-99,99), c(0,0), lty=2)
# consistent within rounding errors


## check climateNA
points_climatena <- points[,c(4,4,2,1,3)]
colnames(points_climatena) <- c("ID1", "ID2", "lat", "long", "el") # rename column names to what climr expects
write.csv(points_climatena, "C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/test.csv", row.names = F)
# Run climateNA and then return to script. 
climatena_ref <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/test_Normal_1961_1990MP.csv")
climatena_ts <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/test_1961-2020MP.csv")
climatena_pd1 <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/test_Decade_2001_2010MP.csv")
climatena_pd2 <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/test_Decade_2011_2020MP.csv")
climatena_pd <- rbindlist(list(climatena_pd1, climatena_pd2),idcol = 'ID1')
climatena_ref <- climatena_ref[, .SD, .SDcols = grep("Tmin|Tmax|PPT", names(climatena_ref))]
climatena_ts <- climatena_ts[, .SD, .SDcols = grep("Year|Tmin|Tmax|PPT", names(climatena_ts))]
climatena_pd <- climatena_pd[, .SD, .SDcols = grep("Tmin|Tmax|PPT", names(climatena_pd))]

ts_avg <- apply(climatena_ts[climatena_ts$Year %in% 2001:2020,-c(1)], 2, "mean")  
pd_avg <-  apply(climatena_pd, 2, "mean")  
diff <- ts_avg[-grep("PPT", names(ts_avg))]-pd_avg[-grep("PPT", names(ts_avg))]
plot(diff, xaxt="n", xlab="", ylab="diff in 2001-2020 value (time series - normal)")
axis(1, at=1:length(var), labels = var, las=2, tck=0)
lines(c(-99,99), c(0,0), lty=2)
# consistent within rounding errors


#-----------------------------------------
# check the CRU time series
e=1
m=2
element <- elements.cru[e]
# dir <- "//objectstore2.nrs.bcgov/ffec/TimeSeries_gridded_monthly/cru_ts4.07" #takes way too long to read from object storage
dir <- "C:/Users/CMAHONY/OneDrive - Government of BC/Data/cru_ts4.08"
files <- list.files(dir, pattern="\\.nc$")
r <- rast(paste(dir, files[grep(element, files)], sep="/"))

r <- crop(r, studyarea)
r <- mask(r, buff.na)
# plot(r[[1]])

# reduce to selected month
temp <- r[[which(substr(time(r),6,7)==monthcodes[m])]]
names(temp) <- substr(time(temp),1,4)

# extract the cru time series for a single location
point <- vect(matrix(c(-120, 50), 1))
plot(point, add=T)
ts <- extract(temp, point, id=F)
ts.x <- names(ts)[2:123]
ts.y <- as.numeric(ts[2:123])
ts.n <- as.numeric(ts[124:length(ts)])

## climateNA time series from climr Data
points <- data.frame(lon = c(-120), lat = c(50), elev = c(1385), id = 1)
library(climr)
clim <- downscale(points[1,], 
                  which_refmap = "refmap_climatena", 
                  obs_periods = "2001_2020",
                  obs_years = 1901:2020,
                  obs_ts_dataset = "climatena"
)
ts.x.climatena <- clim$PERIOD[clim$PERIOD %in% 1901:2022]
ts.y.climatena <- clim[clim$PERIOD %in% 1901:2022,get(paste(c("Tmin", "Tmax", "PPT")[e], monthcodes[m], sep="_"))]
ref.climatena <- mean(ts.y.climatena[ts.x.climatena%in%1961:1990])
ts.y.climatena <- ts.y.climatena + (ref.cru - ref.climatena) # shift to cru baseline

## plot comparing CRU and climateNA time series. 
par(mar = c(4,4,0.5,0.5), mgp = c(1.75, 0.25, 0))
plot(ts.x, ts.y)
ref.cru <- mean(ts.y[ts.x%in%1961:1990])
lines(1902:2020,ts.y.climatena, col="red")
lines(c(1961,1990), rep(mean(ts.y.climatena[ts.x.climatena%in%1961:1990]),2), col="red")
lines(c(2001,2020), rep(mean(ts.y.climatena[ts.x.climatena%in%2001:2020]),2), col="red")
lines(c(1961,1990), rep(ref.cru,2), lty=2)
lines(c(2001,2020), rep(mean(ts.y[ts.x%in%2001:2020]),2), lty=2)
legend("topleft", legend=c("CRU", "ClimateNA"), pch=c(1, NA), col = c(1,2), lty=c(2,1), bty="n")

# # evaluate in climr
# clim <- plot_timeSeries_input(points, gcms = list_gcms()[c(1)])
# plot_timeSeries(clim, 
#                 var1 = paste(c("Tmin", "Tmax", "PPT")[e], monthcodes[m], sep="_"), 
#                 obs_ts_dataset = c("cru.gpcc", "climatena"),
#                 gcms = list_gcms()[c(1)]
# )
# #same. there is a downward shift after year 2000. 

#-----------------------------------------
## Compare Spatial Patterns of CRU vs. ClimateNA
#-----------------------------------------

lim <- 2
breaks <- seq(0-lim, lim, 0.1)
colscheme <- colorRampPalette(hcl.colors(5,"Blue-Red 3"))(length(breaks)-1)

# CRU
elements <- c("Tmin", "Tmax", "Pr")
e=1
m=7
for(e in 1:3){
  anom <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  par(mfrow=c(3,4), mar=c(0,0,0,0))
  for(m in 1:12){
    X <- anom[[m]]
    if(e==3) X <- log2(X)
    X[X>lim] <- lim
    X[X < 0-lim] <- 0-lim
    plot(X, col=colscheme, breaks=breaks, main=paste(month.name[m], elements[e]), axes=F, type="continuous")
    # plot(bdy.na, add=T)
  }
}

# ClimateNA
#low res dem for north america
dem <- rast("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/dem_noram_lowres.tif")
dem <- project(dem, anom, method="mode") 
my_grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
climna_grid <- my_grid[,c(1,1,3,2,4)]
colnames(climna_grid) <- c("id1", "id2", "lat", "lon", "el") # rename column names to what climr expects
write.csv(climna_grid, "C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow.csv", row.names = F)
## Run climateNA and then return to script. 
climna.ref <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow_Normal_1961_1990MP.csv")
climna.2001 <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow_Decade_2001_2010MP.csv")
climna.2011 <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow_Decade_2011_2020MP.csv")
climna.recent <- (climna.2001 + climna.2011)/2
climna.anom <- climna.recent - climna.ref
ppt_columns <- grep("PPT", names(climna.anom), value = TRUE)
climna.anom[, (ppt_columns) := climna.recent[, .SD, .SDcols = ppt_columns] / climna.ref[, .SD, .SDcols = ppt_columns]]
climna.anom[, c(1, 2, 3, 4, 5) := climna.ref[, c(1, 2, 3, 4, 5), with = FALSE]]

X <- rast(dem) # use the DEM as a template raster
for(e in 1:3){
  par(mfrow=c(3,4), mar=c(0,0,0,0))
  for(m in 1:12){
    X[climatena_anom[, id1]] <- climatena_anom[,get(paste0(elements[e], monthcodes[m]))]
    if(e==3) X <- log2(X)
    X[X>lim] <- lim
    X[X < 0-lim] <- 0-lim
    plot(X, col=colscheme, breaks=breaks, main=paste(month.name[m], elements[e]), axes=F, type="continuous")
    # plot(bdy.na, add=T)
  }
}

## pairwise comparison of CRU and ClimateNA
X <- rast(dem) # use the DEM as a template raster
for(e in 1:3){
  anom <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  for(m in 1:12){
    par(mfrow=c(1,2), mar=c(0,0,0,0))

    X <- anom[[m]]
    if(e==3) X <- log2(X)
    X[X>lim] <- lim
    X[X < 0-lim] <- 0-lim
    plot(X, col=colscheme, breaks=breaks, main=paste(month.name[m], elements[e], "(CRU)"), axes=F, type="continuous")
    
    X[climna.anom[, id1]] <- climna.anom[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
    if(e==3) X <- log2(X)
    X[X>lim] <- lim
    X[X < 0-lim] <- 0-lim
    plot(X, col=colscheme, breaks=breaks, main=paste(month.name[m], elements[e], "(ClimateNA)"), axes=F, type="continuous")
  }
}

