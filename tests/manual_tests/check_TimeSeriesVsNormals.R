## Manual test to determine whether historical anomalies calculated from time series and normals are consistent
## Colin Mahony, Sept 12, 2024

library(climr)
library(data.table)
library(terra)
library(scales)

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

library(terra)
library(rworldxtra)
data("countriesHigh")

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
elements.cru <- c("tmn", "tmx", "pre", "tmp")

studyarea <- ext(c(-170, -52.25, 13.75, 83.75))
bdy.na <- vect(countriesHigh[grep("Canada|United States|Mexico", countriesHigh$NAME),])
bdy.na <- erase(bdy.na, ext(c(-170, -140, 13, 30))) # erase hawaii
bdy.na <- aggregate(bdy.na) # dissolve
buff.na <- buffer(bdy.na, width=50000) # buffer coastline

e=1
m=7
element <- elements.cru[e]
# dir <- "//objectstore2.nrs.bcgov/ffec/TimeSeries_gridded_monthly/cru_ts4.07" #takes way too long to read from object storage
dir <- "C:/Users/CMAHONY/OneDrive - Government of BC/Data/cru_ts4.08"
files <- list.files(dir, pattern=".nc$")
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
ref.cru <- mean(ts.y[ts.x%in%1961:1990])

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
lines(1902:2020,ts.y.climatena, col="red")
lines(c(1961,1990), rep(mean(ts.y.climatena[ts.x.climatena%in%1961:1990]),2), col="red")
lines(c(2001,2020), rep(mean(ts.y.climatena[ts.x.climatena%in%2001:2020]),2), col="red")
lines(c(1961,1990), rep(ref.cru,2), lty=2)
lines(c(2001,2020), rep(mean(ts.y[ts.x%in%2001:2020]),2), lty=2)
legend("topleft", legend=c("CRU", "ClimateNA"), pch=c(1, NA), col = c(1,2), lty=c(2,1), bty="n")
## big difference between CRU and ClimateNA

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
    X[climna.anom[, id1]] <- climna.anom[,get(paste0(elements[e], monthcodes[m]))]
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
    X <- mask(X, bdy.na)
    if(e==3) X <- log2(X)
    X[X>lim] <- lim
    X[X < 0-lim] <- 0-lim
    plot(X, col=colscheme, breaks=breaks, main=paste(month.name[m], elements[e], if(e==3) "(GPCC)" else "(CRU)"), axes=F, type="continuous")
    
    X[climna.anom[, id1]] <- climna.anom[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
    X <- mask(X, bdy.na)
    if(e==3) X <- log2(X)
    X[X>lim] <- lim
    X[X < 0-lim] <- 0-lim
    plot(X, col=colscheme, breaks=breaks, main=paste(month.name[m], elements[e], "(ClimateNA)"), axes=F, type="continuous")
  }
}


# ==========================================
# two-panel comparison of CRU and climateNA
# ==========================================


element.names <- c("mean daily minimum temperature (K)", "mean daily maximum temperature (K)", " precipitation (%)")

dem.lcc <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Projects/2021_CMIP6Eval_NA/inputs//dem.na.lcc.tif")
ipccregions.lcc <- vect("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Shiny_Apps\\cmip6-NA-eval\\data\\ipccregions_lcc.shp")
bdy.lcc <- project(bdy.na, ipccregions.lcc)

e=1
m=2

png(filename=paste("vignettes/CRUvsClimateNA_2Panel", elements[e], monthcodes[m], "png",sep="."), type="cairo", units="in", width=6.5, height=4.5, pointsize=10, res=300)
# pdf(file=paste("results//CMIP6Eval.Fig3", metric,"pdf",sep="."), width=7.5, height=14, pointsize=12)

mat <- matrix(c(5,1,2,5,3,4), 3)
layout(mat, widths=c(1,1), heights=c(0.2, .05 ,1))

sequence <- 1:12

par(mar=c(0.1,0.1,0.1,0.1))

lim.upper <- if(elements[e]=="Pr") 1 else 3
lim.lower <- if(elements[e]=="Pr") -1 else -3

inc=(lim.upper-lim.lower)/100
breaks=seq(lim.lower, lim.upper+inc, inc)
colscheme <- colorRampPalette(if(elements[e]=="Pr") rev(hcl.colors(5,"Blue-Red 3")) else hcl.colors(5,"Blue-Red 3"))(length(breaks)-1)

anom <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
pct <- if(elements[e]=="Pr") 100 else 1

for(source in c("cru.gpcc", "climatena")){
  
  source.name <- if(source=="climatena") "ClimateNA" else if(elements[e]=="Pr") "GPCC" else "CRU"
  plot(1, type="n", axes=F, xlab="", ylab="")  
  text(1,1, source.name, font=2,cex=1.35)  
  
  par(mar=c(0.1,0.1,0.1,0.1))
  if(source=="cru.gpcc"){
    X <- anom[[m]]
    
  } else {
    X <- rast(dem) # use the DEM as a template raster
    X[climna.anom[, id1]] <- climna.anom[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
  }     
  X <- project(X, dem.lcc)
  X <- crop(X, ipccregions.lcc)
  X <- mask(X, ipccregions.lcc)
  if(e==3) X <- log2(X)
  X[X>lim.upper] <- lim.upper
  X[X < lim.lower] <- lim.lower
  
  image(X, col=colscheme, breaks=breaks, xaxt="n", yaxt="n", bty="n")
  lines(bdy.lcc)
  
  print(source)
}

## legend
plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))  
xl <- 0.2; yb <- 0.3; xr <- 0.8; yt <- 0.5
rect(head(seq(xl,xr,(xr-xl)/length(colscheme)),-1), yb,  tail(seq(xl,xr,(xr-xl)/length(colscheme)),-1),  yt,  border=NA, col=colscheme)
rect(xl,  yb,  xr,  yt)
labels <- if(elements[e]=="Pr") paste(round(2^seq(lim.lower,lim.upper,(lim.upper-lim.lower)/2), 2)*pct-pct, "%", sep="") else round(seq(lim.lower,lim.upper,(lim.upper-lim.lower)/2), 2)*pct
text(seq(xl,xr,(xr-xl)/(length(labels)-1)),rep(yb,length(labels)),labels,pos=1,cex=1.5,font=1, offset=0.5)
text(mean(c(xl,xr)), yt+0.01, paste("Change in", month.name[m], element.names[e], "\n1961-1990 to 2001-2020"), pos=3, cex=1.5, font=2)

dev.off()



# ==========================================
# multipanel comparison of CRU and climateNA
# ==========================================


element.names <- c("mean daily\nminimum temperature (K)", "mean daily\nmaximum temperature (K)", "\nprecipitation (%)")

dem.lcc <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Projects/2021_CMIP6Eval_NA/inputs//dem.na.lcc.tif")
ipccregions.lcc <- vect("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Shiny_Apps\\cmip6-NA-eval\\data\\ipccregions_lcc.shp")

png(filename=paste("vignettes/CRUvsClimateNA_Diffplots", "png",sep="."), type="cairo", units="in", width=6.5, height=15, pointsize=10, res=300)
# pdf(file=paste("results//CMIP6Eval.Fig3", metric,"pdf",sep="."), width=7.5, height=14, pointsize=12)

mat <- matrix(0:(13*9-1), 13)
mat[,8:9] <- mat[,6:7]+2
mat[,5:6] <- mat[,4:5]+1
mat <- rbind(matrix(rep(c(39, 66, 93), each=3), 1), mat)
mat[,c(4,7)] <- max(mat)+1
mat[1,1] <- max(mat)
layout(mat, widths=c(0.25,1,1,0.15,1,1,0.15,1,1), heights=c(0.7, .2,rep(1,12)))

sequence <- 1:12

par(mar=c(0.1,0.1,0.1,0.1))

for(month in month.abb){
  plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,2), ylim=c(0,2))  
  text(1,1, month, font=2,cex=1.5, srt=90)  
}

element=elements[1]
for(e in 1:3){
  
  lim.upper <- if(elements[e]=="Pr") 1 else 3
  lim.lower <- if(elements[e]=="Pr") -1 else -3
  
  inc=(lim.upper-lim.lower)/100
  breaks=seq(lim.lower, lim.upper+inc, inc)
  colscheme <- colorRampPalette(if(elements[e]=="Pr") rev(hcl.colors(5,"Blue-Red 3")) else hcl.colors(5,"Blue-Red 3"))(length(breaks)-1)
  
  anom <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  pct <- if(elements[e]=="Pr") 100 else 1
  
  for(source in c("cru.gpcc", "climatena")){
    
    source.name <- if(source=="climatena") "ClimateNA" else if(elements[e]=="Pr") "GPCC" else "CRU"
    plot(1, type="n", axes=F, xlab="", ylab="")  
    text(1,1, source.name, font=2,cex=1.35)  
    
    par(mar=c(0.1,0.1,0.1,0.1))
    for(m in 1:12){
      if(source=="cru.gpcc"){
        X <- anom[[m]]
        
      } else {
        X <- rast(dem) # use the DEM as a template raster
        X[climna.anom[, id1]] <- climna.anom[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
      }     
      X <- project(X, dem.lcc)
      X <- crop(X, ipccregions.lcc)
      X <- mask(X, ipccregions.lcc)
      if(e==3) X <- log2(X)
      X[X>lim.upper] <- lim.upper
      X[X < lim.lower] <- lim.lower
      
      image(X, col=colscheme, breaks=breaks, xaxt="n", yaxt="n")
      box(col="black", lwd=0.5)
      
      print(m)
    }
    
    print(source)
  }
  
  ## legend
  plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))  
  xl <- 0.1; yb <- 0.3; xr <- 0.9; yt <- 0.5
  rect(head(seq(xl,xr,(xr-xl)/length(colscheme)),-1), yb,  tail(seq(xl,xr,(xr-xl)/length(colscheme)),-1),  yt,  border=NA, col=colscheme)
  rect(xl,  yb,  xr,  yt)
  labels <- if(elements[e]=="Pr") paste(round(2^seq(lim.lower,lim.upper,(lim.upper-lim.lower)/2), 2)*pct-pct, "%", sep="") else round(seq(lim.lower,lim.upper,(lim.upper-lim.lower)/2), 2)*pct
  text(seq(xl,xr,(xr-xl)/(length(labels)-1)),rep(yb,length(labels)),labels,pos=1,cex=1.5,font=1, offset=0.5)
  text(mean(c(xl,xr)), yt+0.01, paste("Change in", element.names[e]), pos=3, cex=1.5, font=2)
  
  print(elements[e])
  }
dev.off()




#-----------------------------------------
## Compare to AHCCD station data over BC and Alberta
#-----------------------------------------

#calculate the ahccd anomalies
stn <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/AHCCD/AHCCD_location.csv")
ahccd <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/AHCCD/AHCCD.csv")
ahccd <- ahccd[,- "tmean"]
names(ahccd) <- c("id", "year", "month", "Tmin", "Tmax", "PPT")

ahccd.ref <- ahccd[year %in% 1961:1990, 
                   lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                   by = .(id, month), 
                   .SDcols = c("Tmin", "Tmax", "PPT")]
ahccd.recent <- ahccd[year %in% 2001:2010, 
                   lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                   by = .(id, month), 
                   .SDcols = c("Tmin", "Tmax", "PPT")]
ahccd.ref <- ahccd.ref[which(ahccd.ref$id %in% unique(ahccd.recent$id))]
ahccd.recent <- ahccd.recent[which(ahccd.recent$id %in% unique(ahccd.ref$id))]

# subtract ref from recent
ahccd.anom <- copy(ahccd.ref)
ahccd.anom[, (3:ncol(ahccd.anom)) := lapply(3:ncol(ahccd.recent),
                                            function(i) ahccd.recent[[i]] - ahccd.ref[[i]])]
ppt_columns <- grep("PPT", names(ahccd.anom), value = TRUE)
ahccd.anom[, (ppt_columns) := ahccd.recent[, .SD, .SDcols = ppt_columns] / ahccd.ref[, .SD, .SDcols = ppt_columns]]

## Plot of pairwise comparison of CRU and ClimateNA (with AHCCD stations)
element.names <- c("mean daily minimum temperature", "mean daily maximum temperature", " precipitation")

X <- rast(dem) # use the DEM as a template raster
e=1
for(e in 1:3){
  
  lim.upper <- if(elements[e]=="Pr") 1 else 3
  lim.lower <- if(elements[e]=="Pr") -1 else -3
  
  inc=(lim.upper-lim.lower)/100
  breaks=seq(lim.lower, lim.upper+inc, inc)
  colscheme <- colorRampPalette(if(elements[e]=="Pr") rev(hcl.colors(5,"Blue-Red 3")) else hcl.colors(5,"Blue-Red 3"))(length(breaks)-1)
  
  anom <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  pct <- if(elements[e]=="Pr") 100 else 1
  
  m=2
  for(m in 1:12){
    
    png(filename=paste("vignettes/CRUvsClimateNA_BCAB", elements[e], monthcodes[m], "png",sep="."), type="cairo", units="in", width=6.5, height=3.3, pointsize=10, res=300)
    # pdf(file=paste("results//CMIP6Eval.Fig3", metric,"pdf",sep="."), width=7.5, height=14, pointsize=12)
    
    mat <- matrix(c(5,1,2,5,3,4), 3)
    layout(mat, widths=c(1,1), heights=c(0.3, .05 ,1))
    
    par(mar=c(0.1,0.1,0.1,0.1))
    
    ahccd.ptData <- ahccd.anom[month==m, get(c("Tmin", "Tmax", "PPT")[e])]
    hasData <- is.finite(ahccd.ptData)
    ahccd.ptData <- ahccd.ptData[hasData]
    ahccd.locations <- stn[which(stn$id %in% ahccd.anom[month==m,id][hasData])]
    if(e==3) ahccd.ptData <- log2(ahccd.ptData)
    ahccd.ptData[ahccd.ptData>lim.upper] <- lim.upper
    ahccd.ptData[ahccd.ptData < lim.lower] <- lim.lower
    z_cut <- cut(ahccd.ptData, breaks = breaks, include.lowest = TRUE, labels = colscheme)
    
    for(source in c("cru.gpcc", "climatena")){
      
      source.name <- if(source=="climatena") "ClimateNA" else if(elements[e]=="Pr") "GPCC" else "CRU"
      plot(1, type="n", axes=F, xlab="", ylab="")  
      text(1,1, source.name, font=2,cex=1.35)  
      
      par(mar=c(0.1,0.1,0.1,0.1))
      if(source=="cru.gpcc"){
        X <- anom[[m]]
        
      } else {
        X <- rast(dem) # use the DEM as a template raster
        X[climna.anom[, id1]] <- climna.anom[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
      }     
      if(e==3) X <- log2(X)
      X[X>lim.upper] <- lim.upper
      X[X < lim.lower] <- lim.lower
      
      image(X, xlim=c(-135, -110), ylim=c(48.4, 60.1), col=colscheme, breaks=breaks, xaxt="n", yaxt="n")
      lines(bdy.na, lwd=0.4)
      points(ahccd.locations$lon, ahccd.locations$lat, bg = as.character(z_cut), pch = 21, cex=1.6, lwd=1.5)
      
      legend("bottomleft", legend="AHCCD stations", bg = as.character(z_cut), pch = 21, pt.cex=1.6, pt.lwd=1.5, bty="n")
      
      print(source)
    }
    
    ## legend
    plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))  
    xl <- 0.2; yb <- 0.25; xr <- 0.8; yt <- 0.45
    rect(head(seq(xl,xr,(xr-xl)/length(colscheme)),-1), yb,  tail(seq(xl,xr,(xr-xl)/length(colscheme)),-1),  yt,  border=NA, col=colscheme)
    rect(xl,  yb,  xr,  yt)
    labels <- if(elements[e]=="Pr") paste(round(2^seq(lim.lower,lim.upper,(lim.upper-lim.lower)/4), 2)*pct-pct, "%", sep="") else paste0(round(seq(lim.lower,lim.upper,(lim.upper-lim.lower)/4), 2)*pct, "\u00B0", "C")
    text(seq(xl,xr,(xr-xl)/(length(labels)-1)),rep(yb,length(labels)),labels,pos=1,cex=1.5,font=1, offset=0.5)
    text(mean(c(xl,xr)), yt+0.01, paste("Change in", month.name[m], element.names[e], "\n1961-1990 to 2001-2020"), pos=3, cex=1.5, font=2)
    
    dev.off()
    
    month.abb[m]
  }
  print(elements[e])
}

#-----------------------------------------
## Compare to AHCCD stations in a small region
#-----------------------------------------

# ## pairwise comparison of CRU and ClimateNA (with AHCCD stations)
# X <- rast(dem) # use the DEM as a template raster
#   anom <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
#     par(mfrow=c(1,2), mar=c(0,0,0,0))
#     
#     ahccd.ptData <- ahccd.anom[month==m, get(c("Tmin", "Tmax", "PPT")[e])]
#     hasData <- is.finite(ahccd.ptData)
#     ahccd.ptData <- ahccd.ptData[hasData]
#     ahccd.locations <- stn[which(stn$id %in% ahccd.anom[month==m,id][hasData])]
#     if(e==3) ahccd.ptData <- log2(ahccd.ptData)
#     ahccd.ptData[ahccd.ptData>lim] <- lim
#     ahccd.ptData[ahccd.ptData < 0-lim] <- 0-lim
#     z_cut <- cut(ahccd.ptData, breaks = breaks, include.lowest = TRUE, labels = colscheme)
#     
#     X <- anom[[m]]
#     if(e==3) X <- log2(X)
#     X[X>lim] <- lim
#     X[X < 0-lim] <- 0-lim
#     plot(X, xlim=c(-120, -118.8), ylim=c(49, 50.4), col=colscheme, breaks=breaks, main=paste(month.name[m], elements[e], "(CRU)"), axes=F, type="continuous")
#     points(ahccd.locations$lon, ahccd.locations$lat, bg = as.character(z_cut), pch = 21, cex=1.6, lwd=2)
#     
#     X[climna.anom[, id1]] <- climna.anom[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
#     if(e==3) X <- log2(X)
#     X[X>lim] <- lim
#     X[X < 0-lim] <- 0-lim
#     plot(X, xlim=c(-120, -118.8), ylim=c(49, 50.4), col=colscheme, breaks=breaks, main=paste(month.name[m], elements[e], "(ClimateNA)"), axes=F, type="continuous")
#     points(ahccd.locations$lon, ahccd.locations$lat, bg = as.character(z_cut), pch = 21, cex=1.6, lwd=2)

    

#Okanagan Valley study area
studyarea <- ext(c(-120, -118.8, 49, 50.4))
stn.spat <- vect(stn, geom = c("lon", "lat"), crs = "EPSG:4326", keepgeom=T)
stn.spat <- crop(stn.spat, studyarea)
stn.s <- as.data.table(stn.spat)
ahccd.s <- ahccd[id %in% stn.s$id]
stations <- unique(stn.s$id)

## climateNA time series from climr Data
clim <- downscale(stn.s, 
                  which_refmap = "refmap_climatena", 
                  obs_periods = "2001_2020",
                  obs_years = 1901:2020,
                  obs_ts_dataset = c("cru.gpcc", "climatena")
)

e=1
# for(e in 1:3){
m=2
# for(m in 1:12){
ts.x.climatena <- as.numeric(unique(clim[PERIOD %in% 1901:2020 & DATASET == "climatena", PERIOD]))
ts.y.climatena <- unname(unlist(clim[clim$PERIOD %in% 1901:2020 & DATASET == "climatena",
                                     lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = PERIOD, 
                                     .SDcols = paste(c("Tmin", "Tmax", "PPT")[e], monthcodes[m], sep="_")][, 2]))
ts.x.cru <- as.numeric(unique(clim[PERIOD %in% 1901:2020 & DATASET == "cru.gpcc", PERIOD]))
ts.y.cru <- unname(unlist(clim[clim$PERIOD %in% 1901:2020 & DATASET == "cru.gpcc",
                               lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = PERIOD, 
                               .SDcols = paste(c("Tmin", "Tmax", "PPT")[e], monthcodes[m], sep="_")][, 2]))
ts.ref <- mean(ts.y.climatena[ts.x.climatena%in%1961:1990])

## plot comparing CRU and climateNA time series with AHCCD station data. 
png(filename=paste("vignettes/CRUvsClimateNA_ts", elements[e], monthcodes[m], "png",sep="."), type="cairo", units="in", width=6.5, height=4, pointsize=10, res=300)

par(mar = c(2,3,0.5,0.5), mgp = c(1.75, 0.25, 0), mfrow=c(1,1))
ylim <- range(ts.y.climatena, na.rm = T) + c(-1.5,1.5)
plot(ts.x.cru, rep(NA, length(ts.x.cru)), ylim=ylim, type="o", tck=-0.01, col="white", xlab="", ylab=element.names[e])

recent <- vector()
for(station in stations){
  data <- ahccd.s[id==station & month==m]
  x <- data$year
  y <- data[,get(c("Tmin", "Tmax", "PPT")[e])]
  if(sum(is.finite(y[x%in%2001:2020]))>16){
    y.ref <- mean(y[x%in%1961:1990])
    y <- y + (ts.ref-y.ref) # shift to cru baseline
    lines(x,y, col=alpha("gray50", .25), lwd=2)
    norm <- mean(y[x%in%2001:2020], na.rm=T)
    lines(c(2001,2020), rep(norm,2), lty=2, col=alpha("gray50", 1))
    recent <- if(station==stations[1]) norm else c(recent, norm)
  }
}

lines(ts.x.cru,ts.y.cru, col="dodgerblue", lwd=3)
lines(ts.x.climatena,ts.y.climatena, col="black", lwd=2)
lines(c(1961,1990), rep(mean(ts.y.cru[ts.x.cru%in%1961:1990]),2), lty=2, col=1, lwd=2)
lines(c(2001,2020), rep(mean(ts.y.cru[ts.x.cru%in%2001:2020]),2), lty=2, col="dodgerblue", lwd=2)
lines(c(2001,2020), rep(mean(ts.y.climatena[ts.x.climatena%in%2001:2020]),2), lty=2, lwd=2)
boxplot(recent, add=T, at=2021, width=5, range=0)
legend("topleft", legend=c("ClimateNA", "CRU", "AHCCD stations"), pch=c(NA, NA, NA), 
       col = c(1,"dodgerblue", "gray80"), lty=c(1,1,1), lwd=c(3,2,2), bty="n")

dev.off()
month.abb[m]
# }
print(elements[e])
# }




#-----------------------------------------
## Compare ERA5, ClimateNA, and AHCCD station data over BC and Alberta
#-----------------------------------------

#calculate the ahccd anomalies
stn <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/AHCCD/AHCCD_location.csv")
ahccd <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/AHCCD/AHCCD.csv")
ahccd <- ahccd[,- "tmean"]
names(ahccd) <- c("id", "year", "month", "Tmin", "Tmax", "PPT")

ahccd.ref <- ahccd[year %in% 1981:2010, 
                   lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                   by = .(id, month), 
                   .SDcols = c("Tmin", "Tmax", "PPT")]
ahccd.recent <- ahccd[year %in% 2001:2010, 
                      lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                      by = .(id, month), 
                      .SDcols = c("Tmin", "Tmax", "PPT")]
ahccd.ref <- ahccd.ref[which(ahccd.ref$id %in% unique(ahccd.recent$id))]
ahccd.recent <- ahccd.recent[which(ahccd.recent$id %in% unique(ahccd.ref$id))]

# subtract ref from recent
ahccd.anom <- copy(ahccd.ref)
ahccd.anom[, (3:ncol(ahccd.anom)) := lapply(3:ncol(ahccd.recent),
                                            function(i) ahccd.recent[[i]] - ahccd.ref[[i]])]
ppt_columns <- grep("PPT", names(ahccd.anom), value = TRUE)
ahccd.anom[, (ppt_columns) := ahccd.recent[, .SD, .SDcols = ppt_columns] / ahccd.ref[, .SD, .SDcols = ppt_columns]]

## Plot of pairwise comparison of CRU and ClimateNA (with AHCCD stations)
element.names <- c("mean daily minimum temperature", "mean daily maximum temperature", " precipitation")

X <- rast(dem) # use the DEM as a template raster
e=1
for(e in 1:3){
  
  lim.upper <- if(elements[e]=="Pr") 1 else 2
  lim.lower <- if(elements[e]=="Pr") -1 else -2
  
  inc=(lim.upper-lim.lower)/100
  breaks=seq(lim.lower, lim.upper+inc, inc)
  colscheme <- colorRampPalette(if(elements[e]=="Pr") rev(hcl.colors(5,"Blue-Red 3")) else hcl.colors(5,"Blue-Red 3"))(length(breaks)-1)
  
  anom <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  pct <- if(elements[e]=="Pr") 100 else 1
  
  m=2
  for(m in c(1,2,4,7,10)){
    
    png(filename=paste("vignettes/ERA5vsClimateNA_BCAB", elements[e], monthcodes[m], "png",sep="."), type="cairo", units="in", width=6.5, height=3.3, pointsize=10, res=300)

    mat <- matrix(c(5,1,2,5,3,4), 3)
    layout(mat, widths=c(1,1), heights=c(0.3, .05 ,1))
    
    par(mar=c(0.1,0.1,0.1,0.1))
    
    ahccd.ptData <- ahccd.anom[month==m, get(c("Tmin", "Tmax", "PPT")[e])]
    hasData <- is.finite(ahccd.ptData)
    ahccd.ptData <- ahccd.ptData[hasData]
    ahccd.locations <- stn[which(stn$id %in% ahccd.anom[month==m,id][hasData])]
    if(e==3) ahccd.ptData <- log2(ahccd.ptData)
    ahccd.ptData[ahccd.ptData>lim.upper] <- lim.upper
    ahccd.ptData[ahccd.ptData < lim.lower] <- lim.lower
    z_cut <- cut(ahccd.ptData, breaks = breaks, include.lowest = TRUE, labels = colscheme)
    
    for(source in c("era5", "climatena")){
      
      source.name <- if(source=="climatena") "ClimateNA" else "ERA5-Land"
      plot(1, type="n", axes=F, xlab="", ylab="")  
      text(1,1, source.name, font=2,cex=1.35)  
      
      par(mar=c(0.1,0.1,0.1,0.1))
      if(source=="era5"){
        anom <- mean(rast(paste0("C:/Users/CMAHONY/OneDrive - Government of BC/Data/Western North America_",c("tmin", "tmax", "prcp")[e], "_anomaly_",month.abb[m] ,"_2001_2020_data.tif")))
        X <- anom
        # X <- mask(X, bdy.na)
        if(e==3) X <- log2(X/100+1)
      } else {
        X <- rast(dem) # use the DEM as a template raster
        X[climna.anom[, id1]] <- climna.anom[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
        if(e==3) X <- log2(X)
      }     
      X[X>lim.upper] <- lim.upper
      X[X < lim.lower] <- lim.lower
      
      image(X, xlim=c(-135, -110), ylim=c(48.4, 59.8), col=colscheme, breaks=breaks, xaxt="n", yaxt="n")
      lines(bdy.na, lwd=0.4)
      points(ahccd.locations$lon, ahccd.locations$lat, bg = as.character(z_cut), pch = 21, cex=1.6, lwd=1.5)
      
      legend("bottomleft", legend="AHCCD stations", bg = as.character(z_cut), pch = 21, pt.cex=1.6, pt.lwd=1.5, bty="n")
      
      print(source)
    }
    
    ## legend
    plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))  
    xl <- 0.2; yb <- 0.25; xr <- 0.8; yt <- 0.45
    rect(head(seq(xl,xr,(xr-xl)/length(colscheme)),-1), yb,  tail(seq(xl,xr,(xr-xl)/length(colscheme)),-1),  yt,  border=NA, col=colscheme)
    rect(xl,  yb,  xr,  yt)
    # legend.labels <- if(e==3) paste(round(2^seq(0-lim,lim,lim/2), 2)*pct-pct, "%", sep="") else paste0(round(seq(0-lim,lim,lim/2), 2)*pct, "\u00B0", "C")
    labels <- if(elements[e]=="Pr") paste(round(2^seq(lim.lower,lim.upper,(lim.upper-lim.lower)/4), 2)*pct-pct, "%", sep="") else paste0(round(seq(lim.lower,lim.upper,(lim.upper-lim.lower)/4), 2)*pct, "\u00B0", "C")
    text(seq(xl,xr,(xr-xl)/(length(labels)-1)),rep(yb,length(labels)),labels,pos=1,cex=1.5,font=1, offset=0.5)
    text(mean(c(xl,xr)), yt+0.01, paste("Change in", month.name[m], element.names[e], "\n1981-2010 to 2001-2020"), pos=3, cex=1.5, font=2)
    
    dev.off()
    
    month.abb[m]
  }
  print(elements[e])
}

