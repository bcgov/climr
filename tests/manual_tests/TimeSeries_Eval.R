## Manual test to compare the CRU/GPCC, ClimateNA, and MSWX time series. 
## Colin Mahony, January 29, 2025

library(climr)
library(data.table)
library(terra)
library(scales)
library(rworldxtra)
data("countriesHigh")

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
elements.cru <- c("tmn", "tmx", "pre", "tmp")
elements <- c("Tmin", "Tmax", "Pr")

studyarea <- ext(c(-170, -52.25, 13.75, 83.75))
bdy.na <- vect(countriesHigh[grep("Canada|United States|Mexico", countriesHigh$NAME),])
bdy.na <- erase(bdy.na, ext(c(-170, -140, 13, 30))) # erase hawaii
bdy.na <- aggregate(bdy.na) # dissolve
buff.na <- buffer(bdy.na, width=50000) # buffer coastline

# ClimateNA
#low res dem for north america
dir <- "C:/Users/CMAHONY/OneDrive - Government of BC/Data/data_climr_blend_monthly_anomalies/clmr_blend_ts_1901_2024/"
files <- list.files(dir, pattern = paste0(c("tmin", "tmax", "prcp"), "_.*"))
temp <- rast(paste0(dir, files[1]))
dem <- rast("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/dem_noram_lowres.tif")
dem <- project(dem, temp, method="mode") 
my_grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
climna_grid <- my_grid[,c(1,1,3,2,4)]
colnames(climna_grid) <- c("id1", "id2", "lat", "lon", "el") # rename column names to what climr expects
write.csv(climna_grid, "C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow.csv", row.names = F)
## Run climateNA and then return to script. 
ref.climna <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow_Normal_1961_1990MP.csv")
y2001.climna <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow_Decade_2001_2010MP.csv")
y2011.climna <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow_Decade_2011_2020MP.csv")
y1981.climna <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow_Normal_1981_2010MP.csv")
recent.climna <- (y2001.climna + y2011.climna)/2
anom.climna <- recent.climna - ref.climna
ppt_columns <- grep("PPT", names(anom.climna), value = TRUE)
anom.climna[, (ppt_columns) := recent.climna[, .SD, .SDcols = ppt_columns] / ref.climna[, .SD, .SDcols = ppt_columns]]
anom.climna[, c(1, 2, 3, 4, 5) := ref.climna[, c(1, 2, 3, 4, 5), with = FALSE]]
anom.climna.1981 <- ref.climna - y1981.climna
anom.climna.1981[, (ppt_columns) := ref.climna[, .SD, .SDcols = ppt_columns] / y1981.climna[, .SD, .SDcols = ppt_columns]]
anom.climna.1981[, c(1, 2, 3, 4, 5) := ref.climna[, c(1, 2, 3, 4, 5), with = FALSE]]

# ==========================================
# three-panel comparison of CRU and MSWX anomaly to 2001-2020
# ==========================================


element.names <- c("mean daily minimum temperature (\u00B0C)", "mean daily maximum temperature (\u00B0C)", "precipitation (%)")

dem.lcc <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Projects/2021_CMIP6Eval_NA/inputs//dem.na.lcc.tif")
ipccregions.lcc <- vect("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Shiny_Apps\\cmip6-NA-eval\\data\\ipccregions_lcc.shp")
bdy.lcc <- project(bdy.na, ipccregions.lcc)

e=1
m=2

for(e in 1:3){
anom.cru <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/CRUGPCC_2024/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
anom.mswx <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))

for(m in 1:12){
  
png(filename=paste("vignettes/plots_timeseries/CRUvsMSWXvsClimateNA_3Panel_30arcminute", elements[e], monthcodes[m], "png",sep="."), type="cairo", units="in", width=6.5, height=3.5, pointsize=10, res=300)
# pdf(file=paste("results//CMIP6Eval.Fig3", metric,"pdf",sep="."), width=7.5, height=14, pointsize=12)

mat <- matrix(c(7,1,2,7,3,4,7,5,6), 3)
layout(mat, widths=c(1,1,1), heights=c(0.275, .05 ,1))

sequence <- 1:12

par(mar=c(0.1,0.1,0.1,0.1))

lim.upper <- if(elements[e]=="Pr") 1 else 3
lim.lower <- if(elements[e]=="Pr") -1 else -3

inc=(lim.upper-lim.lower)/100
breaks=seq(lim.lower, lim.upper+inc, inc)
colscheme <- colorRampPalette(if(elements[e]=="Pr") rev(hcl.colors(5,"Blue-Red 3")) else hcl.colors(5,"Blue-Red 3"))(length(breaks)-1)

pct <- if(elements[e]=="Pr") 100 else 1

for(source in c("cru.gpcc", "mswx", "climatena")){
  
  source.name <- if(source=="mswx") "MSWX blend" else if(source=="climatena") "ClimateNA" else if(elements[e]=="Pr") "GPCC" else "CRU"
  plot(1, type="n", axes=F, xlab="", ylab="")  
  text(1,1, source.name, font=2,cex=1.35)  
  
  par(mar=c(0.1,0.1,0.1,0.1))
  if(source=="cru.gpcc"){
    X <- anom.cru[[m]]
  } else if(source=="mswx") {
    # # dir <- "//objectstore2.nrs.bcgov/ffec/data_climr_blend_monthly_anomalies/clmr_blend_ts_1901_2024/"
    # dir <- "C:/Users/CMAHONY/OneDrive - Government of BC/Data/data_climr_blend_monthly_anomalies/clmr_blend_ts_1901_2024/"
    # file <- list.files(dir, pattern = paste0(c("tmin", "tmax", "prcp")[e], ".*._", monthcodes[m], "_.*"))
    # ts.mswx <- rast(paste0(dir, file))
    # ts.years <- substr(time(ts.mswx), 1,4)
    # ref.mswx <- mean(ts.mswx[[ts.years%in%1961:1990]]) 
    # curr.mswx <- mean(ts.mswx[[ts.years%in%2001:2020]])
    # anom.mswx <- if(e==3) (curr.mswx+1) / (ref.mswx+1) else curr.mswx - ref.mswx
    # X <- anom.mswx
    X <- anom.mswx[[m]]
    X <- project(X, anom.cru)
  } else {
    X <- dem # use the DEM as a template raster
    X[anom.climna[, id1]] <- anom.climna[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
  }     
   
X <- project(X, dem.lcc)
  X <- crop(X, ipccregions.lcc)
  X <- mask(X, ipccregions.lcc)
  if(e==3) X <- log2(X)
  X[X>lim.upper] <- lim.upper
  X[X < lim.lower] <- lim.lower
  
  image(X, col=colscheme, breaks=breaks, xaxt="n", yaxt="n", bty="n")
  plot(bdy.lcc, lwd=0.4, add=T)
  
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
print(m)
}
print(e)
}

# ==========================================
# three-panel comparison of CRU and MSWX anomaly to from 1981-2010 to 1961-1990
# ==========================================


element.names <- c("mean daily minimum temperature (\u00B0C)", "mean daily maximum temperature (\u00B0C)", "precipitation (%)")

dem.lcc <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Projects/2021_CMIP6Eval_NA/inputs//dem.na.lcc.tif")
ipccregions.lcc <- vect("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Shiny_Apps\\cmip6-NA-eval\\data\\ipccregions_lcc.shp")
bdy.lcc <- project(bdy.na, ipccregions.lcc)

e=1
m=2

for(e in 1:3){
  anom.cru <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/CRUGPCC_2024/delta.from.1981_2010.to.1961_1990.", elements[e], ".tif"))
  anom.mswx <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1981_2010.to.1961_1990.", elements[e], ".tif"))
  
  for(m in 1:12){
    
    png(filename=paste("vignettes/plots_timeseries/CRUvsMSWXvsClimateNA_3Panel_1981rev_30arcminute", elements[e], monthcodes[m], "png",sep="."), type="cairo", units="in", width=6.5, height=3.5, pointsize=10, res=300)

    mat <- matrix(c(7,1,2,7,3,4,7,5,6), 3)
    layout(mat, widths=c(1,1,1), heights=c(0.275, .05 ,1))
    
    sequence <- 1:12
    
    par(mar=c(0.1,0.1,0.1,0.1))
    
    lim.upper <- if(elements[e]=="Pr") 1 else 3
    lim.lower <- if(elements[e]=="Pr") -1 else -3
    
    inc=(lim.upper-lim.lower)/100
    breaks=seq(lim.lower, lim.upper+inc, inc)
    colscheme <- colorRampPalette(if(elements[e]=="Pr") rev(hcl.colors(5,"Blue-Red 3")) else hcl.colors(5,"Blue-Red 3"))(length(breaks)-1)
    
    pct <- if(elements[e]=="Pr") 100 else 1
    
    for(source in c("cru.gpcc", "mswx", "climatena")){
      
      source.name <- if(source=="mswx") "MSWX blend" else if(source=="climatena") "ClimateNA" else if(elements[e]=="Pr") "GPCC" else "CRU"
      plot(1, type="n", axes=F, xlab="", ylab="")  
      text(1,1, source.name, font=2,cex=1.35)  
      
      par(mar=c(0.1,0.1,0.1,0.1))
      if(source=="cru.gpcc"){
        X <- anom.cru[[m]]
      } else if(source=="mswx") {
        # # dir <- "//objectstore2.nrs.bcgov/ffec/data_climr_blend_monthly_anomalies/clmr_blend_ts_1901_2024/"
        # dir <- "C:/Users/CMAHONY/OneDrive - Government of BC/Data/data_climr_blend_monthly_anomalies/clmr_blend_ts_1901_2024/"
        # file <- list.files(dir, pattern = paste0(c("tmin", "tmax", "prcp")[e], ".*._", monthcodes[m], "_.*"))
        # ts.mswx <- rast(paste0(dir, file))
        # ts.years <- substr(time(ts.mswx), 1,4)
        # ref.mswx <- mean(ts.mswx[[ts.years%in%1981:2010]]) 
        # curr.mswx <- mean(ts.mswx[[ts.years%in%1961:1990]])
        # anom.mswx <- if(e==3) (curr.mswx+1) / (ref.mswx+1) else curr.mswx - ref.mswx
        # X <- anom.mswx
        X <- anom.mswx[[m]]
        X <- project(X, anom.cru)
      } else {
        X <- dem # use the DEM as a template raster
        X[anom.climna.1981[, id1]] <- anom.climna.1981[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
      }     
      
      X <- project(X, dem.lcc)
      X <- crop(X, ipccregions.lcc)
      X <- mask(X, ipccregions.lcc)
      if(e==3) X <- log2(X)
      X[X>lim.upper] <- lim.upper
      X[X < lim.lower] <- lim.lower
      
      image(X, col=colscheme, breaks=breaks, xaxt="n", yaxt="n", bty="n")
      plot(bdy.lcc, lwd=0.4, add=T)
      
      print(source)
    }
    
    ## legend
    plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))  
    xl <- 0.2; yb <- 0.3; xr <- 0.8; yt <- 0.5
    rect(head(seq(xl,xr,(xr-xl)/length(colscheme)),-1), yb,  tail(seq(xl,xr,(xr-xl)/length(colscheme)),-1),  yt,  border=NA, col=colscheme)
    rect(xl,  yb,  xr,  yt)
    labels <- if(elements[e]=="Pr") paste(round(2^seq(lim.lower,lim.upper,(lim.upper-lim.lower)/2), 2)*pct-pct, "%", sep="") else round(seq(lim.lower,lim.upper,(lim.upper-lim.lower)/2), 2)*pct
    text(seq(xl,xr,(xr-xl)/(length(labels)-1)),rep(yb,length(labels)),labels,pos=1,cex=1.5,font=1, offset=0.5)
    text(mean(c(xl,xr)), yt+0.01, paste("Difference in", month.name[m], element.names[e], "\n1981-2010 to 1961-1990"), pos=3, cex=1.5, font=2)
    
    dev.off()
    print(m)
  }
  print(e)
}

# ==========================================
# multipanel comparison of CRU and MSWX blend, for three elements
# ==========================================


element.names <- c("mean daily\nminimum temperature (\u00B0C)", "mean daily\nmaximum temperature (\u00B0C)", "\nprecipitation (%)")

dem.lcc <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Projects/2021_CMIP6Eval_NA/inputs//dem.na.lcc.tif")
ipccregions.lcc <- vect("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Shiny_Apps\\cmip6-NA-eval\\data\\ipccregions_lcc.shp")

png(filename=paste("vignettes/plots_timeseries/CRUvsMSWX_Diffplots", "png",sep="."), type="cairo", units="in", width=6.5, height=15, pointsize=10, res=300)

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
  
  anom.cru <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/CRUGPCC_2024/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  anom.mswx <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  pct <- if(elements[e]=="Pr") 100 else 1
  
  for(source in c("cru.gpcc", "mswx")){
    
    source.name <- if(source=="mswx") "MSWX" else if(elements[e]=="Pr") "GPCC" else "CRU"
    plot(1, type="n", axes=F, xlab="", ylab="")  
    text(1,1, source.name, font=2,cex=1.35)  
    
    par(mar=c(0.1,0.1,0.1,0.1))
    for(m in 1:12){
      if(source=="cru.gpcc"){
        X <- anom.cru[[m]]
      } else {
        X <- anom.mswx[[m]]
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

# ==========================================
# multipanel comparison of all datasets, for two elements
# ==========================================


element.names <- c("mean daily\nminimum temperature (\u00B0C)", "mean daily\nmaximum temperature (\u00B0C)", "\nprecipitation (%)")

dem.lcc <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Projects/2021_CMIP6Eval_NA/inputs//dem.na.lcc.tif")
ipccregions.lcc <- vect("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Shiny_Apps\\cmip6-NA-eval\\data\\ipccregions_lcc.shp")

png(filename=paste("vignettes/plots_timeseries/CRUvsMSWXvsClimateNA_Diffplots", "png",sep="."), type="cairo", units="in", width=6.5, height=5.5, pointsize=10, res=300)

nmonths <- 4
mat <- matrix(0:((nmonths+1)*8-1), (nmonths+1))
mat[,6:8] <- mat[,5:7]+1
mat <- rbind(matrix(rep(c(max(mat[,4])+1, max(mat[,8])+1), each=4), 1), mat)
mat[,5] <- max(mat)+1
mat[1,1] <- max(mat)
layout(mat, widths=c(0.25,1,1,1,0.15,1,1,1), heights=c(0.7, .2,rep(1,nmonths)))

sequence <- c(1,4,7,10)

par(mar=c(0.1,0.1,0.1,0.1))

for(month in month.abb[sequence]){
  plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,2), ylim=c(0,2))  
  text(1,1, month, font=2,cex=1.5, srt=90)  
}

element=elements[1]
for(e in 2:3){
  
  lim.upper <- if(elements[e]=="Pr") 1 else 3
  lim.lower <- if(elements[e]=="Pr") -1 else -3
  
  inc=(lim.upper-lim.lower)/100
  breaks=seq(lim.lower, lim.upper+inc, inc)
  colscheme <- colorRampPalette(if(elements[e]=="Pr") rev(hcl.colors(5,"Blue-Red 3")) else hcl.colors(5,"Blue-Red 3"))(length(breaks)-1)
  
  anom.cru <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/CRUGPCC_2024/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  anom.mswx <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  pct <- if(elements[e]=="Pr") 100 else 1
  
  for(source in c("cru.gpcc", "mswx", "climatena")){
    
    source.name <- if(source=="mswx") "MSWX blend" else if(source=="climatena") "ClimateNA" else if(elements[e]=="Pr") "GPCC" else "CRU"
    plot(1, type="n", axes=F, xlab="", ylab="")  
    text(1,1, source.name, font=2,cex=1.35)  
    
    par(mar=c(0.1,0.1,0.1,0.1))
    for(m in sequence){
      if(source=="cru.gpcc"){
        X <- anom.cru[[m]]
      } else if(source=="mswx") {
        X <- anom.mswx[[m]]
      }  else {
        X <- dem # use the DEM as a template raster
        X[anom.climna[, id1]] <- anom.climna[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
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
## compare raw gpcc to climr gpcc
#-----------------------------------------

library(terra)
library(data.table)
library(scales)
library(rworldxtra)
data("countriesHigh")

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
elements.cru <- c("tmn", "tmx", "pre", "tmp")
elements <- c("Tmin", "Tmax", "Pr")
element.names <- c("mean daily\nminimum temperature (\u00B0C)", "mean daily\nmaximum temperature (\u00B0C)", "\nprecipitation (%)")

studyarea <- ext(c(-170, -52.25, 13.75, 83.75))
bdy.na <- vect(countriesHigh[grep("Canada|United States|Mexico", countriesHigh$NAME),])
bdy.na <- erase(bdy.na, ext(c(-170, -140, 13, 30))) # erase hawaii
bdy.na <- aggregate(bdy.na) # dissolve
buff.na <- buffer(bdy.na, width=50000) # buffer coastline


dem.lcc <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Projects/2021_CMIP6Eval_NA/inputs//dem.na.lcc.tif")
ipccregions.lcc <- vect("C:\\Users\\CMAHONY\\OneDrive - Government of BC\\Shiny_Apps\\cmip6-NA-eval\\data\\ipccregions_lcc.shp")
bdy.lcc <- project(bdy.na, ipccregions.lcc)

# ClimateNA
#low res dem for north america
dir <- "C:/Users/CMAHONY/OneDrive - Government of BC/Data/data_climr_blend_monthly_anomalies/clmr_blend_ts_1901_2024/"
files <- list.files(dir, pattern = paste0(c("tmin", "tmax", "prcp"), "_.*"))
temp <- rast(paste0(dir, files[1]))
dem <- rast("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/dem_noram_lowres.tif")
dem <- project(dem, temp, method="mode") 
my_grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
climna_grid <- my_grid[,c(1,1,3,2,4)]
colnames(climna_grid) <- c("id1", "id2", "lat", "lon", "el") # rename column names to what climr expects
## Run climateNA and then return to script. 
ref.climna <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow_Normal_1961_1990MP.csv")
y2001.climna <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow_Decade_2001_2010MP.csv")
y2011.climna <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow_Decade_2011_2020MP.csv")
y1981.climna <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/noramLow_Normal_1981_2010MP.csv")
recent.climna <- (y2001.climna + y2011.climna)/2
anom.climna <- recent.climna - ref.climna
ppt_columns <- grep("PPT", names(anom.climna), value = TRUE)
anom.climna[, (ppt_columns) := recent.climna[, .SD, .SDcols = ppt_columns] / ref.climna[, .SD, .SDcols = ppt_columns]]
anom.climna[, c(1, 2, 3, 4, 5) := ref.climna[, c(1, 2, 3, 4, 5), with = FALSE]]
anom.climna.1981 <- ref.climna - y1981.climna
anom.climna.1981[, (ppt_columns) := ref.climna[, .SD, .SDcols = ppt_columns] / y1981.climna[, .SD, .SDcols = ppt_columns]]
anom.climna.1981[, c(1, 2, 3, 4, 5) := ref.climna[, c(1, 2, 3, 4, 5), with = FALSE]]

e=3
m=2

# ---------------------------------
# multipanel plot of map and time series for a selected location

lim.upper <- if(elements[e]=="Pr") 1 else 3
lim.lower <- if(elements[e]=="Pr") -1 else -3

inc=(lim.upper-lim.lower)/100
breaks=seq(lim.lower, lim.upper+inc, inc)
colscheme <- colorRampPalette(if(elements[e]=="Pr") rev(hcl.colors(5,"Blue-Red 3")) else hcl.colors(5,"Blue-Red 3"))(length(breaks)-1)

anom.cru <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/CRUGPCC_2024/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
anom.mswx <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))

xyz <- data.frame(
  lon = c(-105, -101, -123),
  lat = c(20, 65, 50),
  elev = c(50, 50, 50),
  id = c(1,2, 3), 
  name = c("mexico", "centralArctic", "BC")
)

location <- 3

pt <- vect(xyz[location,1:2], crs = "EPSG:4326")
pt.lcc <- project(pt, dem.lcc)

ts.all <- downscale(xyz[location,], obs_ts_dataset = c("cru.gpcc", "mswx.blend", "climatena"), obs_years = 1901:2023)

sources <- c("cru.gpcc", "mswx.blend", "climatena")
source.names <- c("CRU/GPCC", "MSWX blend", "ClimateNA")

## initiate plot
mat <- matrix(c(1,2,5,1,3,6,1,4,7), 3)
layout(mat, widths=c(1,1,1), heights=c(0.5, 1 ,1.2))

pct <- if(elements[e]=="Pr") 100 else 1

## legend
par(mar=c(0.1,0.1,0.1,0.1))
plot(1, type="n", axes=F, xlab="", ylab="", xlim=c(0,1), ylim=c(0,1))  
xl <- 0.2; yb <- 0.3; xr <- 0.8; yt <- 0.5
rect(head(seq(xl,xr,(xr-xl)/length(colscheme)),-1), yb,  tail(seq(xl,xr,(xr-xl)/length(colscheme)),-1),  yt,  border=NA, col=colscheme)
rect(xl,  yb,  xr,  yt)
labels <- if(elements[e]=="Pr") paste(round(2^seq(lim.lower,lim.upper,(lim.upper-lim.lower)/2), 2)*pct-pct, "%", sep="") else round(seq(lim.lower,lim.upper,(lim.upper-lim.lower)/2), 2)*pct
text(seq(xl,xr,(xr-xl)/(length(labels)-1)),rep(yb,length(labels)),labels,pos=1,cex=1.5,font=1, offset=0.5)
text(mean(c(xl,xr)), yt+0.01, paste("Change in", month.name[m], element.names[e], "\n1961-1990 to 2001-2020"), pos=3, cex=1.5, font=2)

## change Maps
for(source in sources){
  i <- which(sources==source)
  if(source=="cru.gpcc"){
    X <- anom.cru[[m]]
  } else if(source=="mswx.blend") {
    X <- anom.mswx[[m]]
  }  else {
    X <- dem # use the DEM as a template raster
    X[anom.climna[, id1]] <- anom.climna[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
  }     
  X <- project(X, dem.lcc)
  X <- crop(X, ipccregions.lcc)
  X <- mask(X, ipccregions.lcc)
  if(e==3) X <- log2(X)
  X[X>lim.upper] <- lim.upper
  X[X < lim.lower] <- lim.lower
  
  plot(X, col=colscheme, breaks=breaks, axes=F, legend=F, main=source.names[i])

  plot(pt.lcc, add=T, pch=1, cex=2)
  plot(pt.lcc, add=T, pch=1, cex=5)
  
}

## time series
par(mar=c(2,3,0.1,0.1), mgp=c(2,0.2,0), tck=-0.01)
for(source in sources){
  i <- which(sources==source)
  
  temp <- ts.all[DATASET==source, c("PERIOD", paste0(c("Tmin", "Tmax", "PPT")[e], "_", monthcodes[m])), with=FALSE]
  plot(temp, ylim=c(0, max(ts.all[,paste0(c("Tmin", "Tmax", "PPT")[e], "_", monthcodes[m]), with=FALSE], na.rm=T)))
  colname <- paste0(c("Tmin", "Tmax", "PPT")[e], "_", monthcodes[m])
  lines(c(1961, 1990), rep(mean(temp[PERIOD %in% 1961:1990, get(colname)], na.rm = TRUE), 2))
  lines(c(2001, 2020), rep(mean(temp[PERIOD %in% 2001:2020, get(colname)], na.rm = TRUE), 2))
}


# ---------------------------------
# why is the gpcc and mswx different pre-1990? they should be the same


# convert to positive longitude for gpcc (easier than rotating the gpcc raster)
pt.r <- vect(crds(pt)+c(360,0), crs = "EPSG:4326")

e=3
m=2

# import GPCC
# ts.cru <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TimeSeries_gridded_monthly/GPCC/precip.comb.v2020to2019-v2020monitorafter.total.nc"))
ts.cru <- rast(paste0("C:/Users/CMAHONY/OneDrive - Government of BC/Data/GPCC/precip.comb.v2020to2019-v2020monitorafter.total.nc"))
ts.mswx <- rast(paste0("C:/Users/CMAHONY/OneDrive - Government of BC/Data/data_climr_blend_monthly_anomalies/clmr_blend_ts_1901_2024/", c("tmin", "tmax", "prcp")[e], "_climrblend_ano_dt_NA_mon_", monthcodes[m], "_1901_2024.nc"))
test <- aggregate(ts.mswx, fact=3)

par(mfrow=c(1,1), mar=c(3,3,1,1))

# gpcc raw time series
temp <- ts.cru[[which(substr(time(ts.cru),6,7)==monthcodes[m])]]
ts.pt <- as.vector(unlist(extract(temp, pt.r)))
years <- unique(substr(time(ts.cru),1,4))[1:length(ts.pt)]
plot(years, ts.pt, ylab=c())

# mswx raw time series
ts.pt <- as.vector(unlist(extract(ts.mswx, pt)))[-1]+1
years <- unique(substr(time(ts.mswx),1,4))[1:length(ts.pt)]
plot(years, ts.pt, ylab=c())
lines(c(-9999,9999), c(0,0), lty=2)
## this is correct, i.e., not the same as the climr output. 

# ## time series on aggregated mswx raster data
# ts.pt <- as.vector(unlist(extract(test, pt)))[-1]+1
# plot(years, ts.pt, ylab=c())
# lines(c(-9999,9999), c(0,0), lty=2)
# ## the difference is not due to aggregation. 

baseline.1961 <- mean(ts.pt[years %in% 1961:1990], na.rm = TRUE)
baseline.1981 <- mean(ts.pt[years %in% 1981:2000], na.rm = TRUE)
baseline.2001 <- mean(ts.pt[years %in% 2001:2020], na.rm = TRUE)

lines(c(1961, 1990), rep(baseline.1961, 2))
lines(c(1981, 2000), rep(baseline.1981, 2))
lines(c(2001, 2020), rep(baseline.2001, 2))
lines(c(-9999,9999), c(1,1), lty=2, col="gray")

ts.1961 <- ts.pt/baseline.1961
points(years, ts.1961, pch=16)

plot(years, ts.1961, ylab=c())
lines(c(-9999,9999), c(0,0), lty=2)
baseline.1961 <- mean(ts.1961[years %in% 1961:1990], na.rm = TRUE)
lines(c(1961, 1990), rep(baseline.1961, 2))
lines(c(-9999,9999), c(1,1), lty=2, col="gray")


# climr mswx input raster time series
fnames <- list.files("//objectstore2.nrs.bcgov/ffec/data_climr_blend_monthly_anomalies/clmr_blend_ts_1901_2024/", pattern = "prcp.*.nc$", full.names = TRUE)
ppt <- rast(fnames)
ppt_tm <- time(ppt)
ord <- order(ppt_tm)
ppt <- ppt[[ord]]
ppt_tm <- time(ppt)
ppt_nrm <- ppt[[ppt_tm >= as.Date("1961-01-01") & ppt_tm <= as.Date("1990-12-31")]] + 1 ## small error here: the end date should be "1990-12-31" 
unique(time(ppt_nrm))
nrm <- tapp(ppt_nrm, index = "months", fun = mean)
ppt_delta <- (ppt + 1)/(nrm)
time(ppt_delta)

temp <- ppt_delta[[which(substr(time(ppt_delta),6,7)==monthcodes[m])]]
ts.pt.raw <- as.vector(unlist(extract(temp, pt)[-1]))
years.raw <- unique(substr(time(ppt_delta),1,4))[1:length(ts.pt.raw)]
plot(years.raw, ts.pt.raw, ylab=c())
lines(c(-9999,9999), c(0,0), lty=2)
baseline.1961 <- mean(ts.pt.raw[years.raw %in% 1961:1990], na.rm = TRUE)

#climr output for comparison
temp <- ts.all[DATASET=="mswx.blend", c("PERIOD", paste0(c("Tmin", "Tmax", "PPT")[e], "_", monthcodes[m])), with=FALSE]
ts.pt.climr <- as.vector(unlist(temp[,-1]))
years.climr <- as.numeric(unlist(temp[,1]))
baseline.1961.climr <- mean(ts.pt.climr[years.climr %in% 1961:1990], na.rm = TRUE)
ts.pt.climr <- ts.pt.climr/baseline.1961.climr
points(years.climr, ts.pt.climr, pch=16)
# points(years.raw, ts.pt.raw^0.5, pch=17)

# Climr raw data
obs.ts <- input_obs_ts(
  dataset = c("mswx.blend"),
  bbox = get_bb(xyz[2,]),
  years = 1901:2023,
  cache = F
)
obs.ts <- obs.ts$mswx.blend

# plot(obs.ts[[2]])
# plot(pt, add=T, pch=1, cex=2)
# plot(pt, add=T, pch=1, cex=5)
# names(obs.ts)
# 
temp <- obs.ts[[which(substr(names(obs.ts),12,17) == paste0(c("Tmin", "Tmax", "PPT")[e], "_", monthcodes[m]))]]
ts.pt.input <- as.vector(unlist(extract(temp, pt)[-1]))
years.input <- unique(substr(names(obs.ts),19,22))[1:length(ts.pt.input)]
points(years.input, ts.pt.input, col=2, cex=1.3, pch=2)

legend

# mswx input vs. climr output. 
par(mfrow=c(1,1), mar=c(3,3,1,1))
plot(ts.pt.raw[which(years.raw%in%years.climr)], ts.pt.climr)
# points(ts.pt.raw, ts.pt.raw^0.5, pch=17)
text(ts.pt.raw[which(years.raw%in%years.climr)], ts.pt.climr, years.climr, col="gray", cex=0.7, pos=4)
lines(c(-999, 999), c(-999, 999))
# what is this transform? also, note that points deviating from linear are post-1998. 

# climr input vs. output. 
par(mfrow=c(1,1), mar=c(3,3,1,1))
plot(ts.pt.input, ts.pt.climr)
# points(ts.pt.raw, ts.pt.raw^0.5, pch=17)
text(ts.pt.input[which(years.raw%in%years.climr)], ts.pt.climr, years.climr, col="gray", cex=0.7, pos=4)
lines(c(-999, 999), c(-999, 999))



#-----------------------------------------
## map comparison to AHCCD station data over BC and Alberta
#-----------------------------------------

#calculate the ahccd anomalies
stn <- fread("//objectstore2.nrs.bcgov/ffec/AHCCD/AHCCD_location.csv")
ahccd <- fread("//objectstore2.nrs.bcgov/ffec/AHCCD/AHCCD.csv")
ahccd <- ahccd[,- "tmean"]
names(ahccd) <- c("id", "year", "month", "Tmin", "Tmax", "PPT")

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
  
  anom.cru <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/CRUGPCC_2024/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  anom.mswx <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  # anom.mswx <- project(anom.mswx, anom.cru) # put on same grid for easier visual comparison
  pct <- if(elements[e]=="Pr") 100 else 1
  
  m=2
  for(m in 1:12){
    
    #calculate the ahccd anomalies (do this by month to ensure that we have complete data for as many stations as possible)
    stn.all <- ahccd[month == m,c("id", "year", c("Tmin", "Tmax", "PPT")[e]), with = FALSE]
    
    # plot(table(stn.all$year[!is.na(stn.all[,3])])) 
    
    count.ref <- stn.all[year %in% 1961:1990, 
                         .(n = sum(!is.na(get(c("Tmin", "Tmax", "PPT")[e])))), 
                         by = .(id)]
    # hist(count.ref$n, breaks = 0:30)
    count.recent <- stn.all[year %in% 2001:2020, 
                            .(n = sum(!is.na(get(c("Tmin", "Tmax", "PPT")[e])))), 
                            by = .(id)]
    # hist(count.recent$n, breaks = 0:20)
    
    # Select stations where complete records are >27 in baseline period and > 17 years in recent period
    include <- count.ref[n > 27]
    include <- include[id %in% count.recent[n > 17, id]]
    ahccd.include <- stn.all[id %in% include[,id]]
    
    ahccd.ref <- ahccd.include[year %in% 1961:1990, 
                               lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                               by = .(id)]
    ahccd.recent <- ahccd.include[year %in% 2001:2020, 
                                  lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                                  by = .(id)]
    
    # subtract ref from recent
    ahccd.anom <- copy(ahccd.ref)
    ahccd.anom[, (3:ncol(ahccd.anom)) := lapply(3:ncol(ahccd.recent),
                                                function(i) ahccd.recent[[i]] - ahccd.ref[[i]])]
    ppt_columns <- grep("PPT", names(ahccd.anom), value = TRUE)
    ahccd.anom[, (ppt_columns) := ahccd.recent[, .SD, .SDcols = ppt_columns] / ahccd.ref[, .SD, .SDcols = ppt_columns]]
    
    
    studyarea <- ext(c(-120, -118.8, 49, 50.4)) #okanagan valley study area used in time series plots below
    
    
    png(filename=paste("vignettes/plots_timeseries/CRUvsMSWXvsClimateNA_BCAB", elements[e], monthcodes[m], "png",sep="."), type="cairo", units="in", width=7.5, height=3, pointsize=10, res=300)

    mat <- matrix(c(7,1,2,7,3,4,7,5,6), 3)
    layout(mat, widths=c(1,1,1), heights=c(0.3, .08 ,1))
    
    par(mar=c(0.1,0.1,0.1,0.1))
    
    ahccd.ptData <- as.vector(unlist(ahccd.anom[,3]))
    ahccd.locations <- stn[which(stn$id %in% ahccd.anom[,id])]
    if (e == 3) ahccd.ptData <- log2(ahccd.ptData)
    ahccd.ptData[ahccd.ptData>lim.upper] <- lim.upper
    ahccd.ptData[ahccd.ptData < lim.lower] <- lim.lower
    z_cut <- cut(ahccd.ptData, breaks = breaks, include.lowest = TRUE, labels = colscheme)
    
    sources <- c("cru.gpcc", "mswx", "climatena")
    for(source in sources){
      
      source.name <- if(source=="mswx") "MSWX blend" else if(source=="climatena") "ClimateNA" else if(elements[e]=="Pr") "GPCC" else "CRU"
      plot(1, type="n", axes=F, xlab="", ylab="")  
      text(1,1, source.name, font=2,cex=1.35)  
      
      par(mar=c(0.1,0.1,0.1,0.1))
      if(source=="cru.gpcc"){
        X <- anom.cru[[m]]
      } else if(source=="mswx") {
        X <- anom.mswx[[m]]
      } else {
        X <- dem # use the DEM as a template raster
        X[anom.climna[, id1]] <- anom.climna[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
      }     
      if(e==3) X <- log2(X)
      X[X>lim.upper] <- lim.upper
      X[X < lim.lower] <- lim.lower
      
      image(X, xlim=c(-130, -110), ylim=c(48.4, 60.1), col=colscheme, breaks=breaks, xaxt="n", yaxt="n")
      plot(bdy.na, lwd=0.4, add=T)
      points(ahccd.locations$lon, ahccd.locations$lat, bg = as.character(z_cut), pch = 21, cex=1.6, lwd=1.5)
      mtext(paste0("(",letters[which(sources==source)],")"), side=3, line=-1.5, font=2, adj=0.01)
      
      plot(studyarea, add=T)
      
      if(source=="cru.gpcc") legend("topright", legend="AHCCD stations", pt.bg = as.character(z_cut), pch = 21, cex=0.9, pt.cex=1.6, pt.lwd=1.5, inset=c(0.015,0.015))

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
## Assess trend bias in cru, climatena, and MSWX time series (western Canada)
#-----------------------------------------

X <- rast(dem) # use the DEM as a template raster

png(filename=paste("vignettes/plots_timeseries/Boxplots_AHCCD_Wcan", "png",sep="."), type="cairo", units="in", width=6.5, height=5.5, pointsize=10, res=300)
par(mfrow=c(2,1))
for (e in 1:2) {
  
  # storage vectors
  values <- c()
  groups <- c()
  
  an.cru <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/CRUGPCC_2024/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  an.mswx <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  an.mswx <- project(an.mswx, an.cru) # put on same grid for easier visual comparison
  
  m = 2
  for (m in 1:12) {
    
    #calculate the ahccd anomalies (do this by month to ensure that we have complete data for as many stations as possible)
    stn.all <- ahccd[month == m,c("id", "year", c("Tmin", "Tmax", "PPT")[e]), with = FALSE]
    
    # plot(table(stn.all$year[!is.na(stn.all[,3])])) 
    
    count.ref <- stn.all[year %in% 1961:1990, 
                         .(n = sum(!is.na(get(c("Tmin", "Tmax", "PPT")[e])))), 
                         by = .(id)]
    # hist(count.ref$n, breaks = 0:30)
    count.recent <- stn.all[year %in% 2001:2020, 
                            .(n = sum(!is.na(get(c("Tmin", "Tmax", "PPT")[e])))), 
                            by = .(id)]
    # hist(count.recent$n, breaks = 0:20)
    
    # Select stations where complete records are >27 in baseline period and > 17 years in recent period
    include <- count.ref[n > 27]
    include <- include[id %in% count.recent[n > 17, id]]
    # include <- include[id %in% stn[pr == "SK", id]]
    include <- include[id %in% stn[lon < -101, id]]
    ahccd.include <- stn.all[id %in% include[,id]]
    
    ahccd.ref <- ahccd.include[year %in% 1961:1990, 
                               lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                               by = .(id)]
    ahccd.recent <- ahccd.include[year %in% 2001:2020, 
                                  lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                                  by = .(id)]
    
    # subtract ref from recent
    ahccd.anom <- copy(ahccd.ref)
    ahccd.anom[, (3:ncol(ahccd.anom)) := lapply(3:ncol(ahccd.recent),
                                                function(i) ahccd.recent[[i]] - ahccd.ref[[i]])]
    ppt_columns <- grep("PPT", names(ahccd.anom), value = TRUE)
    ahccd.anom[, (ppt_columns) := ahccd.recent[, .SD, .SDcols = ppt_columns] / ahccd.ref[, .SD, .SDcols = ppt_columns]]
    
    
    ahccd.ptData <- as.vector(unlist(ahccd.anom[,3]))
    ahccd.locations <- stn[which(stn$id %in% ahccd.anom[,id])]
    if (e == 3) ahccd.ptData <- log2(ahccd.ptData)
    
    an.climna <- dem
    an.climna[anom.climna[, id1]] <- anom.climna[, get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
    
    # extract from spatial data
    cru.ptData <- as.vector(extract(an.cru[[m]], ahccd.locations[,3:4])[,2])
    mswx.ptData <- as.vector(extract(an.mswx[[m]], ahccd.locations[,3:4])[,2])
    climna.ptData <- as.vector(extract(an.climna, ahccd.locations[,3:4])[,2])
    
    cru.error <- cru.ptData - ahccd.ptData
    mswx.error <- mswx.ptData - ahccd.ptData
    climna.error <- climna.ptData - ahccd.ptData
    
    # accumulate values
    values <- c(values, cru.error, mswx.error, climna.error)
    
    # accumulate groups
    groups <- c(
      groups,
      rep(paste0("CRU-", m), length(cru.ptData)),
      rep(paste0("MSWX-", m), length(mswx.ptData)),
      rep(paste0("ClimateNA-", m), length(climna.ptData))
    )
    
    print(m)  
  }
  
  # duplicate values for annual boxplot
  values <- c(values, values)
  groups <- c(groups, sapply(strsplit(groups, "-"), `[`, 1))
  
  
  # ensure correct order
  groups <- factor(groups, levels=unique(groups))
  
  # 3 datasets per month
  n_per_month <- 3
  
  # position calculations
  month_indices <- rep(1:13, each=n_per_month)
  spacing <- 1 # you can adjust this value
  at_positions <- (month_indices - 1) * (n_per_month + spacing) + rep(1:n_per_month, times=13)
  
  # plot
  par(mar=c(c(0.1,2)[e],4,c(2,0.1)[e],1), mgp=c(1.75, 0.25, 0), tck=-0.01)
  boxplot(values ~ groups, outline = FALSE, xlab="",
          at=at_positions, # <<< HERE
          las=2,
          col=rep(c("lightblue", "gold1", "forestgreen"), 12),
          xaxt="n",
          ylab=paste0(c("Tmin", "Tmax", "PPT")[e], " error (\u00B0C)"))
  par(xpd=TRUE)
  if(e==1) legend("topright", legend=c("CRU", "MSWX-blend", "ClimateNA"), 
                  # inset = c(-0, -0.125), 
                  # bty="n", 
                  ncol=3, fill = c("lightblue", "gold1", "forestgreen"))
  par(xpd=FALSE)
  if(e==1) title(main="2001-2020 anomaly error relative to AHCCD weather stations")
  mtext(paste0("(",letters[e],")"), side=3, line=-1, font=2, adj=0.01)
  # mtext(c("Tmin", "Tmax", "PPT")[e], side=3, line=-1, font=2, adj=0.03)
  
  if(e==2){
    # add month labels under clusters
    month_centers <- tapply(at_positions, month_indices, mean)
    axis(1, at=month_centers, labels=c(month.abb, "All"), las=1, cex.axis=0.8)
  }
  
  abline(h=0, col="grey")    
  
  print(e)  
}
dev.off()

#-----------------------------------------
## Assess trend bias in cru, climatena, and MSWX time series (separate provinces)
#-----------------------------------------

X <- rast(dem) # use the DEM as a template raster
e = 1
for(e in 1:2){
png(filename=paste("vignettes/plots_timeseries/Boxplots_AHCCD_provinces", elements[e], "png",sep="."), type="cairo", units="in", width=6.5, height=5.5, pointsize=10, res=300)
provinces <- c("BC", "AB", "SK")
par(mfrow=c(3,1))
for(p in 1:3) {

  # storage vectors
  values <- c()
  groups <- c()
  
  an.cru <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/CRUGPCC_2024/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  an.mswx <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  an.mswx <- project(an.mswx, an.cru) # put on same grid for easier visual comparison
  
  m = 2
  for (m in 1:12) {
    
    #calculate the ahccd anomalies (do this by month to ensure that we have complete data for as many stations as possible)
    stn.all <- ahccd[month == m,c("id", "year", c("Tmin", "Tmax", "PPT")[e]), with = FALSE]
    
    # plot(table(stn.all$year[!is.na(stn.all[,3])])) 
    
    count.ref <- stn.all[year %in% 1961:1990, 
                         .(n = sum(!is.na(get(c("Tmin", "Tmax", "PPT")[e])))), 
                         by = .(id)]
    # hist(count.ref$n, breaks = 0:30)
    count.recent <- stn.all[year %in% 2001:2020, 
                            .(n = sum(!is.na(get(c("Tmin", "Tmax", "PPT")[e])))), 
                            by = .(id)]
    # hist(count.recent$n, breaks = 0:20)
    
    # Select stations where complete records are >27 in baseline period and > 17 years in recent period
    include <- count.ref[n > 27]
    include <- include[id %in% count.recent[n > 17, id]]
    include <- include[id %in% stn[pr == provinces[p], id]]
    # include <- include[id %in% stn[lon < -101, id]]
    ahccd.include <- stn.all[id %in% include[,id]]
    
    ahccd.ref <- ahccd.include[year %in% 1961:1990, 
                               lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                               by = .(id)]
    ahccd.recent <- ahccd.include[year %in% 2001:2020, 
                                  lapply(.SD, function(x) mean(x, na.rm = TRUE)), 
                                  by = .(id)]
    
    # subtract ref from recent
    ahccd.anom <- copy(ahccd.ref)
    ahccd.anom[, (3:ncol(ahccd.anom)) := lapply(3:ncol(ahccd.recent),
                                                function(i) ahccd.recent[[i]] - ahccd.ref[[i]])]
    ppt_columns <- grep("PPT", names(ahccd.anom), value = TRUE)
    ahccd.anom[, (ppt_columns) := ahccd.recent[, .SD, .SDcols = ppt_columns] / ahccd.ref[, .SD, .SDcols = ppt_columns]]
    
    
    ahccd.ptData <- as.vector(unlist(ahccd.anom[,3]))
    ahccd.locations <- stn[which(stn$id %in% ahccd.anom[,id])]
    if (e == 3) ahccd.ptData <- log2(ahccd.ptData)
    
    an.climna <- dem
    an.climna[anom.climna[, id1]] <- anom.climna[, get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
    
    # extract from spatial data
    cru.ptData <- as.vector(extract(an.cru[[m]], ahccd.locations[,3:4])[,2])
    mswx.ptData <- as.vector(extract(an.mswx[[m]], ahccd.locations[,3:4])[,2])
    climna.ptData <- as.vector(extract(an.climna, ahccd.locations[,3:4])[,2])
    
    cru.error <- cru.ptData - ahccd.ptData
    mswx.error <- mswx.ptData - ahccd.ptData
    climna.error <- climna.ptData - ahccd.ptData
    
    # accumulate values
    values <- c(values, cru.error, mswx.error, climna.error)
    
    # accumulate groups
    groups <- c(
      groups,
      rep(paste0("CRU-", m), length(cru.ptData)),
      rep(paste0("MSWX-", m), length(mswx.ptData)),
      rep(paste0("ClimateNA-", m), length(climna.ptData))
    )
    
    print(m)  
  }
  
  # duplicate values for annual boxplot
  values <- c(values, values)
  groups <- c(groups, sapply(strsplit(groups, "-"), `[`, 1))
  
  
  # ensure correct order
  groups <- factor(groups, levels=unique(groups))
  
  # 3 datasets per month
  n_per_month <- 3
  
  # position calculations
  month_indices <- rep(1:13, each=n_per_month)
  spacing <- 1 # you can adjust this value
  at_positions <- (month_indices - 1) * (n_per_month + spacing) + rep(1:n_per_month, times=13)
  
  # plot
  par(mar=c(c(0,1,2)[p],4,c(2,1,0)[p],1), mgp=c(1.75, 0.25, 0), tck=-0.01)
  boxplot(values ~ groups, outline = FALSE, xlab="",
          at=at_positions, # <<< HERE
          las=2,
          col=rep(c("lightblue", "gold1", "forestgreen"), 12),
          xaxt="n",
          ylab=paste0(c("Tmin", "Tmax", "PPT")[e], " error (\u00B0C)"))
  par(xpd=TRUE)
  if(p==1) legend("topright", legend=c("CRU", "MSWX-blend", "ClimateNA"), 
                  # inset = c(-0, -0.125), 
                  # bty="n", 
                  ncol=3, fill = c("lightblue", "gold1", "forestgreen"))
  par(xpd=FALSE)
  if(p==1) title(main="2001-2020 anomaly error relative to AHCCD weather stations")
  mtext(paste0("(",letters[p],")", " ", provinces[p]), side=3, line=-1.5, font=2, adj=0.01)
  # mtext(c("Tmin", "Tmax", "PPT")[e], side=3, line=-1, font=2, adj=0.03)
  
  if(p==3){
    # add month labels under clusters
    month_centers <- tapply(at_positions, month_indices, mean)
    axis(1, at=month_centers, labels=c(month.abb, "All"), las=1, cex.axis=0.8)
  }
  
  abline(h=0, col="grey")    
  
  print(p)  
}
dev.off()
print(e)  
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
#     X[anom.climna[, id1]] <- anom.climna[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
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
                  which_refmap = "refmap_climr", 
                  obs_periods = "2001_2020",
                  obs_years = 1901:2020,
                  obs_ts_dataset = c("cru.gpcc", "mswx.blend")
)

e=1
# for(e in 1:3){
m=2
# for(m in 1:12){
ts.x.mswx <- as.numeric(unique(clim[PERIOD %in% 1901:2020 & DATASET == "climatena", PERIOD]))
ts.y.mswx <- unname(unlist(clim[clim$PERIOD %in% 1901:2020 & DATASET == "climatena",
                                     lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = PERIOD, 
                                     .SDcols = paste(c("Tmin", "Tmax", "PPT")[e], monthcodes[m], sep="_")][, 2]))
ts.x.cru <- as.numeric(unique(clim[PERIOD %in% 1901:2020 & DATASET == "cru.gpcc", PERIOD]))
ts.y.cru <- unname(unlist(clim[clim$PERIOD %in% 1901:2020 & DATASET == "cru.gpcc",
                               lapply(.SD, function(x) mean(x, na.rm = TRUE)), by = PERIOD, 
                               .SDcols = paste(c("Tmin", "Tmax", "PPT")[e], monthcodes[m], sep="_")][, 2]))
ts.ref <- mean(ts.y.climatena[ts.x.climatena%in%1961:1990])

## plot comparing CRU and climateNA time series with AHCCD station data. 
png(filename=paste("vignettes/plots_timeseries/CRUvsClimateNA_ts", elements[e], monthcodes[m], "png",sep="."), type="cairo", units="in", width=6.5, height=4, pointsize=10, res=300)

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
    
    png(filename=paste("vignettes/plots_timeseries/ERA5vsClimateNA_BCAB", elements[e], monthcodes[m], "png",sep="."), type="cairo", units="in", width=6.5, height=3.3, pointsize=10, res=300)

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
        X[anom.climna[, id1]] <- anom.climna[,get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))]
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

