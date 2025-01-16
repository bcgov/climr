
# EDA plots comparing US and BC PRISM Tmin to station data. 
# Colin Mahony 
# Jan 5, 2024

library(terra)
library(data.table)
library(leaflet)
library(RColorBrewer)
library(ranger)
library(rworldmap)

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
elements <- c("Tmin", "Tmax", "Pr")

pilotarea <- ext(c(-122.75, -113, 47.5, 50.5))

# PRISM dems
dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_akbcus/dem/", sep="")
dem.bc <- rast(paste(dir, "PRISM_dem.asc", sep=""))
dem.us <- rast(paste(dir, "PRISM_us_dem_800m_asc.asc", sep=""))
dem.bc <- crop(dem.bc, pilotarea)
dem.us <- crop(dem.us, pilotarea)
dem <- merge(dem.us, dem.bc)


#---------------------------------
# Climate data
#---------------------------------

e <- 1
m <- 10

for(m in 1:12){
  
  # load the source STATION data for the BC prism
  dir <- "//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/"
  stn.info <- fread(paste(dir, "Stations/",c("Tmin", "Tmax", "Pr")[e],"_uscdn_8110.csv", sep="")) #read in
  for (i in which(names(stn.info)%in%c(month.abb, "Annual"))) stn.info[get(names(stn.info)[i])==c(-9999), (i):=NA, ] # replace -9999 with NA
  stn.info <- stn.info[-which(El_Flag=="@"),]
  stn.data <- stn.info[,get(month.abb[m])]
  stn.info <- stn.info[is.finite(stn.data),]
  stn.info <- stn.info[-which(stn.info$Elevation == -9999),]
  stn.pts <- vect(stn.info, geom=c("Long", "Lat"), keepgeom=T)
  stn.pts <- crop(stn.pts, dem)
  stn.dem <- extract(dem, stn.pts, method="bilinear")[,-1]
  stn.pts$el.diff <- stn.dem-stn.pts$Elevation
  stn.pts <- stn.pts[is.finite(stn.pts$el.diff)]
  # hist(stn.pts$el.diff)
  cutoff <- max(abs(quantile(stn.pts$el.diff, c(0.05, 0.95)))) # tolerance (in meters) for the difference between the DEM and the station elevation
  stn.pts <- stn.pts[0-cutoff < stn.pts$el.diff & stn.pts$el.diff < cutoff] # prune stations that have an elevation difference beyond the tolerance
  stn.data <- as.data.frame(stn.pts[,which(names(stn.pts)==month.abb[m])])[,1] # vector of the climate variable of interest
  stn.data <- if(elements[e]=="Pr") log2(stn.data) else stn.data/10
  
  # BC prism
  dir <- "//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/1991-2020/"
  prism.bc <- rast(paste(dir, list.files(dir, pattern=paste(c("tasmin", "tasmax", "pr")[e],".*.nc", sep="")), sep=""))[[m]]
  prism.bc <- crop(prism.bc, pilotarea) #crop to a smaller test area
  values(prism.bc)[!is.finite(values(prism.bc))] <- NA
  
  # US PRISM (1981-2010) 
  dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_US/1991_2020/PRISM_",c("tmin", "tmax", "ppt")[e] ,"_30yr_normal_800mM4_all_asc/", sep="")
  file <- paste("PRISM_", c("tmin", "tmax", "ppt")[e], "_30yr_normal_800mM4_", monthcodes[m], "_asc.asc", sep="")
  prism.us <- rast(paste(dir, file, sep=""))
  prism.us <- crop(prism.us, pilotarea) #crop to a smaller test area
  values(prism.us)[!is.finite(values(prism.us))] <- NA
  
  # merged prism
  prism <- merge(prism.us, prism.bc)
  
  #---------------------------------
  # 
  #---------------------------------
  
  
  # function for rounding to the nearest odd integer. 
  round_to_odd <- function(x) {
    if (round(x) %% 2 == 0) {
      return(floor(x/2) * 2 + 1)
    } else {
      return(round(x))
    }
  }
  
  #============================================
  ## smoothed and residual elevation
  latFactor <- cos(mean(ext(dem)[3:4])*(pi/180)) # latitudinal correction for longitudinal length of cell
  w <- 77
  
  dem.d <- focal(dem, w=c(w, round_to_odd(w*latFactor)), fun=mean, na.rm=T)
  dem.o <- dem
  dem.r <- dem.o-dem.d
  
  stn.el.o <- stn.pts$Elevation
  stn.el.r <- extract(dem.r, stn.pts, method="bilinear")[,-1]
  stn.el.d <- stn.el.o - stn.el.r
  
  #============================================
  ## smoothed and residual tmin
  tn.d <- focal(prism, w=c(w, round_to_odd(w*latFactor)), fun=mean, na.rm=T)
  tn.o <- prism
  tn.r <- tn.o-tn.d
  # breaks=seq(0-max(values(abs(tn.d), na.rm=T)), max(values(abs(tn.d), na.rm=T)), diff(range(as.vector(values(tn.d)), na.rm=T))/200)
  # plot(tn.d, col=colorRampPalette(rev(brewer.pal(9, "RdBu")))(length(breaks)), breaks=breaks)
  # breaks=seq(0-max(values(abs(tn.r), na.rm=T)), max(values(abs(tn.r), na.rm=T)), diff(range(as.vector(values(tn.r)), na.rm=T))/200)
  # plot(tn.r, col=colorRampPalette(rev(brewer.pal(9, "RdBu")))(length(breaks)), breaks=breaks)
  
  stn.tn.o <- stn.data
  stn.tn.r <- extract(tn.r, stn.pts, method="bilinear")[,-1]
  stn.tn.d <- stn.tn.o - stn.tn.r
  
  
  #============================================
  # moving window regression
  #============================================
  
  # rasterize the station values (so that we can apply terra's focalReg function (for EDA and Plotting)
  stn.tn.o.pts <- stn.pts
  values(stn.tn.o.pts) <- stn.tn.o
  stn.tn.o.raster <- rasterize(stn.tn.o.pts, y=aggregate(dem, 3), field="value")
  
  stn.el.o.pts <- stn.pts
  values(stn.el.o.pts) <- stn.el.o
  stn.el.o.raster <- rasterize(stn.el.o.pts, y=aggregate(dem, 3), field="value")
  # plot(stn.el.o.raster)
  
  # rasterize the station residuals so that we can apply terra's focalReg function
  stn.tn.r.pts <- stn.pts
  values(stn.tn.r.pts) <- stn.tn.r
  stn.tn.r.raster <- rasterize(stn.tn.r.pts, y=aggregate(dem, 3), field="value")
  
  stn.el.r.pts <- stn.pts
  values(stn.el.r.pts) <- stn.el.r
  stn.el.r.raster <- rasterize(stn.el.r.pts, y=aggregate(dem, 3), field="value")
  # plot(stn.el.r.raster)



  #============================================
  # scatterplots for selected areas (using residual values) BC - US Comparison
  #============================================

  plotarea <- ext(c(-119, -114, 47.5, 50.5))

  par(mfrow=c(1,1))
  png(filename=paste("tests/manual_tests/plots/prism_US_lapserates_residuals_", elements[e], monthcodes[m], ".png", sep=""), type="cairo", units="in", width=8, height=9, pointsize=14, res=600)

  # window centroids
  point.look <- matrix(c(-117.5, 49.8, -115.5, 49.8, -117.5, 48.35, -115.5, 48.35), nrow = 4, byrow = T)
  box.size <- res(dem)*w

  # mat <- cbind(c(1,2,4), c(1,3,5)); layout(mat)
  mat <- rbind(c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(2,3,4,5)); layout(mat)
  par(mar=c(2,1,1,0.5), mgp=c(1.75, 0.25, 0))

  X <- tn.r
  lim <- max(values(tn.r), na.rm=T)
  X[X>lim] <- lim
  X[X < 0-lim] <- 0-lim
  plot(crop(X, plotarea), legend=TRUE, type="continuous", col=rev(hcl.colors(99, "RdBu")),
       main=paste(month.name[m], elements[e], "residuals ((\u00b0C) above regional average temperature)"), sep=" ")
  points(stn.pts)
  for(i in 1:dim(point.look)[1]){
    box.look <- ext(c(point.look[i,1]+box.size*c(-1, 1)/latFactor, point.look[i,2]+box.size*c(-1, 1)))
    plot(box.look, add=T)
    text(point.look[i,1]+box.size*0.8/latFactor, point.look[i,2]+box.size*0.8, i, cex=3, font=2)
  }

  par(mar=c(4,3,1,1), mgp=c(1.75, 0.25, 0))
  for(i in 1:dim(point.look)[1]){
    box.look <- ext(c(point.look[i,1]+box.size*c(-1, 1)/latFactor, point.look[i,2]+box.size*c(-1, 1)))
    el.look <- values(crop(stn.el.r.raster, box.look))
    tn.look <- values(crop(stn.tn.r.raster, box.look))
    el.look <- el.look[!is.na(el.look)]
    tn.look <- tn.look[!is.na(tn.look)]
    dem.look <- values(crop(dem.r, box.look))
    prism.look <- values(crop(tn.r, box.look))

    plot(dem.look, prism.look, pch=16, cex=0.15, col="gray", xlab="Residual elevation (m)", tck = -0.01, ylab = "Residual temperature (\u00b0C)")
    # points(el.look, tn.look, pch=16, cex=1.5, col="blue")
    # lm <- lm(tn.look~el.look)
    # abline(lm)
    # mtext(i, adj=0.95, line = -2.5, side=3, cex=1.5, font=2)
    # mtext(paste("lapse =", round(lm$coefficients[2]*1000, 1), "(\u00b0C)/km"), adj=0.05, line = -1.5, side=1, cex = 0.6)
  }
  # no evidence for inversion effects even in january
  dev.off()
  
  #============================================
  # scatterplots for selected areas (using raw values) BC - US Comparison
  #============================================
  
  plotarea <- ext(c(-119, -114, 47.5, 50.5))
  
  par(mfrow=c(1,1))
  png(filename=paste("tests/manual_tests/plots/prism_US_lapserates_raw_1991_2020_", elements[e], monthcodes[m], ".png", sep=""), type="cairo", units="in", width=8, height=9, pointsize=14, res=600)
  
  # window centroids
  point.look <- matrix(c(-117.5, 49.8, -115.5, 49.8, -117.5, 48.35, -115.5, 48.35), nrow = 4, byrow = T)
  box.size <- res(dem)*w
  
  # mat <- cbind(c(1,2,4), c(1,3,5)); layout(mat)
  mat <- rbind(c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(2,3,4,5)); layout(mat)
  par(mar=c(1,1,1,0.5), mgp=c(1.5, 0.25, 0))
  
  plot(crop(tn.o, plotarea), legend=TRUE, type="continuous", col=rev(hcl.colors(99, "RdBu")), 
       main=paste(month.name[m], elements[e], "(\u00b0C)", sep=" "))
  plot(stn.pts, add=T)
  for(i in 1:dim(point.look)[1]){
    box.look <- ext(c(point.look[i,1]+box.size*c(-1, 1)/latFactor, point.look[i,2]+box.size*c(-1, 1)))
    plot(box.look, add=T)
    text(point.look[i,1]+box.size*0.8/latFactor, point.look[i,2]+box.size*0.8, i, cex=3, font=2)
  }
  
  par(mar=c(4,3,2,1), mgp=c(1.75, 0.25, 0))
  for(i in 1:dim(point.look)[1]){
    box.look <- ext(c(point.look[i,1]+box.size*c(-1, 1)/latFactor, point.look[i,2]+box.size*c(-1, 1)))
    el.look <- values(crop(stn.el.o.raster, box.look))
    tn.look <- values(crop(stn.tn.o.raster, box.look))
    el.look <- el.look[!is.na(el.look)]
    tn.look <- tn.look[!is.na(tn.look)]
    dem.look <- values(crop(dem.o, box.look))
    prism.look <- values(crop(tn.o, box.look))
    
    plot(dem.look, prism.look, pch=16, cex=0.15, col="gray", xlab="Elevation (m)", tck = -0.01, ylab = "Temperature (\u00b0C)")
    points(el.look, tn.look, pch=16, cex=1)
    lm <- lm(tn.look~el.look)
    abline(lm)
    mtext(i, adj=0.95, line = -2.5, side=3, cex=1.5, font=2)
    mtext(paste("slope =", round(lm$coefficients[2]*1000, 1), "\u00b0C/km"), adj=0.05, line = -1.5, side=1, cex = 0.6)
    if(i==1){
      par(xpd=T)
      xpos <- min(dem.look, na.rm = T)
      ypos <- max(prism.look, na.rm = T)+diff(range(prism.look, na.rm = T))*0.35
      legend(x=xpos, y=ypos, legend = c("PRISM (1991-2020)", "Stations (1981-2010)"), pch=16, pt.cex = c(0.5, 1), col = c("gray", 1), bty="n")
      par(xpd=F)
    }
  }
  # not much evidence for inversion effects even in january
  dev.off()
  
  #============================================
  # scatterplots for selected areas (using raw values) BC - US Comparison
  #============================================
  
  plotarea <- ext(c(-116.5, -114, 47.5, 49))
  
  par(mfrow=c(1,1))
  png(filename=paste("tests/manual_tests/plots/prism_US_lapserates_raw_Montana_1991_2020_", elements[e], monthcodes[m], ".png", sep=""), type="cairo", units="in", width=8, height=9, pointsize=14, res=600)
  
  # window centroids
  point.look <- matrix(c(-115.875, 48.625, -114.625, 48.625, -115.875, 47.875, -114.625, 47.875), nrow = 4, byrow = T)
  box.size <- res(dem)*w/2
  
  # mat <- cbind(c(1,2,4), c(1,3,5)); layout(mat)
  mat <- rbind(c(1,1,1,1), c(1,1,1,1), c(1,1,1,1), c(2,3,4,5)); layout(mat)
  par(mar=c(1,1,1,0.5), mgp=c(1.5, 0.25, 0))
  
  plot(crop(tn.o, plotarea), legend=TRUE, type="continuous", col=rev(hcl.colors(99, "RdBu")), 
       main=paste(month.name[m], elements[e], "(\u00b0C)", sep=" "))
  plot(stn.pts, add=T)
  for(i in 1:dim(point.look)[1]){
    box.look <- ext(c(point.look[i,1]+box.size*c(-1, 1)/latFactor, point.look[i,2]+box.size*c(-1, 1)))
    plot(box.look, add=T)
    text(point.look[i,1]+box.size*0.8/latFactor, point.look[i,2]+box.size*0.8, paste0("4.",i), cex=3, font=2)
  }
  par(mar=c(4,3,2,1), mgp=c(1.75, 0.25, 0))
  for(i in 1:dim(point.look)[1]){
    box.look <- ext(c(point.look[i,1]+box.size*c(-1, 1)/latFactor, point.look[i,2]+box.size*c(-1, 1)))
    el.look <- values(crop(stn.el.o.raster, box.look))
    tn.look <- values(crop(stn.tn.o.raster, box.look))
    el.look <- el.look[!is.na(el.look)]
    tn.look <- tn.look[!is.na(tn.look)]
    dem.look <- values(crop(dem.o, box.look))
    prism.look <- values(crop(tn.o, box.look))
    
    plot(dem.look, prism.look, pch=16, cex=0.15, col="gray", xlab="Elevation (m)", tck = -0.01, ylab = "Temperature (\u00b0C)")
    points(el.look, tn.look, pch=16, cex=1)
    lm <- lm(tn.look~el.look)
    abline(lm)
    mtext(paste0("4.",i), adj=0.95, line = -2.5, side=3, cex=1.5, font=2)
    mtext(paste("slope =", round(lm$coefficients[2]*1000, 1), "\u00b0C/km"), adj=0.05, line = -1.5, side=1, cex = 0.6)
    if(i==1){
      par(xpd=T)
      xpos <- min(dem.look, na.rm = T)
      ypos <- max(prism.look, na.rm = T)+diff(range(prism.look, na.rm = T))*0.35
      legend(x=xpos, y=ypos, legend = c("PRISM (1991-2020)", "Stations (1981-2010)"), pch=16, pt.cex = c(0.5, 1), col = c("gray", 1), bty="n")
      par(xpd=F)
    }
  }
  # no evidence for inverse-tilda effect
  
  dev.off()
  
  print(m)
}
