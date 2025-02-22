
# create coarse-scale anomaly surfaces for adjusting between normal periods
# for use in climr
# Colin Mahony 
# Jan 8, 2024

library(terra)
library(rworldxtra)
data("countriesHigh")

#-----------------
# function for extending coastal values out into the ocean
extend.coastal <- function(x){
  x <- focal(x, w=3, fun="mean", na.policy="only") 
  x <- focal(x, w=5, fun="mean", na.policy="only") 
  values(x)[!is.finite(values(x))] <- NA
  return(x)
}

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
elements <- c("Tmin", "Tmax", "Pr")

studyarea <- ext(c(-170, -52.25, 13.75, 83.75))

bdy.na <- vect(countriesHigh[grep("Canada|United States|Mexico", countriesHigh$NAME),])
bdy.na <- erase(bdy.na, ext(c(-170, -140, 13, 30))) # erase hawaii
bdy.na <- aggregate(bdy.na) # dissolve
buff.na <- buffer(bdy.na, width=50000) # buffer coastline
# plot(bdy.na, add=T)
# plot(buff.na, add=T)

# loop through each variable and month 
# dir <- "//objectstore2.nrs.bcgov/ffec/data_climr_blend_monthly_anomalies/clmr_blend_ts_1901_2024"
dir <- "C:/Users/CMAHONY/OneDrive - Government of BC/Data/data_climr_blend_monthly_anomalies/clmr_blend_ts_1901_2024" #loading locally because it takes forever on object storage

e <- 1
elements.file <- c("tmin", "tmax", "prcp")
counter=0 #counter for total iterations
for(e in 1:3){
  element <- elements.file[e]
    files <- list.files(dir, pattern="\\.nc$")
    r <- rast(paste(dir, files[grep(element, files)], sep="/"))

    r <- extend.coastal(r)
    
  # set reference period for anomaly calculation
  ref.startyear <- 1981
  ref.duration <- 30
  for(ref.startyear in c(1961, 1981)){
    ref.endyear <- ref.startyear+ref.duration-1
    
    # calculate deltas for various averaging periods
    startyear=2001
    duration=20
    for(duration in c(10, 20, 30)){
      for(startyear in seq(1951, c(2011, 2001, 1991)[duration/10], 10)){
        endyear <- startyear+duration-1
        
        # loop through months
        for(m in 1:12){
          counter <- counter+1 #counter for total iterations
          temp <- r[[which(substr(time(r),6,7)==monthcodes[m])]]
          temp <- temp[[grep(element, names(temp))]]
          names(temp) <- substr(time(temp),1,4)
          
          # reference period normal
          ref.normal <- mean(temp[[names(temp)%in%ref.startyear:ref.endyear]], na.rm=T)
          
          normal <- mean(temp[[names(temp)%in%startyear:endyear]])
          
          # Calculate delta anomalies (subtract for temperature and divide for precipitation)
            delta <- if(e!=3) normal-ref.normal else (normal+1)/(ref.normal+1)
            delta <- aggregate(delta, fact=3, na.rm=T) # reduce resolution (the spatial detail isn't warranted)
            
          # plot(delta, main=paste(month.abb[m], element))
          
          # layer the rasters into a stack for export
          deltaStack <- if(m==1) delta else c(deltaStack, delta)
          
          print(m)
        }
        names(deltaStack) <- monthcodes
        outfile <- paste("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.", ref.startyear, "_", ref.endyear, ".to.", startyear, "_", endyear, ".", elements[e],  ".tif", sep="")
        writeRaster(deltaStack, outfile, overwrite=TRUE)
        
        print(startyear)
      }
      print(duration)
    }
    print(ref.startyear)
  }
  print(element)
}

#-----------------------------------------
# Plot the anomalies for quality check 1961-1990 to 2001-2020
elements <- c("Tmin", "Tmax", "Pr")
e=1
m=7
for(e in 1:3){
  anom <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.2001_2020.", elements[e], ".tif"))
  par(mfrow=c(3,4), mar=c(0,0,0,0))
  for(m in 1:12){
    X <- anom[[m]]
    if(e==3) X <- log2(X)
    # lim <- quantile(abs(values(X)), .99, na.rm=T)
      lim <- 3
      breaks <- seq(0-lim, lim, 0.1)
      colscheme <- colorRampPalette(hcl.colors(5,"Blue-Red 3"))(length(breaks)-1)
      if(e==3) colscheme <- rev(colscheme)
      X[X>lim] <- lim
      X[X < 0-lim] <- 0-lim
      plot(X, col=colscheme, breaks=breaks, main=paste(month.name[m], elements[e]), axes=F, type="continuous")
    # plot(bdy.na, add=T)
  }
}

plot(X)

#-----------------------------------------
# Plot the anomalies for quality check 1981-2010 to 1961-1990
elements <- c("Tmin", "Tmax", "Pr")
e=1
m=7
for(e in 1:3){
  anom <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1981_2010.to.1961_1990.", elements[e], ".tif"))
  par(mfrow=c(1,1), mar=c(0,0,0,0))
  for(m in 1:12){
    X <- anom[[m]]
    if(e==3) X <- log2(X)
    # lim <- quantile(abs(values(X)), .99, na.rm=T)
    lim <- 3
    breaks <- seq(0-lim, lim, 0.1)
    colscheme <- colorRampPalette(hcl.colors(5,"Blue-Red 3"))(length(breaks)-1)
    if(e==3) colscheme <- rev(colscheme)
    X[X>lim] <- lim
    X[X < 0-lim] <- 0-lim
    plot(X, col=colscheme, breaks=breaks, main=paste(month.name[m], elements[e]), axes=F, type="continuous")
    # plot(bdy.na, add=T)
  }
}

plot(X)
