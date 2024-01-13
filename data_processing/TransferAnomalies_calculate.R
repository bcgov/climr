
# create coarse-scale anomaly surfaces for adjusting between normal periods
# for use in climr
# Colin Mahony 
# Jan 8, 2024

library(terra)
library(rworldxtra)
data("countriesHigh")

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

# loop through each variable and month using CRU for temperature and GPCC for precipitation
e <- 1
elements.cru <- c("tmn", "tmx", "pre", "tmp")
counter=0 #counter for total iterations
for(e in 1:3){
  element <- elements.cru[e]
  if(element!="pre"){
    dir <- "//objectstore2.nrs.bcgov/ffec/TimeSeries_gridded_monthly/cru_ts4.05"
    files <- list.files(dir, pattern="\\.nc$")
    r <- rast(paste(dir, files[grep(element, files)], sep="/"))
  } else {
    dir <- "//objectstore2.nrs.bcgov/ffec/TimeSeries_gridded_monthly/GPCC"
    files <- list.files(dir, pattern="\\.nc$")
    file <- files[13]
    r <- rast(paste(dir, file, sep="/"))
    r <- rotate(r)
  }
  
  r <- crop(r, studyarea)
  r <- mask(r, buff.na)
  # plot(r[[1]])
  
  # set reference period for anomaly calculation
  ref.startyear <- 1961
  ref.duration <- 30
  ref.endyear <- ref.startyear+ref.duration-1
  
  # calculate deltas for various averaging periods
  startyear=1961
  duration=30
  for(duration in c(10, 20, 30)){
    for(startyear in seq(1951, c(2011, 2001, 1991)[duration/10], 10)){
      endyear <- startyear+duration-1
      
      # loop through months
      for(m in 1:12){
        counter <- counter+1 #counter for total iterations
        temp <- r[[which(substr(time(r),6,7)==monthcodes[m])]]
        names(temp) <- substr(time(temp),1,4)
        
        # reference period normal
        ref.normal <- mean(temp[[names(temp)%in%ref.startyear:ref.endyear]], na.rm=T)
        
        normal <- mean(temp[[names(temp)%in%startyear:endyear]])
        
        # Calculate delta anomalies (subtract for temperature and divide for precipitation)
        if(counter==1) { # since we are using both CRU and GPCC, use the first cru aggregation as a template raster for GPCC
          delta <- if(element!="pre") normal-ref.normal else normal/ref.normal
          delta <- aggregate(delta, fact=3, na.rm=T) # reduce resolution (the spatial detail isn't warranted)
          template <- delta
        } else {
          delta <- if(element!="pre") normal-ref.normal else normal/ref.normal
          delta <- project(delta, template, method="bilinear") 
        }
        delta <- focal(delta, 3, "modal", na.policy="only") # fill interior holes
        delta <- mask(delta, buff.na) #mask to coastline
        
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
  
  print(element)
}

test <- rast("TransferAnomalies/delta.from.1981_2010.to.1961_1990.Tmin.tif")
plot(test[[1]])

