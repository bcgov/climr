
# Plots of mosaic climatology for western North America from multiple sources
# Colin Mahony 
# Jan 1, 2025

library(terra)
library(data.table)
library(RColorBrewer)

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
elements <- c("Tmin", "Tmax", "Pr")
element.names <- c("mean\ndaily minimum temperature (\u00b0C)", "mean\ndaily maximum temperature (\u00b0C)", "precipitation (mm)")

source("log.legend.R")

land <- rast("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic_land.tif")
# land <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/climr_mosaic_land.tif")

#-----------------------
#determine a common colour scale for all months of each element
for(e in 1:length(elements)){
  assign(paste("breaks", elements[e], sep="."), vector())
  
  for(m in 1:12){
    
    comp <- rast(paste("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic_1981_2010_", elements[e], monthcodes[m], ".tif", sep=""))
    # comp <- rast(paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/climr_mosaic_1981_2010_", elements[e], monthcodes[m], ".tif", sep=""))
    if(elements[e]=="Pr") values(comp) <- log2(values(comp))
    
    temp <- values(comp)
    temp <- temp[is.finite(temp)]
    inc=diff(range(temp))/500
    breaks=seq(quantile(temp, 0.005)-inc, quantile(temp, 0.995)+inc, inc)
    assign(paste("breaks", elements[e], sep="."), c(get(paste("breaks", elements[e], sep=".")), breaks))
    
    print(monthcodes[m])  
  }
  print(elements[e])
}

period <- "1981_2010"
# for(period in c("1961_1990", "1981_2010")){
#export map pngs
e=2
m=3
#-----------------------
for(e in 1:length(elements)){
  
  common.colours <- if(elements[e]=="Pr") TRUE else FALSE
  
  for(m in 1:12){
    
    comp <- rast(paste("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic_1981_2010_", elements[e], monthcodes[m], ".tif", sep=""))
    # comp <- rast(paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/climr_mosaic_1981_2010_", elements[e], monthcodes[m], ".tif", sep=""))
    if(elements[e]=="Pr") values(comp) <- log2(values(comp))
    comp <- mask(comp, land)
    
    if(common.colours){
      temp <- get(paste("breaks", elements[e], sep="."))
      inc=diff(range(temp))/500
      breaks=seq(min(temp, na.rm=T)-inc, max(temp, na.rm=T)+inc, inc)
    } else {
      temp <- values(comp)
      temp <- temp[is.finite(temp)]
      inc=diff(range(temp))/500
      breaks=seq(quantile(temp, 0.005)-inc, quantile(temp, 0.995)+inc, inc)
    }
    ColScheme <- colorRampPalette(if(elements[e]=="Pr") brewer.pal(9, "YlGnBu") else rev(brewer.pal(11, "RdYlBu")))(length(breaks)-1)
    
    values(comp)[values(comp)>max(breaks)] <- max(breaks)
    values(comp)[values(comp)<min(breaks)] <- min(breaks)
    
    png(filename=paste("results/climr_mosaic_", period, "_", elements[e], monthcodes[m], ".png", sep=""), type="cairo", units="in", width=9.5, height=7.8, pointsize=12, res=700)
    par(mfrow=c(1,1), mar=c(0,0,0,0))
    plot(comp, col=ColScheme, breaks=breaks, legend=F, main="", axes=F, maxcell=ncell(comp), mar=NA)
    log.legend(comp, title = paste("1981-2010", month.name[m], element.names[e]), ColScheme = ColScheme, breaks = breaks, pos=c(0.2, 0.23, 0.1, 0.5), log = if(e==3) 2 else NULL, horizontal = FALSE, title.height = if(e==3) 1 else 2)
    dev.off()
    
    print(monthcodes[m])  
  }
  print(elements[e])
}
# print(period)
# }

