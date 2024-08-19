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
climr <- downscale(xyz = my_grid, which_refmap = "refmap_climatena", vars = list_vars())

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
for(var in vars){
  png(filename=paste(figDir, "/climrVclimna", var, "png",sep="."), type="cairo", units="in", width=6.5, height=2, pointsize=10, res=600)
  
  par(mfrow=c(1,3), mar=c(0,0,2,2))
  values(X) <- NA
  
  data_climr <-   climr[,get(var)]
  X[climr[, id]] <- data_climr
  plot(X, main=paste("climr", var), axes=F)
  
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

