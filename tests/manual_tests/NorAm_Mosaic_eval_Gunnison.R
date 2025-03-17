
# document the composite climatology
# Colin Mahony 

library(terra)
library(terra)
library(data.table)
library(leaflet)
library(RColorBrewer)
library(ranger)
library(rworldmap)

studyarea.name <- "gunnison"

# function for preparing data
prep <- function(x, studyarea, element, breaks){
  x <- crop(x, studyarea)
  if(element=="Pr") values(x) <- log2(values(x))
  values(x)[!is.finite(values(x))] <- NA
  values(x)[values(x)>max(breaks)] <- max(breaks)
  values(x)[values(x)<min(breaks)] <- min(breaks)
  return(x)
}

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
elements <- c("Tmin", "Tmax", "Pr")
element.names <- c("Tmin (\u00b0C)", "Tmax (\u00b0C)", "precipitation (mm)")

studyarea <- ext(c(-109, -104, 38, 39))

#DEM
# dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/", sep="")
dir <- paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/", sep="")
dem <- rast(paste(dir, "climr_mosaic_dem_800m.tif", sep=""))
dem <- crop(dem, studyarea)
plot(dem)

## climr query points
grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
colnames(grid) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects

## climr climateNA mosaic
climatena.climr.all <- downscale(grid, which_refmap = "refmap_climatena", vars = list_vars(set="Monthly"))

## climr mosaic
# mosaic.raw <- rast("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic.tif")
mosaic.raw <- rast("C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/climr_mosaic.tif")
names(mosaic.raw)[length(names(mosaic.raw))] <- "dem2_WNA" 
attr(mosaic.raw, "builder") <- "climr" # necessary to trick the package into thinking this is climr input
climr.all <- downscale_core(grid, refmap = mosaic.raw, vars = list_vars(set="Monthly"))

#climateNA inputs
pts <- as.data.frame(dem, cells=T, xy=T)
colnames(pts) <- c("ID1", "lon", "lat", "el")
pts$ID2 <- NA
pts <- pts[,c(1,5,3,2,4)] 
write.csv(pts, paste0("C:/Users/CMAHONY/OneDrive - Government of BC/Data/Climatena_v750/",studyarea.name,".csv"), row.names = F)
## climateNA mosaic
climatena.all <- fread(paste0("C:/Users/CMAHONY/OneDrive - Government of BC/Data/Climatena_v750/",studyarea.name,"_Normal_1981_2010MP.csv"))

e <- 1
for(e in 1:3){

  element <- elements[e]
  m <- 1
  for(m in c(1,7)){
    
    var <- paste0(c("Tmin", "Tmax", "PPT")[e], "_", monthcodes[m])
    
    climr <-  dem
    climr[climr.all[, id]] <- climr.all[, get(var)] 
    if(element=="Pr") values(climr) <- log2(values(climr))
    values(climr)[!is.finite(values(climr))] <- NA
    
     # color scheme
    combined <- values(climr)
    combined <- combined[is.finite(combined)]
    inc=diff(range(combined))/500
    breaks=seq(quantile(combined, 0.002)-inc, quantile(combined, 0.998)+inc, inc)
    ColScheme <- colorRampPalette(if(elements[e]=="Pr") brewer.pal(9, "YlGnBu") else rev(brewer.pal(11, "RdYlBu")))(length(breaks)-1)
    ColPal <- colorBin(ColScheme, bins=breaks, na.color = "white")
    ColPal.raster <- colorBin(ColScheme, bins=breaks, na.color = "transparent")
    
    values(climr)[values(climr)>max(breaks)] <- max(breaks)
    values(climr)[values(climr)<min(breaks)] <- min(breaks)
    # values(climatena.climr)[values(climatena.climr)>max(breaks)] <- max(breaks)
    # values(climatena.climr)[values(climatena.climr)<min(breaks)] <- min(breaks)
    
    # load the US PRISM (1981-2010) data for the variable
    dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_US/PRISM_",c("tmin", "tmax", "ppt")[e] ,"_30yr_normal_1981_2010_800m_all_files_asc/", sep="")
    file <- paste("PRISM_", c("tmin", "tmax", "ppt")[e], "_30yr_normal_1981_2010_800m_", monthcodes[m], "_asc.asc", sep="")
    prism.us <- rast(paste(dir, file, sep=""))
    prism.us <- prep(prism.us, studyarea=studyarea, element=elements[e], breaks=breaks)
    
    # load the climatena data for the variable
    climatena <- dem
    climatena[climatena.all[, ID1]] <- climatena.all[, get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))] 
    climatena <- prep(climatena, studyarea=studyarea, element=elements[e], breaks=breaks)
    
    # # leaflet map
    # labels <- paste(stn.info$Name, "(El. ", stn.info$Elevation, "m)", sep="")
    # map <- leaflet(stn.info) %>%
    #   addTiles(group = "basemap") %>%
    #   addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
    #   # addRasterImage(dem, colors =terrain.colors(99), opacity = 1, maxBytes = 6 * 1024 * 1024, group = "elevation") %>%
    #   addRasterImage(prism.bc, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "BC PRISM") %>%
    #   addRasterImage(prism.us, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "US PRISM") %>%
    #   # addRasterImage(climatena.climr, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "ClimateNA.climr") %>%
    #   addRasterImage(climatena, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "ClimateNA") %>%
    #   addRasterImage(climr, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "climr") %>%
    #   addRasterImage(mosaic.raw, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Raw climr mosaic") %>%
    #   addCircleMarkers(lng = ~Long, lat = ~Lat, color="black", fillColor = ~ ColPal(stn.data), opacity = 1, fillOpacity = 1, popup = labels, radius=6, weight=2, group = "Stations") %>%
    #   addLayersControl(
    #     baseGroups = c("basemap", "sat photo"),
    #     # overlayGroups = c("ClimateNA", "WorldClim (1971-2000)", "CHELSA", "Daymet", "W. Can. PRISM", "US PRISM", "BC PRISM", "Composite", "Stations"),
    #     overlayGroups = c("Raw climr mosaic",  "climr", "ClimateNA", "ClimateNA.climr", "US PRISM", "BC PRISM", "Stations"),
    #     options = layersControlOptions(collapsed = FALSE)
    #   )
    # map
    
    
    # =============================================
    # Comparison for BC-US Border
    plotarea <- ext(c(-108.5, -104.7, 38, 39))
    
    sources <- c("prism.bc", "prism.us", "daymet", "prism.canw", "worldclim", "chelsa", "climatena","climr")
    source.names <- c("BC PRISM", "US PRISM", "Daymet", "W. Can. PRISM", "WorldClim", "CHELSA", "ClimateNA", "climr composite")
    
    png(filename=paste("tests/manual_tests/plots/mosaics_compare_",studyarea.name,"_", elements[e], monthcodes[m], ".png", sep=""), type="cairo", units="in", width=9, height=8.75, pointsize=14, res=600)
    
    par(mfrow=c(3,1), mar=c(0.5,0.5,0.5,5))
    
    source <- "prism.us"
    X <- get(source)
    X <- crop(X, plotarea)
    plot(X, col=ColScheme, breaks=breaks, type="continuous", axes=F, box=T, legend=F, mar=NA)
    legend_ramp(X, title = paste(month.name[m], element.names[e]), ColScheme = ColScheme, breaks = breaks, pos=c(1.035, 1.055, 0, 1), log = if(e==3) 2 else NULL, horizontal = FALSE)
    mtext("US PRISM", side=1, adj=0.025, font=2, line=-2)

    source <- "climr"
    X <- get(source)
    X <- crop(X, plotarea)
    plot(X, col=ColScheme, breaks=breaks, type="continuous", axes=F, box=T, legend=F, mar=NA)
    legend_ramp(X, title = paste(month.name[m], element.names[e]), ColScheme = ColScheme, breaks = breaks, pos=c(1.035, 1.055, 0, 1), log = if(e==3) 2 else NULL, horizontal = FALSE)
    mtext("climr mosaic", side=1, adj=0.025, font=2, line=-2)
    
    source <- "climatena"
    X <- get(source)
    X <- crop(X, plotarea)
    plot(X, col=ColScheme, breaks=breaks, type="continuous", axes=F, box=T, legend=F, mar=NA)
    legend_ramp(X, title = paste(month.name[m], element.names[e]), ColScheme = ColScheme, breaks = breaks, pos=c(1.035, 1.055, 0, 1), log = if(e==3) 2 else NULL, horizontal = FALSE)
    mtext("ClimateNA mosaic", side=1, adj=0.025, font=2, line=-2)
    
    dev.off()
    
    print(m)
  }
  print(e)
}
