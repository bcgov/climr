
# document the composite climatology
# Colin Mahony 

library(terra)
library(terra)
library(data.table)
library(leaflet)
library(RColorBrewer)
library(ranger)
library(rworldmap)

studyarea.name <- "yukon"

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

studyarea <- ext(c(-144, -130, 58, 66))

par(mfrow=c(1,1))

#DEM
# dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/", sep="")
dir <- paste("C:/Users/CMAHONY/OneDrive - Government of BC/Data/climr_mosaic/", sep="")
dem <- rast(paste(dir, "climr_mosaic_dem_800m.tif", sep=""))
dem <- crop(dem, studyarea)
dem <- aggregate(dem, 2)
plot(dem)

land <- rast("//objectstore2.nrs.bcgov/ffec/Climatologies/climr_mosaic/climr_mosaic_land_800m.tif")

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
pts <- pts[,c(1,5,3,2,4)] #restructure for climr input
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
    
    # climatena.climr <- dem
    # climatena.climr[climatena.climr.all[, id]] <- climatena.climr.all[, get(var)] 
    # if(element=="Pr") values(climatena.climr) <- log2(values(climatena.climr))
    # values(climatena.climr)[!is.finite(values(climatena.climr))] <- NA
    
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
    
    # load the BC PRISM  data for the variable
    dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/", sep="")
    file <- list.files(dir, pattern=paste(c("tmin", "tmax", "pr")[e],"_.*._",m, ".tif", sep=""))
    prism.bc <- rast(paste(dir, file, sep=""))
    prism.bc <- prep(prism.bc, studyarea=studyarea, element=elements[e], breaks=breaks)
    
    # load the Alaska PRISM (1981-2010) data for the variable
    dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_AK/", sep="")
    prism.ak <- rast(paste(dir, list.files(dir, pattern=paste(".*.", c("tmin", "tmax", "ppt")[e],".*.", monthcodes[m], ".asc", sep="")), sep=""))
    ext.ak <- ext(prism.ak)
    ext.ak[1:2] <- ext.ak[1:2] - 360
    ext(prism.ak) <- ext.ak
    ext.ak[1] <- -179
    prism.ak <- crop(prism.ak, ext.ak)
    prism.ak <- prep(prism.ak, studyarea=studyarea, element=elements[e], breaks=breaks)
    
    # load the worldclim data for the variable
    dir <- "//objectstore2.nrs.bcgov/ffec/Climatologies/WorldClim/"
    worldclim <- rast(paste(dir, list.files(dir, pattern=paste(".*.", c("tmin", "tmax", "prec")[e],"_", monthcodes[m], ".tif", sep="")), sep=""))
    worldclim <- prep(worldclim, studyarea=studyarea, element=elements[e], breaks=breaks)

    # load the daymet data for the variable
    dir <- "//objectstore2.nrs.bcgov/ffec/data_daymet/daymet_climatology_1981_2010/"
    daymet <- rast(paste(dir, list.files(dir, pattern=paste(".*.", month.abb[m], "_",  c("tmin", "tmax", "prcp")[e],".*.nc", sep="")), sep=""))
    daymet <- project(daymet, dem)
    daymet <- prep(daymet, studyarea=studyarea, element=elements[e], breaks=breaks)
    
    # load the climatena data for the variable
    climatena <- dem
    climatena[climatena.all[, ID1]] <- climatena.all[, get(paste0(c("Tmin", "Tmax", "PPT")[e], monthcodes[m]))] 
    climatena <- prep(climatena, studyarea=studyarea, element=elements[e], breaks=breaks)
    
    # # load the western canada 2km PRISM data for the variable
    # dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_canw/PRISM_",c("tmin", "tmax", "ppt")[e],"_canw_1961-1990_normal_2kmM1_", monthcodes[m], "_asc/", sep="")
    # file <- list.files(dir)
    # prism.canw <- rast(paste(dir, file, sep=""))/100
    # # project forward to 1981_2010 using the delta surfaces prepared in ObservedAnomalies_WNA.R
    # delta.e <- rast(paste0("//objectstore2.nrs.bcgov/ffec/TransferAnomalies/delta.from.1961_1990.to.1981_2010.", elements[e], ".tif"))
    # delta <- project(delta.e[[m]], prism.canw)
    # prism.canw <- if(elements[e] == "Pr") prism.canw/delta else prism.canw+delta
    # prism.canw <- prep(prism.canw, studyarea=studyarea, element=elements[e], breaks=breaks)
    
    # # leaflet map
    # labels <- paste(stn.info$Name, "(El. ", stn.info$Elevation, "m)", sep="")
    # map <- leaflet(stn.info) %>%
    #   addTiles(group = "basemap") %>%
    #   addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
    #   # addRasterImage(dem, colors =terrain.colors(99), opacity = 1, maxBytes = 6 * 1024 * 1024, group = "elevation") %>%
    #   addRasterImage(prism.bc, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "BC PRISM") %>%
    #   addRasterImage(prism.ak, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "AK PRISM") %>%
    #   # addRasterImage(climatena.climr, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "ClimateNA.climr") %>%
    #   addRasterImage(climatena, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "ClimateNA") %>%
    #   addRasterImage(climr, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "climr") %>%
    #   addRasterImage(mosaic.raw, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Raw climr mosaic") %>%
    #   addCircleMarkers(lng = ~Long, lat = ~Lat, color="black", fillColor = ~ ColPal(stn.data), opacity = 1, fillOpacity = 1, popup = labels, radius=6, weight=2, group = "Stations") %>%
    #   addLayersControl(
    #     baseGroups = c("basemap", "sat photo"),
    #     # overlayGroups = c("ClimateNA", "WorldClim (1971-2000)", "CHELSA", "Daymet", "W. Can. PRISM", "AK PRISM", "BC PRISM", "Composite", "Stations"),
    #     overlayGroups = c("Raw climr mosaic",  "climr", "ClimateNA", "ClimateNA.climr", "US PRISM", "BC PRISM", "Stations"),
    #     options = layersControlOptions(collapsed = FALSE)
    #   )
    # map
    
    
    # =============================================
    # Comparison 
    plotarea <- ext(c(-143, -131, 58, 65.5))
    
    sources <- c("prism.bc", "daymet", "prism.canw", "worldclim", "chelsa", "climatena","climr")
    source.names <- c("BC PRISM", "Daymet", "W. Can. PRISM", "WorldClim", "CHELSA", "ClimateNA", "climr composite")

    png(filename=paste("tests/manual_tests/plots/mosaics_compare_",studyarea.name,"_climrOnly_", elements[e], monthcodes[m], ".png", sep=""), 
        type="cairo", units="in", width=8.2, height=6, pointsize=10, res=600)
    
    mar = c(0.5,0.5,4,0.5)
    par(mfrow=c(1,2), mar=mar)
    
    X <- merge(prism.bc, prism.ak)
    X <- merge(X, project(daymet,X))
    values(X)[values(X)>max(breaks)] <- max(breaks)
    values(X)[values(X)<min(breaks)] <- min(breaks)
    X <- crop(X, plotarea)
    X <- mask(X, project(land, X))
    plot(X, col=ColScheme, breaks=breaks, type="continuous", axes=F, box=T, legend=F, mar=NA)
    legend_ramp(X, title = paste(month.name[m], element.names[e]), ColScheme = ColScheme, breaks = breaks, pos=c(0.1, 0.9, 1.05, 1.08), log = if(e==3) 2 else NULL, horizontal = TRUE)
    # points(stn.info$Long, stn.info$Lat, bg=ColScheme[cut(stn.data, breaks = breaks)], pch=21, cex=1.5)
    text(-141.85, 65, "Alaska\nPRISM", font=2, cex=1.25)
    mtext("Daymet", side=1, adj=0.95, font=2, line=-12)
    mtext("BC PRISM", side=1, adj=0.95, font=2, line=-3)
    
    source <- "climr"
    X <- get(source)
    X <- crop(X, plotarea)
    X <- mask(X, project(land, X))
    plot(X, col=ColScheme, breaks=breaks, type="continuous", axes=F, box=T, legend=F, mar=NA)
    legend_ramp(X, title = paste(month.name[m], element.names[e]), ColScheme = ColScheme, breaks = breaks, pos=c(0.1, 0.9, 1.05, 1.08), log = if(e==3) 2 else NULL, horizontal = TRUE)
    # points(stn.info$Long, stn.info$Lat, bg=ColScheme[cut(stn.data, breaks = breaks)], pch=21, cex=1.5)
    mtext("climr mosaic", side=1, adj=0.025, font=2, line=-2)
    
    dev.off()
    
    print(m)
  }
  print(e)
}
