
# creation of a composite climatology (mosaic) for north america, from PRISM and daymet
# use ML to create a synthetic "prism-like" blend between PRISM and Daymet in northern mexico 
# Colin Mahony 
# Jan 1, 2025

library(terra)
library(data.table)
library(leaflet)
library(RColorBrewer)
library(ranger)
library(rworldmap)

studyarea <- ext(c(-120, -100, 25, 35))

# composite DEM for study area
dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_US/PRISM_us_dem_800m_asc/", sep="")
dem.us <- rast(paste(dir, "PRISM_us_dem_800m_asc.asc", sep=""))
dem.us <- crop(dem.us, studyarea)
dem <- extend(dem.us, studyarea) # start with the US DEM and extend it to the full study area range
dem.noram <- rast("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/NA_Elevation/data/northamerica/northamerica_elevation_cec_2023.tif") #250m dem downloaded from http://www.cec.org/north-american-environmental-atlas/elevation-2023/
dem.noram <- project(dem.noram, dem, method="near") #project 250m source dem to the study area grid. method="near" to preserve elevation variance 
dem <- dem.noram  
# rm(dem.noram)
dem <- focal(dem, w=13, fun="min", na.policy="only") # add a buffer of minimum values around coastline to allow for a buffer of climate prediction
# plot(dem)

# create an ocean proximity layer
dem.coarse <- aggregate(dem, fact=16)
# plot(dem.coarse)
data(coastsCoarse)
coastsCoarse <- project(vect(coastsCoarse),dem.coarse) #convert to spatvector
# plot(coastsCoarse,add=TRUE,col='blue')
coastal <- distance(dem.coarse, coastsCoarse) 
coastal <- disagg(coastal, fact=16, method="bilinear") 
# plot(coastal)

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
elements <- c("Tmin", "Tmax", "Pr")
month.seasons <- rep(c("wt", "sp", "sm", "at"), each=3)[c(2:12, 1)]

# load the worldclim data for all elements
dir <- "//objectstore2.nrs.bcgov/ffec/Climatologies/WorldClim/"
# worldclim <- rast(paste(dir, list.files(dir, pattern=paste(".*.","_", monthcodes[m], ".tif", sep="")), sep=""))
files <- list.files(dir, pattern=paste(".*.tif", sep=""))
worldclim <- rast(paste(dir, files, sep=""))[[-1]]
worldclim <- crop(worldclim, dem)
worldclim <- focal(worldclim, w=13, fun="mean", na.policy="only") # add a buffer of approximate values
values(worldclim)[!is.finite(values(worldclim))] <- NA
#reorder the variables in a logical sequence
vars <- paste(rep(c("tmin", "tmax", "prec"), each=12), rep(monthcodes, times=3), sep="_")
file_vars <- sapply(strsplit(files, "_"), function(x) paste(x[3], x[4], sep = "_"))[-1]
file_vars <- sapply(strsplit(file_vars, ".", fixed = TRUE), function(x) x[1])
file_order <- match(vars, file_vars)
worldclim <- worldclim[[file_order]]
names(worldclim) <- vars

#--------------------------
# load the daymet data for the variable
dir <- "//objectstore2.nrs.bcgov/ffec/data_daymet/daymet_climatology_1981_2010/"
files <- list.files(dir, pattern=paste(".*.nc", sep=""))
daymet <- rast(paste(dir, files, sep=""))
daymet <- project(daymet, dem)
daymet <- focal(daymet, w=13, fun="mean", na.policy="only") # add a buffer of approximate values 
values(daymet)[!is.finite(values(daymet))] <- NA
#reorder the variables in a logical sequence
vars <- paste(rep(month.abb, times=3), rep(c("tmin", "tmax", "prcp"), each=12), sep="_")
file_vars <- sapply(strsplit(files, "_"), function(x) paste(x[4], x[5], sep = "_"))
file_order <- match(vars, file_vars)
daymet <- daymet[[file_order]]
names(daymet) <- vars


e <- 3
m <- 2

for(e in 1:length(elements)){
  for(m in 1:length(monthcodes)){
    
    # load the PRISM mosaic data for the variable
    dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_US/PRISM_",c("tmin", "tmax", "ppt")[e] ,"_30yr_normal_1981_2010_800m_all_files_asc/", sep="")
    file <- paste("PRISM_", c("tmin", "tmax", "ppt")[e], "_30yr_normal_1981_2010_800m_", monthcodes[m], "_asc.asc", sep="")
    prism <- rast(paste(dir, file, sep=""))
    prism <- crop(prism, studyarea)
    # if(elements[e]=="Pr") values(prism) <- log2(values(prism))
    values(prism)[!is.finite(values(prism))] <- NA
    # plot(prism)
    
    # subset the daymet component of the predictand. 
    temp <- vect(studyarea+c(0, 0, 0, -7)); crs(temp) <- crs(prism)
    daymet.addarea <- mask(daymet[[((e-1)*12+1:12)[m]]], temp)
    
    # plot(prism)
    # plot(daymet.addarea, add=T)
    
    # compile prism and daymet into a training raster
    clim.training <- cover(prism, daymet.addarea)
    # plot(clim.training)
    
    # extract the training data
    points.prism <- crds(prism, na.rm = T)
    # points.prism <- points.prism[sample(1:dim(points.prism)[1], 2500000),] # about half of the total points
    points.daymet <- crds(daymet.addarea, na.rm = T)
    # points.daymet <- points.daymet[sample(1:dim(points.daymet)[1], 1000000),] # about 10% of total points (because there is very little topographic relief)
    points <- rbind(points.prism, points.daymet)
    predictand.points <- extract(clim.training,points)
    elev.points <- extract(dem,points)
    coastal.points <- extract(coastal,points)
    select <- c(1,4,7,10,13,16,19,22,25,28,31,34) # 1 month representing each season for the 3 variables
    if(!m %in% select) select <- c(select, c(1,13,25)+(m-1)) # add the current month to the variable set
    daymet.points <- extract(daymet[[select]],points)
    worldclim.points <- extract(worldclim[[select]],points)
    data.train <- cbind(predictand.points, elev.points, coastal.points, worldclim.points, daymet.points)
    colnames(data.train)[1:3] <- c("predictand","elev", "coastal")
    data.train <- data.train[complete.cases(data.train),]
    # str(data.train)
    
    # train the model
    model <- ranger(y=data.train[,1], x=data.train[,-1], num.trees=300, replace = F, sample.fraction = 0.25, oob.error=F)
    # model <- ranger(y=data.train[,1], x=data.train[,-1], oob.error=F)
    
    # points for unmapped area to be filled
    dem.fillarea <- dem
    dem.fillarea[!is.na(cover(prism, daymet.addarea))] <- NA
    # plot(dem.fillarea)
    points.fillarea <- crds(dem.fillarea, na.rm = F)
    elev.fillarea <- extract(dem.fillarea,points.fillarea)
    daymet.fillarea <- extract(daymet[[select]],points.fillarea)
    worldclim.fillarea <- extract(worldclim,points.fillarea)
    coastal.fillarea <- extract(coastal,points.fillarea)
    data.fillarea <- cbind(points.fillarea,elev.fillarea, coastal.fillarea, worldclim.fillarea, daymet.fillarea)
    colnames(data.fillarea)[1:4] <- c("x","y","elev", "coastal")
    data.fillarea$id <- seq_along(data.fillarea$x)
    data.fillarea <- data.fillarea[complete.cases(data.fillarea),]
    # str(data.fillarea)
    
    # predict to the unmapped area
    pred.fillarea <- predict(model, data = data.fillarea)
    rm(model)
    
    # compile the composite raster
    clim.pred.fillarea <- dem.fillarea
    values(clim.pred.fillarea) <- NA
    clim.pred.fillarea[data.fillarea$id] <- pred.fillarea$predictions
    clim.pred <- cover(prism, clim.pred.fillarea)
    clim.pred <- cover(clim.pred, daymet.addarea)
    # plot(clim.pred)
    
    # write the raster
    writeRaster(clim.pred, paste("//objectstore2.nrs.bcgov/ffec/Climatologies/composite_nMex/composite_nMex_1981_2010_", elements[e], monthcodes[m], ".tif", sep=""), overwrite=T)
    
    print(monthcodes[m])  
  }
  print(elements[e])
}

#---------------------------------
# View results

# rasters for the variable of interest
wc <- worldclim[[((e-1)*12+1:12)[m]]]
dm <- daymet[[((e-1)*12+1:12)[m]]]
us <- prism
cp <- clim.pred

# Color Scheme
combined <- c(values(us), values(dm))
if(elements[e]=="Pr") combined <- log2(combined)
combined <- combined[is.finite(combined)]
inc=diff(range(combined))/500
breaks=seq(quantile(combined, 0.005)-inc, quantile(combined, 0.995)+inc, inc)
ColScheme <- colorRampPalette(if(elements[e]=="Pr") brewer.pal(9, "YlGnBu") else rev(brewer.pal(11, "RdYlBu")))(length(breaks)-1)
ColPal <- colorBin(ColScheme, bins=breaks, na.color = "white")
ColPal.raster <- colorBin(ColScheme, bins=breaks, na.color = "transparent")

if(elements[e]=="Pr"){
  values(us) <- log2(values(us))
  values(cp) <- log2(values(cp))
  values(wc) <- log2(values(wc))
  values(dm) <- log2(values(dm))
} 

# leaflet map
map <- leaflet() %>%
  addTiles(group = "basemap") %>%
  addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
  # addRasterImage(dem, colors =terrain.colors(99), opacity = 1, maxBytes = 6 * 1024 * 1024, group = "elevation") %>%
  addRasterImage(us, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "PRISM") %>%
  addRasterImage(cp, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Predicted") %>%
  addRasterImage(wc, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "WorldClim") %>%
  addRasterImage(dm, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "Daymet") %>%
  addLayersControl(
    baseGroups = c("basemap", "sat photo"),
    overlayGroups = c("Daymet", "WorldClim", "Predicted", "PRISM"),
    # overlayGroups = c("Daymet", "Predicted", "PRISM"),
    options = layersControlOptions(collapsed = FALSE)
  )
map


