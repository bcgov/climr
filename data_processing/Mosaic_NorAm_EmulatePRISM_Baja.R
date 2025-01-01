
# creation of a composite climatology (mosaic) for north america, from PRISM and worldclim
# use ML to create a synthetic "prism-like" climatology outside the PRISM area. 
# Colin Mahony 
# Jan 1, 2024

library(terra)
library(data.table)
library(leaflet)
library(RColorBrewer)
library(ranger)
library(rworldmap)

studyarea <- ext(c(-120, -106, 22, 35))

extend.coastal <- function(x){
  x <- focal(x, w=3, fun="mean", na.policy="only") 
  x <- focal(x, w=5, fun="mean", na.policy="only") 
  x <- focal(x, w=7, fun="mean", na.policy="only") 
  x <- focal(x, w=9, fun="mean", na.policy="only") 
  values(x)[!is.finite(values(x))] <- NA
  return(x)
}

# function for creating a weight surface for blending climatologies together
weightSurface <- function(lat, lon, ext){ # ext is c(lon.start, lon.end, lat.start, lat.end)
  weights.lat <- (lat-ext[4])/(ext[3]-ext[4])
  weights.lat[weights.lat>1] <- 1
  weights.lat[weights.lat<0] <- 0
  weights.lon <- (lon-ext[1])/(ext[2]-ext[1])
  weights.lon[weights.lon>1] <- 1
  weights.lon[weights.lon<0] <- 0
  weights <- weights.lat-weights.lon
  weights[weights>1] <- 1
  weights[weights<0] <- 0
  return(weights)
}

# lat and lon rasters
lon <- init(dem, 'x')
lat <- init(dem, 'y')

# blending block 
weights <- weightSurface(lat, lon, c(-100, -100, 28, 30))
plot(weights)
plot(clim.pred, add=T)

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
worldclim <- crop(worldclim, ext(dem)+c(1,1,1,1))
worldclim <- project(worldclim, dem)
values(worldclim)[!is.finite(values(worldclim))] <- NA
#reorder the variables in a logical sequence
vars <- paste(rep(c("tmin", "tmax", "prec"), each=12), rep(monthcodes, times=3), sep="_")
file_vars <- sapply(strsplit(files, "_"), function(x) paste(x[3], x[4], sep = "_"))[-1]
file_vars <- sapply(strsplit(file_vars, ".", fixed = TRUE), function(x) x[1])
file_order <- match(vars, file_vars)
worldclim <- worldclim[[file_order]]
names(worldclim) <- vars

e <- 3
m <- 12

for(e in 1:length(elements)){
  for(m in 1:length(monthcodes)){
    
    # load the PRISM data for the variable
    dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_US/PRISM_",c("tmin", "tmax", "ppt")[e] ,"_30yr_normal_1981_2010_800m_all_files_asc/", sep="")
    file <- paste("PRISM_", c("tmin", "tmax", "ppt")[e], "_30yr_normal_1981_2010_800m_", monthcodes[m], "_asc.asc", sep="")
    prism <- rast(paste(dir, file, sep=""))
    prism <- project(prism, dem)
    # if(elements[e]=="Pr") values(prism) <- log2(values(prism))
    values(prism)[!is.finite(values(prism))] <- NA
    # plot(prism)
    
    # subset the worldclim component of the predictand. 
    temp <- vect(studyarea+c(0, 0, 0, 0)); crs(temp) <- crs(prism)
    worldclim.addarea <- mask(worldclim[[((e-1)*12+1:12)[m]]], temp)
    
    # plot(prism)
    # plot(worldclim.addarea, add=T)
    
    # compile prism and worldclim into a training raster
    # clim.training <- cover(prism, worldclim.addarea)
    clim.training <- prism
    # plot(clim.training)
    
    # extract the training data
    points.prism <- crds(prism, na.rm = T)
    # points.prism <- points.prism[sample(1:dim(points.prism)[1], 2500000),] # about half of the total points
    points.worldclim <- crds(worldclim.addarea, na.rm = T)
    # points.worldclim <- points.worldclim[sample(1:dim(points.worldclim)[1], 1000000),] # about 10% of total points (because there is very little topographic relief)
    # points <- rbind(points.prism, points.worldclim)
    points <- points.prism
    predictand.points <- extract(clim.training,points)
    elev.points <- extract(dem,points)
    coastal.points <- extract(coastal,points)
    select <- c(c(1,13,25)+rep(c(12, 1:12)[m+c(-1,0,1)], each=3)) # select the current and adjacent months
    # select <- c(c(1,13,25)+(m-1)) # select the current and adjacent months
    worldclim.points <- extract(worldclim[[select]],points)
    data.train <- cbind(predictand.points, points, elev.points, worldclim.points)
    colnames(data.train)[1:4] <- c("predictand","x","y","elev")
    data.train <- data.train[complete.cases(data.train),]
    # str(data.train)
    
    # train the model
    model <- ranger(y=data.train[,1], x=data.train[,-1], num.trees=300, replace = F, sample.fraction = 0.25, oob.error=F)
    # model <- ranger(y=data.train[,1], x=data.train[,-1], oob.error=F)
    
    # points for unmapped area to be filled
    dem.fillarea <- dem
    points.fillarea <- crds(dem.fillarea, na.rm = F)
    elev.fillarea <- extract(dem.fillarea,points.fillarea)
    worldclim.fillarea <- extract(worldclim,points.fillarea)
    coastal.fillarea <- extract(coastal,points.fillarea)
    data.fillarea <- cbind(points.fillarea,elev.fillarea, worldclim.fillarea)
    colnames(data.fillarea)[1:3] <- c("x","y","elev")
    data.fillarea$id <- seq_along(data.fillarea$x)
    data.fillarea <- data.fillarea[complete.cases(data.fillarea),]

    # predict to the unmapped area
    pred.fillarea <- predict(model, data = data.fillarea)
    # rm(model)
    
    # compile the composite raster
    clim.pred.fillarea <- dem.fillarea
    values(clim.pred.fillarea) <- NA
    clim.pred.fillarea[data.fillarea$id] <- pred.fillarea$predictions
    clim.pred <- cover(prism, clim.pred.fillarea)
    clim.pred <- mosaic(clim.pred*(1-weights), worldclim.addarea*weights, fun="sum")
    
    # write the raster
    writeRaster(clim.pred, paste("outputs/composite_baja_1981_2010_", elements[e], monthcodes[m], ".tif", sep=""), overwrite=T)
    
    print(monthcodes[m])  
  }
  print(elements[e])
}

#---------------------------------
# View results

# rasters for the variable of interest
wc <- worldclim[[((e-1)*12+1:12)[m]]]
us <- prism
cp <- clim.pred

# Color Scheme
combined <- c(values(us), values(wc))
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
} 

# leaflet map
map <- leaflet() %>%
  addTiles(group = "basemap") %>%
  addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
  # addRasterImage(dem, colors =terrain.colors(99), opacity = 1, maxBytes = 6 * 1024 * 1024, group = "elevation") %>%
  addRasterImage(us, colors = ColPal.raster, opacity = 1, maxBytes = 8 * 1024 * 1024, group = "PRISM") %>%
  addRasterImage(cp, colors = ColPal.raster, opacity = 1, maxBytes = 8 * 1024 * 1024, group = "Predicted") %>%
  addRasterImage(wc, colors = ColPal.raster, opacity = 1, maxBytes = 8 * 1024 * 1024, group = "WorldClim") %>%
  addLayersControl(
    baseGroups = c("basemap", "sat photo"),
    overlayGroups = c("WorldClim", "Predicted", "PRISM"),
    options = layersControlOptions(collapsed = FALSE)
  )
map


