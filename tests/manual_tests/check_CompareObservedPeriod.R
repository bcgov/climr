## Manual tests: compare climr observed period to time series and source data

library(climr)
library(sf)
library(maps)
library(terra)
library(data.table)

##make study area dem
dem_source <- rast("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/NA_Elevation/data/northamerica/northamerica_elevation_cec_2023.tif") ##DEM - I'm using a 30 m one

# Get the boundary of washington
map <- map("state", "Washington", plot = FALSE, fill = TRUE)
bnd <- vect(st_as_sf(map))
bnd <- project(bnd,"epsg:4326") # project to albers to be able to specify resolution in meters. 
dem <- rast(bnd,res = 0.5) ## ENHANCEMENT NEEDED: CHANGE HARD-CODED RESOLUTION TO DYNAMIC RESOLUTION MATCHING USER-SPECIFIED NUMBER OF CELLS

dem <- project(dem_source,dem, method="near") ## extract 30m dem values to the custom raster. use nearest neighbour to preserve elevation variance. 
dem <- mask(dem,bnd)

## make the climr input file
points_dat <- as.data.frame(dem, cells=T, xy=T)
colnames(points_dat) <- c("id", "lon", "lat", "elev")
points_dat <- points_dat[points_dat$lon > -120,] # select Eastern washington
# points_dat <- points_dat[points_dat$lat > 38,] # select Northern California
points_dat <- points_dat[,c(2,3,4,1)] #restructure for climr input

xvar = "Tmin_sm"
yvar = "PPT_sm"
# percent_x = NULL, TODO: set up an override for ratio variables being expressed as percent anomalies
# percent_y = NULL, TODO: set up an override for ratio variables being expressed as percent anomalies
period_focal = list_gcm_periods()[1]
gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)]
ssp = list_ssps()[2]
obs_period = list_obs_periods()[1]
gcm_periods = list_gcm_periods()
max_run = 10
legend_pos = "bottomleft"
show_runs = TRUE
show_ensMean = TRUE
show_observed = TRUE
show_trajectories = TRUE
interactive = FALSE
cache = TRUE


# variable types for default scaling (percent or absolute)
xvar_type <- variables$Type[which(variables$Code == xvar)]
yvar_type <- variables$Type[which(variables$Code == yvar)]

colors <- c("#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6", "#6A3D9A", "#1e90ff", "#B15928", "#FFFF99")
ColScheme <- colors[1:length(gcms)]

# generate the climate data
data <- downscale(xyz,
                  obs_periods = obs_period,
                  obs_ts_dataset = c("climatena", "cru.gpcc"),
                  obs_years = 2001:2020,
                  vars = c(xvar, yvar),
                  cache = cache
)

# convert absolute values to anomalies
data[, xanom := if (xvar_type == "ratio") (get(xvar) / get(xvar)[1] - 1) else (get(xvar) - get(xvar)[1]), by = id]
data[, yanom := if (yvar_type == "ratio") (get(yvar) / get(yvar)[1] - 1) else (get(yvar) - get(yvar)[1]), by = id]

# mean of observed period anomalies (spatial variation in average)
mean(data[PERIOD == obs_period, xanom])
# mean of observed ts anomalies
mean(data[DATASET=="cru.gpcc" & PERIOD %in% 2001:2020, xanom], na.rm=T)
mean(data[DATASET=="climatena" & PERIOD %in% 2001:2020, xanom], na.rm=T)
# the observed anomalies and cru ts should have the same mean, but they don't. this holds for tave, tmax, and tmin, in multiple months


#-----------------------------
# Raw CRU data
#-----------------------------

monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
elements.cru <- c("tmn", "tmx", "pre", "tmp")
elements <- c("Tmin", "Tmax", "PPT", "Tave")

e=1
m=7
element <- elements.cru[e]
# dir <- "//objectstore2.nrs.bcgov/ffec/TimeSeries_gridded_monthly/cru_ts4.07" #takes way too long to read from object storage
dir <- "C:/Users/CMAHONY/OneDrive - Government of BC/Data/cru_ts4.08"
files <- list.files(dir, pattern=".nc$")
cru <- rast(paste(dir, files[grep(element, files)], sep="/"))

cru <- crop(cru, bnd)
cru <- mask(cru, bnd)
# plot(cru[[1]])

# reduce to selected month
temp <- cru[[which(substr(time(cru),6,7)==monthcodes[m])]]
names(temp) <- substr(time(temp),1,4)

# check the CRU anomaly for a single location
point <- vect(matrix(c(-120, 47), 1))
plot(point, add=T)
ts <- extract(temp, point, id=F)
ts.x <- names(ts)[2:123]
ts.y <- as.numeric(ts[2:123])
ts.n <- as.numeric(ts[124:length(ts)])
ref.cru <- mean(ts.y[ts.x%in%1961:1990])
curr.cru <- mean(ts.y[ts.x%in%2001:2020])
anom.cru <- curr.cru - ref.cru

# check the CRU anomaly for WA state
ts <- subset(temp, 1:123)
ref.cru <- mean(values(subset(ts, as.character(1961:1990))), na.rm=T)
curr.cru <- mean(values(subset(ts, as.character(2001:2020))), na.rm=T)
anom.cru <- curr.cru - ref.cru

#-----------------------------
# climr transfer anomalies
#-----------------------------

element <- elements[e]
dir <- "//objectstore2.nrs.bcgov/ffec/TransferAnomalies" 
files <- list.files(dir, pattern=".tif")
r <- rast(paste(dir, "delta.from.1961_1990.to.2001_2010.Tmin.tif", sep="/"))[[m]]
plot(r)
mean(values(r))

#-----------------------------
# climr input raster
#-----------------------------

thebb <- get_bb(points_dat)
dbCon <- data_connect()
r <- input_obs(dbCon, bbox=thebb)[[1]]
r <- subset(r, paste(elements[e], monthcodes[m], sep="_"))
plot(r)

#-----------------------------
# climr output
#-----------------------------


dem.na <- aggregate(dem_source, fact=10) 
dem.na <- project(dem.na,"epsg:4326") # project to albers to be able to specify resolution in meters. 
dem.na <- aggregate(dem.na, fact=10) 
dem.save <- dem.na
dem.na <- crop(dem.na, ext(c(-170, -50, 14, 89)))
plot(dem.na)

grid.na <- as.data.frame(dem.na, cells=T, xy=T)
colnames(grid.na) <- c("id", "lon", "lat", "elev")
grid.na <- grid.na[,c(2,3,4,1)] #restructure for climr input

var <- paste(elements[e], monthcodes[m], sep="_")

data.na <- downscale(grid.na,
                     obs_periods = obs_period,
                     obs_ts_dataset = c("climatena", "cru.gpcc"),
                     obs_years = 2001:2020,
                     vars = var,
                     cache = cache
)

anom <- data.na[PERIOD=="2001_2020", get(var)] - data.na[PERIOD=="1961_1990", get(var)]

X <- dem.na
X[grid.na$id] <- anom
plot(X)

# convert absolute values to anomalies
data.na[, xanom := (get(var) - get(var)[1]), by = id]

anom.ts.mean <- data.na[DATASET=="cru.gpcc", .(mean_value = mean(xanom)), by = id][,2]
X[grid.na$id] <- anom.ts.mean
plot(X)

# mean of observed period anomalies (spatial variation in average)
mean(data.na[PERIOD == obs_period, xanom])
# mean of observed ts anomalies
mean(data.na[DATASET=="cru.gpcc" & PERIOD %in% 2001:2020, xanom], na.rm=T)
mean(data.na[DATASET=="climatena" & PERIOD %in% 2001:2020, xanom], na.rm=T)
# the observed anomalies and cru ts should have the same mean, but they don't. this holds for tave, tmax, and tmin, in multiple months
