library(data.table)
library(terra)
library(climr)


library(terra)
library(data.table)
library(climr)
library(sf)

## get the sample digital elevation model (dem) provided with `climr`
dem <- get(data("dem_vancouver")) |> terra::unwrap()
## convert the DEM to a data.frame
grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
## rename column names to what climr expects
colnames(grid) <- c("id", "lon", "lat", "elev")
## A simple climr query.
## This will return the observed 1961-1990 normals for the raster grid points.
var <- "MAP"
ds_out <- downscale(grid, which_refmap = "refmap_climr", vars = var)
clim <- terra::rast(dem) # use the DEM as a template raster
## populate the raster cells with the 2001-2020 annual precipitation (MAP) values,
## using the `id` field as the link.
clim[ds_out[, id]] <- ds_out[, var, with = FALSE]
## log-transform precipitation for more meaningful scaling
clim <- log2(clim)
## increment for the ramp
inc=diff(range(terra::values(clim)))/500
## color breaks
breaks=seq(min(terra::values(clim))-inc, max(terra::values(clim))+inc, inc)
## color scheme
ColScheme <- rev(hcl.colors(length(breaks)-1, "GnBu"))
terra::plot(clim, col=ColScheme, breaks=breaks, legend=FALSE, main="", mar=NA)
legend_ramp(
  clim,
  title = paste(var, "(mm)"),
  ColScheme = ColScheme,
  breaks = breaks,
  pos=c(0.05, 0.45, 0.1, 0.125),
  log = 2,
  horizontal = TRUE
)

## weather station locations
weather_stations <- get(data("weather_stations")) |>
  unwrap()

## study area of interest (North Vancouver)
vancouver_poly <- get(data("vancouver_poly")) |>
  unwrap()

## subset to points in study area
weather_stations <- mask(weather_stations, vancouver_poly)

## convert to data.table and subset/rename columns needed by climr
xyzDT <- as.data.table(weather_stations, geom = "XY")
cols <- c("Station ID", "x", "y", "Elevation (m)")
xyzDT <- xyzDT[, ..cols]
setnames(xyzDT, c("id", "lon", "lat", "elev"))

## join BEC zones and colours
BECz_vancouver <- get(data("BECz_vancouver")) |>
  unwrap()

BECz_points <- extract(BECz_vancouver, weather_stations) |>
  as.data.table()
BECz_points <- BECz_points[, .(ZONE, HEX)]

xyzDT <- cbind(xyzDT, BECz_points)

## remove duplicates
xyzDT <- unique(xyzDT)

## there are some duplicate stations with slightly different
## coordinates. We'll take the first
xyzDT <- xyzDT[!duplicated(id)]

ds_out_ts <- downscale(
  xyz = xyzDT,
  obs_years = 2001:2023,
  gcm_hist_years = 2001:2014,
  gcm_ssp_years = 2015:2040,
  gcms = list_gcms()[1],
  ssps = "ssp245",
  max_run = 0,
  vars = c("MAT", "PPT_an", "PAS_an")
)


bgc <- st_read("../Common_Files/BEC13Draft_Simplified.gpkg")
BG <- bgc[grep("^BG.*", bgc$BGC),]
ar <- sum(st_area(BG))

pre_cache(region = "BC", gcms = list_gcms()[1:3], ssps = list_ssps()[2], gcm_periods = list_gcm_periods())



list.files(file.path(cache_path(),"reference","refmap_climr"))
tmp <- rast(file.path(cache_path(),"reference","refmap_climr","fc34492b-0013-4e3a-b293-c5e78a1a54b6.tif"))
wlrdem <- rast(file.path(cache_path(),"reference","refmap_climr","climr_mosaic_wlrdem.tif"))
names(wlrdem) <- names(tmp)
writeRaster(wlrdem,file.path(cache_path(),"reference","refmap_climr","climr_mosaic_wlrdem2.tif"), overwrite = T)

rorig <- get(data("dem_vancouver")) |> unwrap()
rtile <- makeTiles(rorig, 150)
rvrt <- vrt(rtile)

dat <- downscale(rvrt, return_refperiod = TRUE, vars = c("DD5_an"))

# Download the GeoTIFF
download.file(
  url = "https://github.com/bcgov/climr/raw/devl/data-raw/dem_mosaic/climr_mosaic_dem_800m.tif",
  destfile = "climr_dem_800.tif",
  mode = "wb"  # write as binary!
)

# Load as SpatRaster
dem <- rast("climr_dem_800.tif")
wrf_ext <- ext(-147,-108,43,67)
d2 <- crop(dem, wrf_ext)
plot(d2)
writeCDF(d2, "WRF_Region.nc")

# define an extent
# e <- ext(-120.93, -119.73, 50.22, 51.12)
e <- ext(-179.054166666518, -52.0041666680861, 13.9958333334196, 83.4958333325618)
e2 <- ext(-179.0625 + 1, -51.5625 -1, 14.375 + 1, 83.125 -1)
dem_wgs <- project(dem, "EPSG:4326")

dem_cropped <- crop(dem_wgs, e) |> crop(e2)
tmp <- makeTiles(dem_cropped, 1500, filename = "temp_rast/tile_.tif")
demvrt <- vrt(tmp)
#demvrt <- crop(demvrt, e)
plot(dem_cropped)

tmp <- rast("temp_rast/tile_24.tif")

data <- downscale(
  xyz = dem_cropped,
  return_refperiod = TRUE,
  vars = c("DD5_an","DDsub18_an")
)

plot(data[[33]])

tmp <- as.data.frame(dem_cropped, xy = TRUE, cells = TRUE) ##convert to data.frame
names(tmp) <- c("id","lon","lat","elev")
setDT(tmp)
tmp <- tmp[elev > 0,]



bnds <- vect("../CCISS_ShinyApp/app/cciss_spatial/flp_bnds.gpkg")
lrp <- bnds[bnds$ORG_UNIT == "Lakes Resiliency Project",]
dem <- rast("../CCISS_ShinyApp/BC_DEM_100m.tif")
lrpdem <- crop(dem, lrp)
lrpdem <- aggregate(lrpdem, fact = 2)
lrpdem <- mask(lrpdem, lrp)

tmp <- as.data.frame(lrpdem, xy = TRUE, cells = TRUE)
names(tmp) <- c("id","lon","lat","elev")
lrp_ds <- downscale(tmp, 
                    gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
                    ssps = list_ssps()[-4],
                    gcm_ssp_years = c(2035, 2080),
                    vars = "DD18_an",
                    return_refperiod = FALSE,
                    db_option = "local")

lrp_summary <- lrp_ds[,.(DD18_mean = mean(DD18_an)), by = .(id,PERIOD)]
dd18_2035 <- copy(lrpdem)
values(dd18_2035) <- NA
dd18_2035[lrp_summary[PERIOD == 2035,id]] <- lrp_summary[PERIOD == 2035,DD18_mean]
plot(dd18_2035)


dat <- fread("./bivariate_data 1.csv")
dat[,V1 := NULL]
dat[,id := 1]
plot_bivariate(dat, xvar = "MAT", yvar = "MAP", interactive = FALSE)

plot_timeSeries(dat, var1 = "Tmax_an")

dat <- fread("../../../Downloads/bivariate_data.csv")
plot_bivariate(dat, xvar = "MAT", yvar = "MAP")

in_xyz <- data.frame(
  lon = c(-127.7052, -127.6227, -127.5623, -127.7162, -127.1858, -127.125, -126.9495, -126.9550),
  lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
  elev = c(291, 296, 626, 377, 424, 591, 723, 633),
  id = 1:8
)

ds_out <- downscale(xyz = in_xyz, obs_periods = list_obs_periods(), gcms = list_gcms()[1], ssps = "ssp245", gcm_periods = "2041_2060",
                    vars = c("Tmax_05", "Tmin_05","Tmax_06", "Tmin_06","Tmax_07", "Tmin_07","Tmax_08", "Tmin_08","CMI_05", "CMI_06","CMI_07","CMI_08", "CMD_05", "CMD_06","CMD_07","CMD_08", "PPT_05", "PPT_06","PPT_07","PPT_08" )) 


dat <- plot_bivariate_input(in_xyz,
                            obs_period = "2001_2020",
                            gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)], #
                            ssps = list_ssps()[c(2)],
                            gcm_periods = list_gcm_periods(),
                            max_run = 10)

plot_bivariate(dat, xvar = "MAT", yvar = "MAP")
data <- downscale(
  xyz = in_xyz,
  obs_period = "2001_2020",
  gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)], #
  ssps = list_ssps()[c(4)],
  gcm_periods = list_gcm_periods(),
  max_run = 10,
  vars = list_vars(),
  db_option = "database")

data <- na.omit(data)
plot_bivariate(data, xvar = "AHM", yvar = "CMD_an", interactive = TRUE)

dem_vancouver <- get(data("dem_vancouver")) |> unwrap() 
cache_clear("gcms")
ds_out <- downscale(xyz = dem_vancouver,
                    return_refperiod = TRUE)

dem_vancouver <- get(data("dem_vancouver")) |> unwrap() 
cache_clear("gcms")
ds_out <- downscale( xyz = dem_vancouver, obs_periods = NULL, gcms = list_gcms()[1], 
                     ssps = list_ssps()[1], gcm_periods = "2001_2020", vars = c("AHM") ) 

ds_out <- downscale( xyz = dem_vancouver,  obs_periods = "2001_2020",  gcms = list_gcms()[1], ssps = list_ssps()[2],
                     gcm_periods = "2041_2060", vars = c("FFP") )


dem_vancouver <- get(data("dem_vancouver")) |> unwrap() 
ds_out <- downscale( xyz = dem_vancouver, obs_periods = NULL, gcms = list_gcms()[1], ssps = list_ssps()[1], gcm_periods = "2001_2020", vars = c("AHM") ) 
ds_out <- downscale( xyz = dem_vancouver, obs_periods = NULL, gcms = list_gcms()[7], ssps = list_ssps()[1], gcm_periods = "2001_2020", vars = c("AHM") ) 

xyz <- data.table(sg_id = 1, id = 9999, lon = -125.9912109375, lat = 54.901882187385, elev = 0)
dat <- downscale(xyz = xyz, which_refmap = "refmap_climr", obs_periods = "2001_2020", 
                 obs_years = NULL, obs_ts_dataset = NULL, return_refperiod = FALSE, 
                 gcms = NULL, ssps = NULL, gcm_periods = NULL, gcm_ssp_years = NULL, 
                 gcm_hist_years = NULL, max_run = 0, run_nm = NULL, vars = NULL, ppt_lr = FALSE)
dat <- climr::downscale(xyz = in_xyz, which_refmap = "refmap_climr", obs_periods = "2001_2020", obs_years = NULL, obs_ts_dataset = NULL, return_refperiod = FALSE, gcms = NULL, ssps = NULL, gcm_periods = NULL, gcm_ssp_years = NULL, gcm_hist_years = NULL, max_run = 0, run_nm = NULL, ppt_lr = FALSE)

dbCon <- data_connect()
in_xyz <- data.frame(
   lon = c(-127.7052, -127.6227, -127.5623, -127.7162, -127.1858, -127.125, -126.9495, -126.9550),
   lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
   elev = c(291, 296, 626, 377, 424, 591, 723, 633),
   id = 1:8
  )

dat <- downscale(in_xyz[1:3,], which_refmap = "refmap_climr", return_refperiod = TRUE)

dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/PRISM_dem/", sep="")
dem.bc <- rast("../Common_Files/PRISM_dem.tiff")

my_grid <- as.data.frame(dem.bc, cells = TRUE, xy = TRUE)
colnames(my_grid) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects

varsl = c("DD5", "DDsub0_at", "DDsub0_wt", "PPT_05", "PPT_06", "PPT_07", "PPT_08",
          "PPT_09", "CMD", "PPT_at", "PPT_wt", "CMD_07", "SHM", "AHM", "NFFD", "PAS",
          "CMI", "Tmax_sm", "TD", "PPT_sm", "DD5_sp",
          "PPT_sp", "PPT_sm", "Tmax_at", "Tmax_wt", "Tmax_sp", "Tmax_sm","Tmin_at", "Tmin_wt", "Tmin_sp", "Tmin_sm")
varsl<-unique(varsl)#make sure no dups


clim.bcv <- downscale(
  xyz = my_grid[0:100000,],
  which_refmap = "refmap_climr",
  vars = varsl)

rtemp <- unwrap(dem_vancouver)

clim_out <- downscale(rtemp, gcms = list_gcms()[4], ssps = "ssp245", gcm_periods = "2041_2060", vars = c("MAP","MAT"))

vars_reg <- sort(sprintf(c("PPT_%02d", "Tmax_%02d", "Tmin_%02d"), sort(rep(1:12, 3))))
temp <- climr::downscale(xyz = rtemp, which_refmap = "refmap_climr", 
                        obs_periods = "2001_2020",  obs_years = 2024, 
                         obs_ts_dataset = "mswx.blend", return_refperiod = FALSE,
                         gcms = NULL, ssps = NULL, gcm_periods = NULL, 
                         gcm_ssp_years = NULL, gcm_hist_years = NULL, 
                         max_run = 0, run_nm = NULL, vars = vars_reg, ppt_lr = FALSE)

clim <- downscale(in_xyz, 
                  which_refmap = "refmap_climr", 
                  obs_periods = "2001_2020",
                  obs_years = 1901:2020,
                  obs_ts_dataset = c("cru.gpcc", "mswx.blend"),
                  db_option = "database")

test <- downscale(in_xyz[1:4,], obs_years = 2001:2023, obs_ts_dataset = c("mswx.blend","cru.gpcc"), 
                  return_refperiod = FALSE, vars = c("PPT_04","Tmin_12","Tmax_08")) 
ts.climateNA <- downscale(in_xyz, which_refmap = "refmap_climr", obs_years = 1901:2023, obs_ts_dataset = "cru.gpcc", return_refperiod=F)

t1 <- test[id == 1,]
library(ggplot2)
ggplot(t1, aes(x = PERIOD, y = PPT_04, colour = DATASET, group = DATASET)) +
  geom_line()

#low res dem for north america
dem <- rast("//objectstore2.nrs.bcgov/ffec/DEM/DEM_NorAm/dem_noram_lowres.tif")

my_grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
colnames(my_grid) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects
climr <- downscale(xyz = my_grid, which_refmap = "refmap_climr", 
                   obs_periods = "2001_2020", vars = c("Tmin_01","Tmin_07"))

thebb <- get_bb(my_grid)

dbCon <- data_connect()

refmap <- input_refmap(dbCon, thebb, reference = "refmap_climr")

var="Tmin_01"
par(mfrow=c(1,2), mar=c(0,0,2,2))
values(X) <- NA

plot(refmap[[names(refmap)==var]], main=paste("climr refmap", var), axes=F)

data_climr <-   climr[,get(var)]
X[climr[, id]] <- data_climr
plot(X, main=paste("climr downscaled", var), axes=F)
s <- which(data_climr>100)
points(my_grid$lon[s], my_grid$lat[s])

## climr data
climr <- downscale(xyz = my_grid, which_refmap = "refmap_climatena", vars = list_vars())



# Get lat, long, and elevation for 7 locations in BC
test_pts<-data.frame(id = seq(1,6,by=1),
                     lon = c(-120.1879,-120.4258,-121.9251,-120.3030,-127.5062,-127.6785),
                     lat = c(59.3396, 57.4644, 59.9900, 55.2420, 54.0191, 54.1638),
                     elev = c(441.9092,901.2709,461.7851,926.7590,1098.2932,1022.2858)
)

clim_vars <- climr::downscale(test_pts,  
                              vars = list_vars(), 
                              return_refperiod = TRUE, 
                              obs_periods = c("2001_2020"),
                              obs_years = 2022)

clim_vars <- climr::downscale(test_pts,  
                              vars = list_vars(), 
                              return_refperiod = TRUE, 
                              obs_periods = c("2001_2020"),
                              obs_years = 2022, 
                              obs_ts_dataset = "cru.gpcc",
                              db_option = "database")

plot_bivariate(test_pts[1,])

ds_out <- downscale_db(xyz = test_pts, 
                       which_refmap = "refmap_climr",
                       gcm_hist_years = 1960:2014,
                       gcm_ssp_years = 2015:2100,
                       ssps = c("ssp245"), 
                       max_run = 1,
                       gcms = list_gcms()[4],
                       vars = c("Tmax_sm", "PPT_sm"))

ds_out <- downscale(
  xyz = test_pts,
  which_refmap = "refmap_climr",
  gcm_hist_years = 1960:2014,
  gcm_ssp_years = 2015:2100,
  ssps = c("ssp245"),
  max_run = 3,
  gcms = list_gcms()[3:4],
  vars = c("Tmax_sm", "PPT_sm"),
  db_option = "auto")

ds_out <- downscale(
  xyz = test_pts,
  gcms = list_gcms()[1:4],
  ssps = "ssp245",
  gcm_periods = list_gcm_periods(),
  db_option = "local")

ds_out <- downscale(
  xyz = test_pts,
  which_refmap = "refmap_climr",
  gcms = list_gcms()[1:4],
  ssps = "ssp245",
  gcm_periods = list_gcm_periods(),
  db_option = "database")

ds_out <- downscale(
  xyz = test_pts,
  which_refmap = "refmap_climr",
  gcms = list_gcms()[1:4],
  ssps = "ssp245",
  gcm_periods = list_gcm_periods(),
  obs_years = 1951:2010,
  obs_ts_dataset = "cru.gpcc",
  db_option = "auto")

ds_out2<-ds_out[!is.na(GCM),]

par(mfrow=c(3,2))
for(i in 1:3){
  x <- ds_out2[GCM=="CNRM-ESM2-1" & id == i & RUN == "r1i1p1f2", PERIOD]
  y <- ds_out2[GCM=="CNRM-ESM2-1" & id == i & RUN == "r1i1p1f2", Tmax_sm]
  plot(x,y, type="l", main="CNRM-ESM2-1")
  
  x <- ds_out2[GCM=="CanESM5" & id == i & RUN == "r2i1p1f1", PERIOD]
  y <- ds_out2[GCM=="CanESM5" & id == i & RUN == "r2i1p1f1", Tmax_sm]
  plot(x,y, type="l", main="CanESM5")
}

par(mfrow=c(2,2))
for(i in unique(ds_out2[GCM=="CNRM-ESM2-1", RUN])){
  x <- ds_out2[GCM=="CNRM-ESM2-1" & id == 1 & RUN == i, PERIOD]
  y <- ds_out2[GCM=="CNRM-ESM2-1" & id == 1 & RUN == i, Tmax_sm]
  plot(x,y, type="l", main=i)
  
}

my_points <- data.frame(
  lon = c(-127.7300, -127.7500),
  lat = c(55.34114, 55.25),
  elev = c(711, 500),
  id = 1:2
)

bb <- get_bb(my_points)
dbCon <- data_connect()
res <- input_refmap(dbCon, bb)
res2 <- input_refmap(dbCon, bb, reference = "refmap_climatena")


rf <- "normal_na"
    q <- "
        SELECT min(ST_UpperLeftX(rast)) xmin,
               max(ST_UpperLeftX(rast)+ST_Width(rast)*ST_PixelWidth(rast)) xmax,
               min(ST_UpperLeftY(rast)-ST_Height(rast)*abs(ST_PixelHeight(rast))) ymin,
               max(ST_UpperLeftY(rast)) ymax
        FROM %s" |> sprintf(rf)

tst <- climr:::db_safe_query(q)    

plot(res2$Tmax_07)
# draw the plot
plot_bivariate(my_points)

my_data <- plot_timeSeries_input(my_points)
plot_timeSeries(my_data, var1 = "DD5")

in_xyz <- data.frame(
     lon = c(-127.7052, -127.6227, -127.5623, -127.7162, -127.1858, -127.125, -126.9495, -126.9550),
     lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
     elev = c(291, 296, 626, 377, 424, 591, 723, 633),
     id = 1:8
   )
 clim3 <- downscale(xyz = in_xyz, 
                   which_refmap = "refmap_climr", 
                    gcms = list_gcms()[1], 
                    ssps = list_ssps()[2],
                    gcm_periods = list_gcm_periods()[3], 
                    gcm_ssp_years = 2030:2040,
                    db_option = "auto"
                     )

 t1 <- input_refmap(get_bb(in_xyz))
 
 t2 <- input_refmap(get_bb(test_pts), reference = "refmap_climatena")
 
 
 bc <- bcmaps::bc_bound()
 dem <- rast("../Common_Files/PRISM_dem.tiff")
 dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/PRISM_dem/", sep="")
 dem <- rast(paste(dir, "PRISM_dem.asc", sep=""))
 dem <- aggregate(dem, fact=3)
 bc <- vect(bc)
 bc <- project(bc, crs(dem))
 dem <- mask(dem, bc)
 dem <- trim(dem)
 
 res <- downscale(dem, vars = c("MAT","CMD"))
 
 # climr data
 grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
 colnames(grid) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects
 setDT(grid)
 
 bbox <- c(-134,-124,61,65)
 bb2 <- ext(bbox)
 
 test2 <- input_gcms(bb2, gcms = list_gcms()[1], ssps = list_ssps()[2], period = "2041_2060")

 test <- input_refmap(bbox, reference = "refmap_climr")
 plot(test[[18]])
 clim.grid <- downscale(xyz = grid, 
                        which_refmap = "refmap_climr", 
                        gcms = list_gcms()[1], 
                        ssps = list_ssps()[3],
                        gcm_periods = list_gcm_periods()[3], 
                        ensemble_mean = TRUE,
                        max_run = 0,
                        vars = list_vars("Annual")
 )
 
 clim.grid <- downscale(xyz = grid, 
                        which_refmap = "refmap_climr", 
                        gcms = list_gcms()[1], 
                        ssps = list_ssps()[2],
                        gcm_periods = list_gcm_periods()[3], 
                        max_run = 2,
                        vars = c("CMD", "Tmin_07")
 )
 
res <- downscale(dem, 
                 which_refmap = "refmap_climr", 
                 gcms = list_gcms()[1], 
                 ssps = list_ssps()[2],
                 gcm_periods = list_gcm_periods(),
                 vars = c("CMD", "Tmin_07"))


library(climr)
library(tidyverse)
library(terra)
library(data.table)

## provide a data.frame or data.table of point coordinates, IDs and elevation
BEC_data<-readRDS("../../../Downloads/BEC_data.rds") #change this to OS #13 

#pull out plot data 
plot_dat<-BEC_data$env #70,547 plots

#make dataframe for extracting climate data
my_points <- select(plot_dat, Longitude, Latitude, Elevation, PlotNumber, ProjectID) %>%
  rename(lon = Longitude,   lat = Latitude, 
         elev = Elevation)%>%
  mutate(id = seq_along(lon)) %>%
  na.omit() #remove NAs- 59,345 plots with GPS locations 

#look at options 
#what to select here?
list_obs_periods()
list_obs_years()
list_vars() 

vars<-climr::variables #look up table for vars 
var_names<-vars$Code

## climr query for the historic data - only using 1961-1990 for now 
## what is the resolution/scale of these data? PRISM 800m downscaled to plot-level (accuracy of GPS points and elevation- double checks elev vals make bigger difference)
#cache_clear()
setDT(my_points)
clim_dat <- downscale(
  xyz = my_points, which_refmap = "refmap_climr", 
  #historic_period = "2001_2020", 
  #historic_ts = C(1961:1990),
  #gcm_models = c("GFDL-ESM4", "EC-Earth3"), # specify two global climate models
  #ssp = c("ssp370", "ssp245"), # specify two greenhouse gas concentration scenarios
  #gcm_period = c("2001_2020", "2041_2060"), # specify two 20-year periods
  #max_run = 3, # specify 3 individual runs for each model
  #  vars = c("PPT", "MAT", "CMD", 'AHM', 'CMI', 'DD5', 'TD', "PPT_10"))  #TD variable?? continentality??
  vars=list_vars()) 

boundary <- bb
dbCon <- data_con()
bc_bb <- c(59,57,-120,-121)

in_xyz <- data.frame(
  lon = c(-127.7052, -127.6227, -127.5623, -127.7162, -127.1858, -127.125, -126.9495, -126.9550),
  lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
  elev = c(291, 296, 626, 377, 424, 591, 723, 633),
  id = 1:8
)
data <- downscale(in_xyz, which_refmap = "refmap_climr", obs_periods = "2001_2020", vars = c("MAT","MAP","CMD"))
dem <- rast("data-raw/dem_mosaic/climr_mosaic_dem.tif")
dem2 <- crop(dem, ext(res2))

res3 <- downscale(dem2, which_refmap = "refmap_climr", vars = c("MAT","MAP","CMD"))

library(tictoc)
tic()
res <- dbGetRaster(dbCon, "normal_composite", TRUE, bands = 1:73, boundary = bc_bb)
toc()

conn <- data_con()
tic()
res2 <- pgGetTerra(conn, "normal_composite", TRUE, bands = 1:73, boundary = bc_bb)
toc()

projID <- 4326L
rids <- climr:::db_safe_query("
    select rid
    from \"%s\"
    WHERE ST_Intersects(ST_ConvexHull(%s), ST_GeomFromText('POLYGON((%s %s,%s %s,%s %s,%s %s,%s %s))', %s))" |> 
                        sprintf("normal_composite", "rast", boundary[4], boundary[1], boundary[4], boundary[2],
                                 boundary[3], boundary[2], boundary[3], boundary[1], boundary[4], boundary[1],
                                 projID)
)

txt <- "select rid
    from \"%s\"
    WHERE ST_Intersects(ST_ConvexHull(%s), ST_GeomFromText('POLYGON((%s %s,%s %s,%s %s,%s %s,%s %s))', %s))" |> 
  sprintf("normal_composite", "rast", boundary[4], boundary[1], boundary[4], boundary[2],
           boundary[3], boundary[2], boundary[3], boundary[1], boundary[4], boundary[1],
           4326)

rids <- climr:::db_safe_query("
    select rid
    from \"%s\"
    WHERE ST_Intersects(ST_ConvexHull(%s), ST_GeomFromText('POLYGON((%s %s,%s %s,%s %s,%s %s,%s %s))', %s))"
                      |> sprintf("normal_composite", "rast", boundary[4], boundary[1], boundary[4], boundary[2],
                                 boundary[3], boundary[2], boundary[3], boundary[1], boundary[4], boundary[1],
                                 4326L)
)
res <- input_refmap(dbCon, bb)
res2 <- input_refmap(dbCon, bb, reference = "refmap_climatena")

projID <- climr:::db_safe_query("
    select ST_SRID(%s) as srid
    from \"%s\" where rid = 1;
  " |> sprintf("rast", "normal_bc")
)$srid[1]

# generate the input data
my_data <- plot_timeSeries_input_db(my_points[1,], gcms = list_gcms()[1])

# use the input to create a plot
plot_timeSeries(my_data, var1 = "Tmin_sm")

plot_bivariate_db(my_points)

plot_climate_stripes()

dbCon <- data_connect()
bbox <- c(55,51.5,-115,-128)

cache_clear("reference")
climna <- input_refmap(dbCon, bbox)
plot(climna[[15]])

pnts <- data.table(lon = c(-127.18, -123.46, -125), lat = c(54.89, 48.78, 48.3), elev = c(540,67, 102), id = 1:3)
pnts <- data.table(lon = c(-121.20225,-126.39689,-117.97568,-127.29956,-127.12704,-118.7898,-123.45617,-123.37194), 
                   lat = c(50.77239,54.73596,50.28127,50.28127,54.83288,50.24118,48.79616,52.49457), 
                   elev = c(588,985,1067,55,563,799,306,1103), 
                   id = c("BGxm1","SBSmc2","ICHmw2","CWHvm1","SBSdk","ICHxm1","CDFmm","SBPSdc"))

res <- downscale(pnts, obs_years = 1961:1990, obs_ts_dataset = "cru.gpcc", return_refperiod = FALSE, indiv_tiles = TRUE, db_option = "local")

name <- "normal_composite"
bands <- 1:73

dbGetTiles <- function(dbCon, name, pnts, rast = "rast", bands = 1:73){
  
  pnts[,mls := paste0("(",lon," ",lat,")")]
  wkt_str <- paste0("MULTIPOINT(", paste(pnts$mls, collapse = ", "),")")
  qry <- paste0("select rid as id,
            st_xmax(st_envelope(rast)) as xmx,
            st_xmin(st_envelope(rast)) as xmn,
            st_ymax(st_envelope(rast)) as ymx,
            st_ymin(st_envelope(rast)) as ymn,
            st_width(rast) as cols,
            st_height(rast) as rows
            from
            ", name," 
            WHERE ST_Intersects(
              rast,ST_SetSRID(
                ST_GeomFromText('",wkt_str,"'),
                4326
              )
            )
            ")
  
  info <- dbGetQuery(dbCon,qry)
  
  bandqs1 <- paste0("UNNEST(ST_Dumpvalues(rast, ", bands, ")) as vals_", bands)
  
  qry2 <- paste0("select rid, ", 
         paste(bandqs1, collapse = ", "),
         " from ", name,
         " where rid IN (",paste(info$id, collapse = ", "),")")
  r_values <- dbGetQuery(dbCon,qry2)
  
  rast_vals <- suppressWarnings(melt(r_values, id.vars = c("rid")))
  out_list <- list()
  for(tile in unique(info$id)){
    info_tl <- info[info$id == tile,]
    rout <- rast(
      nrows = info_tl$rows, ncols = info_tl$cols, xmin = info_tl$xmn,
      xmax = info_tl$xmx, ymin = info_tl$ymn, ymax = info_tl$ymx, nlyrs = length(bands),
      crs = paste0("EPSG:", 4326), vals = rast_vals$value[rast_vals$rid == tile]
    )
    out_list[[as.character(tile)]] <- rout
  }
  
  rsrc <- sprc(out_list)
  rast_res <- merge(rsrc)
  return(rast_res)
}

out <- dbGetTiles(dbCon, "\"gcm_mpi-esm1-2-hr\"", pnts)
plot(out[[15]])
out <- dbGetTiles(dbCon, "normal_composite", pnts)
plot(out[[15]])


varsl = c("DD5", "DDsub0_at", "DDsub0_wt", "PPT_05", "PPT_06", "PPT_07", "PPT_08",
          "PPT_09", "CMD", "PPT_at", "PPT_wt", "CMD_07", "SHM", "AHM", "NFFD", "PAS", 
          "CMI", "Tmax_sm", "TD", "PPT_sm", "DD5_sp", "PPT_sp", "PPT_sm", "Tmax_at", "Tmax_wt", "Tmax_sp", "Tmax_sm",
          "Tmin_at", "Tmin_wt", "Tmin_sp", "Tmin_sm")
varsl = unique(varsl)

cache_clear()
load("my_grid.Rdata")
setDT(my_grid)
my_grid <- my_grid[lat < 60,]
clim.bc <- downscale(
  xyz = my_grid,   which_refmap = "auto",
  obs_periods = "2001_2020", 
  vars = varsl)



bc_pnts <- c(59.9987500001111, 48.3126388889072, -114.057083333295, -138.998750000111)

wlr <- rast("../Common_Files/composite_WNA_dem.tif")
plot(wlr)

dem <- rast("../Common_Files/dem_noram_lowres.tif")
test <- rast("../Common_Files/climatena_normals/Normal_1961_1990MP/Tmin07.asc")
plot(test)

my_grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
colnames(my_grid) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects
db <- data_connect()
bbox <- get_bb(my_grid)
bbox2 <- c(20,14.83,-80,-120)
refmap <- input_refmap(db, bbox)

db <- data_connect()
refmap_na <- input_refmap(db, bbox = bc_pnts, reference = "refmap_climr")
plot(refmap_na$Tmax_07)

plot(refmap$Tmax_07)

pts <- data.frame(lon = c(-124.11, -125.11), lat = rep(48.82, 2), elev = rep(25,2), id = 1:2)

bbox <- get_bb(pts[2,])
dbcon <- data_connect()
test <- input_refmap(dbcon, bbox)
plot(test[[8]])

dem <- rast("../Common_Files/dem_noram_lowres.tif")
test <- rast("../Common_Files/climatena_normals/Normal_1961_1990MP/Tmin07.asc")
plot(test)

my_grid <- as.data.frame(dem, cells = TRUE, xy = TRUE)
colnames(my_grid) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects
climr <- downscale(
  xyz = my_grid, which_refmap = "refmap_climatena", gcms = list_gcms()[1], ssps = list_ssps()[1:3], 
  gcm_periods = list_gcm_periods(), run_nm = "r1i1p1f1", vars = "MAT"
)

X <- rast(dem) 
X[climr[, id]] <- climr$MAT 
plot(X)


db <- data_connect()
bbox <- get_bb(my_grid)
bbox2 <- c(20,14.83,-80,-120)
refmap <- input_refmap(db, bbox)


projected <- climr_downscale(pts[2,], 
                             gcm_models = list_gcm()[c(4)],
                             ssp = list_ssp()[c(1,2)],
                             max_run = 3,
                             gcm_hist_years = 1851:2014,
                             gcm_ts_years = 2015:2100
                              )

dat <- fread("../climatena/Perioddat/Year_1905.ann")
idx <- fread("../climatena/Perioddat/cru_index.dat")
na <- fread('../climatena/prismdat/prismLR.dat')
plot(na)

my_points <- data.frame(
  lon = c(-123.4404, -123.5064, -124.2317),
  lat = c(48.52631, 48.46807, 49.21999),
  elev = c(52, 103, 357),
  id = seq_len(3)
)

## climr query for the data.frame
ds_out <- downscale(
  xyz = my_points, 
  which_refmap = "auto",
  gcms = list_gcms()[2], # specify two global climate models
  ssps = c("ssp370", "ssp245"), # specify two greenhouse gas concentration scenarios
  gcm_periods = c("2001_2020", "2041_2060"), # specify two 20-year periods
  gcm_ssp_years = 2024:2050,
  gcm_hist_years = 1870:1930,
  obs_years = 2015:2023,
  obs_ts_dataset = c("climatena","cru.gpcc"),
  obs_periods = "2001_2020",
  max_run = 3, # specify 3 individual runs for each model
  vars = c("PPT", "CMD", "CMI", "Tmin_01")
)

pt <- data.frame(lon = c(-125.13), lat = c(48.825), elev = c(7), id = 1)

data <- downscale(xyz = pt, 
                  gcms  = list_gcms()[c(1)],
                  ssps  = list_ssps(),
                  max_run = 3,
                  obs_ts_dataset  = c("cru.gpcc", "climatena"), 
                  obs_years  = 1902:2022,
                  gcm_hist_years = 1850:2014, 
                  gcm_ssp_years  = 2015:2100, 
                  vars = list_vars()
)

"#E404D1"
plot_timeSeries(data, var1 = "Tmax_08")
library(abind)
library(dplyr)
library(terra)
data <- plot_timeSeries_input(pt, gcms = list_gcms()[1], max_run = 5)
data <- downscale(pt, gcms = list_gcms()[1], gcm_ssp_years = list_gcm_ssp_years(),
                  ssps = list_ssps()[1:3], max_run = 5L)
plot_timeSeries(data, var1 = "MAT")

data <- downscale(xyz = pt, 
                        gcm_models = list_gcms()[5],
                        ssp = list_ssps(),
                        max_run = 10,
                        historic_ts_dataset = c("cru.gpcc", "climatena"), 
                        historic_ts = 1901:2022,
                        gcm_hist_years = 1850:2014, 
                        gcm_ts_years = 2015:2100, 
                        vars = list_vars()
)

variable <- "MAP"
plot_timeSeries(data, variable1 = variable, historic_ts_dataset = c("cru.gpcc", "climatena"), mar=c(3,3,2,4), refline = T)


library(climr)
library(terra)
library(data.table)

my_points <- data.frame(
  lon = c(-123.4404, -123.5064, -124.2317),
  lat = c(48.52631, 48.46807, 49.21999),
  elev = c(52, 103, 357),
  id = seq_len(3)
)

# draw the plot
plot_bivariate(my_points)

points_downscale_ref <- readRDS("tests/testthat/data/points_downscale_ref.rds")
pt <- points_downscale_ref

dbcon <- data_connect()
bbox <- get_bb(pt)
hist <- historic_input_ts(dbcon, dataset = c("cru.gpcc","climatena"), bbox = bbox, years = 1950:2015)

t1 <- hist[[1]]
t2 <- values(t1, mat = TRUE)
t2 <- data.table(t2)
t2[,cellid := 1:nrow(t2)]
t3 <- melt(t2, id.vars = "cellid")
t3[,c("dataset","var","year") := tstrsplit(variable,"_")]
t3 <- t3[,.(dataset,var,year,value)]

dbWriteTable(conn, "test_hist", t3, row.names = F)
dat <- dbGetQuery(conn, "select * from test_array;")

my_data <- plot_timeSeries_input(pt,gcm_models = list_gcm()[1:2], historic_ts_dataset = c("cru.gpcc","climatena"), vars = list_variables("Monthly"))
plot_timeSeries(my_data, variable1 = "CMI08", ssps = list_ssp(),historic_ts_dataset = c("cru.gpcc","climatena"), gcm_models = list_gcm()[1:2])

test_cru <- climr_downscale(pt, which_normal = "auto", 
                         historic_ts = 1950:2022,
                         historic_ts_dataset = c("climatena","cru.gpcc"),
                         return_normal = FALSE,
                         vars = paste0("Tmin", sprintf("%02d", 1:12))
)

test_climna <- climr_downscale(pt, which_normal = "auto", 
                            historic_ts = 1950:2015,
                            historic_ts_dataset = "climate_na",
                            return_normal = FALSE,
                            vars = paste0("Tmin", sprintf("%02d", 1:12))
)

test_cru[,dataset := "cru_gpcc"]
test_climna[,dataset := "climna"]
hist_all <- rbind(test_cru,test_climna)

hist <- melt(hist_all, id.vars = c("id","PERIOD","dataset"))
hist <- hist[id == 2,]
hist[,month := gsub("[^0-9.-]","",variable)]
hist[,date := as.Date(paste0(PERIOD,"-",month,"-01"))]

library(ggplot2)
ggplot(hist, aes(x = date, y = value, col = dataset)) +
  geom_line() +
  facet_wrap(~variable) +
  ylab("Tmin")
ggsave("Historic_TS_plots.png", width = 8, height = 9)


projected <- climr_downscale(pt, 
                             gcm_models = list_gcm()[3],
                             ssp = list_ssp()[c(1,2,4)],
                             max_run = 10,
                             gcm_hist_years = 1851:2014, 
                             gcm_ts_years = 2015:2100, 
                             vars = "Tmin07"
)

tic()
projected <- climr_downscale(pt, 
                             gcm_models = list_gcm()[3],
                             ssp = list_ssp()[c(1,2,4)],
                             max_run = 10,
                             gcm_hist_years = 1851:2014, 
                             gcm_ts_years = 2015:2100, 
                             vars = "Tmin07"
)
toc()


xyz <- fread("US_TrainingPoints_07April2024.csv")
xyz <- xyz[,.(ID1, LAT,LON,ELEV_m)]
setnames(xyz, c("id","lat","lon","elev"))

clim_vars <- climr_downscale(xyz, which_normal = "auto", vars = climr::list_variables(), return_normal = TRUE, cache = TRUE)
climdup <- clim_vars[duplicated(clim_vars$id),]

pt <- temp[1,]
projected <- climr_downscale(pt, 
                             gcm_models = list_gcm()[1],
                             ssp = list_ssp()[c(4)],
                             max_run = 10,
                             gcm_hist_years = 1851:2014, 
                             gcm_ts_years = 2015:2100, 
                             vars = "Tmax07"
)

gcms <- na.omit(unique(projected$GCM))[1]
ssps <- na.omit(unique(projected$SSP))[1]
runs <- unique(projected$RUN)
runs <- runs[grep("r", runs)]

par(mar=c(3,3,0.1, 0.1), mgp=c(1.5, 0.25, 0), tck=-0.01)
plot(projected$PERIOD, projected$Tmax07, col="white")
for(run in runs){
  s <- which(projected$RUN==run)
  lines(projected$PERIOD[s], projected$Tmax07[s], col=which(runs==run))
  # Sys.sleep(1)
}
lines(projected$PERIOD[projected$RUN=="ensembleMean"], projected$Tmax07[projected$RUN=="ensembleMean"], col=1, lwd=3)
mtext(paste(gcms, ssps), side=3, line=-1.5, adj=0.05)


data("xyzDT")
temp <- xyzDT[1:4,]
dbCon <- data_connect()
test <- gcm_ts_input(dbCon, bbox = get_bb(temp),
                     gcm = list_gcm()[3], 
                     ssp = list_ssp()[c(1,2,4)], 
                     max_run = 3,
                     years = 2015:2100)
t1 <- test$CanESM5
writeRaster(t1, "Test.grd", filetype = "ENVI", overwrite = T)
t2 <- rast("Test.grd")
plot(t1)


projected <- climr_downscale(temp, gcm_models = list_gcm()[3], 
                             ssp = list_ssp()[c(1,2,4)], 
                             max_run = 10,
                             gcm_hist_years = 1851:2014, 
                             gcm_ts_years = 2015:2100, 
                             vars = "Tmax07")

par(mar=c(1,1,1,1), mfrow=c(3,5))
dbCon <- data_connect()
gcm <- gcm_input(
  dbCon = dbCon,
  bbox = c(80, 20, -50, -170),
  gcm = list_gcm(),
  ssp = list_ssp()[2],
  period = list_gcm_periods()[2],
)

for(i in 1:length(list_gcm())){
  plot(gcm[[i]][[7]], legend=F, main=list_gcm()[i])
  plot(countriesCoarse, add = T, xlim=c(-170, -50), ylim=c(20,80))
}
poolClose(dbCon)

list_variables("Monthly")
## A data.table of point coordinates, IDs and elevation
data("xyzDT")
temp <- xyzDT[1:4,]
mods <- c(c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3", 
            "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", 
            "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"))

mods <- c("ACCESS-ESM1-5", "IPSL-CM6A-LR", "MIROC6", 
            "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")

gcmres <- gcm_input(
  dbCon = data_connect(),
  bbox = c(80, 20, -50, -170),
  gcm = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5"),
  ssp = list_ssp()[2],
  period = list_gcm_periods()[2])

plot(gcmres$`ACCESS-ESM1-5`[[6]])

library(rworldmap)
data("countriesCoarse")
plot(countriesCoarse, xlim=c(-170, -50), ylim=c(20,80))
plot(gcmres[[1]][[7]], legend=F)
plot(countriesCoarse, xlim=c(-170, -50), ylim=c(20,80), add = T)

plot(gcmres[[1]])
## if you just want to downscale points and not think about what happening behind the scenes, use this function

ds_out <- climr_downscale(
  xyz = temp, 
  which_normal = "auto",
  gcm_models = mods,
  gcm_period = list_gcm_periods()[2],
  gcm_ts_years = 2020:2050,
  ssp = list_ssp()[4],
  max_run = 3, # we want 3 individual runs for each model
  vars = c("PPT", "CMI04","CMI06","CMI07", "CMI")
)



ds_out <- climr_downscale(
  xyz = temp, 
  which_normal = "auto",
  gcm_models = mods,
  historic_ts = 1950:1965,
  gcm_hist_years = 1950:1965,
  max_run = 0, # we want 3 individual runs for each model
  vars = c("PPT", "CMI04","CMI06","CMI07", "CMI")
)

ds_out_ts <- climr_downscale(
  xyz = temp,
  which_normal = "auto",
  gcm_ts_years = 2015:2040,     ## currently starting at 2021
  gcm_models = list_gcm()[1:5],
  gcm_hist_years = 2001:2014,
  ssp = "ssp245",
  max_run = 1,
  return_normal = TRUE, ## to return the 1961-1990 normals period
  vars = c("MAT", "PPT", "CMI07")
)

tout <- climr_downscale(xyz = temp,
                        which_normal = "auto",
                        gcm_models = list_gcm(),
                        gcm_ts_years = 2080:2100,
                        ssp = "ssp245")

coords <- fread("../../../Downloads/coords.csv")
get_bb(coords)

bb <- c(57.1763589075024, 52.7889907334224, -124.01035407693, -130.046934182713)
dbCon <- data_connect()
clim_vars <- normal_input(dbCon, bb, normal = "normal_composite")

tic()
clim_vars <- climr_downscale(coords, which_normal = "normal_composite", 
                                              vars = list_variables(), return_normal = T)
toc()

##original connection function
#' Connect to PostGIS database
#' 
#' @return pool object of database connection
#' @param local A logical. Use a local database. Default `FALSE`.
#' @importFrom pool dbPool
#' @importFrom RPostgres Postgres
#' 
#' @export
# data_connect <- function(local = FALSE) {
#   if(local){
#     pool <- dbPool(
#       drv = Postgres(),
#       dbname = "climr",
#       host = "localhost",
#       port = 5432,
#       user = "postgres",
#       password = "climrserver"
#     )
#   }else{
#     pool <- tryCatch(
#       {
#         dbPool(
#           drv = Postgres(),
#           dbname = "climr",
#           host = "146.190.244.244",
#           port = 5432,
#           user = "climr_client",
#           password = "PowerOfBEC2023"
#         )
#       },
#       error = function(e) {
#         tryCatch(
#           {
#             dbPool(
#               drv = Postgres(),
#               dbname = "climr",
#               host = "146.190.244.244",
#               port = 5432,
#               user = "climr_client",
#               password = "PowerOfBEC2023"
#             )
#           },
#           error = function(f) {
#             warning("Could not connect to database. Will try using cached data.")
#             NULL
#           }
#         )
#       }
#     )
#   }
#   
#   return(pool)
# }
