library(data.table)
library(terra)
library(climr)


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
  xyz = my_grid, which_refmap = "refmap_climatena", vars = "MAT"
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
  id = LETTERS[1:3]
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

plot_timeSeries(data, var1 = "Tmax_08")
library(abind)
library(dplyr)
library(terra)
data <- plot_timeSeries_input(pt, gcms = list_gcms()[1], max_run = 5)
data <- downscale(pt, gcms = list_gcms(), gcm_ssp_years = list_gcm_ssp_years(),
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
  id = LETTERS[1:3]
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
