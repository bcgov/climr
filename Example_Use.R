library(climRdev)
library(data.table)
library(terra)
library(pool)
library(sf)

##provide or create a long, lat, elev dataframe
in_xyz <- structure(list(Long = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                                  -127.18585, -127.1254, -126.94957, -126.95507), 
                         Lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025), 
                         Elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L)), row.names = c(NA, -8L), class = "data.frame")

res <- climr_downscale(xyz = in_xyz, which_normal = "auto", 
                       gcm_models = c("ACCESS-ESM1-5", "EC-Earth3"), 
                       ssp = c("ssp370","ssp245"), 
                       gcm_period = c("2021_2040", "2041_2060","2061_2080"),
                       #gcm_ts_years = 2020:2060,
                       max_run = 3,
                       vars = c("PPT","CMD","CMI"))


coords <- fread("C:/Users/kdaust/Government of BC/External Future Forest Ecosystems Centre - Kiri/WNA_BGC/WNA_2km_grid_WHM.csv")
coords <- coords[!is.na(elev),]
#coords <- fread("WNA_2km_grid_WHM.csv")
#setcolorder(coords, c("long","lat","elev","id"))
coords <- as.data.frame(coords)# %>% dplyr::rename(long = 1, lat = 2)

## based on vs_final below
vars_needed <- c("DD5","DD_0_at","DD_0_wt","PPT05","PPT06","PPT07","PPT08","PPT09","CMD","PPT_at","PPT_wt","CMD07","SHM", "AHM", "NFFD", "PAS", "CMI")

clim_vars <- climr_downscale(coords, which_normal = "auto", return_normal = TRUE, vars = vars_needed)
clim_vars <- clim_vars[!is.nan(clim_vars$PPT05),]
fwrite(clim_vars,"WNA_ClimVars.csv")

bc_bnd <- st_read("D:/Prov_bnd/Prov_bnd")
bc_bnd <- bc_bnd['NAME']
bc_bnd <- st_transform(bc_bnd, 4326)
bc_bnd <- st_make_valid(bc_bnd)
st_write(bc_bnd, "BC_Outline.gpkg")

xyz <- fread("Test_Locations_VanIsl.csv")
xyz <- setcolorder(xyz, c("Long","Lat","Elev"))
xyz <- as.data.frame(xyz)

res <- climr_downscale(xyz = xyz, which_normal = "auto", 
                       gcm_models = c("ACCESS-ESM1-5", "EC-Earth3"), 
                       ssp = "ssp370", 
                       gcm_period = c("2021_2040", "2041_2060"),
                       #gcm_ts_years = 2060:2080,
                       vars = c("PPT","CMD","CMI"))

##provide or create a long, lat, elev dataframe
library(climRdev)
in_xyz <- structure(list(Long = c(-127.62279, -127.56235, -127.7162, 
                                  -127.18585, -127.1254, -126.94957, -126.95507), 
                         Lat = c(55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025), 
                         Elev = c(296L, 626L, 377L, 424L, 591L, 723L, 633L)), row.names = c(NA, -7L), class = "data.frame")
thebb <- get_bb(in_xyz) ##get bounding box based on input points
dbCon <- data_connect()
dbCon <- NULL
normal <- normal_input_postgis(dbCon = dbCon, normal = "normal_na", bbox = thebb, cache = TRUE) 
normalbc <- normal_input_postgis(dbCon = dbCon, normal = "normal_bc", bbox = thebb, cache = TRUE) 
plot(normalbc[[14]])
gcm <- gcm_input_postgis(dbCon, bbox = thebb, gcm = c("ACCESS-ESM1-5", "EC-Earth3"), 
                         ssp = c("ssp370"), 
                         period = c("2021_2040","2041_2060","2061_2080"),
                         max_run = 0,
                         cache = TRUE)
results <- downscale(
  xyz = in_xyz,
  normal = normalbc,
  return_normal = TRUE,
  vars = c("CMD","PPT", "PET07", "CMI")
)


thebb <- c(60, 30, -102, -139)

normal <- normal_input_postgis(dbCon = dbCon,normal = "normal_na", bbox = thebb, cache = TRUE)
plot(normal[[14]])
in_locations <- fread("Test_Locations_VanIsl.csv") ##provide or create a 

in_xyz <- as.data.frame(in_locations[,.(Long,Lat,Elev)]) ##currently needs to be a data.frame or matrix, not data.table


thebb <- get_bb(in_xyz) ##get bounding box based on input points
dbCon <- data_connect() ##connect to database
thebb <- c(60.0033688541466, 38.9909731490503, -107.993162013109, -139.037335926101)
conn <- dbCon
name <- "normal_wna"
rast = "rast"
bands = 1:5

normal <- normal_input_postgis(dbCon = dbCon, bbox = thebb, cache = TRUE) ##get normal data and lapse rates
plot(normal[[1]])

dem <- rast("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climRpnw/inputs_pkg/normal/Normal_1961_1990MP/dem/dem2_WNA.nc")

historic <- historic_input(dbCon, bbox = thebb, period = "2001_2020", cache = TRUE)
plot(historic[[1]][[1]])

historic_ts <- historic_input_ts(dbCon, bbox = thebb, years = 1950:2022)
plot(historic_ts[[1]][[1]])
##get GCM anomolies (20 yr periods)
gcm <- gcm_input_postgis(dbCon, bbox = thebb, gcm = c("ACCESS-ESM1-5", "EC-Earth3"), 
                         ssp = c("ssp370"), 
                         period = c("2021_2040","2041_2060","2061_2080"),
                         max_run = 0,
                         cache = TRUE)
plot(gcm[[2]][[1]])

##get GCM anomolies (time series) - note that for multiple runs, this can take a bit to download the data
gcm_ts <- gcm_ts_input(dbCon, bbox = thebb, gcm = c("ACCESS-ESM1-5"), 
                         ssp = c("ssp245","ssp370"), 
                         years = 2020:2100,
                         max_run = 6,
                         cache = TRUE)
plot(gcm_ts[[1]][[1]])

# Downscale!
results <- downscale(
  xyz = in_xyz,
  normal = normal,
  #historic_ts = historic_ts,
  gcm_ts = gcm_ts,
  vars = sprintf(c("Tmax%02d"),1:12)
)

##########make some figures#############
results <- data.table(results)
resdd5 <- results[ID == 1,]
resdd5[,c("ID") := NULL]
resdd5 <- melt(resdd5, id.vars = c("PERIOD","GCM","SSP","RUN"))
setorder(resdd5, PERIOD, variable)
resdd5[,temp := gsub("[A-Z]|[a-z]","",variable)]
resdd5[,PERIOD := paste(PERIOD,temp,"01", sep = "-")]
resdd5[,PERIOD := as.Date(PERIOD)]

res_jan <- resdd5[variable == "Tmax01",]
res_jan <- res_jan[GCM == "ACCESS-ESM1-5",]
res_jan <- res_jan[SSP == "ssp245",]
library(ggplot2)
library(ggsci)
ggplot(res_jan[RUN != "ensembleMean",],aes(x = PERIOD, y = value, col = RUN)) +
  geom_line() +
  geom_line(data = res_jan[RUN == 'ensembleMean',], aes(x = PERIOD, y = value), col = "black", linewidth = 1)+
  xlab("Date") +
  ylab("Tmax") +
  theme_bw() +
  scale_color_jama() +
  ylab("Tmax_01 (C)")

ggsave("TS_Example.png", width = 6, height = 4, dpi = 400)

poolClose(dbCon)
