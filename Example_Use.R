library(climr)
library(pool)
library(data.table)
library(terra)


dbCon <- data_connect()
## smaller example in BC
xyz <- data.frame(lon = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                           -127.18585, -127.1254, -126.94957, -126.95507),
                  lat = c(55.3557, 55.38847, 55.28537, 55.25721, 
                          54.88135, 54.65636, 54.6913, 54.61025),
                  elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
                  id = LETTERS[1:8],
                  Zone = c(rep("CWH",3), rep("CDF",5)),
                  Subzone = c("vm1","vm2","vs1",rep("mm",3),"dk","dc"))

thebb <- get_bb(xyz)

histtsout2 <- historic_input_ts(dbCon, thebb, years = 2019:2022, cache = TRUE)
names(histtsout2[[1]])

gcms <- c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL")
list_gcm()
gcms[-which(gcms%in%list_gcm())]

in_xyz <- structure(list(lon = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                                  -127.18585, -127.1254, -126.94957, -126.95507), 
                         lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025), 
                         elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
                         id = 1:8), row.names = c(NA, -8L), class = "data.frame")


dbCon <- data_connect()
test <- historic_input_ts(dbCon, get_bb(in_xyz))
test <- normal_input(dbCon, get_bb(in_xyz))

clim <- climr_downscale(in_xyz,
                        which_normal = "auto",
                        gcm_models = list_gcm(),
                        ssp = list_ssp()[2],
                        gcm_period = list_gcm_period()[2],
                        max_run = 4L,
                        return_normal = FALSE,
                        vars = c("CMI","DD5","Tmin01")
)
unique(clim$GCM)
gcms[-which(gcms%in%unique(clim$GCM))]



set.seed(123)
dbCon <- data_connect()
xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10), id = 1:10)

## get bounding box based on input points
thebb <- get_bb(xyz)
historic <- historic_input(dbCon, thebb, period = "2001_2020")
plot(historic[[1]][[2]])

##provide or create a long, lat, elev, and optionally id, dataframe - usually read from csv file
in_xyz <- data.frame(Long = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                              -127.18585, -127.1254, -126.94957, -126.95507), 
                     Lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025), 
                     Elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
                     ID = c(2,6,8,5,1,12,3,13),
                     Zone = c(rep("CWH",3), rep("CDF",5)),
                     Subzone = c("vm1","vm2","vs1",rep("mm",3),"dk","dc"))

##show available variables:
list_variables(set = "Annual")
list_ssp()
list_gcm()
list_gcm_period()
list_historic()

##if you just want historic observational time series - note that the first time you run,
##it has to download the data so might take some time. It will then cache the data, so will
##be faster for future runs
ds_hist <- climr_downscale(xyz = in_xyz, which_normal = "auto", 
                           gcm_models = "ACCESS-ESM1-5",
                           gcm_period = "2081_2100",
                           max_run = 8L,
                           return_normal = TRUE, ##put this to TRUE if you want the 1961-1990 period
                           vars = c("PPT","CMD","CMI","Tave01","Tave07"),
                           out_spatial = FALSE, plot = "PPT") ##specify desired variables



##Future periods:
list_gcm()
ds_results <- climr_downscale(xyz = in_xyz, which_normal = "auto", 
                              gcm_models = c("ACCESS-ESM1-5", "MPI-ESM1-2-HR"),
                              gcm_period = c("2021_2040","2041_2060"),
                              ssp = c("ssp245","ssp370"),
                              return_normal = FALSE,
                              max_run = 3L,
                              vars = c("PPT","CMD","CMI","Tave01","Tave07"))

##Future timeseries:
ds_results_ts <- climr_downscale(xyz = in_xyz, which_normal = "auto", 
                                 gcm_models = c("ACCESS-ESM1-5"),
                                 gcm_ts_years = 2020:2080,
                                 ssp = c("ssp245","ssp370"),
                                 return_normal = FALSE,
                                 max_run = 6L,
                                 vars = c("PPT","CMD","CMI","Tave01","Tave07"))

res2 <- ds_results_ts[ID == 1 & SSP == "ssp245",]
res2[,RUN := as.factor(RUN)]
res2[,PERIOD := as.Date(PERIOD,format = "%Y")]

library(ggplot2)
ggplot(res2[RUN != "ensembleMean",],aes(x = PERIOD, y = Tave01, col = RUN, group = RUN)) +
  geom_line() +
  geom_line(data = res2[RUN == 'ensembleMean',], aes(x = PERIOD, y = Tave01), col = "black", linewidth = 1)+
  xlab("Year") +
  ylab("Tave_Jan") +
  theme_bw() +
  ggtitle("ACCESS-ESM1-5: SSP245")

##historic timeseries
ds_results_hist <- climr_downscale(xyz = in_xyz, which_normal = "auto", 
                                   gcm_models = c("ACCESS-ESM1-5"), 
                                   gcm_hist_years = 1910:2010,
                                   historic_ts = 1910:2010,
                                   return_normal = FALSE,
                                   max_run = 6L,
                                   vars = c("PPT","CMD","CMI","Tave01","Tave07"))

ds_results_hist[is.na(GCM), GCM := "Observed",]
res2 <- ds_results_hist[ID == 1,]
res2[,PERIOD := as.Date(PERIOD, format = "%Y")]
#res2[,Linewidth := fifelse(RUN == "ensembleMean" | GCM == "Observed", 1, 0.5)]
ggplot(res2[RUN != "ensembleMean" & GCM != "Observed",],aes(x = PERIOD, y = PPT, col = RUN)) +
  geom_line() +
  geom_line(data = res2[RUN == 'ensembleMean',], aes(x = PERIOD, y = PPT), col = "black", linewidth = 1)+
  geom_line(data = res2[GCM == "Observed",], aes(x = PERIOD, y = PPT, col = "Observed"), col = "red", linewidth = 1)+
  xlab("Date") +
  ylab("Tave_July") +
  theme_bw() +
  ggtitle("Historic")


##without the wrapper function=====================================
thebb <- get_bb(in_xyz) ##get bounding box based on input points
dbCon <- data_connect()
##get normal
normalbc <- normal_input(dbCon = dbCon, normal = "normal_bc", bbox = thebb, cache = TRUE) 
plot(normalbc$Tmax08)

##gcm annomalies
gcm <- gcm_input(dbCon, bbox = thebb, 
                 gcm = c("BCC-CSM2-MR", "INM-CM5-0"), 
                 ssp = c("ssp126", "ssp370"), 
                 period = "2041_2060",
                 max_run = 0,
                 cache = TRUE)
head(names(gcm[[1]]))
plot(gcm[[1]]$BCC.CSM2.MR_Tmax_08_ssp370_ensembleMean_2041_2060)

set.seed(678)
n <- 40000
sample_xyz <- data.frame(lon = runif(n, xmin(normalbc$dem2_WNA), xmax(normalbc$dem2_WNA)), 
                         lat = runif(n, ymin(normalbc$dem2_WNA), ymax(normalbc$dem2_WNA)), 
                         elev = NA)
sample_xyz[, 3] <- terra::extract(normalbc$dem2_WNA, sample_xyz[, 1:2], method = "bilinear")[, -1L]

##downscale function
ds_res_bc <- downscale(sample_xyz, normal = normalbc, gcm = gcm, var = list_variables())

##get historic anomalies
historic <- historic_input(dbCon, bbox = thebb, period = "2001_2020", cache = TRUE)
plot(historic[[1]][[1]])

historic_ts <- historic_input_ts(dbCon, bbox = thebb, years = 1950:2022)
plot(historic_ts[[1]][[1]])


##get GCM anomolies (time series) - note that for multiple runs, this can take a bit to download the data
gcm_ts <- gcm_ts_input(dbCon, bbox = thebb, gcm = c("ACCESS-ESM1-5"), 
                       ssp = c("ssp245"), 
                       years = 2020:2100,
                       max_run = 3,
                       cache = TRUE)
plot(gcm_ts[[1]][[1]])

##close connection (not super necessary but good practice)
poolClose(dbCon)
