library(climr)
library(pool)
library(data.table)
library(terra)

##provide or create a long, lat, elev, and optionally id, dataframe - usually read from csv file
in_xyz <- structure(list(Long = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                                  -127.18585, -127.1254, -126.94957, -126.95507), 
                         Lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025), 
                         Elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L)), row.names = c(NA, -8L), class = "data.frame")

##show available variables:
list_variables()
list_ssp()
list_gcm()
list_gcm_period()
list_historic()

##if you just want historic observational time series - note that the first time you run,
##it has to download the data so might take some time. It will then cache the data, so will
##be faster for future runs
ds_hist <- climr_downscale(xyz = in_xyz, which_normal = "auto", 
                       historic_ts = 1902:2005,
                       return_normal = FALSE, ##put this to TRUE if you want the 1961-1990 period
                       vars = c("PPT","CMD","CMI","Tave01","Tave07")) ##specify desired variables


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
plot(normalbc[[13]])

##gcm annomalies
gcm <- gcm_input_postgis(dbCon, bbox = thebb, gcm = c("ACCESS-ESM1-5", "EC-Earth3"), 
                         ssp = c("ssp370"), 
                         period = c("2021_2040","2041_2060","2061_2080"),
                         max_run = 0,
                         cache = TRUE)
head(names(gcm[[1]]))
plot(gcm[[1]][[1]])

##downscale function
ds_res <- downscale(in_xyz, normal = normalbc, gcm = gcm, return_normal = T)

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
