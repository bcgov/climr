library(climRdev)
library(data.table)
library(terra)
library(pool)

in_locations <- fread("Test_Locations_VanIsl.csv") ##provide or create a 

in_xyz <- as.data.frame(in_locations[,.(Long,Lat,Elev)]) ##currently needs to be a data.frame or matrix, not data.table

##provide or create a long, lat, elev dataframe
in_xyz <- structure(list(Long = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                                  -127.18585, -127.1254, -126.94957, -126.95507), 
                         Lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025), 
                         Elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L)), row.names = c(NA, -8L), class = "data.frame")

thebb <- get_bb(in_xyz) ##get bounding box based on input points
dbCon <- data_connect() ##connect to database
normal <- normal_input_postgis(dbCon = dbCon, bbox = thebb, cache = TRUE) ##get normal data and lapse rates
plot(normal[[1]])

##get GCM anomolies (20 yr periods)
gcm <- gcm_input_postgis(dbCon, bbox = thebb, gcm = c("ACCESS-ESM1-5", "EC-Earth3"), 
                         ssp = c("ssp370"), 
                         period = c("2021_2040","2041_2060","2061_2080"),
                         max_run = 0,
                         cache = TRUE)
plot(gcm[[2]][[1]])

##get GCM anomolies (time series) - note that for multiple runs, this can take a bit to download the data
gcm_ts <- gcm_ts_input(dbCon, bbox = thebb, gcm = c("ACCESS-ESM1-5", "MPI-ESM1-2-HR"), 
                         ssp = c("ssp245"), 
                         years = 2020:2080,
                         max_run = 4,
                         cache = TRUE)
plot(gcm_ts[[2]][[1]])

# Downscale!
results <- downscale(
  xyz = in_xyz,
  normal = normal,
  #gcm = gcm,
  gcm_ts = gcm_ts,
  vars = sprintf(c("Tmax%02d"),1:12)
)

##########make some figures#############
results <- data.table(results)
resdd5 <- results[PERIOD %in% 2020:2080,]
resdd5 <- resdd5[ID == 1,]
resdd5[,c("ID","SSP") := NULL]
resdd5 <- melt(resdd5, id.vars = c("PERIOD","GCM", "RUN"))
setorder(resdd5, PERIOD, variable)
resdd5[,temp := gsub("[A-Z]|[a-z]","",variable)]
resdd5[,PERIOD := paste(PERIOD,temp,"01", sep = "-")]
resdd5[,PERIOD := as.Date(PERIOD)]

res_jan <- resdd5[variable == "Tmax01",]

library(ggplot2)
ggplot(res_jan[RUN != "ensembleMean",],aes(x = PERIOD, y = value, col = GCM, group = factor(interaction(RUN,GCM)))) +
  geom_line() +
  geom_line(data = res_jan[RUN == 'ensembleMean',], aes(x = PERIOD, y = value, group = GCM), col = "black", linewidth = 1)+
  xlab("Date") +
  ylab("Tmax_Jan") +
  ggtitle("ACCESS-ESM1-5")

ggsave("TS_Example.png", width = 6, height = 4, dpi = 400)

poolClose(dbCon)
