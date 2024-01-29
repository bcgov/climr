library(terra)
library(data.table)

###testing
tdat <- rast("Daily_HR_Tmin/Test_March3.nc")
plot(tdat[[42]])


dailymax <- rast("/mnt/c/Users/kirid/Desktop/FFEC/Common_Files/PNWNAmet/PNWNAmet_tasmax.nc.nc")
plot(dailymax[[2]])
yrs <- time(dailymax)
mnths <- months(yrs)
avg_01 <- mean(dailymax[[mnths == "January"]])
plot(avg_01)
plot(dailymax[[1]] - avg_01)

bb <- vect("/mnt/c/Users/kirid/Desktop/FFEC/climr/BBox.kml")
dailymax <- crop(dailymax, bb)
plot(dailymax[[1]])

normal_avg <- list()
for(month in unique(mnths)){
  cat(month)
  avg <- mean(dailymax[[mnths == month]])
  normal_avg[[month]] <- avg
}

daily_anom <- list()
for(month in unique(mnths)){
  cat(month, "\n")
  anom <- dailymax[[mnths == month]]
  anom <- normal_avg[[month]] - anom
  daily_anom[[month]] <- anom
}
plot(daily_anom$January[[452]])

##prism surface
files <- list.files("/mnt/c/Users/kirid/Desktop/FFEC/Common_Files/colin_climatology/", full.names = T)
tmax_files <- files[grep("Tmax",files)]
prism_tmax <- rast(tmax_files)
prism <- crop(prism_tmax, daily_anom[[1]])


# writeCDF(prism[[1]],"Prism_Jan.nc")
# test <- daily_anom[["January"]]
# time(test) <- time(dailymax[[mnths == "January"]])
# test <- project(test, "epsg:4326")
# writeCDF(test,"Daily_Anom.nc", overwrite = T)

dir.create("Daily_Anom_Tmax")
for(i in 1:12){
  cat(months[i], "\n")
  temp <- daily_anom[[mnths = months[i]]]
  writeRaster(temp, paste0("Daily_Anom_Tmax/Daily_Anom_",months[[i]],".tif"), overwrite = T, gdal="COMPRESS=NONE")
  writeRaster(prism[[i]], paste0("Daily_Anom_Tmax/Prism_HR_",months[[i]],".tif"),overwrite = T, gdal="COMPRESS=NONE")
}

var <- "Tmin"
dir.create(paste0("Daily_HR_",var))
process_1 <- function(month){
  daily <- rast(paste0("Daily_Anom_",var,"/Daily_Anom_",month,".tif"))
  prism <- rast(paste0("Daily_Anom_",var,"/Prism_HR_",month,".tif"))
  out <- resample(daily, prism, method = "bilinear") + prism
  out <- as.int(out * 10)
  writeCDF(out, prec = "integer", filename = paste0("Daily_HR_",var,"/",var,"_HR",month,".nc"), varname = tolower(var))
  return(paste0("Daily_HR_",month,".tif"))
}

library(parallel)
months <- unique(mnths)
daily_hr_paths <- mclapply(months, FUN = process_1,
                     mc.cores = 14, mc.preschedule = FALSE)


# test <- rast("Daily_HR_April.tif")
# test2 <- as.int(test * 10)
# writeRaster(test2, datatype = "INT2S", filename = "Test_HR.tif", overwrite = T)
# test3 <- rast("Test_HR.tif")
# test3
# test3[test3 < 50] <- 50
# writeRaster(test2, datatype = "INT2S", filename = "Test_HR3.grd")
# writeCDF(test2, prec = "integer", filename = "TestHR3.nc")
# test <- daily_anom[[mnths = months[6]]]

###############now cdo##################
tbase <- 5
tut <- 35

system(paste0("./cdo_clipp_tmin.sh Daily_HR_Tmin Daily_HR_Tmin_Clipped ",tbase*10, " ",tut*10))
system(paste0("./cdo_clipp_tmax.sh Daily_HR_Tmax Daily_HR_Tmax_Clipped ",tbase*10, " ",tut*10))
system(paste0("./cdo_avg.sh Daily_HR_Tmin_Clipped Daily_HR_Tmax_Clipped HR_Clipped_Avg"))
system(paste0("./cdo_gdd.sh HR_Clipped_Avg Daily_GDD ",tbase))
system("./cdo_sum.sh Daily_GDD Final_GDD_Vars")

test <- rast("Final_GDD_Vars/Tmin_HRFebruary_clipped_avg_GDD_Sum.nc")
plot(test)

gdd_list <- list()
for(month in months){
  gdd_list[[month]] <- rast(paste0("Final_GDD_Vars/Tmin_HR",month,"_clipped_avg_GDD_Sum.nc"))
}
gdd_all <- rast(gdd_list)
plot(gdd_all[["May"]])
ann <- sum(gdd_all)
plot(ann)
gdd_all <- c(ann, gdd_all)
names(gdd_all[[1]]) <- "Annual"
writeRaster(gdd_all, paste0("/mnt/c/Users/kirid/Desktop/FFEC/Common_Files/DD_Outputs/DD_",tbase,"_",tut,"_6190.tif"))


# daily_hr <- list()
# months <- unique(mnths)
# for(i in 1:12){
#   cat(months[i], "\n")
#   hr <- resample(daily_anom[[mnths = months[i]]], prism, method = "bilinear") + prism[[i]]
#   daily_hr[[months[i]]] <- hr
# }
# 
# ###min
# dailymin <- rast("../Common_Files/PNWNAmet/PNWNAmet_tasmin.nc.nc")
# plot(dailymin[[2]])
# yrs <- time(dailymin)
# mnths <- months(yrs)
# 
# dailymin <- crop(dailymin, bb)
# 
# 
# normal_avg <- list()
# for(month in unique(mnths)){
#   cat(month)
#   avg <- mean(dailymin[[mnths == month]])
#   normal_avg[[month]] <- avg
# }
# 
# daily_anom <- list()
# for(month in unique(mnths)){
#   cat(month, "\n")
#   anom <- dailymin[[mnths == month]]
#   anom <- normal_avg[[month]] - anom
#   daily_anom[[month]] <- anom
# }
# plot(daily_anom$January[[452]])
# 
# ##prism surface
# files <- list.files("../Common_Files/colin_climatology/", full.names = T)
# tmax_files <- files[grep("Tmin",files)]
# prism_tmin <- rast(tmax_files)
# prism <- crop(prism_tmin, daily_anom[[1]])
# 
# 
# # writeCDF(prism[[1]],"Prism_Jan.nc")
# # test <- daily_anom[["January"]]
# # time(test) <- time(dailymax[[mnths == "January"]])
# # test <- project(test, "epsg:4326")
# # writeCDF(test,"Daily_Anom.nc", overwrite = T)
# 
# daily_hr_min <- list()
# months <- unique(mnths)
# for(i in 1:12){
#   cat(months[i], "\n")
#   hr <- resample(daily_anom[[mnths = months[i]]], prism, method = "bilinear") + prism[[i]]
#   daily_hr_min[[months[i]]] <- hr
# }
# 
# 
# tbase = 10; tut = 40
# 
# dd_10_40 <- list()
# for(month in unique(mnths)){
#   test_min <- daily_hr_min[[month]]
#   test_max <- daily_hr[[month]]
#   test_max[test_max < tbase] <- tbase
#   test_max[test_max > tut] <- tut
#   test_min[test_min < tbase] <- tbase
#   test_min[test_min > tut] <- tut
#   gdd <- (test_max + test_min)/2 - tbase
#   dd_10_40[[month]] <- app(gdd, sum)/30
# }
