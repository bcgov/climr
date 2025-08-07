## Troubleshooting a mystery bug with the MIROC6 data. 
library(climr)
library(data.table)
library(ggplot2)
library(terra)
# data frame of arbitrary points
my_points <- data.frame(
  lon = c(-127.7300),
  lat = c(55.34114),
  elev = c(711),
  id = 1
)

dat <- rast("../Common_Files/northamerica_elevation_cec_2023.tif")
d2 <- project(dat, "EPSG:4326", res = 0.0022)
d2 <- rast("../Common_Files/NA_DEM_250.tif")

d1 <- plot_bivariate_input(my_points)
plot_bivariate(d1)

system.time(
  my_data <- plot_timeSeries_input(my_points, gcms = list_gcms()[10])
)

# use the input to create a plot
plot_timeSeries(my_data, var1 = "Tmax_sm", simplify = F,
                gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)[6]]
)

dat1 <- downscale(my_points, gcms = list_gcms()[10], ssps = list_ssps()[1], 
                  max_run = 4, gcm_ssp_years = 2015:2050, vars = "Tmax_sm", db_option = "database")
dat1 <- dat1[PERIOD != "1961_1990"]
dat1[, PERIOD := as.numeric(PERIOD)]
dat1[,ssp_run := paste0(SSP,"_", RUN)]

ggplot(dat1, aes(x = PERIOD, y = Tmax_sm)) +
  geom_line() +
  facet_wrap(~ssp_run)

#######################################################
dat1 <- downscale(my_points, gcms = list_gcms()[10], ssps = list_ssps()[1], 
                  run_nm = "r10i1p1f1", ensemble_mean = FALSE, gcm_ssp_years = 2018:2022, vars = c("Tmax_08"),
                  db_option = "database")
dat1 <- dat1[PERIOD != "1961_1990"]
dat1[, PERIOD := as.numeric(PERIOD)]
dat1[,ssp_run := paste0(SSP,"_", RUN)]

ggplot(dat1, aes(x = PERIOD, y = Tmax_08)) +
  geom_line() +
  facet_wrap(~ssp_run)

####testing
labels <- vapply(
  strsplit(names(res), "_"),
  function(x) {
    paste0(x[1:2], collapse = "_")
  },
  character(1)
)

labels_cr <- vapply(
  strsplit(names(climaterast), "_"),
  function(x) {
    paste0(x[2:3], collapse = "_")
  },
  character(1)
)

if (type %in% c("obs")) {
  ## Create match set to match with res names
  labels <- nm
}
setcolorder(res, neworder = c(1,(match(labels_cr[-1], labels[-1])+1)))  
##############################
my_points <- data.frame(
  lon = c(-127.7300, -127.5),
  lat = c(55.34114, 55.6),
  elev = c(711,711),
  id = 1:2
)
bb <- get_bb(my_points)

dat2 <- input_gcm_ssp(bbox = bb, 
                      gcms = list_gcms()[10], 
                      ssps = list_ssps()[1],
                      years = 2018:2025,
                      run_nm = "r10i1p1f1",
                      ensemble_mean = FALSE)
dat2 <- dat2$MIROC6
nms <- data.table(layer = 1:nlyr(dat2))
nms[,c("Mod","Var","Month","Ssp","Run","Year") := tstrsplit(names(dat2), "_")]
dat3 <- dat2[[nms[Var == "Tmax" & Month == "08" & Year %in% 2018:2022,layer]]]
plot(dat3)

system.time(
  # generate the input data
  my_data_reduced_2 <- plot_timeSeries_input(my_points,
                                             gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
                                             ssps = list_ssps()[1:3],
                                             max_run = 10,
                                             obs_ts_dataset = c("mswx.blend", "cru.gpcc", "climatena"),
                                             obs_years = list_obs_years(),
                                             gcm_hist_years = list_gcm_hist_years(),
                                             gcm_ssp_years = list_gcm_ssp_years(),
                                             vars = list_vars()
  )
)

system.time(
  # generate the input data
  my_data_reduced_3 <- plot_timeSeries_input(my_points,
                                             gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)],
                                             ssps = list_ssps()[1:3],
                                             max_run = 4,
                                             obs_ts_dataset = c("mswx.blend", "cru.gpcc", "climatena"),
                                             obs_years = list_obs_years(),
                                             gcm_hist_years = list_gcm_hist_years(),
                                             gcm_ssp_years = list_gcm_ssp_years(),
                                             vars = list_vars()
  )
)

# use the input to create a plot
plot_timeSeries(my_data_reduced_2, var1 = "Tmax_sm", simplify = F,
                gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)[6]]
)

# use the input to create a plot
plot_timeSeries(my_data_reduced_3, var1 = "Tmax_sm", simplify = F,
                gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)[6]]
)



# -----------------
# plot directly from the data object to isolate the issue


# base case
system.time(
  test <- plot_timeSeries_input(my_points)
)

# miroc6 only
system.time(
  test_1 <- plot_timeSeries_input(my_points,
                                  gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)[6]],
                                  ssps = list_ssps()[1:3],
                                  max_run = 10
  )
)

# identical run to test stochasticity
system.time(
  test_1.2 <- plot_timeSeries_input(my_points,
                                    gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)[6]],
                                    ssps = list_ssps()[1:3],
                                    max_run = 10
  )
)

# add another gcm
system.time(
  test_1.3 <- plot_timeSeries_input(my_points,
                                    gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)[5:6]],
                                    ssps = list_ssps()[1:3],
                                    max_run = 10
  )
)

# add ssp585
system.time(
  test_2 <- plot_timeSeries_input(my_points,
                                  gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)[6]],
                                  ssps = list_ssps()[1:4],
                                  max_run = 10
  )
)

# ssp126 only
system.time(
  test_3 <- plot_timeSeries_input(my_points,
                                  gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)[6]],
                                  ssps = list_ssps()[1],
                                  max_run = 10
  )
)

# plot of ssp126 only showing confounded ensemble mean
plot_timeSeries(test_3, var1 = "Tmin_sm", simplify = F,
                gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)[6]]
)

# local files instead of database
system.time(
  test_4 <- plot_timeSeries_input(my_points,
                                  gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)[6]],
                                  ssps = list_ssps()[1],
                                  max_run = 10, 
                                  db_option = "local"
  )
)


#plot comparing outputs from different input queries   
par(mar=c(2,3,0.1,1), mfrow=c(6,1))
x <- 2015:2100
runs <- unique(test_1.2$RUN)
var="Tmax_sm"
for(run in runs[2:7]){
  y0 <- test[GCM=="MIROC6" & SSP=="ssp126" & RUN==run, get(var)]
  y1 <- test_1[SSP=="ssp126" & RUN==run, get(var)]
  y1.2 <- test_1.2[SSP=="ssp126" & RUN==run, get(var)]
  y1.3 <- test_1.3[GCM=="MIROC6" & SSP=="ssp126" & RUN==run, get(var)]
  y2 <- test_2[SSP=="ssp126" & RUN==run, get(var)]
  y3 <- test_3[SSP=="ssp126" & RUN==run, get(var)]
  y4 <- test_3[SSP=="ssp126" & RUN==run, get(var)]
  yrange <- range(c(y1,y1.2,y2,y3))
  plot(x, y0, type="l", ylim=yrange, ylab=var, col="black", lty=1, lwd=2)
  lines(x, y1, col="black", lty=2, lwd=2)
  lines(x, y1.2, col="red", lty=1, lwd=2)
  lines(x, y1.2, col="red", lty=2, lwd=2)
  lines(x, y2, col="green", lty=1, lwd=2)
  lines(x, y3, col="purple1", lty=1, lwd=1)
  lines(x, y4, col="black", lty=2, lwd=2)
  legend("bottomright", legend=run, bty="n")
}

# 1. the error is deterministic, not random; it is consistent with same inputs
# 2. the error is not produced by changing GCMs.
# 3. the error occurs when ssp inputs are changed.
# 4. the error is not limited to individual years
# 5. downscaling ssp126 alone affected the entire ensemble mean.
# 6. error is same with local vs db downscaling
# 7. the years and runs affected are the same for Tmin and Tmax, and for different seasons
# 8. i tried another distant location in southeastern BC and the pattern was the same, so it's not isolated to a single GCM grid cell.

# -----------------
# try the raw (undownscaled) data

raw_3 <- input_gcm_ssp(
  get_bb(my_points),
  gcms = list_gcms()[c(1, 4, 5, 6, 7, 10, 11, 12)[6]],
  ssps = list_ssps()[1],
  years = 2018:2030,
  ensemble_mean = TRUE,
  max_run = 5
)

data <- extract(unlist(raw_3$MIROC6), my_points[,1:2])

# Extract column names (excluding ID)
col_names <- names(data)[-1]

# Parse the names into components
parts <- strsplit(col_names, "_")

# Build a data.frame from parsed components
meta <- do.call(rbind, parts)
colnames(meta) <- c("GCM", "Element", "Month", "Scenario", "Run", "Year")

# Construct the long data.frame
long_data <- data.frame(
  ID    = rep(data$ID, each = length(col_names)),
  GCM   = meta[, "GCM"],
  Element = meta[, "Element"],
  Month = as.integer(meta[, "Month"]),
  Scenario = meta[, "Scenario"],
  Run  = meta[, "Run"],
  Year  = as.integer(meta[, "Year"]),
  value = as.numeric(unlist(data[, -1]))
)

long_data <- data.table(long_data)

par(mar=c(2,3,0.1,1), mfrow=c(6,1))
runs <- unique(long_data$Run)
for(run in runs){
  select <- long_data[Element == "Tmax" & Month == 7 & Run == run]
  x <- select$Year
  y <- select$value
  plot(x,y, type="l")
  legend("bottomright", legend=run, bty="n")
}

