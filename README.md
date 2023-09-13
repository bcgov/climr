# climR: Data for a Changing World

<!-- badges: start -->
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](<Redirect-URL>)
<!-- badges: end -->

# Install

```r
remotes::install_github("bcgov/climR-pnw")
```

# Usage

```r

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
gcm_ts <- gcm_ts_input(dbCon, bbox = thebb, gcm = c("ACCESS-ESM1-5", "EC-Earth3"), 
                         ssp = c("ssp370"), 
                         years = 2020:2080,
                         max_run = 0,
                         cache = TRUE)
plot(gcm_ts[[2]][[1]])

# Downscale!
results <- downscale(
  xyz = in_xyz,
  normal = normal,
  gcm = gcm,
  gcm_ts = gcm_ts,
  vars = sprintf(c("Tmax%02d"),1:12)
)

# Details about available data
list_gcm(dbCon)
list_period(dbCon)
list_run(dbCon)
list_ssp(dbCon)
list_variables()

```

# Data

This development version uses a remote PostGIS database, and optionally caches data locally.

# Adding / Modifying climate variables

All climate variables are computed via [R/append_clim_vars.R](./R/append_clim_vars.R). Add or modify entries in the variables list as needed. Use function `v` to handle dependencies (i.e. `v("Tmin")` instead of `dt$Tmin` / `dt[["Tmin"]]`).

# Lapse rates details

See lapse rates article in the vignettes folder for details.

# Use of terra package

Instead of implementing custom algorithms for bilinear interpolation and raster manipulation, this package use `terra`. Long term, it will benefit from any performance improvement in `terra`. `terra` is a replacement for the `raster` package.

Data pivot using `data.table` dcast is the most resource expensive operation of the `downscale` function according to profiling using `profvis`.

`terra` still has a couple issues that were mitigated in this package. When this is the case, function were anotated. Mainly, we are sushing `terra` functions to prevent messages print to console (see https://github.com/rspatial/terra/issues/287). We are also loading NetCDF via `raster` package as GDAL is significantly slower than the `ncdf4` implementation. Finally, NA values are not correctly transferred from disk to memory.

# climRdev
An R package for downscaled global climate model normals in the Pacific Northwest

Copyright 2021 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
