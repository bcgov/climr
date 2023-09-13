# climR: Data for a Changing World

<!-- badges: start -->
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](<Redirect-URL>)
<!-- badges: end -->

# Install

```r
remotes::install_github("kdaust/climR-dev")
```

# Usage

```r

# Provide or create a points dataframe (lon, lat, elev)

thebb <- get_bb(in_xyz) ##get bounding box based on input points
dbCon <- data_connect() ##connect to database
normal <- normal_input_postgis(dbCon = dbCon, bbox = thebb) ##get normal data and lapse rates
plot(normal[[1]])

##get GCM anomolies
gcm <- gcm_input_postgis(dbCon, bbox = thebb, gcm = c("BCC-CSM2-MR","UKESM1-0-LL"), ssp = "ssp245", period = c("2021_2040","2041_2060","2061_2080"))
plot(gcm[[1]][[1]])

# Downscale!
results <- downscale(
  xyz = in_xyz,
  normal = normal,
  gcm = gcm,
  vars = c("CMD_sp","Tave_wt","PPT_sp")
)

poolClose(dbCon)

# Details about available data
list_data()
list_gcm()
list_normal()
list_period()
list_run()
list_ssp()
list_variables()

```

# Data

This development version switches to a remote Postgis database for getting data. Data are not currently cached, but will be in the future.

# Adding / Modifying climate variables

All climate variables are computed via [R/append_clim_vars.R](./R/append_clim_vars.R). Add or modify entries in the variables list as needed. Use function `v` to handle dependencies (i.e. `v("Tmin")` instead of `dt$Tmin` / `dt[["Tmin"]]`).

# Data functions

Use `data_update()` to download data locally (required), `data_path()` to get local data path, `list_data()` to list all local files and `data_delete()` to remove package local data.

# Lapse rates details

See lapse rates article in the vignettes folder for details.

# Use of terra package

Instead of implementing custom algorithms for bilinear interpolation and raster manipulation, this package use `terra`. Long term, it will benefit from any performance improvement in `terra`. `terra` is a replacement for the `raster` package.

Data pivot using `data.table` dcast is the most resource expensive operation of the `downscale` function according to profiling using `profvis`.

`terra` still has a couple issues that were mitigated in this package. When this is the case, function were anotated. Mainly, we are sushing `terra` functions to prevent messages print to console (see https://github.com/rspatial/terra/issues/287). We are also loading NetCDF via `raster` package as GDAL is significantly slower than the `ncdf4` implementation. Finally, NA values are not correctly transferred from disk to memory.

# climR-dev
An R package for downscaled global climate model normals in the Pacific Northwest

Copyright 2021 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
