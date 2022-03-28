<!-- badges: start -->
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](<Redirect-URL>)
<!-- badges: end -->

# Install

```r
remotes::install_github("bcgov/climR-pnw")
```

# Usage

```r
# Retrieve package data
data_update()

# Create a normal baseline
normal <- normal_input()

# Select GCM
gcm <- gcm_input(
  gcm = c("BCC-CSM2-MR"),
  ssp = c("ssp126"),
  period = "2041_2060",
  max_run = 0
)

# Provide or create a points dataframe (lon, lat, elev)
n <- 100000
xyz <- data.frame(lon = runif(n, -125, -120), lat = runif(n, 51, 53), elev = numeric(n))
xyz[,3] <- terra::extract(attr(normal, "dem"), xyz[,1:2], method = "bilinear")[,-1L]

# Use downscale
results <- downscale(
  xyz = xyz,
  normal = normal,
  gcm = gcm,
  var = c("Tmax01", "DD5_01")
)

# Details about available data
list_data()
list_dem()
list_gcm()
list_normal()
list_period()
list_run()
list_ssp()
list_variables()

```

# Data

See [data branch README.md](https://github.com/bcgov/climR-pnw/blob/data/README.md) for details.

# Adding / Modifying climate variables

All climate variables are computed via [R/append_clim_vars.R](./R/append_clim_vars.R). Add or modifying entries in the variables list. If you need to rely on other variables , use function `v` to access them. This will handle variable dependency for you.

# Caching

This package use local caching, either permanent or in a temporary folder. The data source is currently GitHub, this could be modified by implementing another `content_get` type function. This functions should return a data.table with 3 columns, `url`, `path` and `uid`.
See `?content_get`.

# Data functions

Use `data_update()` to download data locally (required), `data_path()` to get current data local location, `list_data()` to list all downloaded files and `data_delete()` to remove downloaded data.

# Lapse rates details

See lapse rates vignettes for details.

# Use of terra package

Instead of implementing custom algorithms for bilinear interpolation and raster manipulation, this package use `terra` package. Long term, it will benefit from any performance improvement in `terra`. `terra` replaces `raster` package.

The most resources expensive operation of `downscale` is the data pivots using `data.table` dcast, so I would not be too worried about
`terra` performances.

`terra` still has a couple issues that were mitigated in this package. When this is the case, function were anotated. Mainly, we are sushing `terra` functions to prevent messages print to console (see https://github.com/rspatial/terra/issues/287). We are also loading NetCDF via `raster` package as GDAL is significantly slower than `ncdf4` implementation + NA values are not correctly transferred from disk to memory.

# climR-pnw
An R package for downscaled global climate model normals in the Pacific Northwest

Copyright 2021 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
