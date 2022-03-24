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

# Provide or create a points dataframe (lon, lat, elev)
n <- 100000
xyz <- data.frame(lon = runif(n, -125, -120), lat = runif(n, 51, 53), elev = runif(n, 1500, 2500))

# Create a normal baseline + lapse_rates
normal <- normal_input()

# Select GCM
gcm <- gcm_input(
  gcm = c("BCC-CSM2-MR", "CanESM5", "IPSL-CM6A-LR"),
  ssp = c("ssp126", "ssp245"),
  period = "2041_2060",
  max_run = 3
)

# Use downscale
results <- downscale(
  xyz = xyz,
  normal = normal,
  gcm = gcm,
  extra = c("DD_0", "CMD")
)

# Details about available data
list_data()
list_dem()
list_gcm()
list_normal()
list_period()
list_ssp()
list_variables()

```

# climR-pnw
An R package for downscaled global climate model normals in the Pacific Northwest

Copyright 2021 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
