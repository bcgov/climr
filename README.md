# climr: An R package of downscaled climate data for North America

<!-- badges: start -->

[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](Redirect-URL)

<!-- badges: end -->

Copyright 2023 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.

## About climr

climr is an experimental r package that builds on the downscaling concepts operationalized in the <a href='https://climatena.ca/' target='_blank'>ClimateNA</a> tool (Wang et al. 2016). It provides downscaling of observational and simulated climate data using change-factor (a.k.a. climate imprint) downscaling, a simple method that adds low-spatial-resolution climate anomalies to a high-spatial-resolution reference climatological map, with additional elevation adjustment for "scale-free" downscaling. climr is designed to be fast and to minimize local data storage requirements. To do so, it uses a remote PostGIS database, and optionally caches data locally.

#### Features

climr provides the following data:

-   Historical observational time series (1902-2022), currently limited to the ClimateNA time series (Wang et al., 2016)

-   Multiple historical (1851-2010) and future (2001-2100) climate model simulations for each of 13 CMIP6 global climate models, in monthly time series and 20-year normals

-   User selection of single or multiple climate variables, with derived variables following the ClimateNA methodology of Wang et al. (2016).

#### Data Sources

The reference climatologies for British Columbia are the <a href='https://www.pacificclimate.org/data/prism-climatology-and-monthly-timeseries' target='_blank'>BC PRISM maps</a> provided by Pacific Climate Impacts Consortium. Reference climatologies for North America are the ClimateNA (Wang et al. 2016) mosaics of PRISM (BC, US) and WorldClim (rest of North America). The ClimateNA mosaics are accessed from <a href='https://adaptwest.databasin.org/pages/adaptwest-climatena/' target='_blank'>AdaptWest</a>.

Historical observational time series are derived using <a href='https://climatena.ca/' target='_blank'>ClimateNA</a> (Wang et al. 2016).

The CMIP6 global climate model data were downloaded and subsetted to North America by Tongli Wang, Associate Professor at the UBC Department of Forest and Conservation Sciences. The 13 global climate models selected for climr, and best practices for ensemble analysis, are described in Mahony et al. (2022).

#### References

Mahony, C.R., T. Wang, A. Hamann, and A.J. Cannon. 2022. <a href='https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/joc.7566' target='_blank'>A global climate model ensemble for downscaled monthly climate normals over North America</a>. International Journal of Climatology. 42:5871-5891. <a href='https://doi.org/10.1002/joc.7566' target='_blank'>doi.org/10.1002/joc.7566</a>

Wang T., A. Hamann, D. Spittlehouse, and C. Carroll. 2016. <a href='https://doi.org/10.1371/journal.pone.0156720' target='_blank'>Locally Downscaled and Spatially Customizable Climate Data for Historical and Future Periods for North America</a>. PLoS ONE. e0156720. <a href='https://doi.org/10.1371/journal.pone.0156720' target='_blank'>doi.org/10.1371/journal.pone.0156720</a>

## Install

``` r
remotes::install_github("bcgov/climr")
```

## Example Usage

``` r
##provide or create a long, lat, elev dataframe
in_xyz <- structure(list(Long = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                                  -127.18585, -127.1254, -126.94957, -126.95507), 
                         Lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025), 
                         Elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L)), row.names = c(NA, -8L), class = "data.frame")

##if you just want to downscale points and not think about what happening behind the scenes, use this function

res <- climr_downscale(xyz = in_xyz, which_normal = "auto", 
                       gcm_models = c("ACCESS-ESM1-5", "EC-Earth3"), 
                       ssp = c("ssp370","ssp245"), 
                       gcm_period = c("2021_2040", "2041_2060","2061_2080"),
                       #gcm_ts_years = 2020:2060,
                       max_run = 3, # we want 3 individual runs for each model
                       vars = c("PPT","CMD","CMI"))
                       
##Functions to show what data are available:

list_gcm()
list_gcm_period()
list_ssp()
list_variables()

                       
## Otherwise, you can download and investigate the normals and annomalies#####################
thebb <- get_bb(in_xyz) ##get bounding box based on input points
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
```

#### Adding / Modifying climate variables

All climate variables are computed via [R/append_clim_vars.R](./R/append_clim_vars.R). Add or modify entries in the variables list as needed. Use function `v` to handle dependencies (i.e. `v("Tmin")` instead of `dt$Tmin` / `dt[["Tmin"]]`).

#### Lapse rates details

See lapse rates article in the vignettes folder for details.

#### Use of terra package

Instead of implementing custom algorithms for bilinear interpolation and raster manipulation, this package use `terra`. Long term, it will benefit from any performance improvement in `terra`. `terra` is a replacement for the `raster` package.

Data pivot using `data.table` dcast is the most resource expensive operation of the `downscale` function according to profiling using `profvis`.
