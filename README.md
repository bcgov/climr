# `climr` <img src="logo.svg" align="right" alt="" width="120"/>

<!-- badges: start -->

[![Lifecycle:Maturing](https://img.shields.io/badge/Lifecycle-Maturing-007EC6)](<Redirect-URL>)
[![Codecov test coverage - main](https://codecov.io/gh/bcgov/climr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/bcgov/climr/?branch=main)

<!-- badges: end -->

## `climr`: An R package of downscaled climate data for North America

`climr` is an experimental R package that builds on the downscaling concepts operationalized in the <a href='https://climatena.ca/' target='_blank'>ClimateNA</a> tool (Wang et al. 2016).
It provides downscaling of observational and simulated climate data using change-factor (a.k.a. climate imprint) downscaling, a simple method that adds low-spatial-resolution climate anomalies to a high-spatial-resolution reference climatological map, with additional elevation adjustment for "scale-free" downscaling.
`climr` is designed to be fast and to minimize local data storage requirements.
To do so, it uses a remote PostGIS database, and optionally caches data locally.

### Subscribe

We are actively developing `climr` and releasing minor versions every month or two. 
If you would like to receive email updates when new versions of climr are released, 
subscribe to the `climr` GitHub repo using the following steps: 

1. Navigate to [https://github.com/bcgov/climr](https://github.com/bcgov/climr). 
2. Click the "Watch" button at the top right of the repository page.
3. Choose "Custom".
4. Select "Releases".

### Features

`climr` provides the following data:

-   Two historical observational time series: (1) the 1901-2022 combined Climatic Research Unit TS dataset 
(for temperature) and Global Precipitation Climatology Centre dataset (for precipitation) and (2) the 1901-2023 ClimateNA time series 
(Wang et al., 2024). 

-   Multiple historical (1851-2014) and future (2015-2100) climate model simulations 
for each of 13 CMIP6 global climate models, in monthly time series and 20-year normals.

-   User selection of single or multiple climate variables, with derived variables 
following the ClimateNA methodology of Wang et al. (2016).

### Data Sources

The default reference climate maps for North America are a 
custom 2.5km-resolution mosaic of [BC PRISM](https://www.pacificclimate.org/data/prism-climatology-and-monthly-timeseries), 
[US PRISM](https://prism.oregonstate.edu/normals/), 
deep learning prediction (Yukon, Northwest Territories, and Alberta), and 
[Daymet](https://daymet.ornl.gov/overview) (rest of North America). 
The climr mosaic is described in [`vignette("climr_methods_mosaic")`](https://bcgov.github.io/climr/articles/methods_mosaic.html). 
The alternative 4km-resolution ClimateNA mosaics of PRISM (BC, US, W. Canada) and WorldClim (rest of North America) are accessed from [AdaptWest](https://adaptwest.databasin.org/pages/adaptwest-climatena/).

The default historical observational time series are obtained from [Climatic Research Unit](https://crudata.uea.ac.uk/cru/data/hrg/), [Global Precipitation Climatology Centre](https://psl.noaa.gov/data/gridded/data.gpcc.html), and [ClimateNA](https://climatena.ca/) (Wang et al. 2016), . 

CMIP6 global climate model simulations were downloaded from the [Earth System Grid Federation](https://aims2.llnl.gov/search/cmip6). The majority of these downloads were conducted by Tongli Wang, Associate Professor at the UBC Department of Forest and Conservation Sciences.
The 13 global climate models selected for `climr`, and best practices for ensemble analysis, are described in Mahony et al. (2022) and summarized in [`vignette("climr_methods_ensembleSelection")`](https://bcgov.github.io/climr/articles/methods_ensembleSelection.html). 

## Installation

`climr` is only available on GitHub. To install please use:

``` r
remotes::install_github("bcgov/climr")
```

If you want to install the development version:

``` r
remotes::install_github("bcgov/climr@devl")
```

## Usage

See `vignette("climr_workflow_beg")` to get started with simple `climr` workflows;

## Methods

For an overview of downscaling methods used in `climr` see `vignette("methods_downscaling")`

## Known issues

-   Downloads of time series take a long time. We recommend users dedicate some time prior to analysis to cache their time series of interest for their areas of interest in a batch. Once the time series are cached, they don't need to be downloaded again. 
-   We are still working on the documentation, examples, and vignettes. Please let us know if something isn't clear, preferably as a [GitHub](https://github.com/bcgov/climr) issue. 

### License

Copyright 2024 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License.
You may obtain a copy of the License at

<http://www.apache.org/licenses/LICENSE-2.0>

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and limitations under the License.

`climr` logo uses icon designed by Freepik, Flaticon.com, available [here](https://www.flaticon.com/free-icon/pin_6093139).

### Acknowledgements

We acknowledge the World Climate Research Programme, which, through its Working Group on Coupled Modelling, coordinated and promoted CMIP6. We thank the climate modeling groups for producing and making available their model output, the Earth System Grid Federation (ESGF) for archiving the data and providing access, and the multiple funding agencies who support CMIP6 and ESGF. 

### References

Mahony, C.R., T. Wang, A. Hamann, and A.J. Cannon. 2022. [A global climate model ensemble for downscaled monthly climate normals over North America](https://rmets.onlinelibrary.wiley.com/doi/full/10.1002/joc.7566). International Journal of Climatology. 42:5871-5891. [doi.org/10.1002/joc.7566](https://doi.org/10.1002/joc.7566)

Wang, Tongli, Andreas Hamann, Dave Spittlehouse, and Carlos Carroll. 2016. “Locally Downscaled and Spatially Customizable Climate Data for Historical and Future Periods for North America.” Edited by Inés Álvarez. PLOS ONE 11 (6): [e0156720](https://doi.org/10.1371/journal.pone.0156720).

Wang, Tongli, Andreas Hamann, and Zihaohan Sang. 2024. “Monthly High-Resolution Historical Climate Data for North America Since 1901.” International Journal of Climatology. early view: [https://doi.org/10.1002/joc.8726](https://doi.org/10.1002/joc.8726).