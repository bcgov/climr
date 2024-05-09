# `climr` 0.0.5
## Modifications to naming conventions
* changed the climate variable naming convention so that the climate element and the time of year are always separated by an underscore. e.g., Tmin01 becomes Tmin_01; DD_0_01 becomes DDsub0_01. The variables table called by `data(variables)` now has a field "Code_ClimateNA" with the variable codes used by ClimateBC/NA to allow users to crosswalk the two conventions. 

## User Actions Required
* to implement the out-of-bounds bug fix, users will need to discard their cached reference normals by running the following line of code: cache_clear("normal") 

## Bug Fixes
* fixed an out-of-bounds error that affected user queries close to the coastline. 
* Added missing future 1850-2100 time series for the GFDL-ESM4 climate model.  

## Enhancements
* Added `plot_timeSeries()` and `plot_timeSeries_input()` functions to generate plots of 20th and 21st century climate change for user-selected locations and climate variables.
* Added a 1901-2022 observational time series of for the combined Climatic Research Unit TS dataset (for Temperature) and Global Precipitation Climatology Centre dataset (for precipitation). 
* Extended the ClimateNA observational time series to 1901-2023. 

# `climr` 0.0.4
## Bug Fixes
* Updated future timeseries data to include full 2015-2100 period and added missing models (built some cool bash scrips using `parallel` to quickly to `raster2pgsql` conversion for large numbers of files). 
* Updated historic modelled timeseries to extend to December 31, 2014. 
* Restructured naming scheme for timeseries data, updated internal `dbnames` table, and updated `postgresql` functions to allow hyphens in table names. 
* Reprocessed future GCM periods to include all of North America. 
* Fixed bug in `plot_bivariate()` for focal periods after 2001-2020. 
* Fixed caching issue where it would fail for very larger numbers of layers (>65000) by saving as .gri binary files.

## Enhancements
* Added checks for bounding box projection.

# `climr` 0.0.3
## Enhancements
* new tests comparing to reference outputs
* code further streamlined
* new messages warn user about meaningless `downscale`/`climr_downscale` argument combinations
* argument options in `climr_downscale(..., which_normal)` now match the options of `normal_input(..., normal)`
* add `plot_bivariate()` function to generate plots showing climate model ensemble variation in recent and future climate change. 
* new functions `list_historic_ts` and `list_gcm_ts` to get available years for historic/future time series

## Behaviour changes
* `xyz` (argument to `climr_downscale` and `downscale`) and `in_xyz` (argument to `get_bb`), must now be a 4 column `data.table` (or coercible class) with `lon`, `lat`, `elev` and `id` columns. All other columns are ignored and NOT returned. Column order no longer matters.

## Bugfixes
* cache fixes
* fixing geographical checks to get highest resolution beyond BC, Canada
* fixing `historic_input_ts` to get only queried years
* `get_bb` follows column names

## Other
* `climr_downscale` now accepts `...` to pass arguments to `downscale`

# `climr` 0.0.2

## Bugfixes
* fixed temperature values of composite anomalies
* name of composite anomalies changed to "normal_composite" in `normal_input(..., normal)`.

## Documentation
* added vignettes
* `pkgdown` website for `climr` is live

# `climr` 0.0.1

## Enhancements
* continuous testing implemented via GitHub Actions
* code was cleaned up following `tidyverse` syntax recommendations
* internal function definitions are now avoided
* improvements to function documentation
* removal of deprecated functions
* increased code coverage
* some code streamlining
* added new composite climatologies of Western Canada and Western US.

## Bugfixes
* fixed examples
* fixed incomplete changing of package name (`climRpnw` to `climr`)
* added missing pkg imports
* fixed caching problem where cached objects were not being retrieved when the PostGIS server was unavailable 
* fixed model names in PostGIS server, which fixed bugs in `gcm_hist_input` and `gcm_ts_input`.

## Dependency changes
* `methods` removed  from Imports
* `sf` added to Imports

## Other
* old data base access functions removed

# `climr` 0.0.0.9990

Beta-version
