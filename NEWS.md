# `climr` 1.0.0
## Enhancements
* new functions `list_historic_ts` and `list_gcm_ts` to get available years for historic/future time series

# `climr` 0.0.4
## Bug Fixes
* Updated future timeseries  data to include full 2015-2100 period and added missing models. 
* Restructured naming scheme for timeseries data, updated internal `dbnames` table, and updated `postgresql` functions to allow hyphens in table names. 

# `climr` 0.0.3
## Enhancements
* new tests comparing to reference outputs
* code further streamlined
* new messages warn user about meaningless `downscale`/`climr_downscale` argument combinations
* argument options in `climr_downscale(..., which_normal)` now match the options of `normal_input(..., normal)`
* add `plot_bivariate()` function to generate plots showing climate model ensemble variation in recent and future climate change. 

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