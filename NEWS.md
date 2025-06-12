# `climr` 0.2.1

## Bug Fixes

-   

## Enhancements

-   New function "plot_WalterLieth" for creating a Walter-Lieth climate diagram for a specified location(s) and time periods. 

# `climr` 0.2.0

We have lots of exciting improvements in this release! Of major note, we now have:
- Native raster downscaling, allowing SpatRaster elevation input, and returning a SpatRaster stack of downscaled variables at user-specified resolution
- Optional in-database downscaling. This is especially useful for reducing the time required for downscaling timeseries data
- An updated climr mosaic base climatology, covering the full North America domain, and using some cool generative deep-learning models for filling in missing areas
- A third timeseries dataset based on MSWX/MSWEP

## Breaking Changes
- We having improved the database connection algorithm, and have removed the `dbCon` parameter from all `input_*` functions. If you supply a database connection, it will give an error. 
- Previously, `climr` expected the bounding box vector to be in the reverse of the standard order. We have changed this so you can now directly supply a `terra::ext` object. 
- Since all base climatologies now cover the full NA domain, we have removed the `auto` option from `which_refmap`. The default is now `refmap_climr`. 

## Bug Fixes

-   Fixed a bug that caused failure in downloads of gcm time series. 
-   Fixed a bug in the CNRM-ESM2-1 future time series

## Enhancements

-   Added a new parameter `db_option` to downscale, with options `local`, `database`, or `auto` to specify where the downscaling should occur. Option `auto` attempts to choose the best option based on the number of points and type of query. 
-   Added a new boolean parameter `ensemble_mean` to downscale, to optionally not return the ensemble mean of model runs. 
-   Expanded the climr mosaic of 1981-2010 gridded climatologies to the full North American domain. Learn about it [here](https://bcgov.github.io/climr/articles/methods_mosaic.html).
-   Added a 1901-2024 gridded observational time series based on the multi-source weather (MSWX/MSWEP) dataset. Learn about it [here](https://bcgov.github.io/climr/articles/guidance_timeSeries.html)
-   New function `legend_ramp()` for simplified legend plotting on color ramp maps, including accommodation of logarithmic scaling. 
-   New function `logVars()` for logarithmic transformation of specified columns in any data.table. 
-   Added a Known Issues section to the vignettes, including the [FFP, NFFD, and PAS discrepancies with ClimateNA](https://bcgov.github.io/climr/articles/knownIssues_DiscrepanciesWithClimateNA.html) and a [workaround for using custom refmaps](https://bcgov.github.io/climr/articles/knownIssues_customRefmap.html).

# `climr` 0.1.1

## Bug Fixes

-   Modified the Hargreaves method for calculation of monthly solar radiation to allow for calculation of Eref and CMD above the arctic circle in a way that matches ClimateNA results.
-   Fixed an edge case in the tiling that resulted in NA values in southern Mexico if full North American extent was queried.
-   Fixed bug in caching where cache would fail due to incorrect folder name in certain cases.
-   Fixed a bug in `plot_downscale()` that affected colors when `pal = "gcms"` and labels when `endlabel = "gcms"`
-   Fixed a bug where climr didn't download the correct run names for historic GCMs
-   Modified the equations for frost-free period so that they are confined to a range of 0-365.

## Enhancements

-   Redesigned the database to substantially improve speed of downloading gcm ssp timeseries, especially for a small number of points.
-   Added `run_nm` parameter to `downscale` and `input_gcm_*` functions so users can specify desired run(s). Also added `list_runs_ssp()` and `list_runs_historic()`

## Known issues

-   There are discrepancies between climr and ClimateNA in values of frost-free period (eFFP, bFFP, and FFP) and frost-free days (NFFD). For FFP see issue [#297](https://github.com/bcgov/climr/issues/297); For NFFD see issue [#295](https://github.com/bcgov/climr/issues/295). We will attempt to resolve these differences in a future version.

# `climr` 0.1.0

## Implementation of naming conventions

-   We overhauled the naming of functions, parameters, and options to make them more intuitive and internally consistent. You will need to revise the climr code in your workflows to accommodate these changes. A table of correspondence specifying the changes is located at ./data-raw/namingChanges.csv and is viewable by calling `data(name_changes)`.
-   We changed the climate variable naming convention so that the climate element and the time of year are always separated by an underscore. e.g., Tmin01 becomes Tmin_01; DD_0_01 becomes DDsub0_01. The variables table called by `data(variables)` now has a field "Code_ClimateNA" with the variable codes used by ClimateBC/NA to allow users to crosswalk the two conventions.

## User Actions Required

-   To implement this new version, users must clear their cache of climate data by running the following line of code: cache_clear(). We will do our best to avoid the need for wholesale cache-clearing in the future.

## Bug Fixes

-   Fixed an error in the calculation of Hogg's climatic moisture index (CMI). This error was inherited from an unreported error in Equation 3 of Hogg (1997).\
-   Fixed an error in the calculation of precipitation as snow. The PAS parameters in ClimateNA and `climr` differ from those originally published in Wang et al. (2016).
-   fixed an out-of-bounds error that affected user queries close to the coastline.
-   Added missing future 2015-2100 time series for the GFDL-ESM4 climate model.

## Enhancements

-   Added `plot_timeSeries()` and `plot_timeSeries_input()` functions to generate plots of 20th and 21st century climate change for user-selected locations and climate variables.
-   Added a 1901-2022 observational time series of for the combined Climatic Research Unit TS dataset (for Temperature) and Global Precipitation Climatology Centre dataset (for precipitation).
-   Extended the ClimateNA observational time series to 1901-2023.
-   Added a vignette (article) providing guidance for climate model ensemble selection and emissions scenario selection.
-   Added a vignette (article) on the methods used to select the 13 global climate models provided by climr, and the 8-model ensemble recommended for most purposes.

## Known issues

-   Downloads of time series take a long time. We are looking into ways to speed this up, but until then we recommend users dedicate some time prior to analysis to cache their time series of interest for their areas of interest in a batch. Once the time series are cached, they don't need to be downloaded again.
-   Related to the issue of time series download speed, the `plot_timeSeries_input()` function can take \>1hr to run for the first time it is called for a location.

# `climr` 0.0.4

## Bug Fixes

-   Updated future timeseries data to include full 2015-2100 period and added missing models (built some cool bash scrips using `parallel` to quickly to `raster2pgsql` conversion for large numbers of files).
-   Updated historic modelled timeseries to extend to December 31, 2014.
-   Restructured naming scheme for timeseries data, updated internal `dbnames` table, and updated `postgresql` functions to allow hyphens in table names.
-   Reprocessed future GCM periods to include all of North America.
-   Fixed bug in `plot_bivariate()` for focal periods after 2001-2020.
-   Fixed caching issue where it would fail for very larger numbers of layers (\>65000) by saving as .gri binary files.

## Enhancements

-   Added checks for bounding box projection.

# `climr` 0.0.3

## Enhancements

-   new tests comparing to reference outputs
-   code further streamlined
-   new messages warn user about meaningless `downscale`/`climr_downscale` argument combinations
-   argument options in `climr_downscale(..., which_normal)` now match the options of `normal_input(..., normal)`
-   add `plot_bivariate()` function to generate plots showing climate model ensemble variation in recent and future climate change.
-   new functions `list_historic_ts` and `list_gcm_ts` to get available years for historic/future time series

## Behaviour changes

-   `xyz` (argument to `climr_downscale` and `downscale`) and `in_xyz` (argument to `get_bb`), must now be a 4 column `data.table` (or coercible class) with `lon`, `lat`, `elev` and `id` columns. All other columns are ignored and NOT returned. Column order no longer matters.

## Bugfixes

-   cache fixes
-   fixing geographical checks to get highest resolution beyond BC, Canada
-   fixing `historic_input_ts` to get only queried years
-   `get_bb` follows column names

## Other

-   `climr_downscale` now accepts `...` to pass arguments to `downscale`

# `climr` 0.0.2

## Bugfixes

-   fixed temperature values of composite anomalies
-   name of composite anomalies changed to "normal_composite" in `normal_input(..., normal)`.

## Documentation

-   added vignettes
-   `pkgdown` website for `climr` is live

# `climr` 0.0.1

## Enhancements

-   continuous testing implemented via GitHub Actions
-   code was cleaned up following `tidyverse` syntax recommendations
-   internal function definitions are now avoided
-   improvements to function documentation
-   removal of deprecated functions
-   increased code coverage
-   some code streamlining
-   added new composite climatologies of Western Canada and Western US.

## Bugfixes

-   fixed examples
-   fixed incomplete changing of package name (`climRpnw` to `climr`)
-   added missing pkg imports
-   fixed caching problem where cached objects were not being retrieved when the PostGIS server was unavailable
-   fixed model names in PostGIS server, which fixed bugs in `gcm_hist_input` and `gcm_ts_input`.

## Dependency changes

-   `methods` removed from Imports
-   `sf` added to Imports

## Other

-   old data base access functions removed

# `climr` 0.0.0.9990

Beta-version
