# `climr` 0.0.2

## Enhancements
* argument options in `climr_downscale(..., which_normal)` now match the options of `normal_input(..., normal)`

## Bugfixes
* fixed temperature values of composite anomalies
* name of composite anomalies changed to "normal_composite" in `normal_input(..., normal)`.

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