#' Example line data
#'
#' Shapefile of coast of Vancouver, BC, Canada. Mostly
#' used for plotting.
#' To use this data set, you need to `terra::unwrap`
#' it first, e.g.:
#' `vancouver <- terra::unwrap(get(data(vancouver)))`
#'
#' @format `PackedSpatVector`
#' @source todo add/describe source
"vancouver"


#' Example DEM
#'
#' Digital elevation model of North Vancouver, BC, Canada.
#' To use this data set, you need to `terra::unwrap`
#' it first, e.g.:
#' `dem_vancouver <- terra::unwrap(get(data(dem_vancouver)))`
#'
#' @format a `PackedSpatRaster`
#' @source todo add/describe source
"dem_vancouver"

#' Example low resolution DEM
#'
#' A lower resolution version of `dem_vancouver`,
#' re-projected at 0.005 degrees.
#' To use this data set, you need to `terra::unwrap`
#' it first, e.g.:
#' `dem_vancouver_lowres <- terra::unwrap(get(data(dem_vancouver_lowres)))`
#'
#' @format a `PackedSpatRaster`
#' @source todo add/describe source
"dem_vancouver_lowres"


#' Example point locations
#'
#' Shapefile with point locations North Vancouver, BC, Canada.
#' To use this data set, you need to `terra::unwrap`
#' it first, e.g.:
#' `vancouver_points <- terra::unwrap(get(data(vancouver_points)))`
#'
#' @format `PackedSpatVector`
#' @source derived from `dem_vancouver`
"vancouver_points"

#' Example polygon locations
#'
#' Shapefile of North Vancouver in polygon format, BC, Canada.
#' To use this data set, you need to `terra::unwrap`
#' it first, e.g.:
#' `vancouver_poly <- terra::unwrap(get(data(vancouver_poly)))`
#'
#' @format `PackedSpatVector`
#' @source derived from `dem_vancouver`
"vancouver_poly"

#' Example Biogeoclimatic Ecological Classification Zones
#'
#' A shapefile with biogeoclimatic zones in the coast of Vancouver, BC, Canada.
#' To use this data set, you need to `terra::unwrap`
#' it first, e.g.:
#' `BECz_vancouver <- terra::unwrap(get(data(BECz_vancouver)))`
#'
#' @format a `PackedSpatVector`
#' @source obtained from `bcdata` package, using `bcdc_get_data("bec-map-generalized-1-20k-")`
"BECz_vancouver"


#' Example Biogeoclimatic Ecological Classification Zones in raster format
#'
#' A categorical raster of biogeoclimatic zones in the coast of Vancouver, BC, Canada.
#' Derived from `BECz_vancouver` using `dem_vancouver` as the template for extent,
#' resolution and projection information.
#' To use this data set, you need to `terra::unwrap`
#' it first, e.g.:
#' `BECz_vancouver_ras <- terra::unwrap(get(data(BECz_vancouver_ras)))`
#'
#' @format a `PackedSpatRaster`
#' @source derived from `BECz_vancouver`
"BECz_vancouver_ras"


#' Colour sscheme for Biogeoclimatic Ecological Classification Zones
#'
#' A `data.table` with RGB and HEX colours for biogeoclimatic zones in BC.
#'
#' @format a `data.table`
#' @source colour scheme commonly used by the BC provincial government.
"BECcols"


#' BC Weather Station Data - PCDS
#'
#' A points shapefile of weather stations in BC from the
#' Pacific Climate Impacts Consortium. Stations without elevation
#' data were excluded.
#'
#' To use this data set, you need to `terra::unwrap`
#' it first, e.g.:
#' `weather_stations <- terra::unwrap(get(data(weather_stations)))`
#'
#' @format a `PackedSpatVector`
#' @source [https://services.pacificclimate.org/met-data-portal-pcds/app/](https://services.pacificclimate.org/met-data-portal-pcds/app/)
"weather_stations"

#' Example table of point locations
#'
#' A `data.table` of point locations with elevation.
#' It is a simplified and cleaned-up version of the `weather_stations` dataset,
#' where incomplete cases were removed.
#'
#' @format a `data.table`
#' @source [https://services.pacificclimate.org/met-data-portal-pcds/app/](https://services.pacificclimate.org/met-data-portal-pcds/app/)
"xyzDT"
