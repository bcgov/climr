#' Example vector data
#'
#' Shapefile of coast of Vancouver, BC, Canada.
#'
#' @format a `SpatVector`
#' @source todo add/describe source
"vancouver"


#' Example DEM
#'
#' Digital elevation model of North Vancouver, BC, Canada.
#'
#' @format a `SpatRaster`
#' @source todo add/describe source
"dem_vancouver"

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

#' Example grid of point locations for sampling
#'
#' Shapefile with point locations spaced at regular intervals, in
#' North Vancouver, BC, Canada.
#' To use this data set, you need to `terra::unwrap`
#' it first, e.g.:
#' `point_grid <- terra::unwrap(get(data(point_grid)))`
#'
#' @format `PackedSpatVector`
#' @source derived from `dem_vancouver` reprojected at 0.01 degrees.
"point_grid"

#' Example Biogeoclimatic Ecological Classification Zones
#'
#' A shapefile with biogeoclimatic zones in the coast of Vancouver, BC, Canada.
#'
#' @format a `SpatVector`
#' @source obtained from `bcdata` package, using `bcdc_get_data("bec-map-generalized-1-20k-")`
"BECz_vancouver"


#' Colour sscheme for Biogeoclimatic Ecological Classification Zones
#'
#' A `data.table` with RGB and HEX colours for biogeoclimatic zones in BC.
#'
#' @format a `data.table`
#' @source colour scheme commonly used by the BC provincial government.
"BECcols"
