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
