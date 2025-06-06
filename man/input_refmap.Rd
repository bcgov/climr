% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/refmap.R
\name{input_refmap}
\alias{input_refmap}
\alias{input_refmap_db}
\title{Retrieve reference period climate maps}
\usage{
input_refmap(
  bbox,
  reference = "refmap_climr",
  cache = TRUE,
  indiv_tiles = FALSE,
  xyz = NULL
)

input_refmap_db(reference = "refmap_climatena")
}
\arguments{
\item{bbox}{numeric. Vector of length 4 giving bounding box of study region, in the order ymax,ymin,xmax,xmin. In general this is created by \code{get_bb()}, but can also be user-defined.}

\item{reference}{A \code{character} or a \code{SpatRaster}. For character, label of the normal baseline to use.
Can be obtained from \code{\link[=list_refmaps]{list_refmaps()}}. For \code{SpatRaster}, 36 layers normal climate variables with
names PPT01:PPT12, Tmax01:Tmax12 and Tmin01:Tmin12. Defaults to \code{list_refmaps()[1]}.}

\item{cache}{logical. Specifying whether to cache new data locally or no. Defaults to \code{TRUE}.}

\item{indiv_tiles}{logical. Only download necessary tiles instead of full bounding box?}

\item{xyz}{a \code{terra::SpatRaster} with a single layer containing elevation values in metres, or a \code{data.frame} with the following columns
"long", "lat", "elev", and a unique "id". Any extra columns will be ignored and not output.}
}
\value{
A \code{SpatRaster} containing reference period climatologies, lapse rates
and digital elevation model layers, that can be used with \code{\link[=downscale_core]{downscale_core()}}.
}
\description{
This function downloads (or retrieves from cache) monthly Tmin, Tmax, and PPT climatologies (maps of long-term average climate)
from a specified data source for the specified bounding box.
It is intended for use with \code{\link[=downscale_core]{downscale_core()}}, but can also be used as stand-alone raster data.
}
\details{
The first 36 layers of the output raster correspond with the actual climate variables. The raster also contains
lapse rates for each variable, and a corresponding digital elevation model.
}
\seealso{
\code{\link[=downscale]{downscale()}}
}
