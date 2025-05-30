% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{get_bb}
\alias{get_bb}
\alias{get_bb.data.frame}
\alias{get_bb.SpatRaster}
\title{Find bounding box of data}
\usage{
get_bb(xyz)

\method{get_bb}{data.frame}(in_xyz)

\method{get_bb}{SpatRaster}(in_xyz)
}
\arguments{
\item{xyz}{\code{data.table} (or \code{data.frame}) of points to downscale
with columns "lon", "lat", "elev" and "id"}
}
\value{
numeric vector. Bounding box coordinates with order xmin, xmax, ymin, ymax (e.g. \code{c(-122, -121, 50, 51)}).
}
\description{
Find bounding box of data
}
