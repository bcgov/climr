% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gcm.R
\name{gcm_input}
\alias{gcm_input}
\alias{gcm_hist_input}
\alias{gcm_ts_input}
\title{Retrieve GCM anomalies for \code{downscale}.}
\usage{
gcm_input(
  dbCon,
  bbox = NULL,
  gcm = list_gcm(),
  ssp = list_ssp(),
  period = list_gcm_period(),
  max_run = 0L,
  cache = TRUE
)

gcm_hist_input(
  dbCon,
  bbox = NULL,
  gcm = list_gcm(),
  years = 1901:1950,
  max_run = 0L,
  cache = TRUE
)

gcm_ts_input(
  dbCon,
  bbox = NULL,
  gcm = list_gcm(),
  ssp = list_ssp(),
  years = 2020:2030,
  max_run = 0L,
  cache = TRUE
)
}
\arguments{
\item{dbCon}{A db connection object created by \code{data_connect}.}

\item{bbox}{numeric. Vector of length 4 giving bounding box of study region, in the order ymax,ymin,xmax,xmin. In general this is created by \code{get_bb()}, but can also be user-defined.}

\item{gcm}{character. Vector of labels of the global circulation models to use.
Can be obtained from \code{\link[=list_gcm]{list_gcm()}}. Default to all GCMs available.}

\item{ssp}{character. Vector of SSP-RCP scenarios (representative concentration pathways paired with shared socioeconomic pathways).
Options are \code{\link[=list_ssp]{list_ssp()}}. Defaults to all scenarios available.}

\item{period}{character. Vector of labels of the periods to use.
Can be obtained from \code{\link[=list_gcm_period]{list_gcm_period()}}. Defaults to all periods available.}

\item{max_run}{integer. Maximum number of model runs to include.
A value of 0 returns the \code{ensembleMean} only. Runs are included in the order they
are found in the models data until \code{max_run} is reached. Defaults to 0L.}

\item{cache}{logical. Specifying whether to cache new data locally or no. Defaults to \code{TRUE}.}

\item{years}{Numeric or character vector in \code{2020:2100}. Defaults to \code{2020:2030}.}
}
\value{
A \code{list} of \code{SpatRasters}, each with possibly multiple layers, that can
be used with \code{\link[=downscale]{downscale()}}.

A \code{list} of \code{SpatRasters}, each with possibly multiple layers, that can
be used with \code{\link[=downscale]{downscale()}}.

A \code{list} of \code{SpatRasters}, each with possibly multiple layers, that can
be used with \code{\link[=downscale]{downscale()}}.
}
\description{
\code{gcm_input} retrieves anomalies for GCM data, given chosen GCMs, SSPs,
periods and runs.

\code{gcm_hist_input} creates GCM \strong{historic} time series inputs, given chosen GCMs,
years and runs.

\code{gcm_ts_input} creates future GCM time series inputs, given chosen GCMs, SSPs,
years and runs.
}
\details{
This function returns a list with one slot for each requested GCM. Rasters inside the list contain anomalies for all requested SSPs, runs, and periods.
In general this function should only be used in combination with \code{\link[=downscale]{downscale()}}.

This function returns a list with one slot for each requested GCM. Rasters inside the list contain anomalies for all runs and years.
In general this function should only be used in combination with \code{\link[=downscale]{downscale()}}.

This function returns a list with one slot for each requested GCM. Rasters inside the list contain anomalies for all SSPs, runs and years.
In general this function should only be used in combination with \code{\link[=downscale]{downscale()}}. Note that if you request multiple runs, multiple SSPs, and a lot of years,
it will take a while to download the data (there's lot of it).
}
\examples{
library(terra)
xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10), id = 1:10)

## get bounding box based on input points
thebb <- get_bb(xyz)

## get database connection
dbCon <- data_connect() 
on.exit(try(pool::poolClose(dbCon)))

gcm <- gcm_input(dbCon, thebb, list_gcm()[1], list_ssp()[1])

## show ensemble means only
lyrs <- grep("ensemble", names(gcm$`ACCESS-ESM1-5`))

plot(gcm$`ACCESS-ESM1-5`[[lyrs]])

{
library(terra)
xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10), id = 1:10)

## get bounding box based on input points
thebb <- get_bb(xyz)

## get database connection
dbCon <- data_connect()
on.exit(try(pool::poolClose(dbCon)))

gcm_hist <- gcm_hist_input(dbCon, thebb, list_gcm()[1])

## show ensemble means only
lyrs <- grep("ensemble", names(gcm_hist$`ACCESS-ESM1-5`))

plot(gcm_hist$`ACCESS-ESM1-5`[[lyrs]])
}

library(terra)
xyz <- data.frame(lon = runif(10, -140, -106), lat = runif(10, 37, 61), elev = runif(10), id = 1:10)

## get bounding box based on input points
thebb <- get_bb(xyz)

## get database connection
dbCon <- data_connect() 
on.exit(try(pool::poolClose(dbCon)))

gcm_ts <- gcm_ts_input(dbCon, thebb, list_gcm()[1], list_ssp()[1])

## show ensemble means only
lyrs <- grep("ensemble", names(gcm_ts$`ACCESS-ESM1-5`))

plot(gcm_ts$`ACCESS-ESM1-5`[[lyrs]])

}
\seealso{
\code{\link[=downscale]{downscale()}}

\code{\link[=list_gcm_period]{list_gcm_period()}}, \code{\link[=list_gcm_period]{list_gcm_period()}}
}
