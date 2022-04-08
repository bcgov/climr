# Convert to nc for faster download and faster read
# Recompress existing nc file for faster download

# Normal
fname <- list.files("inputs_raw/normal/Normal_1961_1990MP", pattern = "\\.asc$", full.names = TRUE)

dir.create("inputs_pkg/normal/Normal_1961_1990MP", recursive = TRUE, showWarnings = FALSE)
# Making sure NA values are correctly handled for integer precision
from <- terra::rast(raster::brick(raster::stack(fname))+0L)
terra::writeCDF(
  from,
  "inputs_pkg/normal/Normal_1961_1990MP/Normal_1961_1990MP.nc",
  overwrite = TRUE,
  prec = "integer",
  compression = 9,
  shuffle = TRUE,
  missval = 99999
)
write.csv(names(from),"inputs_pkg/normal/Normal_1961_1990MP/Normal_1961_1990MP.csv")

# Dem matching normal
fname <- list.files("inputs_raw/dem/westnorthamerica", pattern = "\\.asc$", full.names = TRUE)

dir.create("inputs_pkg/normal/Normal_1961_1990MP/dem", recursive = TRUE, showWarnings = FALSE)

from_dem <- terra::rast(fname)

if (!terra::compareGeom(from, from_dem)) {
  warning("SpatRaster for Normal and Digital Elevation Model have a different extents. They must be the same. Resampling.")
  from_dem <- terra::resample(from_dem, from, method = "bilinear")
}

terra::writeCDF(
  from_dem,
  "inputs_pkg/normal/Normal_1961_1990MP/dem/dem2_WNA.nc",
  overwrite = TRUE,
  compression = 9,
  missval = NA
)
write.csv(names(from_dem),"inputs_pkg/normal/Normal_1961_1990MP/dem/dem2_WNA.csv")

# GCM

files_csv <- list.files("inputs_raw/gcm", pattern = "\\.csv$", recursive = TRUE)
lapply(file.path("inputs_pkg/gcm", unique(dirname(files_csv))), dir.create, recursive = TRUE, showWarnings = FALSE)
file.copy(file.path("inputs_raw/gcm", files_csv), file.path("inputs_pkg/gcm", files_csv))
files_nc <- list.files("inputs_raw/gcm", pattern = "\\.nc$", recursive = TRUE)

for (file in files_nc) {
  from <- terra::rast(file.path("inputs_raw/gcm", file))
  terra::writeCDF(
    from,
    file.path("inputs_pkg/gcm", file),
    overwrite = TRUE,
    varname = terra::varnames(from),
    unit = unique(terra::units(from)),
    longname = "",
    zname = "index",
    compression = 9,
    missval = NA
  )
}
