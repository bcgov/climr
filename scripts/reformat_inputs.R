# Convert to nc for faster download and faster read
# Recompress existing nc file for faster download

# Normal
fname <- list.files("inputs_raw/normal/Normal_1961_1990MP", pattern = "\\.asc$", full.names = TRUE)

dir.create("inputs_pkg/normal/Normal_1961_1990MP", recursive = TRUE, showWarnings = FALSE)
from <- terra::rast(fname)
terra::writeCDF(
  from,
  "inputs_pkg/normal/Normal_1961_1990MP/Normal_1961_1990MP.nc",
  overwrite = TRUE,
  prec = "integer",
  compression = 9,
  shuffle = TRUE
)
write.csv(names(from),"inputs_pkg/normal/Normal_1961_1990MP/Normal_1961_1990MP.csv")

# Dem
fname <- list.files("inputs_raw/dem/westnorthamerica", pattern = "\\.asc$", full.names = TRUE)

dir.create("inputs_pkg/dem/westnorthamerica", recursive = TRUE, showWarnings = FALSE)
from <- terra::rast(fname)
terra::writeCDF(
  from,
  "inputs_pkg/dem/westnorthamerica/dem2_WNA.nc",
  overwrite = TRUE,
  compression = 9
)
write.csv(names(from),"inputs_pkg/dem/westnorthamerica/dem2_WNA.csv")

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
    compression = 9
  )
}

# Tentative, files is too big

# Lapse rates
dname <- list_normal()[1]
normal <- normal_input(dname)
from <- attr(normal, "lapse_rates")
dir.create(sprintf("inputs_pkg/normal/%s/lr", dname), recursive = TRUE, showWarnings = FALSE)
terra::writeCDF(
  from,
  sprintf("inputs_pkg/normal/%s/lr/%s.nc", dname, dname),
  overwrite = TRUE,
  compression = 9
)
write.csv(names(from),sprintf("inputs_pkg/normal/%s/lr/%s.csv", dname, dname))