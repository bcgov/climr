## internal data objects

## `param` data object
library(data.table)

base <- "data-raw/derivedVariables/optimizedParameterTables"

param <- list(
  DD_lt_0 = fread(file = file.path(base, "param_DD_S1.csv")),
  DD_gt_5 = fread(file = file.path(base, "param_DD_S2.csv")),
  DD_lt_18 = fread(file = file.path(base, "param_DD_S3.csv")),
  DD_gt_18 = fread(file = file.path(base, "param_DD_S4.csv")),
  NFFD = fread(file = file.path(base, "param_NFFD.csv")),
  PAS = fread(file = file.path(base, "param_PAS.csv"))
)

## `dbnames*` data objects

dbnames <- structure(list(
  GCM = c(
    "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5",
    "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0",
    "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
  ),
  dbname = c(
    "gcm_access", "gcm_bcc", "gcm_canesm", "gcm_cnrm",
    "gcm_ecearth", "gcm_gfdl", "gcm_giss", "gcm_inm", "gcm_ipsl",
    "gcm_miroc6", "gcm_mpi1", "gcm_mpi2", "gcm_ukesm"
  )
), class = "data.frame", row.names = c(NA, -13L))

dbnames_hist <- structure(list(
  GCM = c(
    "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5",
    "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0",
    "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
  ),
  dbname = c(
    "access_hist", "bcc_hist", "canesm_hist", "cnrm_hist",
    "ec_earth_hist", "gfdl_hist", "giss_hist", "inm_hist", "ipsl_hist",
    "miroc6_hist", "mpi_hist", "mri_hist", "ukesm_hist"
  )
), class = "data.frame", row.names = c(NA, -13L))

dbnames_ts <- structure(list(
  GCM = c(
    "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5",
    "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0",
    "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
  ),
  dbname = c(
    "gcmts_access", "gcmts_bcc", "gcmts_canesm", "gcmts_cnrm",
    "gcmts_ecearth", "gcmts_gfdl", "gcmts_giss", "gcmts_inm", "gcmts_ipsl",
    "gcmts_miroc6", "gcmts_mpi1", "gcmts_mpi2", "gcmts_ukesm"
  )
), class = "data.frame", row.names = c(NA, -13L))

usethis::use_data(param, dbnames, dbnames_hist, dbnames_ts, 
                  overwrite = TRUE, internal = TRUE)
