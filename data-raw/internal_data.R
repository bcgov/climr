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

# dbnames <- structure(list(
#   GCM = c(
#     "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5",
#     "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0",
#     "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
#   ),
#   dbname = c(
#     "gcm_access", "gcm_bcc", "gcm_canesm", "gcm_cnrm",
#     "gcm_ecearth", "gcm_gfdl", "gcm_giss", "gcm_inm", "gcm_ipsl",
#     "gcm_miroc6", "gcm_mpi1", "gcm_mpi2", "gcm_ukesm"
#   )
# ), class = "data.frame", row.names = c(NA, -13L))

dbnames <- data.table(
  GCM = c(
    "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5",
    "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0",
    "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
  ),
  dbname = c("gcm_access-esm1-5", "gcm_bcc-csm2-mr", "gcm_canesm5", "gcm_cnrm-esm2-1", 
            "gcm_ec-earth3", "gcm_gfdl-esm4", "gcm_giss-e2-1-g", "gcm_inm-cm5-0", 
            "gcm_ipsl-cm6a-lr", "gcm_miroc6", "gcm_mpi-esm1-2-hr", "gcm_mri-esm2-0", 
            "gcm_ukesm1-0-ll")
)

dbnames_hist <- data.table(
  GCM = c(
    "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5",
    "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0",
    "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
  ),
  dbname = c("hist_access-esm1-5", "hist_bcc-csm2-mr", "hist_canesm5", "hist_cnrm-esm2-1", 
             "hist_ec-earth3", "hist_gfdl-esm4", "hist_giss-e2-1-g", "hist_inm-cm5-0", 
             "hist_ipsl-cm6a-lr", "hist_miroc6", "hist_mpi-esm1-2-hr", "hist_mri-esm2-0", 
             "hist_ukesm1-0-ll")
)

dbnames_hist_obs <- data.table(dataset = c("cru.gpcc", "climatena"),
                               dbname = c("historic_cru_gpcc","historic_ts"))

dbnames_ts <- structure(list(
  GCM = c(
    "ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5",
    "CNRM-ESM2-1", "EC-Earth3", "GISS-E2-1-G", "INM-CM5-0",
    "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"
  ),
  dbname = c(
    "gcmts_VAR_access-esm1-5", "gcmts_VAR_bcc-csm2-mr", "gcmts_VAR_canesm5", "gcmts_VAR_cnrm-esm2-1",
    "gcmts_VAR_ec-earth3", "gcmts_VAR_giss-e2-1-g", "gcmts_VAR_inm-cm5-0", "gcmts_VAR_ipsl-cm6a-lr",
    "gcmts_VAR_miroc6", "gcmts_VAR_mpi-esm1-2-hr", "gcmts_VAR_mri-esm2-0", "gcmts_VAR_ukesm1-0-ll"
  )
), class = "data.frame", row.names = c(NA, -12L))

usethis::use_data(param, dbnames, dbnames_hist, dbnames_ts, dbnames_hist_obs,
  overwrite = TRUE, internal = TRUE
)
