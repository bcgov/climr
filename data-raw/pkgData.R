library(data.table)

# param (internal)

base <- "data-raw/derivedVariables/optimizedParameterTables"

param <- list(
  DD_lt_0 = fread(file = file.path(base, "param_DD_S1.csv")),
  DD_gt_5 = fread(file = file.path(base, "param_DD_S2.csv")),
  DD_lt_18 = fread(file = file.path(base, "param_DD_S3.csv")),
  DD_gt_18 = fread(file = file.path(base, "param_DD_S4.csv")),
  NFFD = fread(file = file.path(base, "param_NFFD.csv")),
  PAS = fread(file = file.path(base, "param_PAS.csv"))
)

usethis::use_data(param, overwrite = TRUE, internal = TRUE)

# variables (exported)

variables <- fread("data-raw/derivedVariables/Variables_ClimateBC.csv")

usethis::use_data(variables, overwrite = TRUE, internal = FALSE)

usethis::use_data(bc_bnd, overwrite = TRUE, internal = FALSE)

usethis::use_data(bc_outline, overwrite = TRUE, internal = FALSE)
