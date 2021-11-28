library(data.table)

param <- list(
  DD_lt_0 = fread(file = "inputs/derivedVariables/optimizedParameterTables/param_DD_S1.csv"),
  DD_gt_5 = fread(file = "inputs/derivedVariables/optimizedParameterTables/param_DD_S2.csv"),
  DD_lt_18 = fread(file = "inputs/derivedVariables/optimizedParameterTables/param_DD_S3.csv"),
  DD_gt_18 = fread(file = "inputs/derivedVariables/optimizedParameterTables/param_DD_S4.csv"),
  NFFD = fread(file = "inputs/derivedVariables/optimizedParameterTables/param_NFFD.csv"),
  PAS = fread(file = "inputs/derivedVariables/optimizedParameterTables/param_PAS.csv")
)

usethis::use_data(param, overwrite = TRUE, internal = TRUE)
