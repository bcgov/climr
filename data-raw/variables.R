## `variables` data object (external)

library(data.table)

variables <- fread("data-raw/derivedVariables/Variables_ClimateBC.csv")

usethis::use_data(variables, overwrite = TRUE, internal = FALSE)
