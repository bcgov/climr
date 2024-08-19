## `variables` data object (external)

library(data.table)

variables <- fread("data-raw/derivedVariables/variables.csv")

usethis::use_data(variables, overwrite = TRUE, internal = FALSE)
