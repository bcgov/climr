library(data.table)
library(terra)
library(climr)

## A data.table of point coordinates, IDs and elevation
data("xyzDT")
temp <- xyzDT[1:4,]
## if you just want to downscale points and not think about what happening behind the scenes, use this function
ds_out <- climr_downscale(
  xyz = temp, 
  which_normal = "auto",
  gcm_models = c("ACCESS-ESM1-5"),
  ssp = c("ssp245"),
  gcm_period = list_gcm_period()[2],
  max_run = 3, # we want 3 individual runs for each model
  vars = c("PPT", "CMD", "CMI")
)
