library(terra)
library(data.table)

monthdays <- c(31, 28.25, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

dirs <- list.dirs("C:\\Users\\kdaust\\LocalFiles\\CMIP6_GCMs")
gcms <- unique(sapply(strsplit(dirs, "/"), "[", 2))
select <- c(2,4,5,8,9,10,12,13,15,16,18)
gcms <- gcms[select]

startyear.ref <- 1961
endyear.ref <- 1990

mod <- "C:\\Users\\kdaust\\LocalFiles\\CMIP6_GCMs/ACCESS-ESM1-5/tasmin_Amon_ACCESS-ESM1-5_ssp585_r9i1p1f1_gn_201501-210012.nc"
r <- rast(mod, drivers = "NETCDF")

##reference period mean
element <- "pr"
fnames <- list.files("C:\\Users\\kdaust\\LocalFiles\\CMIP6_GCMs/ACCESS-ESM1-5")
dir <- "C:\\Users\\kdaust\\LocalFiles\\CMIP6_GCMs/ACCESS-ESM1-5/"
ref.files <- fnames[grep("pr.*historical",fnames)]

rast.ref <- rast(paste0(dir,ref.files))
