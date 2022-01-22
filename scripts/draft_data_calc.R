library(curl)
# Gather urls of BC historical data ----
bc_hist_catalog <- jsonlite::fromJSON("https://data.pacificclimate.org/portal/bc_prism/catalog/catalog.json")
# select only variables of interest
bc_hist_catalog <- bc_hist_catalog[grep("[_]mon[_]PRISM_historical_19700101-20001231_bc$", names(bc_hist_catalog))]
# Download historical data of interest
if(!dir.exists("../climR-data/BC")) dir.create("../climR-data/BC", recursive = TRUE)
lapply(bc_hist_catalog, 
       \(x) curl_download(x, destfile = file.path("../climR-data/BC/", basename(x)), quiet = TRUE))

# load into memory
bc_tasmax <- raster::brick("../climR-data/BC/tasmax_mClimMean_PRISM_historical_19710101-20001231.nc")
bc_tasmin <- raster::brick("../climR-data/BC/tasmin_mClimMean_PRISM_historical_19710101-20001231.nc")
bc_pr <- raster::brick("../climR-data/BC/pr_mClimMean_PRISM_historical_19710101-20001231.nc")

xy_coords <- cbind(c(-123.132), c(49.287))

bilinear_values <- lapply(list(pr = bc_pr, 
                               tasmin = bc_tasmin, 
                               tasmax = bc_tasmax), 
                          raster:::.bilinearValue, 
                          xyCoords = xy_coords)

monthly_values <- lapply(bilinear_values, t)
monthly_values <- data.table(month = 1:12, 
                             monthly_values$pr, 
                             monthly_values$tasmin,
                             monthly_values$tasmax)
setnames(monthly_values, c("month", "pr", "tasmin", "tasmax"))

set(monthly_values, j = "Tavg", value = (monthly_values$tasmin + monthly_values$tasmax )/2)

set(monthly_values, j = "DD_below_0", value = calc_DD_below_0(monthly_values$month, tm = monthly_values$Tavg))

set(monthly_values, j = "DD_above_5", value = calc_DD_above_5(monthly_values$month, tm = monthly_values$Tavg, region = "West"))

set(monthly_values, j = "DD_below_18", value = calc_DD_below_18(monthly_values$month, tm = monthly_values$Tavg))

set(monthly_values, j = "DD_above_18", value = calc_DD_above_18(monthly_values$month, tm = monthly_values$Tavg, region = "The rest"))

set(monthly_values, j = "RH", value = calc_RH(monthly_values$tasmin, monthly_values$tasmax))

set(monthly_values, j = "RH", value = calc_RH(monthly_values$tasmin, monthly_values$tasmax))

set(monthly_values, j = "NFFD", value = calc_NFFD(1:12, tm = monthly_values$tasmin))

set(monthly_values, j = "PAS", value = calc_PAS(1:12, tm = monthly_values$tasmin))
    




