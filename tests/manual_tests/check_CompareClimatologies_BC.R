
# compare climatological maps from various source using the BC PRISM DEM domain
# Colin Mahony 

library(terra)
library(climr)
library(data.table)
library(leaflet)
library(RColorBrewer)
library(ranger)
library(rworldmap)

# function for preparing data
prep <- function(x, studyarea, element, breaks){
  x <- crop(x, studyarea)
  if(element=="Pr") values(x) <- log2(values(x))
  values(x)[!is.finite(values(x))] <- NA
  values(x)[values(x)>max(breaks)] <- max(breaks)
  values(x)[values(x)<min(breaks)] <- min(breaks)
  return(x)
}

# lists
monthcodes <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
elements <- c("Tmin", "Tmax", "Pr")
month.abb.lowercase <- c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

# PRISM DEM
dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/PRISM_dem/", sep="")
dem.bc <- rast(paste(dir, "PRISM_dem.asc", sep=""))

# climr data
my_grid <- as.data.frame(dem.bc, cells = TRUE, xy = TRUE)
colnames(my_grid) <- c("id", "lon", "lat", "elev") # rename column names to what climr expects
climr_climatena_all <- downscale(xyz = my_grid, which_refmap = "refmap_climatena", vars = list_vars(set="Monthly"))
climr_prism_all <- downscale(xyz = my_grid, which_refmap = "refmap_climr", vars = list_vars(set="Monthly"))

# climateNA data
climatena_grid <- my_grid[,c(1,1,3,2,4)]
colnames(climatena_grid) <- c("ID1", "ID2", "lat", "long", "el") # rename column names to what climr expects
write.csv(climatena_grid, "C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/bc.csv", row.names = F)
writeRaster(dem.bc, "C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/bc.asc", overwrite=T)
## Run climateNA and then return to script. 
climatena_all <- fread("C:/Users/CMAHONY/OneDrive - Government of BC/Data/ClimateNA_v750/bc_Normal_1961_1990MP.csv")


# ----------------------------------
# specify climate variable
e <- 2 # element
m <- 3 # month
var <- paste(c("Tmin", "Tmax", "PPT")[e], monthcodes[m], sep="_")

# load the BC PRISM  data for the variable
dir <- paste("//objectstore2.nrs.bcgov/ffec/Climatologies/PRISM_BC/", sep="")
file <- list.files(dir, pattern=paste(c("tmin", "tmax", "pr")[e],"_.*._",m, ".tif", sep=""))
prism.bc <- rast(paste(dir, file, sep=""))
if(elements[e]=="Pr") values(prism.bc) <- log2(values(prism.bc))
values(prism.bc)[!is.finite(values(prism.bc))] <- NA

# color scheme
combined <- c(values(prism.bc))
combined <- combined[is.finite(combined)]
inc=diff(range(combined))/500
breaks=seq(quantile(combined, 0)-inc, quantile(combined, 1)+inc, inc)
ColScheme <- colorRampPalette(if(elements[e]=="Pr") brewer.pal(9, "YlGnBu") else rev(brewer.pal(11, "RdYlBu")))(length(breaks)-1)
ColPal <- colorBin(ColScheme, bins=breaks, na.color = "white")
ColPal.raster <- colorBin(ColScheme, bins=breaks, na.color = "transparent")

# load the climr data for the variable (climateNA reference)
var <- paste(c("Tmin", "Tmax", "PPT")[e], monthcodes[m], sep="_")
climr_climatena <- rast(dem.bc) # use the DEM as a template raster
climr_climatena[climr_climatena_all[, id]] <- climr_climatena_all[,get(var)]
climr_climatena <- prep(climr_climatena, studyarea=dem.bc, element=elements[e], breaks=breaks)

# load the climr data for the variable (prism reference)
var <- paste(c("Tmin", "Tmax", "PPT")[e], monthcodes[m], sep="_")
climr_prism <- rast(dem.bc) # use the DEM as a template raster
climr_prism[climr_prism_all[, id]] <- climr_prism_all[,get(var)]
climr_prism <- prep(climr_prism, studyarea=dem.bc, element=elements[e], breaks=breaks)

# load the climr data for the variable (prism reference)
var_climatena <- paste(c("Tmin", "Tmax", "PPT")[e], monthcodes[m], sep="")
data_climatena <- climatena_all[,get(var_climatena)]
data_climatena[data_climatena == -9999] <- NA
climatena <- rast(dem.bc) # use the DEM as a template raster
climatena[climatena_all[, ID1]] <- data_climatena
climatena <- prep(climatena, studyarea=dem.bc, element=elements[e], breaks=breaks)


# leaflet map
map <- leaflet() %>%
  addTiles(group = "basemap") %>%
  addProviderTiles('Esri.WorldImagery', group = "sat photo") %>%
  addRasterImage(prism.bc, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "BC PRISM") %>%
  addRasterImage(climr_climatena, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "climr (ClimateNA)") %>%
  addRasterImage(climr_prism, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "climr (PRISM)") %>%
  addRasterImage(climatena, colors = ColPal.raster, opacity = 1, maxBytes = 7 * 1024 * 1024, group = "ClimateNA (points)") %>%
  addLayersControl(
    baseGroups = c("basemap", "sat photo"),
    overlayGroups = c("ClimateNA (points)", "climr (PRISM)", "climr (ClimateNA)", "BC PRISM"),
    options = layersControlOptions(collapsed = FALSE)
  )
map



