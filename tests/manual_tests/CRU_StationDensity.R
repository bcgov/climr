## make plots of CRU station density from 1900-2023
## Colin Mahony 
## March 3, 2025


library(data.table)
library(terra)
library(rworldxtra)
data("countriesHigh")

# function to read CRU .dtb files of source station observations. 
read_cru_dtb <- function(filepath) {
  # Read all lines into a vector
  lines <- readLines(filepath)
  
  # Initialize storage
  results <- list()
  i <- 1
  
  while (i <= length(lines)) {
    # Read header line
    header <- unlist(strsplit(lines[i], " +"))
    station_id <- as.integer(header[1])
    latitude <- as.integer(header[2]) / 100
    longitude <- as.integer(header[3]) / 100
    altitude <- as.integer(header[4])
    station_name <- paste(header[5:(length(header) - 2)], collapse = " ")
    start_year <- as.integer(header[length(header) - 1])
    end_year <- as.integer(header[length(header)])
    
    # Read normals line (ignored)
    i <- i + 2  # Move to first data line
    
    # Read years of data
    station_records <- list()
    for (year in start_year:end_year) {
      if (i > length(lines)) break
      year_line <- lines[i]
      year_val <- as.integer(substr(year_line, 1, 4))
      if (year_val != year) break  # Stop if unexpected year
      
      # Read monthly values (12x 5-character fixed-width)
      monthly_values <- sapply(1:12, function(j) {
        as.integer(substr(year_line, 5 + (j - 1) * 5, 9 + (j - 1) * 5))/10 #divide by 10 to get the true value
      })
      
      # Store as table-friendly format
      station_records[[length(station_records) + 1]] <- data.table(
        station_id = station_id,
        station_name = station_name,
        latitude = latitude,
        longitude = longitude,
        altitude = altitude,
        year = year,
        month = 1:12,
        value = monthly_values
      )
      
      i <- i + 1
    }
    
    # Append station data
    results <- append(results, station_records)
  }
  
  return(rbindlist(results, use.names = TRUE, fill = TRUE))
}

# spatial boundaries
studyarea <- ext(c(-179.9, -52.25, 13.75, 83.75))
bdy.na <- vect(countriesHigh[grep("Canada|United States|Mexico", countriesHigh$NAME),])
bdy.na <- erase(bdy.na, ext(c(-170, -140, 13, 30))) # erase hawaii
bdy.na <- crop(bdy.na, studyarea)
plot(bdy.na)

elements <- c("tmn", "tmx")
for(element in elements){
# read in CRU station data
# dir <- "C:/Users/CMAHONY/OneDrive - Government of BC/Data/cru_ts4.08/"
dir <- "//objectstore2.nrs.bcgov/ffec/TimeSeries_gridded_monthly/cru_ts4.08/"
file <- paste0(element, ".2406262226.clean.dtb")
cru_data <- read_cru_dtb(paste0(dir, file))
cru_data[value == -999.9, value := NA_real_]

# identify NorAm stations
cru_stations <- unique(cru_data[,1:5])
cru_spatial <- vect(cru_stations, geom = c("longitude", "latitude"))
inside_na <- intersect(cru_spatial, bdy.na)
coords <- as.data.table(crds(inside_na))
attributes <- as.data.table(inside_na)[,1:4]
cru_stations_filtered <- cbind(coords, attributes)
cru_stations_filtered[, ne_10m_adm := factor(ne_10m_adm)]
cru_stations_filtered[, unique := paste(station_id, station_name)]
table(cru_stations_filtered$unique)

# check
plot(bdy.na)
points(cru_stations_filtered$x, cru_stations_filtered$y, col = cru_stations_filtered$ne_10m_adm, pch = 16)

# filter out non-NorAm stations and attribute with country
cru_filtered <- cru_data[station_name %in% cru_stations_filtered$station_name]
cru_filtered[, unique := paste(station_id, station_name)]
cru_filtered <- merge(cru_filtered, cru_stations_filtered[, .(unique, ne_10m_adm)], by = "unique", all.x = TRUE)
setnames(cru_filtered, "ne_10m_adm", "country")

# get the average station count by country and year (very little variation among months, so average is representative)
count <- cru_filtered[!is.na(value), .(non_na_years = uniqueN(station_name)), by = .(year, country, month)]
count <- count[, .(mean_value = mean(non_na_years, na.rm = TRUE)), by = .(year, country)]
setorder(count, year)

# plot
png(filename=paste("vignettes/plots_timeseries/CRU_StationDensity", element, "png",sep="."), type="cairo", units="in", width=6.5, height=2.5, pointsize=10, res=300)
countries <- c("CAN", "USA", "MEX")
par(mfrow=c(1,3), mar=c(3,3,1,1), mgp=c(1.75, 0.25, 0), tck=-0.01)
for(c in countries){
  s <- which(count$country==c)
  plot(count$year[s], count$mean_value[s], type="l", yaxs="i", xlim=c(1900, 2023), 
       ylim=c(0, max(count$mean_value[s])*1.05), xaxs="i",
       ylab = "Number of contributing stations", xlab = "Year")
  mtext(c, side=3, line=-1.5, adj=0.05)
}
dev.off()

}
