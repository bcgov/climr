library(data.table)
library(climr)
library(terra)
library(ranger)

library(ncdf4)
nc <- nc_open("SCDNA_v1.1.nc4")

# station ids
id_raw <- ncvar_get(nc, "ID")
station_id <- sapply(seq_len(nchar(id_raw[1])), function(i) {
  paste0(substr(id_raw, i, i), collapse = "")
})

station_id <- trimws(station_id)

# metadata
lle <- ncvar_get(nc, "LLE")
station_meta <- data.table(
  id   = station_id,
  lat  = lle[, 1],
  lon  = lle[, 2],
  elev = lle[, 3]
)

# dates
date_raw <- ncvar_get(nc, "date")
dates <- as.Date(as.character(date_raw), format = "%Y%m%d")
months <- unique(format(dates, "%Y-%m"))

nstn <- nrow(station_meta)

out <- vector("list", length(months))

for (i in seq_along(months)) {
  m <- months[i]
  cat(m, "\n")
  idx <- which(format(dates, "%Y-%m") == m)
  
  start_day <- min(idx)
  n_days <- length(idx)
  
  tmax <- ncvar_get(nc, "tmax",
                    start = c(start_day, 1),
                    count = c(n_days, nstn))
  
  tmin <- ncvar_get(nc, "tmin",
                    start = c(start_day, 1),
                    count = c(n_days, nstn))
  
  prcp <- ncvar_get(nc, "prcp",
                    start = c(start_day, 1),
                    count = c(n_days, nstn))
  
  tmax[tmax == -999] <- NA_real_
  tmin[tmin == -999] <- NA_real_
  prcp[prcp == -999] <- NA_real_
  
  Tmax <- colMeans(tmax, na.rm = TRUE)
  Tmin <- colMeans(tmin, na.rm = TRUE)
  PPT  <- colSums(prcp, na.rm = TRUE)
  
  Tmax[is.nan(Tmax)] <- NA_real_
  Tmin[is.nan(Tmin)] <- NA_real_
  PPT[is.nan(PPT)]   <- NA_real_
  
  out[[i]] <- data.table(
    id = station_meta$id,
    month = m,
    Tmax = Tmax,
    Tmin = Tmin,
    PPT = PPT
  )
}

dat_all <- rbindlist(out)
dat_all[,dataset := "SCDNA"]
dat_all[,period := as.Date(paste0(month, "-01"))]
dat_all[,month := NULL]
setnames(dat_all, old = c("Tmin","Tmax","PPT"), new = c("tmin","tmax","ppt"))
setorder(dat_all, dataset, id, period)

station_meta[,dataset := "SCDNA"]

library(RPostgres)
con <- dbConnect(RPostgres::Postgres(),dbname = 'climr',
                  host = '146.190.244.244',
                  port = 5432,
                  user = 'postgres',
                  password = '')

dbExecute(con, "CREATE TABLE station_data (
  dataset text,
  id text,
  period date,
  tmin double precision,
  tmax double precision,
  ppt double precision,
  primary key (dataset, id, period)
);")

dbWriteTable(con, "station_data", dat_all, row.names = F, append = T)
dbExecute(con, "CREATE INDEX station_data_dataset_id_idx ON station_data (dataset, id);")

dbExecute(con, "CREATE TABLE station_meta (
  dataset text,
  id text,
  lat double precision,
  lon double precision,
  elev double precision,
  primary key (dataset, id)
);")

dbWriteTable(con, "station_meta", station_meta, row.names = F, append = T)
dbExecute(con, "CREATE INDEX station_meta_dataset ON station_meta (dataset);")

stdat <- fread("station_data/AHCCD.csv")
metadat <- fread("station_data/AHCCD_location.csv")
stdat[,dataset := "AHCCD"]
stdat[,period := as.Date(paste(year,month,"01", sep = "-"))]
setnames(stdat, old = "pr", new = "ppt")
stdat <- stdat[,.(dataset,id,period,tmin,tmax,ppt)]

dbWriteTable(con, "station_data", stdat, row.names = FALSE, append = TRUE)

dbExecute(con, "ALTER TABLE station_data
ADD COLUMN year int GENERATED ALWAYS AS (EXTRACT(YEAR FROM period)) STORED,
ADD COLUMN month int GENERATED ALWAYS AS (EXTRACT(MONTH FROM period)) STORED;")

metadat[,pr := NULL]
metadat[,dataset := "AHCCD"]
dbWriteTable(con, "station_meta", metadat, row.names = FALSE, append = TRUE)


#############Done processing data########################################
library(climr)
in_xyz <- data.frame(
  lon = c(-127.7052, -127.6227, -127.5623, -127.7162, -127.1858, -127.125, -126.9495, -126.9550),
  lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
  elev = c(291, 296, 626, 377, 424, 591, 723, 633),
  id = 1:8
)
bb <- get_bb(in_xyz)
dat <- get_station_data(in_xyz, dataset = "SCDNA", k = 3, only_complete = FALSE)
dat2 <- get_station_data(bb, dataset = "SCDNA", only_complete = TRUE, vars = c("DD5_an","CMD_07"))
st_meta <- get_station_metadata(unique(dat2$station_id))

