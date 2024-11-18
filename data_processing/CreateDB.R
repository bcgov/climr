library(terra)
library(climr)
library(RPostgres)
library(data.table)
library(analogsea)
##add normals
library(ssh)


# bc_rast <- rast("../Common_Files/composite_wna_wlrdem.tif")[[32]]
# plot(bc_rast)
# bc_rast[!is.na(bc_rast)] <- 1L
# plot(bc_rast)
# bc_rast2 <- terra::aggregate(bc_rast, fact = 4, fun = "max")
# plot(bc_rast2)
# 
# writeRaster(bc_rast2, filename = "inst/extdata/wna_outline.tif", datatype = "INT1U")

##process NA normals
# Load normal files
dir_normal <- "../Common_Files/climatena_normals/Normal_1961_1990MP/"
files <- list.files(dir_normal, full.names = T)
files <- files[grep("Tmin|Tmax|PPT",files)]
d <- rast(files)
# names(d) <- c("PPT01", "PPT02", "PPT03", "PPT04", "PPT05", "PPT06", "PPT07", "PPT08", "PPT09", "PPT10", 
#               "PPT11", "PPT12", "Tmax01", "Tmax02", "Tmax03", "Tmax04", "Tmax05", "Tmax06", "Tmax07", 
#               "Tmax08", "Tmax09", "Tmax10", "Tmax11", "Tmax12", "Tmin01", "Tmin02", "Tmin03", "Tmin04",
#               "Tmin05", "Tmin06", "Tmin07", "Tmin08", "Tmin09", "Tmin10", "Tmin11", "Tmin12")


library(terra)
library(data.table)
dem <- rast("../climatena/InputFiles/na4000.asc")
temp <- as.data.frame(dem, xy = TRUE, cells = TRUE, na.rm = TRUE)
colnames(temp) <- c("ID1","Long","Lat","El")
setDT(temp)
temp[,ID2 := ID1]
setcolorder(temp,  c("ID1","ID2","Lat","Long","El"))
setnames(temp, c("ID1","ID2","lat","long","el"))
fwrite(temp, "climNA_4km_grid.csv")

dat <- fread("climNA_4km_grid_Normal_1961_1990MP.csv")
dat <- dat[Tmax07 > -100,]
plot(dem)
values(dem) <- NA
dem[dat$ID1] <- dat$Tmax07
plot(dem)

###create layers
vars <- colnames(dat)[-c(1:5)]
vars <- vars[!grepl("Rad|Tave",vars)]
vars <- c("PPT01", "PPT02", "PPT03", 
          "PPT04", "PPT05", "PPT06", "PPT07", "PPT08", "PPT09", "PPT10", 
          "PPT11", "PPT12","Tmax01", "Tmax02", "Tmax03", "Tmax04", "Tmax05", "Tmax06", 
          "Tmax07", "Tmax08", "Tmax09", "Tmax10", "Tmax11", "Tmax12", "Tmin01", 
          "Tmin02", "Tmin03", "Tmin04", "Tmin05", "Tmin06", "Tmin07", "Tmin08", 
          "Tmin09", "Tmin10", "Tmin11", "Tmin12")
#r <- r[[grep("PPT|Tmin|Tmax", names(r))]]
r_temp <- copy(dem)
for(var in vars){
  cat(".")
  values(r_temp) <- NA
  r_temp[dat$ID1] <- dat[,get(var)]
  names(r_temp) <- var
  add(dem) <- r_temp
}
dem <- dem[[-1]]
d <- rast("../climatena/InputFiles/na4000.asc")

lr <- lapse_rate(
  reference = dem,
  dem = d,
  NA_replace = TRUE,
  nthread = 4,
  rasterize = TRUE
)
names(lr) <- paste0("lr_",names(lr))

# Actual writing
terra::writeRaster(
  c(dem, lr, d),
  file.path("../Common_Files/climatena4km_wlrdem.tif"),
  overwrite = TRUE,
  gdal="COMPRESS=NONE"
)

############# now upload
session <- ssh_connect("root@146.190.244.244")
scp_upload(session, "../Common_Files/composite_wna_wlrdem.tif", to = "/share")
ssh_exec_wait(session, command = c("cd /share",
                                   "ls",
                                   "raster2pgsql -s 4326 -I -C -M Normal_1961_1990MP.wlrdem.tif -t 50x50 normal_wna > normal_wna.sql"))


#####################################################################################


session <- ssh_connect("root@146.190.244.244")
ssh_exec_wait(session, command = c("cd /share",
                                   "ls",
                                   "raster2pgsql -s 4326 -I -C -M Normal_1961_1990MP.wlrdem.tif -t 50x50 normal_wna > normal_wna.sql"))

curr_per <- rast("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climr/inputs_pkg/historic/Historic_2001_2020/2001_2020.tif")
plot(curr_per[[35]])
scp_upload(session, paste0("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climr/inputs_pkg/historic/Historic_2001_2020/2001_2020.tif"), to = "/share")
pgisfn <- paste0("raster2pgsql -s 4326 -I -C -M 2001_2020.tif -t 6x6 historic_periods > 2001_2020.sql")

"GTIFF_DIRECT_IO=YES raster2pgsql -s 4326 -I -M composite_wna_wlrdem.tif -t 50x50 normal_composite > normal_composite.sql &"


metadt <- data.table(period = "2001_2020",fullnm = names(curr_per))
metadt[,laynum := seq_along(metadt$period)]

conn <- dbConnect(RPostgres::Postgres(),dbname = 'climr',
                  host = '146.190.244.244',
                  port = 5432,
                  user = 'postgres',
                  password = 'climr2022')


dbWriteTable(conn, "historic_layers", metadt, row.names = FALSE)

dbExecute(conn, "grant select on all tables in schema public to readaccess")

dbExecute(conn,"drop table normal_wna")
ssh_exec_wait(session, command = c("cd /share",
                                   "ls",
                                   "su postgres",
                                   "psql -d climr -f normal_wna.sql"))

allgcms <- list.files("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climr/inputs_pkg/gcm/")
nms <- c("gcm_access","gcm_bcc","gcm_canesm","gcm_cnrm","gcm_ecearth","gcm_gfdl","gcm_giss","gcm_inm","gcm_ipsl","gcm_miroc6","gcm_mpi1","gcm_mpi2","gcm_ukesm")

t1 <- data.frame(GCM = list_gcm(), dbname = nms)

temp <- rast(paste0("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climr/inputs_pkg/gcm/",gcm,"/gcmData.",gcm,".deltas.tif"))

for(i in 2:13) {
  gcm <- allgcms[i]
  scp_upload(session, paste0("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climr/inputs_pkg/gcm/",gcm,"/gcmData.",gcm,".deltas.tif"), to = "/share")
  pgisfn <- paste0("raster2pgsql -s 4326 -I -C -M gcmData.",gcm,".deltas.tif -t 6x6 ",nms[i]," > ", nms[i],".sql")
  ssh_exec_wait(session, command = c("cd /share",
                                     "ls",
                                     pgisfn))
}


###upload historic data
scp_upload(session, "Historic_Anomolies.tif", to = "/share")
pgisfn <- paste0("raster2pgsql -s 4326 -I -C -M Historic_Anomolies.tif -t 6x6 historic_ts > historic_ts.sql")
ssh_exec_wait(session, command = c("cd /share",
                                   "ls",
                                   pgisfn))
"GTIFF_DIRECT_IO=YES raster2pgsql -s 4326 -I -M Historic_Anomolies.tif -t 6x6 historic_ts > historic_ts.sql &"


metadt <- data.table(Orig = names(all_hist))
metadt[,c("var","period") := tstrsplit(Orig, "_")]
metadt[,laynum := seq_along(Orig)]
setnames(metadt, old = "Orig", new = "fullnm")
dbWriteTable(conn, name = "historic_ts_layers", metadt, row.names = FALSE)
dbExecute(conn, "create index on historic_ts_layers(period)")

for(i in 2:13) {
  gcm <- allgcms[i]
  cat(gcm,"\n")
  temp <- rast(paste0("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climr/inputs_pkg/gcm/",gcm,"/gcmData.",gcm,".deltas.tif"))
  metadat <- names(temp)
  test <- data.table(strsplit(metadat, "_", fixed = T))

  metadt <- data.table(Orig = metadat)
  metadt[,c("Mod","Var","Month","Scenario","Run","DtStart","DtEnd") := tstrsplit(Orig, "_")]
  metadt[,Period := paste(DtStart,DtEnd,sep = "_")]
  metadt[,c("DtStart","DtEnd") := NULL]
  setnames(metadt,c("fullnm", "mod","var","month","scenario","run","period"))
  metadt[,laynum := seq_along(metadt$mod)]
  dbWriteTable(conn, name = "esm_layers", metadt, row.names = FALSE, append = TRUE)
}

dbExecute(conn,"create index on esm_layers(mod,scenario)")

