library(terra)
library(climr)
library(RPostgres)
library(data.table)
library(analogsea)
##add normals
library(ssh)

library(terra)
dat <- rast("../Common_Files/climr_mosaic_2025/climr_mosiac_wlrdem_compressed.tif")
clamped <- rast("../Common_Files/climr_mosaic_2025/climr_mosaic_clamped.tif")
dat[[37:72]] <- clamped[[37:72]]
plot(dat[[49]])

# Actual writing
terra::writeRaster(
  dat,
  "../Common_Files/climr_mosaic_wlrdem.tif",
  overwrite = TRUE,
  gdal="COMPRESS=NONE"
)

##clamp lapse rates
library(RPostgres)
conn <- dbConnect(RPostgres::Postgres(),dbname = 'climr',
                  host = '146.190.244.244',
                  port = 5432,
                  user = 'postgres',
                  password = 'climr2022')

lim <- 20/1000 # set maximum absolute lapse rate to ±20 K/km (but data is in K/m, so limit is 0.01 K/m)
layers <- c(
  "PPT_01", "PPT_02", "PPT_03", "PPT_04", "PPT_05", "PPT_06", "PPT_07",
  "PPT_08", "PPT_09", "PPT_10", "PPT_11", "PPT_12", "Tmax_01", "Tmax_02",
  "Tmax_03", "Tmax_04", "Tmax_05", "Tmax_06", "Tmax_07", "Tmax_08", "Tmax_09",
  "Tmax_10", "Tmax_11", "Tmax_12", "Tmin_01", "Tmin_02", "Tmin_03", "Tmin_04",
  "Tmin_05", "Tmin_06", "Tmin_07", "Tmin_08", "Tmin_09", "Tmin_10", "Tmin_11",
  "Tmin_12", "lr_PPT_01", "lr_PPT_02", "lr_PPT_03", "lr_PPT_04", "lr_PPT_05",
  "lr_PPT_06", "lr_PPT_07", "lr_PPT_08", "lr_PPT_09", "lr_PPT_10", "lr_PPT_11",
  "lr_PPT_12", "lr_Tmax_01", "lr_Tmax_02", "lr_Tmax_03", "lr_Tmax_04",
  "lr_Tmax_05", "lr_Tmax_06", "lr_Tmax_07", "lr_Tmax_08", "lr_Tmax_09",
  "lr_Tmax_10", "lr_Tmax_11", "lr_Tmax_12", "lr_Tmin_01", "lr_Tmin_02",
  "lr_Tmin_03", "lr_Tmin_04", "lr_Tmin_05", "lr_Tmin_06", "lr_Tmin_07",
  "lr_Tmin_08", "lr_Tmin_09", "lr_Tmin_10", "lr_Tmin_11", "lr_Tmin_12",
  "dem2_WNA"
)
idx <- grep("lr_T", layers) # identify temperature lapse rate layers

clamp_q <- 
"UPDATE refmap_climr
SET rast = ST_AddBand(
    NULL,
    ARRAY(
        SELECT 
            CASE 
                WHEN band >= 49 AND band <=72 THEN 
                    ST_MapAlgebraExpr(
                        rast,
                        band,
                        ST_BandPixelType(rast, band),
                        '[rast] * 2'
                    )
                ELSE ST_Band(rast, band)
            END
        FROM generate_series(1, ST_NumBands(rast)) AS band
    )
);"
dbExecute(conn, clamp_q)


clamp_create <- 
  "CREATE TABLE refmap_climr_clamped AS
SELECT rid,
       ST_AddBand(
           NULL,
           ARRAY(
               SELECT 
                   CASE 
                       WHEN band >= 49 AND band <= 72 THEN
                           ST_MapAlgebraExpr(
                               rast,
                               band,
                               NULL,
                               'CASE 
                                    WHEN [rast] < -0.02 THEN -0.02  
                                    WHEN [rast] > 0.02 THEN 0.02
                                    ELSE [rast] 
                                END'::text
                           )
                       ELSE ST_Band(rast, band)
                   END
               FROM generate_series(1, ST_NumBands(rast)) AS band
           )
       ) AS rast
FROM refmap_climr_save;"

'CASE 
WHEN [rast] < -0.02 THEN -0.02  
WHEN [rast] > 0.02 THEN 0.02
ELSE [rast] 
END'
dbExecute(conn, clamp_create)
clamped <- pgGetTerra("refmap_climr", FALSE, bands = 49, boundary = get_bb(test_pts))
plot(clamped)
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
  dat,
  file.path("../Common_Files/climr_mosaic_wlrdem.tif"),
  overwrite = TRUE
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

"GTIFF_DIRECT_IO=YES raster2pgsql -s 4326 -I -M climr_mosiac_wlrdem_compressed.tif -t 50x50 refmap_climr > climr_mosaic.sql &"


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

