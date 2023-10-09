library(terra)
library(climRpnw)
library(RPostgres)
library(data.table)
library(analogsea)
##add normals
library(ssh)

dat <- fread("C:/Program Files/ClimateNA/Perioddat/Year_1908.ann")
d2 <- fread("C:/Program Files/ClimateNA/Perioddat/cru_index.dat")


dat <- rast("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climRpnw/inputs_pkg/normal/Normal_1961_1990MP/Normal_1961_1990MP.wlrdem.tif")
d2 <- dat[[73]]
library(raster)
tif <- raster(d2)
NAvalue(tif) <- -9999
writeRaster(tif, "C:/DataFiles/wna_normal.asc", overwrite = T)


session <- ssh_connect("root@146.190.244.244")
ssh_exec_wait(session, command = c("cd /share",
                                   "ls",
                                   "raster2pgsql -s 4326 -I -C -M Normal_1961_1990MP.wlrdem.tif -t 50x50 normal_wna > normal_wna.sql"))

curr_per <- rast("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climRpnw/inputs_pkg/historic/Historic_2001_2020/2001_2020.tif")
plot(curr_per[[35]])
scp_upload(session, paste0("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climRpnw/inputs_pkg/historic/Historic_2001_2020/2001_2020.tif"), to = "/share")
pgisfn <- paste0("raster2pgsql -s 4326 -I -C -M 2001_2020.tif -t 6x6 historic_periods > 2001_2020.sql")

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

allgcms <- list.files("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climRpnw/inputs_pkg/gcm/")
nms <- c("gcm_access","gcm_bcc","gcm_canesm","gcm_cnrm","gcm_ecearth","gcm_gfdl","gcm_giss","gcm_inm","gcm_ipsl","gcm_miroc6","gcm_mpi1","gcm_mpi2","gcm_ukesm")

t1 <- data.frame(GCM = list_gcm(), dbname = nms)

temp <- rast(paste0("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climRpnw/inputs_pkg/gcm/",gcm,"/gcmData.",gcm,".deltas.tif"))

for(i in 2:13){
  gcm <- allgcms[i]
  scp_upload(session, paste0("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climRpnw/inputs_pkg/gcm/",gcm,"/gcmData.",gcm,".deltas.tif"), to = "/share")
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

for(i in 2:13){
  gcm <- allgcms[i]
  cat(gcm,"\n")
  temp <- rast(paste0("C:\\Users\\kdaust\\AppData\\Local/R/cache/R/climRpnw/inputs_pkg/gcm/",gcm,"/gcmData.",gcm,".deltas.tif"))
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

