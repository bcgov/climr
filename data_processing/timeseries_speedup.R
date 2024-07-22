library(terra)
library(data.table)
library(climr)
library(stringi)
library(DBI)
library(dplyr)

library(RPostgres)
conn <- dbConnect(RPostgres::Postgres(),dbname = 'climr',
                  host = '146.190.244.244',
                  port = 5432,
                  user = 'postgres',
                  password = 'climr2022')

pts <- data.frame(lon = c(-124.11, -125.11), lat = rep(48.82, 2), elev = rep(25,2), id = 1:2)

bbox <- get_bb(pts)
dbcon <- data_connect()

res <- input_gcm_ssp(dbcon, bbox, gcms = "GFDL-ESM4", years = 2020:2050, 
                     max_run = 1L, cache = T, fast = T)

input_gcm_ssp_fast <- function(dbCon, bbox = NULL, gcm_nm = list_gcms(), ssps = list_ssps(),
                               years = 2020:2030, max_run = 0L, cache = TRUE){
  gcmcode <- "gfdl_template"
  gcmarray <- "gfdl_array"
  ssps <- c("ssp245", "ssp370")
  years = 2020:2030
  
  template <- pgGetTerra(dbCon, name = gcmcode, tile = F, bands = 1, boundary = bbox)
  
  if(length(years) >= 79){ ##faster if almost all years are selected
    results <- tbl(dbCon, sql(paste0("select cellid, ssp, year, run, vals from ",gcmarray," where cellid in (",paste0(values(template)[,1], collapse = ','),") 
                                  and ssp in ('",paste(ssps, collapse = "','"),"')")))
  }else{
    results <- tbl(dbCon, sql(paste0("select cellid, ssp, year, run, vals from ",gcmarray," where cellid in (",paste0(values(template)[,1], collapse = ','),") 
                                  and year in ('",paste(years, collapse = "','"),"') and ssp in ('",paste(ssps, collapse = "','"),"')")))
  }
  
  dat <-
    results %>% 
    mutate(vals = unnest(vals)) %>%
    collect()
  setDT(dat)
  setorder(dat, cellid, year, ssp, run)
  dat[, month := rep(sort(sprintf(c("PPT_%02d", "Tmax_%02d", "Tmin_%02d"), sort(rep(1:12, 3)))), 
                            nrow(dat)/(12*3))]
  
  dat[, fullnm := paste(month, ssp, run, year, sep = "_")]
  cell_nums = cells(template)
  coords <- rowColFromCell(template, cell_nums)
  cellcoords <- data.table(coords, values(template))
  setnames(cellcoords, c("row","col","cellid"))
  dat[cellcoords, `:=`(row = i.row, col = i.col), on = "cellid"]
  temp <- dat[,.(row,col,fullnm,vals)]
  t2 <- dcast(temp, fullnm + row ~ col, value.var = "vals")
  
  t_array = split(as.data.frame(t2[,!c("fullnm","row")]), t2$fullnm)
  t3 <- abind(t_array, along = 3)
  t_rast <- rast(t3)
  ext(t_rast) <- ext(template)
  names(t_rast) <- names(t_array)
  return(t_rast)
}

bbox <- get_bb(pts)
dbcon <- data_connect()

res <- input_gcm_ssp_fast(dbCon = dbcon, bbox, ssps = ssps, years = 2020:2050)

template <- pgGetTerra(conn, name = "gfdl_template", tile = F, bands = 1, boundary = bbox)
#plot(template)
results <- tbl(conn, sql(paste0("select cellid, ssp, year, run, vals from gfdl_array where cellid in (",paste0(values(template)[,1], collapse = ','),") order by cellid, year, ssp, run")))
results <- tbl(conn, sql(paste0("select cellid, ssp, year, run, vals from gfdl_array where cellid in (",paste0(values(template)[,1], collapse = ','),")")))

approach_2 <-
  results %>% 
  mutate(vals = unnest(vals)) %>%
  collect()

setDT(approach_2)
setorder(approach_2, cellid, year, ssp, run)
approach_2[, month := rep(sort(sprintf(c("PPT_%02d", "Tmax_%02d", "Tmin_%02d"), sort(rep(1:12, 3)))), 
                          nrow(approach_1)/(12*3))]

approach_1$month = rep(sort(sprintf(c("PPT_%02d", "Tmax_%02d", "Tmin_%02d"), sort(rep(1:12, 3)))), 
                       nrow(approach_1)/(12*3))
setDT(approach_1)

approach_1[, fullnm := paste(month, ssp, run, year, sep = "_")]
cell_nums = cells(template)
coords <- rowColFromCell(template, cell_nums)
cellcoords <- data.table(coords, values(template))
setnames(cellcoords, c("row","col","cellid"))
approach_1[cellcoords, `:=`(row = i.row, col = i.col), on = "cellid"]
temp <- approach_1[,.(row,col,fullnm,vals)]
t2 <- dcast(temp, fullnm + row ~ col, value.var = "vals")
library(abind)
t2[,fullnm := as.numeric(as.factor(fullnm))]
t_array = split(as.data.frame(t2[,!c("fullnm","row")]), t2$fullnm)
t3 <- abind(t_array, along = 3)
t_rds <- sds(x = t3)
t_rast <- rast(t3)
ext(t_rast) <- ext(template)
names(t_rast) <- names(t_array)
plot(t_rast)

library(str2str)
library(torch)
Sys.setenv(CUDA=FALSE)
torch_tensor(1)
t_array <- d2a(temp, dim.nm = c("row","col","fullnm"))
t_array <- array(data = temp$vals, 
             dim=c(length(unique(df$x)), 
                   length(unique(df$y)), 
                   length(unique(df$i))), 
             dimnames=list(unique(df$x), unique(df$y), unique(df$i))
)

t2 <- dcast(approach_1, fullnm ~ cellid, value.var = "vals")

temp <- copy(template)
cellids <- as.numeric(names(t2)[-1])
t3 <- as.matrix(t2[,!"fullnm"])
for(i in 1:nrow(t2)){ ##too slow!
  values(template) <- t3[i,]
  if(i == 1){
    ts_out <- copy(template)
  }else{
    add(ts_out) <- template
  }
}

##need x and y - use getXYfromCell, convert to array
gcm_nm <- "GFDL-ESM4"
template_nm <- "access_template.tif"
table_nm <- "access_ts_tbl"

create_ts_table <- function(gcm_nm, template_nm, tmp_table_nm, table_nm){
  fnames <- list.files("../Common_Files/gcmts_deltas/", pattern = gcm_nm, full.names = T)
  for(fname in fnames){
    cat("Processing",fname,"\n")
    orig <- rast(fname)
    if(fname == fnames[[1]]){
      template <- orig[[1]]
      temp <- 1:ncell(template)
      values(template) <- temp
      writeRaster(template, paste0("../Common_Files/gcmts_templates/",template_nm))
    }
    nlays <- nlyr(orig)
    splits <- c(seq(1,nlays,by = 10000),nlays+1)
    for(i in 1:(length(splits)-1)){
      cat("Start:",splits[i],"Stop:",(splits[i+1]-1),"\n")
      t2 <- values(orig[[splits[i]:(splits[i+1]-1)]], mat = TRUE) ##need to process in chunks
      t2 <- data.table(t2)
      t2[,cellid := 1:nrow(t2)]
      t3 <- melt(t2, id.vars = "cellid")
      t3[,c("model","var","month","ssp","run","year") := transpose(stri_split_fixed(variable,"_"))]
      t3[,c("variable","model") := NULL] 
      dbWriteTable(conn, tmp_table_nm, t3, row.names = F, append = TRUE)
    }
  }
  q <- 
    paste0("create table ",table_nm," as (
        select cellid, ssp, run, year, array_agg(value) as vals
        from ",tmp_table_nm,"
        group by cellid, ssp, run, year
        )")
  cat("array aggregate\n")
  dbExecute(conn, q)
  
}


q <- paste0("create table gfdl_array as (
        select cellid, ssp, run, year, array_agg(value) as vals
        from (select * from gfdl_temp order by cellid, year, ssp, run, var, month) as a 
        group by cellid, ssp, run, year
        )")

dbExecute(conn, q)
create_ts_table("GFDL-ESM4","gfdl_template.tif","gfdl_temp","gfdl_array")

orig <- rast("../Common_Files/gcmts_deltas/gcmts.tmin.ACCESS-ESM1-5.deltas.tif")
template <- orig[[1]]
temp <- 1:ncell(template)
values(template) <- temp
plot(template)

writeRaster(template, "access_template.tif")


dat2 <- dbGetQuery(conn, "select cellid, ssp, year, vals from access_array where cellid in (1998,1999,2000,2001,2002,2003)")
setDT(dat2)
results <- tbl(conn, sql("select cellid, ssp, year, run, vals from access_array where cellid in (1998,1999,2000,2001,2002,2003)"))
approach_1 <-
  results %>% 
  mutate(vals = unnest(vals)) %>%
  collect()

s <- orig[[1:12]]
values(s) <- NA

setDT(approach_1)
tmp <- approach_1[ssp == "ssp370" & year == "2050" & run == "ensembleMean",]
tmp[, month := rep(1:12, nrow(tmp)/12)]
t2 <- dcast(tmp, cellid ~ month, value.var = "vals")
set.values(s, t2$cellid, as.matrix(t2[,!"cellid"]), layer = 1:12)
plot(s)
##rows are cellids, cols are layer values

library(terra)
s <- rast(ncols=5, nrows=5, nlyrs=3)
set.values(s, 1:10, runif(10), layer=2)

set.values(s, 11:20, cbind(1, runif(10)), layer=2:3)
