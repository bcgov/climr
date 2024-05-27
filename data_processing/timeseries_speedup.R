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


orig <- rast("../Common_Files/gcmts_deltas/gcmts.ppt.ACCESS-ESM1-5.deltas.tif")
t2 <- values(orig[[30001:nlyr(orig)]], mat = TRUE)
t2 <- data.table(t2)
t2[,cellid := 1:nrow(t2)]
t3 <- melt(t2, id.vars = "cellid")
t3[,c("model","var","month","ssp","run","year") := transpose(stri_split_fixed(variable,"_"))]
t3[,c("variable","model") := NULL] 
dbWriteTable(conn, "access_test", t3, row.names = F, append = TRUE)

q <- 
"create table access_array as (
select cellid, ssp, run, year, array_agg(value) as vals
from access_test
group by cellid, ssp, run, year
)"

dbExecute(conn, q)

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
