#' Download raster with bounding box from PostGIS
#' @param conn a DBI or RPostgres connection object
#' @param name Character. Table name in database
#' @param rast Character. Name of column which stores raster data. Defaul to `rast`
#' @param bands Which raster bands to return. Default 37:73
#' @param boundary Numeric vector of length 4 (ymax,ymin,xmax,xmin). Default `NULL`
#' @return terra rast 
#' @importFrom terra rast add<-
#' @import RPostgres
#' @import DBI
#' @export

pgGetTerra <- function(conn, name, rast = "rast", bands = 37:73,
                       boundary = NULL) {
  
  ## Check and prepare the schema.name
  name1 <- name
  nameque <- paste(name1, collapse = ".")
  namechar <- gsub("'","''",paste(gsub('^"|"$', '', name1),collapse="."))
  
  ## rast query name
  rastque <- dbQuoteIdentifier(conn, rast)
  
  projID <- dbGetQuery(conn, paste0("select ST_SRID(",rastque,") as srid from ",nameque," where rid = 1;"))$srid[1]
  
  # get rast
  if (is.null(boundary)) {
    
    info <- dbGetQuery(conn, paste0("select 
              st_xmax(st_envelope(rast)) as xmx,
              st_xmin(st_envelope(rast)) as xmn,
              st_ymax(st_envelope(rast)) as ymx,
              st_ymin(st_envelope(rast)) as ymn,
              st_width(rast) as cols,
              st_height(rast) as rows
              from
              (select st_union(",rastque,",",1,") rast from ",nameque," ", clauses,") as a;"))
    for (b in bands) {
      
      
      vals <- dbGetQuery(conn,paste0("select
            unnest(st_dumpvalues(rast, 1)) as vals 
            from
            (select st_union(",rastque,",",b,") rast from ",nameque," ", clauses,") as a;"))$vals
      
      rout <- terra::rast(nrows = info$rows, ncols = info$cols, xmin = info$xmn, 
                          xmax = info$xmx, ymin = info$ymn, ymax = info$ymx,
                          crs = paste0("EPSG:",projID), vals = vals)
      
      if(length(bands) > 1) {
        if (b == bands[1]) {
          rb <- rout
        } else {
          add(rb) <- rout ##add layer in place
        }
      }
    }
    
  } else {
    if (typeof(boundary) != "double") {
      boundary <- c(boundary@bbox[2, 2], boundary@bbox[2,
                                                       1], boundary@bbox[1, 2], boundary@bbox[1, 1])
    }
    
    info <- dbGetQuery(conn, paste0("select 
              st_xmax(st_envelope(rast)) as xmx,
              st_xmin(st_envelope(rast)) as xmn,
              st_ymax(st_envelope(rast)) as ymx,
              st_ymin(st_envelope(rast)) as ymn,
              st_width(rast) as cols,
              st_height(rast) as rows
              from
              (select st_union(",rastque,",",1,") rast from ",nameque, "\n
              WHERE ST_Intersects(",
                                    rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
                                    " ", boundary[1], ",", boundary[4], " ", boundary[2],
                                    ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
                                    " ", boundary[1], ",", boundary[4], " ", boundary[1],
                                    "))'),", projID, "))) as a;"))
    
    
    bandqs1 <- paste0("UNNEST(ST_Dumpvalues(rast, ",bands,")) as vals_",bands)
    bandqs2 <- paste0("ST_Union(rast",rastque,",",bands,") rast_",bands)
      
    rast_vals <- dbGetQuery(conn,paste0("SELECT ",paste(bandqs1,collapse = ","), 
           
          " from (SELECT ST_Union(rast) rast FROM ",nameque," WHERE ST_Intersects(",
                                   rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
                                   " ", boundary[1], ",", boundary[4], " ", boundary[2],
                                   ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
                                   " ", boundary[1], ",", boundary[4], " ", boundary[1],
                                   "))'),", projID, "))) as a;"))  
      
    for(b in 1:length(bands)){
      
      rout <- terra::rast(nrows = info$rows, ncols = info$cols, xmin = info$xmn, 
                          xmax = info$xmx, ymin = info$ymn, ymax = info$ymx,
                          crs = paste0("EPSG:",projID), vals = rast_vals[,b])
      
      if(length(bands) > 1) {
        if (b == 1) {
          rb <- rout
        } else {
          add(rb) <- rout ##add layer in place
        }
      }
    }  

    }
  
  if(length(bands) > 1) {rout <- rb}
  return(rout)
}

library(foreach)
dbExtractNormal <- function(dbCon, points, bands = 1:73){

  temp <- paste(points$Long,in_xyz$Lat)
  pts <- paste0("MULTIPOINT((",paste(temp, collapse = "),("),"))")
  
  bandqs <- paste0("ST_Value(normal_wna.rast, ",bands,", geom, true, 'bilinear') as val_",bands)

  q <- paste0("SELECT ",paste(bandqs,collapse = ","),"
FROM ST_Dump(ST_GeomFromText('", pts ,"', 4326)) as dp
JOIN normal_wna ON ST_Intersects(normal_wna.rast, dp.geom)")
  
  dat <- dbGetQuery(dbCon, q)
  
}


#' Find bounding box of data
#' @param in_xyz data.table of points to downscale
#' @return bounding box (e.g. c(51,50,-121,-122))
#' @export

get_bb <- function(in_xyz){
  return(c(max(in_xyz[,2]),min(in_xyz[,2]),max(in_xyz[,1]),min(in_xyz[,1])))
}
