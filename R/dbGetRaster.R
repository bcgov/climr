#' Download raster with bounding box from PostGIS
#' @param conn a DBI or RPostgres connection object
#' @param name Character. Table name in database
#' @param rast Character. Name of column which stores raster data. Defaul to `rast`
#' @param bands Which raster bands to return. Default 37:73
#' @param boundary Numeric vector of length 4 (ymax,ymin,xmax,xmin). Default `NULL`
#' @template boundary
#' @importFrom data.table setDT copy cbind
#' @importFrom data.table setDT copy
#' @importFrom terra rast merge
#' @importFrom RPostgres dbQuoteIdentifier dbGetQuery
#' @importFrom DBI dbQuoteIdentifier dbGetQuery
#' @export

pgGetTerra <- function(conn, name, tile, rast = "rast", bands = 37:73,
                       boundary = NULL) {
  
  ## Check and prepare the schema.name
  name1 <- name
  nameque <- paste(name1, collapse = ".")
  namechar <- gsub("'","''",paste(gsub('^"|"$', '', name1),collapse="."))
  
  ## rast query name
  rastque <- dbQuoteIdentifier(conn, rast)
  
  projID <- dbGetQuery(conn, paste0("select ST_SRID(",rastque,") as srid from ",nameque," where rid = 1;"))$srid[1]

  make_raster <- function(boundary){
    cat(".")
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
    rast_vals <- dbGetQuery(conn,paste0("SELECT ",paste(bandqs1,collapse = ","), 
                                        
                                        " from (SELECT ST_Union(rast) rast FROM ",nameque," WHERE ST_Intersects(",
                                        rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
                                        " ", boundary[1], ",", boundary[4], " ", boundary[2],
                                        ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
                                        " ", boundary[1], ",", boundary[4], " ", boundary[1],
                                        "))'),", projID, "))) as a;"))
    setDT(rast_vals)
    if(all(is.na(rast_vals[,1]))){
      warning("Empty tile - not enough data")
      return(NULL)
    }else{
      rast_vals <- suppressWarnings(melt(rast_vals, id.vars = c())) 
      rout <- terra::rast(nrows = info$rows, ncols = info$cols, xmin = info$xmn, 
                          xmax = info$xmx, ymin = info$ymn, ymax = info$ymx, nlyrs = length(bands),
                          crs = paste0("EPSG:",projID), vals = rast_vals$value)
      rout
    }
    
  }

  if(length(bands) > 1664){ ##maximum number of columns
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
    brks <- c(seq(1,length(bands),by = 1663),(length(bands)+1))
    for(i in 1:(length(brks)-1)){
      bands_temp <- bands[brks[i]:(brks[i+1]-1)]
      bandqs1 <- paste0("UNNEST(ST_Dumpvalues(rast, ",bands_temp,")) as vals_",bands_temp)
      bandqs2 <- paste0("ST_Union(rast",rastque,",",bands_temp,") rast_",bands_temp)
      
      rast_vals_temp <- dbGetQuery(conn,paste0("SELECT ",paste(bandqs1,collapse = ","), 
                                          
                                          " from (SELECT ST_Union(rast) rast FROM ",nameque," WHERE ST_Intersects(",
                                          rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
                                          " ", boundary[1], ",", boundary[4], " ", boundary[2],
                                          ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
                                          " ", boundary[1], ",", boundary[4], " ", boundary[1],
                                          "))'),", projID, "))) as a;"))
      setDT(rast_vals_temp)
      if(i == 1){
        rast_vals <- copy(rast_vals_temp)
      }else{
        rast_vals <- cbind(rast_vals, rast_vals_temp)
      }
    }
    rast_vals <- suppressWarnings(melt(rast_vals, id.vars = c()))
    rout <- rast(nrows = info$rows, ncols = info$cols, xmin = info$xmn, 
                        xmax = info$xmx, ymin = info$ymn, ymax = info$ymx, nlyrs = length(bands),
                        crs = paste0("EPSG:",projID), vals = rast_vals$value)
    
  }else{
    if(!tile){
      rout <- make_raster(boundary)
      return(rout)
    }
    max_dist <- 5
    #if(boundary[1] - boundary[2] > max_dist | boundary[3] - boundary[4] > max_dist){
    x_seq <- unique(c(seq(boundary[2], boundary[1], by = max_dist), boundary[1]))
    y_seq <- unique(c(seq(boundary[4], boundary[3], by = max_dist), boundary[3]))
    
    boundary_ls <- list()
    if(length(x_seq) < 2 | length(y_seq) < 2){
      boundary_ls[["11"]] <- boundary
    }else{
      for(i in 1:(length(x_seq) - 1)){
        for(j in 1:(length(y_seq)-1)){
          boundary_ls[[paste0(i,j)]] <- c(x_seq[i+1],x_seq[i],y_seq[j+1],y_seq[j])
        }
      }
    }
    
    
    r_list <- lapply(boundary_ls, FUN = make_raster)
    r_list <- r_list[!sapply(r_list,is.null)]
    if(length(r_list) > 1){
      rout <- merge(sprc(r_list))
    }else{
      rout <- r_list[[1]]
    }
    
  }
  return(rout)
}

# library(foreach)
# dbExtractNormal <- function(dbCon, points, bands = 1:73){
# 
#   temp <- paste(points$Long,in_xyz$Lat)
#   pts <- paste0("MULTIPOINT((",paste(temp, collapse = "),("),"))")
#   
#   bandqs <- paste0("ST_Value(normal_wna.rast, ",bands,", geom, true, 'bilinear') as val_",bands)
# 
#   q <- paste0("SELECT ",paste(bandqs,collapse = ","),"
# FROM ST_Dump(ST_GeomFromText('", pts ,"', 4326)) as dp
# JOIN normal_wna ON ST_Intersects(normal_wna.rast, dp.geom)")
#   
#   dat <- dbGetQuery(dbCon, q)
#   
# }

# info2 <- dbGetQuery(conn, paste0("select 
#               rid,
#               st_xmax(st_envelope(rast)) as xmx,
#               st_xmin(st_envelope(rast)) as xmn,
#               st_ymax(st_envelope(rast)) as ymx,
#               st_ymin(st_envelope(rast)) as ymn,
#               st_width(rast) as cols,
#               st_height(rast) as rows
#               from
#               ",nameque, "
#               WHERE ST_Intersects(",
#                                  rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
#                                  " ", boundary[1], ",", boundary[4], " ", boundary[2],
#                                  ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
#                                  " ", boundary[1], ",", boundary[4], " ", boundary[1],
#                                  "))'),", projID, "))"))
# 
# 
# test <- dbGetQuery(conn, paste0("select distinct rid
#               from ",nameque, "\n
#               WHERE ST_Intersects(",
#                                 rastque, ",ST_SetSRID(ST_GeomFromText('POLYGON((", boundary[4],
#                                 " ", boundary[1], ",", boundary[4], " ", boundary[2],
#                                 ",\n  ", boundary[3], " ", boundary[2], ",", boundary[3],
#                                 " ", boundary[1], ",", boundary[4], " ", boundary[1],
#                                 "))'),", projID, "))"))
# 
# bands <- 1:73
# bandqs1 <- paste0("UNNEST(ST_Dumpvalues(rast, ",bands,")) as vals_",bands)
# get_tile <- function(id){
#   setDT(dbGetQuery(conn,paste0("SELECT ",paste(bandqs1,collapse = ",")," FROM ",nameque," WHERE rid = ",id)))
# }
# 
# tmp <- lapply(info2$rid[1:100], FUN = get_tile)
# t2 <- data.table::rbindlist(tmp)
# 
# rout <- terra::rast(nrows = info$rows, ncols = info$cols, xmin = info$xmn, 
#                     xmax = info$xmx, ymin = info$ymn, ymax = info$ymx, nlyrs = length(bands),
#                     crs = paste0("EPSG:",projID))
# t3 <- as.matrix(info2[,c("xmx","ymx")])
# cells <- cellFromXY(rout, t3)


#' Find bounding box of data
#' @param in_xyz data.table (or data.frame) of points to downscale
#' @return bounding box (e.g. c(51,50,-121,-122))
#' @export
get_bb <- function(in_xyz){
  return(c(max(in_xyz[,2]),min(in_xyz[,2]),max(in_xyz[,1]),min(in_xyz[,1])))
}
