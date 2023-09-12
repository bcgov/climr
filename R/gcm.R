

#' Create gcm input for `downscale` using data on Postgis database.
#' @param dbCon A db connection object created by `data_connect`.
#' @param bbox Numeric vector of length 4 giving bounding box of study region, create by `get_bb()`
#' @param gcm A character vector. Label of the global circulation models to use.
#' Can be obtained from `list_gcm()`. Default to `list_gcm()`.
#' @param ssp A character vector. Label of the shared socioeconomic pathways to use.
#' Can be obtained from `list_ssp()`. Default to `list_ssp()`.
#' @param period A character vector. Label of the period to use.
#' Can be obtained from `list_period()`. Default to `list_period()`.
#' @param max_run An integer. Maximum number of model runs to include.
#' A value of 0 is `ensembleMean` only. Runs are included in the order they are found in the
#' models data untile `max_run` is reached. Default to 0L.
#' @param cache Logical specifying whether to cache new data locally or no. Default `TRUE`
#' @return An object to use with `downscale`. A list of `SpatRaster` with, possibly, multiple layers.
#' @importFrom terra rast
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @import uuid
#' @import data.table
#' @export
gcm_input_postgis <- function(dbCon, bbox = NULL, gcm = list_gcm(), ssp = list_ssp(), period = list_period(), max_run = 0L, cache = TRUE) {

  dbnames <- structure(list(GCM = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", 
                                    "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", 
                                    "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"), 
                            dbname = c("gcm_access", "gcm_bcc", "gcm_canesm", "gcm_cnrm", 
                "gcm_ecearth", "gcm_gfdl", "gcm_giss", "gcm_inm", "gcm_ipsl", 
                "gcm_miroc6", "gcm_mpi1", "gcm_mpi2", "gcm_ukesm")), class = "data.frame", row.names = c(NA, -13L))
  
  # Load each file individually + select layers
  process_one_gcm <- function(gcm_nm, ssp, period) { ##need to update to all GCMs
    gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
    gcm_nm <- gsub("-",".",gcm_nm)
    
    if(dir.exists(paste0(cache_path(),"/gcm/",gcmcode))){
      bnds <- fread(paste0(cache_path(),"/gcm/",gcmcode,"/meta_area.csv"))
      data.table::setorder(bnds, -numlay)
      for(i in 1:nrow(bnds)){
        isin <- is_in_bbox(bbox, matrix(bnds[i,2:5]))
        if(isin) break
      }
      if(isin){
        oldid <- bnds$uid[i]
        periods <- fread(paste0(cache_path(),"/gcm/",gcmcode,"/meta_period.csv"))
        ssps <- fread(paste0(cache_path(),"/gcm/",gcmcode,"/meta_ssp.csv"))
        if(all(period %in% periods[uid == oldid,period]) & all(ssp %in% ssps[uid == oldid,ssp]) & max_run <= bnds[uid == oldid, max_run]){
          message("Retrieving from cache...")
          gcm_rast <- terra::rast(paste0(cache_path(),"/gcm/",gcmcode,"/",oldid,".tif"))
          runs <- sort(dbGetQuery(dbCon, paste0("select distinct run from esm_layers where mod = '",gcm_nm,"'"))$run)
          sel_runs <- runs[1:(max_run+1L)]
          
          layinfo <- data.table(fullnm = names(gcm_rast))
          layinfo[,c("Mod","Var","Month","Scenario","Run","Period1","Period2") := tstrsplit(fullnm, "_")]
          layinfo[,Period := paste(Period1,Period2, sep = "_")]
          layinfo[, laynum := seq_along(fullnm)]
          sel <- layinfo[Scenario %in% ssp & Period %in% period & Run %in% sel_runs,laynum]
          return(gcm_rast[[sel]])
        }else{
          message("Not fully cached :( Will download more")
        }
        
      }
    }
    
    runs <- sort(dbGetQuery(dbCon, paste0("select distinct run from esm_layers where mod = '",gcm_nm,"'"))$run)
    sel_runs <- runs[1:(max_run+1L)]
    
    q <- paste0("select fullnm, laynum from esm_layers where mod = '",gcm_nm,"' and scenario in ('",paste(ssp,collapse = "','"),
                "') and period in ('",paste(period,collapse = "','"),"') and run in ('",paste(sel_runs, collapse = "','"),"')")
    #print(q)
    layerinfo <- dbGetQuery(dbCon, q)
    message("Downloading GCM anomalies")
    gcm_rast <- pgGetTerra(dbCon, gcmcode, bands = layerinfo$laynum, boundary = bbox)
    names(gcm_rast) <- layerinfo$fullnm
    
    if(cache){
      message("Caching data...")
      uid <- uuid::UUIDgenerate()
      if(!dir.exists(paste0(cache_path(), "/gcm/",gcmcode))) dir.create(paste0(cache_path(), "/gcm/",gcmcode), recursive = TRUE)
      terra::writeRaster(gcm_rast, paste0(cache_path(),"/gcm/",gcmcode, "/", uid,".tif"))
      rastext <- terra::ext(gcm_rast)
      t1 <- data.table::data.table(uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1], 
                                   numlay = terra::nlyr(gcm_rast),  max_run = max_run)
      t2 <- data.table::data.table(uid = rep(uid, length(period)),period = period)
      t3 <- data.table::data.table(uid = rep(uid, length(ssp)),ssp = ssp)
      data.table::fwrite(t1, file = paste0(cache_path(),"/gcm/",gcmcode,"/meta_area.csv"), append = TRUE)
      data.table::fwrite(t2, file = paste0(cache_path(),"/gcm/",gcmcode,"/meta_period.csv"), append = TRUE)
      data.table::fwrite(t3, file = paste0(cache_path(),"/gcm/",gcmcode,"/meta_ssp.csv"), append = TRUE)
    }
    
    return(gcm_rast)
  }
  
  res <- lapply(gcm, process_one_gcm, ssp = ssp, period = period)
  attr(res, "builder") <- "climRpnw" 
  
  # Return a list of SpatRaster, one element for each model
  return(res)
  
}

# gcm_nm <- "ACCESS-ESM1-5"
# ssp <- c("ssp245","ssp370")
# period <- 2020:2050

#' Create gcm timeseries input for `downscale` using data on Postgis database.
#' @param dbCon A db connection object created by `data_connect`.
#' @param bbox Numeric vector of length 4 giving bounding box of study region, create by `get_bb()`
#' @param gcm A character vector. Label of the global circulation models to use.
#' Can be obtained from `list_gcm()`. Default to `list_gcm()`.
#' @param ssp A character vector. Label of the shared socioeconomic pathways to use.
#' Can be obtained from `list_ssp()`. Default to `list_ssp()`.
#' @param years Numeric or character vector in in `2020:2100`
#' @param max_run An integer. Maximum number of model runs to include.
#' A value of 0 is `ensembleMean` only. Runs are included in the order they are found in the
#' models data untile `max_run` is reached. Default to 0L.
#' @param cache Logical specifying whether to cache new data locally or no. Default `TRUE`
#' @return An object to use with `downscale`. A list of `SpatRaster` with, possibly, multiple layers.
#' @importFrom terra rast
#' @importFrom utils head
#' @importFrom RPostgres dbGetQuery
#' @import uuid
#' @import data.table
#' @export
gcm_ts_input <- function(dbCon, bbox = NULL, gcm = list_gcm_ts(), ssp = list_ssp(), years = list_years_ts(), max_run = 0L, cache = TRUE) {
  
  period <- years
  dbnames <- structure(list(GCM = c("ACCESS-ESM1-5", "BCC-CSM2-MR", "CanESM5", 
                                    "CNRM-ESM2-1", "EC-Earth3", "GFDL-ESM4", "GISS-E2-1-G", "INM-CM5-0", 
                                    "IPSL-CM6A-LR", "MIROC6", "MPI-ESM1-2-HR", "MRI-ESM2-0", "UKESM1-0-LL"), 
                            dbname = c("gcmts_access", "gcmts_bcc", "gcmts_canesm", "gcmts_cnrm", 
                                       "gcmts_ecearth", "gcmts_gfdl", "gcmts_giss", "gcmts_inm", "gcmts_ipsl", 
                                       "gcmts_miroc6", "gcmts_mpi1", "gcmts_mpi2", "gcmts_ukesm")), class = "data.frame", row.names = c(NA, -13L))
  if(nrow(dbnames) < 1) stop("That isn't a valid GCM")
  # Load each file individually + select layers
  process_one_gcm <- function(gcm_nm, ssp, period) { ##need to update to all GCMs
    gcmcode <- dbnames$dbname[dbnames$GCM == gcm_nm]
    
    if(dir.exists(paste0(cache_path(),"/gcmts/",gcmcode))){
      bnds <- fread(paste0(cache_path(),"/gcmts/",gcmcode,"/meta_area.csv"))
      data.table::setorder(bnds, -numlay)
      for(i in 1:nrow(bnds)){
        isin <- is_in_bbox(bbox, matrix(bnds[i,2:5]))
        if(isin) break
      }
      if(isin){
        oldid <- bnds$uid[i]
        periods <- fread(paste0(cache_path(),"/gcmts/",gcmcode,"/meta_period.csv"))
        ssps <- fread(paste0(cache_path(),"/gcmts/",gcmcode,"/meta_ssp.csv"))
        if(all(period %in% periods[uid == oldid,period]) & all(ssp %in% ssps[uid == oldid,ssp]) & max_run <= bnds[uid == oldid, max_run]){
          message("Retrieving from cache...")
          gcm_rast <- terra::rast(paste0(cache_path(),"/gcmts/",gcmcode,"/",oldid,".tif"))
          runs <- sort(dbGetQuery(dbCon, paste0("select distinct run from esm_layers_ts where mod = '",gcm_nm,"'"))$run)
          sel_runs <- runs[1:(max_run+1L)]
          
          layinfo <- data.table(fullnm = names(gcm_rast))
          layinfo[,c("Mod","Var","Month","Scenario","Run","Year") := tstrsplit(fullnm, "_")]
          layinfo[, laynum := seq_along(fullnm)]
          sel <- layinfo[Scenario %in% ssp & Year %in% period & Run %in% sel_runs,laynum]
          return(gcm_rast[[sel]])
        }else{
          message("Not fully cached :( Will download more")
        }
        
      }
    }

    runs <- sort(dbGetQuery(dbCon, paste0("select distinct run from esm_layers_ts where mod = '",gcm_nm,"'"))$run)
    if(length(runs) < 1) stop("That GCM isn't in our database yet.")
    sel_runs <- runs[1:(max_run+1L)]
    
    q <- paste0("select fullnm, laynum from esm_layers_ts where mod = '",gcm_nm,"' and scenario in ('",paste(ssp,collapse = "','"),
                "') and period in ('",paste(period,collapse = "','"),"') and run in ('",paste(sel_runs, collapse = "','"),"')")
    #print(q)
    layerinfo <- dbGetQuery(dbCon, q)
    message("Downloading GCM anomalies")
    message("Precip...")
    gcm_rast_ppt <- pgGetTerra(dbCon, paste0(gcmcode,"_ppt"), bands = layerinfo$laynum, boundary = bbox)
    names(gcm_rast_ppt) <- gsub("Tmax","PPT", layerinfo$fullnm)
    message("Tmax...")
    gcm_rast_tmax <- pgGetTerra(dbCon, paste0(gcmcode,"_tmax"), bands = layerinfo$laynum, boundary = bbox)
    names(gcm_rast_tmax) <- layerinfo$fullnm
    message("Tmin...")
    gcm_rast_tmin <- pgGetTerra(dbCon, paste0(gcmcode,"_tmin"), bands = layerinfo$laynum, boundary = bbox)
    names(gcm_rast_tmin) <- gsub("Tmax","Tmin", layerinfo$fullnm)
    gcm_rast <- c(gcm_rast_ppt, gcm_rast_tmax, gcm_rast_tmin)
    
    if(cache){
      message("Caching data...")
      uid <- uuid::UUIDgenerate()
      if(!dir.exists(paste0(cache_path(), "/gcmts/",gcmcode))) dir.create(paste0(cache_path(), "/gcmts/",gcmcode), recursive = TRUE)
      terra::writeRaster(gcm_rast, paste0(cache_path(),"/gcmts/",gcmcode, "/", uid,".tif"))
      rastext <- terra::ext(gcm_rast)
      t1 <- data.table::data.table(uid = uid, ymax = rastext[4], ymin = rastext[3], xmax = rastext[2], xmin = rastext[1], 
                                   numlay = terra::nlyr(gcm_rast), max_run = max_run)
      t2 <- data.table::data.table(uid = rep(uid, length(period)),period = period)
      t3 <- data.table::data.table(uid = rep(uid, length(ssp)),ssp = ssp)
      data.table::fwrite(t1, file = paste0(cache_path(),"/gcmts/",gcmcode,"/meta_area.csv"), append = TRUE)
      data.table::fwrite(t2, file = paste0(cache_path(),"/gcmts/",gcmcode,"/meta_period.csv"), append = TRUE)
      data.table::fwrite(t3, file = paste0(cache_path(),"/gcmts/",gcmcode,"/meta_ssp.csv"), append = TRUE)
    }
    
    return(gcm_rast)
  }
  
  res <- lapply(gcm, process_one_gcm, ssp = ssp, period = period)
  attr(res, "builder") <- "climRpnw" 
  
  # Return a list of SpatRaster, one element for each model
  return(res)
  
}


#' Read and parse gcm models csv files
#' @param files A character vector. File paths.
#' @param col_num An integer vector. Positions of elements to retrieve in label. Label is split
#' by "_" before processing.
#' @return A character vector of unique values.
list_unique <- function(files, col_num) {
  collection <- character()
  for (file in files) {
    # Read in csv file with headers
    values <- data.table::fread(file, header = TRUE)
    # Remove reference lines
    values <- values[which(!grepl("_reference_", values[["x"]], fixed = TRUE)),]
    # Split and extract sub part of x according to col_num
    values <- vapply(strsplit(values[["x"]], "_"), `[`, character(length(col_num)), col_num)
    # In case we have more than one col_num, put them back together
    if (length(col_num) > 1L) {
      values <- apply(values, 2, paste0, collapse = "_")
    }
    # Reassign collection to unique values
    collection <- unique(c(values, collection))
  }
  # Sort and return
  return(sort(collection))
}

#' Read and parse gcm models csv files
#' @param gcm An optional character vector. Limit list to provided global circulation models.
#' @param col_num An integer vector. 
#' @return A character vector of unique values.
list_parse <- function(gcm, col_num = 1) {
  
  #Default pattern csv extension
  pattern <- "\\.csv$"
  
  # In case we need to filter gcm
  if (!missing(gcm)) {
    pattern <- paste0("(", paste0(gcm, collapse = "|"), ").*", pattern)
  }
  files <- list.files(
    file.path(
      data_path(),
      getOption("climRpnw.gcm.path", default = "inputs_pkg/gcm")
    ),
    recursive = TRUE,
    full.names = TRUE,
    pattern = pattern
  )
  
  # Extract all different unique values
  list_unique(files, col_num)
}

#' List available global circulation models
#' @importFrom RPostgres dbGetQuery
#' @export
list_gcm <- function(dbCon) {
  sort(dbGetQuery(dbCon, "SELECT DISTINCT mod FROM esm_layers")[,1])
}

#' List available shared socioeconomic pathways
#' @param dbCon database connection
#' @importFrom RPostgres dbGetQuery
#' @export
list_ssp <- function(dbCon) {
  sort(dbGetQuery(dbCon, "SELECT DISTINCT scenario FROM esm_layers")[,1])
}

#' List available period
#' @param dbCon database connection
#' @importFrom RPostgres dbGetQuery
#' @export
list_period <- function(dbCon) {
  sort(dbGetQuery(dbCon, "SELECT DISTINCT period FROM esm_layers")[,1])
}

#' List available runs
#' @param dbCon database connection
#' @param gcm An optional character vector. Limit list to provided global circulation models.
#' @importFrom RPostgres dbGetQuery
#' @export
list_run <- function(dbCon, gcm) {
  sort(dbGetQuery(dbCon, paste0("SELECT DISTINCT run FROM esm_layers WHERE mod IN ('",paste(gcm, collapse = "','","')")))[,1])
}
