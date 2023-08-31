#' Create gcm input for `downscale`.
#' @param gcm A character vector. Label of the global circulation models to use.
#' Can be obtained from `list_gcm()`. Default to `list_gcm()`.
#' @param ssp A character vector. Label of the shared socioeconomic pathways to use.
#' Can be obtained from `list_ssp()`. Default to `list_ssp()`.
#' @param period A character vector. Label of the period to use.
#' Can be obtained from `list_period()`. Default to `list_period()`.
#' @param max_run An integer. Maximum number of model runs to include.
#' A value of 0 is `ensembleMean` only. Runs are included in the order they are found in the
#' models data untile `max_run` is reached. Default to 0L.
#' @return An object to use with `downscale`. A `SpatRaster` with, possibly, multiple layers.
#' @importFrom terra rast
#' @importFrom utils head
#' @export
gcm_input <- function(gcm = list_gcm(), ssp = list_ssp(), period = list_period() , max_run = 0L) {
  
  # Check if we have data, if not download some.
  data_check()
  
  # Get relevant files
  get_rel_files <- function(pattern, gcm) {
    res <- lapply(
      file.path(
        data_path(),
        getOption("climRpnw.gcm.path", default = "inputs_pkg/gcm"),
        gcm
      ),
      list.files, recursive = TRUE, full.names = TRUE, pattern = pattern
    )
    names(res) <- gcm
    res
  }
  files_tif <- get_rel_files("\\.tif$", gcm)
  
  # Load each file individually + select layers
  process_one_gcm <- function(file_tif, ssp, period) {
    
    # Initiate raster
    r <- terra::rast(file_tif)
    nm <- names(r)
    
    # Select runs + ensembleMean (since alphabetical sort, ensembleMean will be first element)
    runs <- utils::head(
      sort(unique(vapply(strsplit(nm, "_"), `[`, character(1), 5))),
      max_run + 1L
    )
    
    # Select layers
    pattern <- paste0(
      "(",
      paste0(ssp, collapse = "|"),
      ")_(",
      paste0(runs, collapse = "|"),
      ")_(",
      paste0(period, collapse = "|"),
      ")$"
    )
    lyrs <- which(grepl(pattern, nm))
    
    return(r[[lyrs]])
    
  }
  
  res <- lapply(files_tif, process_one_gcm, ssp = ssp, period = period)
  attr(res, "builder") <- "climRpnw" 
  
  # Return a list of SpatRaster, one element for each model
  return(res)
  
}


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
        if(all(period %in% periods[uid == oldid,period]) & all(ssp %in% ssps[uid == oldid,ssp])){
          message("Retrieving from cache...")
          gcm_rast <- terra::rast(paste0(cache_path(),"/gcm/",gcmcode,"/",oldid,".tif"))
          return(gcm_rast)
        }else{
          message("Not fully cached :( Will download more")
        }
        
      }
    }
    
    q <- paste0("select fullnm, laynum from esm_layers where mod = '",gcm_nm,"' and scenario in ('",paste(ssp,collapse = "','"),
                "') and period in ('",paste(period,collapse = "','"),"') and run = 'ensembleMean'")
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
                                   numlay = terra::nlyr(gcm_rast))
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
