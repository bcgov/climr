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
#' @details Will use raster package for now. Switch to terra methods once it gets
#' better performance. See
#' https://gis.stackexchange.com/questions/413105/terrarast-vs-rasterbrick-for-loading-in-nc-files.
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
#' @export
list_gcm <- function() {
  list.files(
    file.path(
      data_path(),
      getOption("climRpnw.gcm.path", default = "inputs_pkg/gcm")
    )
  )
}

#' List available shared socioeconomic pathways
#' @param gcm An optional character vector. Limit list to provided global circulation models.
#' @export
list_ssp <- function(gcm) {
  list_parse(gcm, 4)
}

#' List available period
#' @param gcm An optional character vector. Limit list to provided global circulation models.
#' @export
list_period <- function(gcm) {
  list_parse(gcm, 6:7)
}

#' List available runs
#' @param gcm An optional character vector. Limit list to provided global circulation models.
#' @export
list_run <- function(gcm) {
  list_parse(gcm, 5)
}
