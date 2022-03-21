future <- function(gcm, ssp, period, max_run) {
  
}
  
list_unique <- function(files, col_num) {
  collection <- character()
  for (file in files) {
    # Read in csv file with headers
    values <- data.table::fread(file, header = TRUE)
    # Remove reference lines
    values <- values[which(!grepl("_reference_", x, fixed = TRUE))]
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

list_parse <- function(gcm, col_num) {
  
  #Default pattern csv extension
  pattern <- "csv$"
  
  # In case we need to filter gcm
  if (!missing(gcm)) {
    pattern <- paste0("(", paste0(gcm, collapse = "|"), ").*", pattern)
  }
  files <- list.files(file.path(data_path(), getOption("climRpnw.gcm.path")), recursive = TRUE, full.names = TRUE, pattern = pattern)
  
  # Extract all different unique values
  list_unique(files, col_num)
}

list_gcm <- function() {
  list.files(file.path(data_path(), getOption("climRpnw.gcm.path")))
}

list_ssp <- function(gcm) {
  list_parse(gcm, 4)
}

list_period <- function(gcm) {
  list_parse(gcm, 6:7)
}