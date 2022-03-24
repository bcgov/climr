#' Check if we have a matching index for each data in two filename vector
#' @noRd
index_check <- function(findex, fdata) {
  i <- gsub("\\.csv$", ".nc", gsub("Index", "Data", basename(unlist(fdata))))
  d <- basename(unlist(fdata))
  if (any(nomatch <- !d %in% i)) {
    stop("No gcmIndex match found for ", paste0(d[which(nomatch)], collapse = ", "),".")
  }
}
