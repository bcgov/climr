#' Pre cache data for presets or custom areas
#' @description
#' TODO
#' @param region character. Preset region; options are one of "BC", "WNA", and "NA"
#' @param bbox optional custom bounding box (or SpatExtent) for region of interest. If bbox is specified, region will be ignored.
#'
#' @importFrom terra rast writeRaster ext
#' @importFrom data.table data.table
#' @importFrom uuid UUIDgenerate
#' @importFrom httr GET content
#' @importFrom curl curl_download
#' @export
#' 
pre_cache <- function(region = c("BC","WNA","NA"), bbox = NULL) {
  region <- match.arg(region)
  reg_box <- data.table(rname = c("BC","WNA","NA"),
                        xmin = c(-139.1, -139.17, -179.054166666518),
                        xmax = c(-114.0, -89.9, -52.0041666680861),
                        ymin = c(48.3, 29.92, 13.9958333334196),
                        ymax = c(60.3, 60.3, 83.4958333325618))
  if(!is.null(bbox)){
    message("Custom bounding box provided. Region will be ignored.")
    bb_use <- as.vector(bbox)
  } else {
    temp <- reg_box[rname == region,]
    bb_use <- c(temp$xmin,temp$xmax,temp$ymin,temp$ymax)
  }
  
  res <- httr::GET("http://146.190.244.244:8000/clipr", 
             query = list(rname = "climr_mosaic_clamped.tif", 
                          xmin = bb_use[1], xmax = bb_use[2], 
                          ymin = bb_use[3], ymax = bb_use[4]))
  url <- httr::content(res)$url[[1]]
  tmp <- tempfile()
  curl::curl_download(content(res)$url[[1]], tmp)
  rst <- terra::rast(tmp)
  
  names(rst)[73] <- "dem2_WNA"
  return(rst)
}