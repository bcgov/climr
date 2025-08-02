# plumber.R
library(plumber)
library(glue)
library(uuid)

#* @apiTitle climr-clipr
#* @apiDescription Clip a section from the climatology raster by bounding box

#* Clip a raster by bounding box
#* @param rname name of raster file
#* @param xmin Minimum longitude
#* @param xmax Maximum longitude
#* @param ymin Minimum latitude
#* @param ymax Maximum latitude
#* @get /clipr
function(rname, xmin, xmax, ymin, ymax) {
  xmin <- as.numeric(xmin)
  xmax <- as.numeric(xmax)
  ymin <- as.numeric(ymin)
  ymax <- as.numeric(ymax)

  # Define paths
  src_raster <- glue("/opts/climatology/{rname}")
  out_id <- UUIDgenerate()
  out_file <- glue("/var/www/html/downloads/clip_{out_id}.tif")
  url <- glue("http://146.190.244.244/downloads/clip_{out_id}.tif")
  
  # Run gdalwarp
  cmd <- c(
    "-te", xmin, ymin, xmax, ymax,
    "-of", "GTiff",
    "-co", "COMPRESS=LZW",
    src_raster,
    out_file
  )
  message("Running gdalwarp: ", paste("gdalwarp", cmd))
  result <- system2("gdalwarp", cmd, stdout = TRUE, stderr = TRUE)
  
  if (!file.exists(out_file)) {
    return(list(error = "gdalwarp failed", details = result))
  }
  
  list(
    url = url
  )
}


###test query
library(httr)

res <- GET("http://146.190.244.244:8000/clipr", query = list(rname = "climr_mosaic_clamped.tif",
                                                             xmin = -127, xmax = -123, ymin = 48, ymax = 53))
httr::content(res)
library(curl)
tmp <- tempfile()
curl_download(content(res)$url[[1]], tmp)
rst <- rast(tmp)


h <- curl::new_handle()
curl::handle_setheaders(h, "Content-Type" = "application/json")
bbox <- list(rname = "climr_mosaic_clamped.tif", xmin = -139.1, xmax = -114.0, ymin = 48.3, ymax = 60.0)
json <- jsonlite::toJSON(bbox, auto_unbox = TRUE)
curl::handle_setopt(h, customrequest = "POST", postfields = json)

con <- curl::curl_fetch_memory("http://146.190.244.244:8000/clipr", handle = h)
clip_url <- readLines(con)
close(con)