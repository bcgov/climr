library(curl)
library(glue)
library(parallel)

url_template <- "https://ftp.prism.oregonstate.edu/monthly/{var}/{year}/PRISM_{var}_stable_4kmM3_{year}{month}_bil.zip"
years <- 1990:2021
vars <- c("tmax", "tmin", "ppt")
months <- formatC(1:12, width = 2, format = "d", flag = "0")
grid <- expand.grid(var = vars, year = years, month = months)
urls <- glue_data(grid, url_template)

dir.create("../PRISM/DATA/", recursive = TRUE, showWarnings = FALSE)

mclapply(urls, FUN = function(url) {
  try({
    curl_download(url, quiet = TRUE, destfile = file.path("../PRISM/DATA", basename(url)))
  })
})
