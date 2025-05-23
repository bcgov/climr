#!/bin/bash

# Install R and docker
sudo apt-get install -qq -y r-base libcurl4-openssl-dev gdal-bin proj-bin

# Should probably create your own data processing pipeline to ingest climate data
sudo Rscript -e 'if (!requireNamespace("curl", quietly = TRUE)) install.packages("curl")'
sudo Rscript -e '
library(curl)
urls <- expand.grid(
  "https://media.forestry.ubc.ca",
  "ClimateNA",
  c("WNA"),
  c("800m"),
  c("Normal_1981_2010MSY"),
  c("MAT","MWMT","MCMT","TD","MAP","MSP","AHM","SHM","DD_0","DD5","DD_18","DD18","NFFD","bFFP","eFFP","FFP","PAS","EMT","Eref","CMD","RH","CMI","DD1040",
    "Tmax_wt","Tmax_sp","Tmax_sm","Tmax_at",
    "Tmin_wt","Tmin_sp","Tmin_sm","Tmin_at",
    "Tave_wt","Tave_sp","Tave_sm","Tave_at",
    "PPT_wt","PPT_sp","PPT_sm","PPT_at",
    "Rad_wt","Rad_sp","Rad_sm","Rad_at",
    "DD_0_wt","DD_0_sp","DD_0_sm","DD_0_at",
    "DD5_wt","DD5_sp","DD5_sm","DD5_at",
    "DD_18_wt","DD_18_sp","DD_18_sm","DD_18_at",
    "DD18_wt","DD18_sp","DD18_sm","DD18_at",
    "NFFD_wt","NFFD_sp","NFFD_sm","NFFD_at",
    "PAS_wt","PAS_sp","PAS_sm","PAS_at",
    "Eref_wt","Eref_sp","Eref_sm","Eref_at",
    "CMD_wt","CMD_sp","CMD_sm","CMD_at",
    "RH_wt","RH_sp","RH_sm","RH_at",
    "CMI_wt","CMI_sp","CMI_sm","CMI_at",
    "Tmax01","Tmax02","Tmax03","Tmax04","Tmax05","Tmax06","Tmax07","Tmax08","Tmax09","Tmax10","Tmax11","Tmax12",
    "Tmin01","Tmin02","Tmin03","Tmin04","Tmin05","Tmin06","Tmin07","Tmin08","Tmin09","Tmin10","Tmin11","Tmin12",
    "Tave01","Tave02","Tave03","Tave04","Tave05","Tave06","Tave07","Tave08","Tave09","Tave10","Tave11","Tave12",
    "PPT01","PPT02","PPT03","PPT04","PPT05","PPT06","PPT07","PPT08","PPT09","PPT10","PPT11","PPT12",
    "Rad01","Rad02","Rad03","Rad04","Rad05","Rad06","Rad07","Rad08","Rad09","Rad10","Rad11","Rad12",
    "DD_0_01","DD_0_02","DD_0_03","DD_0_04","DD_0_05","DD_0_06","DD_0_07","DD_0_08","DD_0_09","DD_0_10","DD_0_11","DD_0_12",
    "DD5_01","DD5_02","DD5_03","DD5_04","DD5_05","DD5_06","DD5_07","DD5_08","DD5_09","DD5_10","DD5_11","DD5_12",
    "DD_18_01","DD_18_02","DD_18_03","DD_18_04","DD_18_05","DD_18_06","DD_18_07","DD_18_08","DD_18_09","DD_18_10","DD_18_11","DD_18_12",
    "DD18_01","DD18_02","DD18_03","DD18_04","DD18_05","DD18_06","DD18_07","DD18_08","DD18_09","DD18_10","DD18_11","DD18_12",
    "NFFD01","NFFD02","NFFD03","NFFD04","NFFD05","NFFD06","NFFD07","NFFD08","NFFD09","NFFD10","NFFD11","NFFD12",
    "PAS01","PAS02","PAS03","PAS04","PAS05","PAS06","PAS07","PAS08","PAS09","PAS10","PAS11","PAS12",
    "Eref01","Eref02","Eref03","Eref04","Eref05","Eref06","Eref07","Eref08","Eref09","Eref10","Eref11","Eref12",
    "RH01","RH02","RH03","RH04","RH05","RH06","RH07","RH08","RH09","RH10","RH11","RH12",
    "CMD01","CMD02","CMD03","CMD04","CMD05","CMD06","CMD07","CMD08","CMD09","CMD10","CMD11","CMD12",
    "CMI01","CMI02","CMI03","CMI04","CMI05","CMI06","CMI07","CMI08","CMI09","CMI10","CMI11","CMI12") |> paste("tif", sep = ".")
) |> apply(1, paste0, collapse = "/")

unlink("/var/www/html/climr-tif", recursive = TRUE)
destfiles <- gsub("https://media.forestry.ubc.ca/ClimateNA", "/var/www/html/climr-tif", urls, fixed = TRUE)
dirname(destfiles) |> unique() |> lapply(dir.create, recursive = TRUE, showWarnings = FALSE) |> unlist() |> all()

res <- curl::multi_download(urls, destfiles)

failed <- which(res$status_code != 200)
if (length(failed)) {
  cat("Failed:", res$url[failed], sep = "\n")
  unlink(res$destfile[failed])
}'

cd /var/www/html/climr-tif/WNA/800m/Normal_1981_2010MSY
mkdir compress

# Recompress tif using LERC, adjust MAX_Z_ERROR for higher compression when it will be supported by geotiff.js
# and included in georaster-layer-for-leaflet release.
# Currently georaster-layer-for-leaflet is 3.10.0 in leafem and geotiff supports LZW : 
# https://gdal.org/en/stable/drivers/raster/gtiff.html#configuration-options
# for file in *.tif; do
#     gdal_translate -co COMPRESS=LERC_ZSTD -co MAX_Z_ERROR=0.5 --quiet "$file" "compress/$file"
# done &

# Using available compression in geotiff.js
# https://github.com/GeoTIFF/georaster-layer-for-leaflet/issues/151
# https://github.com/r-spatial/leafem/commit/01a1202dcdc22316b50eb575a8c5da04d4270f4c
# Currently 0.2.3 which relies on geotiff.js 1.0.0-beta13
for file in *.tif; do
    gdal_translate -co COMPRESS=LZW -co PREDICTOR=2 -b 1 --quiet "$file" "compress/$file"
done

mv compress/* ./
rm -R compress