library(curl)
library(climr)
library(terra)

# ClimateNA tif dir
dir1 <- "../climna-tif/WNA/800m/Normal_1981_2010MSY"
# Testing dir
dir2 <- "../climna-tif/test"

# --- Download tif in ClimateNA folder

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

unlink(dir1, recursive = TRUE)
destfiles <- gsub("https://media.forestry.ubc.ca/ClimateNA", dir, urls, fixed = TRUE)
dirname(destfiles) |> unique() |> lapply(dir.create, recursive = TRUE, showWarnings = FALSE) |> unlist() |> all()

res <- curl::multi_download(urls, destfiles)

failed <- which(res$status_code != 200)
if (length(failed)) {
  cat("Failed:", res$url[failed], sep = "\n")
  unlink(res$destfile[failed])
}

# --- Elevation

curl::curl_download("http://www.cec.org/files/atlas_layers/0_reference/0_03_elevation/elevation_tif.zip", "elevation_tif.zip")
unzip("elevation_tif.zip", files = "Elevation_TIF/NA_Elevation/data/northamerica/northamerica_elevation_cec_2023.tif", junkpaths = TRUE)

# --- Copy to test dir

f <- expand.grid(c("Tmin","Tmax","PPT"), sprintf("%02d.tif", 1:12)) |> do.call(what = paste0)
dir.create(dir2, recursive = TRUE, showWarnings = FALSE)
file.copy(file.path(dir1, f), file.path(dir2, gsub("([0-9]{2})", "_\\1", f)))

# --- Add elevation matching raster and latitute

climr::get_elev_raster(
  r = file.path(dir1, f[1]) |> terra::rast(),
  elev = "northamerica_elevation_cec_2023.tif" |> terra::rast(),
  out = file.path(dir2, "elev.tif"),
  gdal = "PREDICTOR=2",
  datatype="FLT4S",
  overwrite = TRUE
)
climr::get_latitude_raster(
  r = file.path(dir1, f[1]) |> terra::rast(),
  out = file.path(dir2, "lat.tif") |> terra::rast(),
  gdal = "PREDICTOR=2",
  datatype="FLT4S",
  overwrite = TRUE
)

# --- Running test

tif1 <- list.files(dir1, full.names = TRUE)
names(tif1) <- basename(tif1)

tif2 <- list.files(dir2, full.names = TRUE)
names(tif2) <- basename(tif2)
names(tif2) <- gsub("^CMD_([01])", "CMD\\1", names(tif2))
names(tif2) <- gsub("^Tmin_([01])", "Tmin\\1", names(tif2))
names(tif2) <- gsub("^Tmax_([01])", "Tmax\\1", names(tif2))
names(tif2) <- gsub("^Tave_([01])", "Tave\\1", names(tif2))
names(tif2) <- gsub("^PPT_([01])", "PPT\\1", names(tif2))
names(tif2) <- gsub("^NFFD_([01])", "NFFD\\1", names(tif2))
names(tif2) <- gsub("^PAS_([01])", "PAS\\1", names(tif2))
names(tif2) <- gsub("^RH_([01])", "RH\\1", names(tif2))
names(tif2) <- gsub("^Eref_([01])", "Eref\\1", names(tif2))
names(tif2) <- gsub("^CMI_([01])", "CMI\\1", names(tif2))
names(tif2) <- gsub("^DDsub", "DD_", names(tif2))

setdiff(names(tif1), names(tif2))
nms <- character()
for (nm in intersect(names(tif1), names(tif2))) {
  r1 <- terra::rast(tif1[nm]) |> terra::values()
  r2 <- terra::rast(tif2[nm]) |> terra::values()
  d <- 0.01 * (max(r1, na.rm = TRUE) - min(r1, na.rm = TRUE))
  r <- abs(r1 - r2) |> max(na.rm = TRUE)
  if (r > d) {
    nms <- c(nms, nm)
    cat("Diff greater than 1/100 of value range [%s : %s]\n" |> sprintf(nm, r))
  }
}

# Diff great [AHM.tif : 3.42449951171875]
# Diff great [bFFP.tif : 386.093170166016]
# Diff great [CMD.tif : 49.3239135742188]
# Diff great [CMD_at.tif : 25.0139007568359]
# Diff great [CMD_sm.tif : 17.7144775390625]
# Diff great [CMD_sp.tif : 36.5646667480469]
# Diff great [CMD_wt.tif : 21.9290657043457]
# Diff great [CMD01.tif : 17.4473686218262]
# Diff great [CMD02.tif : 21.9290657043457]
# Diff great [CMD03.tif : 37.910213470459]
# Diff great [CMD04.tif : 36.0780487060547]
# Diff great [CMD05.tif : 30.674690246582]
# Diff great [CMD06.tif : 4.46575927734375]
# Diff great [CMD07.tif : 7.12744140625]
# Diff great [CMD08.tif : 7.58460998535156]
# Diff great [CMD09.tif : 6.46281433105469]
# Diff great [CMD10.tif : 4.14728546142578]
# Diff great [CMD11.tif : 19.3652896881104]
# Diff great [CMD12.tif : 10.962043762207]
# Diff great [CMI_sm.tif : 3.92394256591797]
# Diff great [CMI06.tif : 1.14522266387939]
# Diff great [CMI07.tif : 1.50902938842773]
# Diff great [DD_0.tif : 421.60107421875]
# Diff great [DD_0_02.tif : 9.79901123046875]
# Diff great [DD_0_03.tif : 12.8479614257812]
# Diff great [DD_0_05.tif : 15.1445007324219]
# Diff great [DD_0_06.tif : 167]
# Diff great [DD_0_07.tif : 134]
# Diff great [DD_0_08.tif : 101]
# Diff great [DD_0_09.tif : 27.0429534912109]
# Diff great [DD_0_10.tif : 7.9012451171875]
# Diff great [DD_0_sm.tif : 403]
# Diff great [DD18.tif : 49.2410049438477]
# Diff great [DD18_01.tif : 4.08891344070435]
# Diff great [DD18_02.tif : 3.64555692672729]
# Diff great [DD18_03.tif : 5.72125434875488]
# Diff great [DD18_04.tif : 8.50337600708008]
# Diff great [DD18_05.tif : 11.1737041473389]
# Diff great [DD18_06.tif : 4.21340179443359]
# Diff great [DD18_09.tif : 5.53520202636719]
# Diff great [DD18_10.tif : 13.7069616317749]
# Diff great [DD18_11.tif : 5.07001399993896]
# Diff great [DD18_12.tif : 3.1611008644104]
# Diff great [DD18_at.tif : 18.5691719055176]
# Diff great [DD18_sp.tif : 22.6721954345703]
# Diff great [DD18_wt.tif : 10.0511417388916]
# Diff great [DD5.tif : 79.5703125]
# Diff great [DD5_01.tif : 13.6930541992188]
# Diff great [DD5_02.tif : 16.8125762939453]
# Diff great [DD5_03.tif : 27.3280906677246]
# Diff great [DD5_04.tif : 18.3931407928467]
# Diff great [DD5_05.tif : 4.76759338378906]
# Diff great [DD5_11.tif : 15.1592559814453]
# Diff great [DD5_12.tif : 13.9898300170898]
# Diff great [DD5_at.tif : 16.6695556640625]
# Diff great [DD5_sp.tif : 42.6187133789062]
# Diff great [DD5_wt.tif : 42.9378662109375]
# Diff great [eFFP.tif : 290.035758972168]
# Diff great [EMT.tif : 12.4451484680176]
# Diff great [Eref.tif : 79.3153076171875]
# Diff great [Eref_at.tif : 39.4773635864258]
# Diff great [Eref_sm.tif : 62.2797546386719]
# Diff great [Eref_sp.tif : 70.403923034668]
# Diff great [Eref_wt.tif : 47.9436798095703]
# Diff great [Eref01.tif : 27.8221530914307]
# Diff great [Eref02.tif : 33.9290657043457]
# Diff great [Eref03.tif : 49.0174903869629]
# Diff great [Eref04.tif : 62.817024230957]
# Diff great [Eref05.tif : 70.403923034668]
# Diff great [Eref06.tif : 60.5051803588867]
# Diff great [Eref07.tif : 58.2895126342773]
# Diff great [Eref08.tif : 43.8408508300781]
# Diff great [Eref09.tif : 27.8304710388184]
# Diff great [Eref10.tif : 38.1808967590332]
# Diff great [Eref11.tif : 29.2792072296143]
# Diff great [Eref12.tif : 23.962043762207]
# Diff great [FFP.tif : 365]
# Diff great [NFFD_sm.tif : 1.08108520507812]
# Diff great [NFFD_wt.tif : 0.880374908447266]
# Diff great [NFFD01.tif : 0.633146286010742]
# Diff great [NFFD02.tif : 0.622203826904297]
# Diff great [NFFD03.tif : 0.645286560058594]
# Diff great [NFFD04.tif : 0.633195877075195]
# Diff great [NFFD05.tif : 0.662938117980957]
# Diff great [NFFD06.tif : 0.687604904174805]
# Diff great [NFFD07.tif : 0.711000442504883]
# Diff great [NFFD08.tif : 0.714864730834961]
# Diff great [NFFD09.tif : 0.635313987731934]
# Diff great [NFFD10.tif : 0.651657104492188]
# Diff great [NFFD11.tif : 0.61573600769043]
# Diff great [NFFD12.tif : 0.639187812805176]
# Diff great [RH.tif : 3.57956314086914]
# Diff great [RH_at.tif : 3.21304321289062]
# Diff great [RH_sm.tif : 1.34122467041016]
# Diff great [RH_sp.tif : 2.16805267333984]
# Diff great [RH_wt.tif : 1.74424362182617]
# Diff great [RH01.tif : 0.950111389160156]
# Diff great [RH02.tif : 0.92364501953125]
# Diff great [RH03.tif : 0.943824768066406]
# Diff great [RH04.tif : 0.881156921386719]
# Diff great [RH05.tif : 0.886215209960938]
# Diff great [RH06.tif : 0.830184936523438]
# Diff great [RH07.tif : 0.877166748046875]
# Diff great [RH08.tif : 0.830757141113281]
# Diff great [RH09.tif : 1.7294921875]
# Diff great [RH10.tif : 0.904144287109375]
# Diff great [RH11.tif : 0.924400329589844]
# Diff great [RH12.tif : 0.942039489746094]
# Diff great [SHM.tif : 26.0684204101562]