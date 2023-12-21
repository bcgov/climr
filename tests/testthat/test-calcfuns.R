test_that("calc_* functions work", {
  expect_identical(round(climr:::calc_DD_below_0(2, -14), 4), 393.9186)
  expect_identical(climr:::calc_DD_below_0(2, NA_real_), NA_real_)
  
  expect_identical(round(climr:::calc_DD_above_5(2, -14, "All"), 4), 0.2144)
  expect_identical(climr:::calc_DD_above_5(2, NA, "All"), NA_real_)

  expect_identical(round(climr:::calc_DD_below_18(2, -14), 4), 892.0826)
  expect_identical(climr:::calc_DD_below_18(2, NA_real_), NA_real_)

  expect_identical(round(climr:::calc_DD_above_18(2, -14, "All"), 4), 0.0001)
  expect_identical(climr:::calc_DD_above_18(2, NA, "All"), NA_real_)

  t_min_list <- list(
    "1" = -35, "2" = -32, "3" = -25, "4" = -10,
    "5" = -5, "6" = 3, "7" = 15, "8" = 17, "9" = 10, "10" = -5,
    "11" = -20, "12" = -30
  )

  expect_identical(round(climr:::calc_bFFP(td = 30, NFFD = 10, t_min_list = t_min_list), 4),
                   214.5964)
  expect_identical(climr:::calc_bFFP(td = 30, NFFD = NA, t_min_list = t_min_list), NA_real_)
  expect_identical(climr:::calc_bFFP(td = NA, NFFD = 10, t_min_list = t_min_list), NA_real_)

  expect_identical(round(climr:::calc_eFFP(NFFD = 10, t_min_list = t_min_list), 4),
                   265.4581)
  expect_identical(climr:::calc_eFFP(NFFD = NA, t_min_list = t_min_list), NA_real_)

  expect_identical(round(climr:::calc_FFP(bFFP = 214.5964, eFFP = 265.4581), 4),
                   50.8617)
  expect_identical(climr:::calc_FFP(bFFP = NA, eFFP = 265.4581), NA_real_)
  expect_identical(climr:::calc_FFP(bFFP = 214.5964, eFFP = NA), NA_real_)

  expect_identical(round(climr:::calc_NFFD(3, 2.05), 4), 21.1018)
  expect_identical(climr:::calc_NFFD(3, NA_real_), NA_real_)

  expect_identical(round(climr:::calc_PAS(4, 2, 600), 4), 308.4204)
  expect_identical(climr:::calc_PAS(4, NA, 600), NA_real_)
  expect_identical(climr:::calc_PAS(4, 2, NA_real_), NA_real_)

  expect_identical(round(climr:::calc_RH(tmin = 10, tmax = 40), 4), 28.5378)
  expect_identical(climr:::calc_RH(tmin = NA, tmax = 40), NA_real_)
  expect_identical(climr:::calc_RH(tmin = 10, tmax = NA), NA_real_)
})


test_that("calc_* give sensible outputs", {
  library(pool)
  library(data.table)
  library(terra)
  
  dbCon <- data_connect()
  on.exit(poolClose(dbCon), add = TRUE)
  
  ## the following includes NAs for the test
  xyz <- data.frame(lon = c(-128, -125, -128, -125), lat = c(50, 50, 48, 48), elev = runif(4))
  thebb <- get_bb(xyz)
  
  normalbc <- normal_input(dbCon = dbCon, normal = "normal_bc", bbox = thebb, cache = TRUE) 
  
  gcm <- gcm_input(dbCon, bbox = thebb, 
                   gcm = c("BCC-CSM2-MR"), 
                   ssp = c("ssp126"), 
                   period = "2041_2060",
                   max_run = 0,
                   cache = TRUE)
  
  set.seed(678)  ## a situation with known NAs (ocean where elev is 0)
  n <- 20
  sample_xyz <- data.frame(lon = runif(n, xmin(normalbc$dem2_WNA), xmax(normalbc$dem2_WNA)), 
                           lat = runif(n, ymin(normalbc$dem2_WNA), ymax(normalbc$dem2_WNA)), 
                           elev = NA)
  sample_xyz[, 3] <- terra::extract(normalbc$dem2_WNA, sample_xyz[, 1:2], method = "bilinear")[, -1L]
  
  ds_res_bc <- downscale(sample_xyz, normal = normalbc, gcm = gcm, var = list_variables())
  
  sample_xyz$ID <- 1:nrow(sample_xyz)
  ds_res_bc <- as.data.table(sample_xyz)[ds_res_bc, on = .(ID)]
  ds_res_bc[, .(lat, lon, elev, Tmax, PPT01, CMD, Eref)]
  
  ## if elevation of inout climate data are NA, downscaled variables should be as well.
  expect_true(all(is.na(ds_res_bc[is.na(elev), .SD, .SDcols = list_variables()])))
  expect_true(all(is.na(ds_res_bc[is.na(PPT01), .SD, .SDcols = list_variables()])))
  ## conversely, if there is input data, there should be no NAs in downscaled vars
  expect_true(all(!is.na(ds_res_bc[!is.na(PPT01), .SD, .SDcols = list_variables()])))
  
  ## TODO: more sanity checks?
})
