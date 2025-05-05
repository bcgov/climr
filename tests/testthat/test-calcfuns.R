test_that("calc_* functions work", {

  tm <- c(2,4,5,NA,6,-14,-12,-11, NA)
  tmr <- terra::rast(tm |> matrix(3,3, byrow = TRUE))
  
  expect_identical(round(climr:::calc_DD_below_0(2, -14), 4), 393.9186)
  expect_identical(climr:::calc_DD_below_0(2, NA_real_), NA_real_)

  expect_identical(round(climr:::calc_DD_above_5(2, -14, "All"), 4), 0.2144)
  expect_identical(climr:::calc_DD_above_5(2, NA, "All"), NA_real_)

  expect_identical(round(climr:::calc_DD_below_18(2, -14), 4), 892.0826)
  expect_identical(climr:::calc_DD_below_18(2, NA_real_), NA_real_)

  expect_identical(round(climr:::calc_DD_above_18(2, -14, "All"), 4), 0.0001)
  expect_identical(climr:::calc_DD_above_18(2, NA, "All"), NA_real_)

  expect_s4_class(climr:::calc_DD_above_5(6, tmr), "SpatRaster")
  expect_identical(climr:::calc_DD_above_5(6, tmr) |> terra::values(FALSE), climr:::calc_DD_above_5(6, tm))

  t_min_list <- list(
    "1" = -35, "2" = -32, "3" = -25, "4" = -10,
    "5" = -5, "6" = 3, "7" = 15, "8" = 17, "9" = 10, "10" = -5,
    "11" = -20, "12" = -30
  )
  t_max_list <- lapply(t_min_list, \(x) -x)
  t_min_list_r <- lapply(t_min_list, \(x) terra::rast(x |> matrix(1,1)))
  t_max_list_r <- lapply(t_max_list, \(x) terra::rast(x |> matrix(1,1)))

  a <- climr:::calc_EMT(t_min_list, climr:::calc_MCMT(t_min_list), climr:::calc_TD(climr:::calc_MCMT(t_min_list),climr:::calc_MWMT(t_max_list)))
  b <- climr:::calc_EMT(t_min_list_r, climr:::calc_MCMT(t_min_list_r), climr:::calc_TD(climr:::calc_MCMT(t_min_list_r),climr:::calc_MWMT(t_max_list_r)))
  expect_s4_class(b, "SpatRaster")
  expect_equal(a, b |> terra::values(FALSE))

  expect_identical(
    round(climr:::calc_bFFP(td = 30, NFFD = 10, t_min_list = t_min_list), 4),
    214.5964
  )
  expect_identical(climr::calc_bFFP(td = 30, NFFD = NA, t_min_list = t_min_list), NA_real_)
  expect_identical(climr::calc_bFFP(td = NA, NFFD = 10, t_min_list = t_min_list), NA_real_)
  expect_s4_class(climr::calc_bFFP(td = terra::rast(30 |> matrix(1,1)), NFFD = terra::rast(10 |> matrix(1,1)), t_min_list = t_min_list_r), "SpatRaster")

  expect_identical(
    round(climr::calc_eFFP(NFFD = 10, t_min_list = t_min_list), 4),
    265.4581
  )
  expect_identical(climr::calc_eFFP(NFFD = NA, t_min_list = t_min_list), NA_real_)
  expect_s4_class(climr::calc_eFFP(NFFD = terra::rast(10 |> matrix(1,1)), t_min_list = t_min_list), "SpatRaster")

  expect_identical(
    round(climr::calc_FFP(bFFP = 214.5964, eFFP = 265.4581), 4),
    50.8617
  )
  expect_identical(climr::calc_FFP(bFFP = NA, eFFP = 265.4581), NA_real_)
  expect_identical(climr::calc_FFP(bFFP = 214.5964, eFFP = NA), NA_real_)
  expect_s4_class(climr::calc_FFP(bFFP = terra::rast(214.5964 |> matrix(1,1)), eFFP = terra::rast(265.4581 |> matrix(1,1))), "SpatRaster")

  expect_identical(round(climr::calc_NFFD(3, 2.05), 4), 21.1018)
  expect_identical(climr::calc_NFFD(3, NA_real_), NA_real_)
  expect_s4_class(climr::calc_NFFD(3, terra::rast(2.05 |> matrix(1,1))), "SpatRaster")

  expect_identical(round(climr::calc_PAS(4, 2, 600), 4), 308.4204)
  expect_identical(climr::calc_PAS(4, NA, 600), NA_real_)
  expect_identical(climr::calc_PAS(4, 2, NA_real_), NA_real_)
  expect_s4_class(climr::calc_PAS(4, terra::rast(2 |> matrix(1,1)), terra::rast(600 |> matrix(1,1))), "SpatRaster")

  expect_identical(round(climr::calc_RH(tmmin = 10, tmmax = 40), 4), 28.5378)
  expect_identical(climr::calc_RH(tmmin = NA, tmmax = 40), NA_real_)
  expect_identical(climr::calc_RH(tmmin = 10, tmmax = NA), NA_real_)
  expect_s4_class(climr::calc_RH(terra::rast(10 |> matrix(1,1)), terra::rast(40 |> matrix(1,1))), "SpatRaster")
})


test_that("calc_* give sensible outputs", {
  testInit(c("data.table", "terra"))

  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)

  ## the following includes NAs for the test and should be small enough
  xyz <- data.frame(
    lon = c(-128, -125, -128, -125),
    lat = c(50, 50, 48, 48),
    elev = runif(4),
    id = 1:4
  )
  thebb <- get_bb(xyz)

  normalbc <- input_refmap(dbCon = dbCon, reference = "refmap_climr", bbox = thebb, cache = TRUE)

  gcm <- input_gcms(dbCon,
    bbox = thebb,
    gcms = c("BCC-CSM2-MR"),
    ssps = c("ssp126"),
    period = "2041_2060",
    max_run = 0,
    cache = TRUE
  )

  set.seed(678) ## a situation with known NAs (ocean where elev is 0)
  n <- 20
  sample_xyz <- data.frame(
    lon = runif(n, xmin(normalbc$dem2_WNA), xmax(normalbc$dem2_WNA)),
    lat = runif(n, ymin(normalbc$dem2_WNA), ymax(normalbc$dem2_WNA)),
    elev = NA,
    id = 1:n
  )
  sample_xyz[, 3] <- terra::extract(normalbc$dem2_WNA, sample_xyz[, 1:2], method = "bilinear")[, -1L]

  ds_res_bc <- downscale_core(sample_xyz, refmap = normalbc, gcms = gcm, vars = list_vars())

  sample_xyz$ID <- 1:nrow(sample_xyz)
  ds_res_bc <- as.data.table(sample_xyz)[ds_res_bc, on = .(id)]
  ds_res_bc[, .(lat, lon, elev, Tmax, PPT_01, CMD, Eref)]

  ## if elevation of input climate data are NA, downscaled variables should be as well.
  expect_true(all(is.na(ds_res_bc[is.na(elev), .SD, .SDcols = list_vars()])))
  expect_true(all(is.na(ds_res_bc[is.na(PPT_01), .SD, .SDcols = list_vars()])))
  ## conversely, if there is input data, there should be no NAs in downscaled vars
  expect_true(all(!is.na(ds_res_bc[!is.na(PPT_01), .SD, .SDcols = list_vars()])))

  ## TODO: more sanity checks?
})
