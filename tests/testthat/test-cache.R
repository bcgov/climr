test_that("test cache in default location", {
  xyz <- data.frame(
    lon = c(
      -127.70521, -127.62279, -127.56235, -127.7162,
      -127.18585, -127.1254, -126.94957, -126.95507
    ),
    lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
    elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
    id = seq_len(8)
  )

  thebb <- get_bb(xyz)

  expect_identical(cache_path(), R_user_dir("climr", "cache"))

  expecteddirs <- normalizePath(file.path(cache_path(), c("gcm", "normal", "historic")),
    winslash = "/", mustWork = FALSE
  )

  #cache_clear()
  normal <- input_refmap(bbox = thebb, cache = FALSE)
  cachedirs <- normalizePath(list.dirs(cache_path(), recursive = FALSE), winslash = "/")
  #expect_false(any(expecteddirs %in% cachedirs))

  gcm1 <- input_gcms(
    thebb,
    gcms = "BCC-CSM2-MR",
    ssps = c("ssp126"),
    period = "2041_2060",
    max_run = 1, cache = FALSE
  )
  cachedirs <- normalizePath(list.dirs(cache_path(), recursive = FALSE), winslash = "/")
  #expect_false(any(expecteddirs %in% cachedirs))

  ds_res <- downscale(xyz,
    which_refmap = "refmap_climr", obs_periods = "2001_2020",
    cache = FALSE
  )
  cachedirs <- normalizePath(list.dirs(cache_path(), recursive = FALSE), winslash = "/")
  #expect_false(any(expecteddirs %in% cachedirs))

  normal <- input_refmap(bbox = thebb)
  expect_true("reference" %in% list.files(cache_path()))

  gcm1 <- input_gcms(
    thebb,
    gcms = "BCC-CSM2-MR",
    ssps = c("ssp126"),
    period = "2041_2060",
    max_run = 1
  )
  expect_true("gcms" %in% list.files(cache_path()))

  ds_res <- downscale(xyz, which_refmap = "refmap_climr", obs_periods = "2001_2020")
  expect_true("obs" %in% list.files(cache_path()))


  cache_clear("gcms")
  expect_false("gcms" %in% list.files(cache_path()))
  expect_true("reference" %in% list.files(cache_path()))

  # gcm1 <- gcm_input(
  #   dbCon,
  #   thebb,
  #   gcm = "BCC-CSM2-MR",
  #   ssp = c("ssp126"),
  #   period = "2041_2060",
  #   max_run = 1
  # )
  # cache_clear("normal")
  # expect_false("normal" %in% list.files(cache_path()))
  # expect_true("gcm" %in% list.files(cache_path()))
  # 
  # cache_clear("historic")
  # expect_true("gcm" %in% list.files(cache_path()))
  # expect_false("historic" %in% list.files(cache_path()))
  # 
  # cache_clear()
  # ds_res <- climr_downscale(xyz,
  #   which_normal = "normal_bc", historic_period = "2001_2020",
  #   gcm_models = list_gcm()[1], gcm_period = "2041_2060",
  #   ssp = list_ssp()[2]
  # )
  # cachedirs <- normalizePath(list.dirs(cache_path(), recursive = FALSE), winslash = "/")
  # test <- all(expecteddirs %in% cachedirs)
  # expect_true(test)
  # 
  # cache_clear()
  # cachedirs <- normalizePath(list.dirs(cache_path(), recursive = FALSE), winslash = "/")
  # test <- all(expecteddirs %in% cachedirs)
  # expect_false(test)
})

# test_that("test cache in custom location", {
#   opts <- options("climr.cache.path" = "~/test_climr")
#   on.exit(options(opts), add = TRUE)
#   expect_identical(cache_path(), "~/test_climr")
# 
#   expecteddirs <- suppressWarnings(normalizePath(file.path(cache_path(), c("gcm", "normal", "historic")), winslash = "/"))
# 
#   xyz <- data.frame(
#     lon = c(
#       -127.70521, -127.62279, -127.56235, -127.7162,
#       -127.18585, -127.1254, -126.94957, -126.95507
#     ),
#     lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
#     elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
#     id = seq_len(8)
#   )
# 
#   cache_clear()
#   ds_res <- climr_downscale(xyz,
#     which_normal = "auto", historic_period = "2001_2020",
#     gcm_models = list_gcm()[1], gcm_period = "2041_2060",
#     ssp = list_ssp()[2]
#   )
# 
#   cachedirs <- normalizePath(list.dirs(cache_path(), recursive = FALSE), winslash = "/")
#   test <- all(expecteddirs %in% cachedirs)
#   expect_true(test)
# 
#   cache_clear()
#   cachedirs <- normalizePath(list.dirs(cache_path(), recursive = FALSE), winslash = "/")
#   test <- all(expecteddirs %in% cachedirs)
#   expect_false(test)
# })

test_that("test cache works within same bbox", {

  xyz <- data.frame(
    lon = c(
      -127.70521, -127.62279, -127.56235, -127.7162,
      -127.18585, -127.1254, -126.94957, -126.95507
    ),
    lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
    elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
    id = seq_len(8)
  )
  thebb <- get_bb(xyz)

  #cache_clear()
  normal <- input_refmap(bbox = thebb)
  normal2 <- input_refmap(bbox = thebb)

  expect_true(terra::compareGeom(normal, normal2, res = TRUE, lyrs = TRUE, stopOnError = FALSE))
})
