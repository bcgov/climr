test_that("test cache in default location", {
  dbCon <- data_connect()
  on.exit(poolClose(dbCon), add = TRUE)
  xyz <- data.frame(Long = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                             -127.18585, -127.1254, -126.94957, -126.95507), 
                    Lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025), 
                    Elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
                    ID = LETTERS[1:8],
                    Zone = c(rep("CWH",3), rep("CDF",5)),
                    Subzone = c("vm1","vm2","vs1",rep("mm",3),"dk","dc"))
  
  thebb <- get_bb(xyz)
  
  expect_identical(cache_path(), R_user_dir("climr", "cache"))
  on.exit(unlink(cache_path(), recursive = TRUE, force = TRUE), add = TRUE)
  
  expecteddirs <- normalizePath(file.path(cache_path(), c("gcm", "normal", "historic")), winslash = "/")

  cache_clear()
  normal <- normal_input(dbCon = dbCon, bbox = thebb, cache = FALSE)
  cachedirs <- normalizePath(list.dirs(cache_path(), recursive = FALSE), winslash = "/")
  expect_false(any(expecteddirs %in% cachedirs))
  
  gcm1 <- gcm_input(
    dbCon, 
    thebb,
    gcm = "BCC-CSM2-MR",
    ssp = c("ssp126"),
    period = "2041_2060",
    max_run = 1, cache = FALSE
  )
  cachedirs <- normalizePath(list.dirs(cache_path(), recursive = FALSE), winslash = "/")
  expect_false(any(expecteddirs %in% cachedirs))
  
  ds_res <- climr_downscale(xyz, which_normal = "BC", historic_period = "2001_2020",
                            cache = FALSE)
  cachedirs <- normalizePath(list.dirs(cache_path(), recursive = FALSE), winslash = "/")
  expect_false(any(expecteddirs %in% cachedirs))
  
  normal <- normal_input(dbCon = dbCon, bbox = thebb)
  expect_true("normal" %in% list.files(cache_path()))
  
  gcm1 <- gcm_input(
    dbCon, 
    thebb,
    gcm = "BCC-CSM2-MR",
    ssp = c("ssp126"),
    period = "2041_2060",
    max_run = 1
  )
  expect_true("gcm" %in% list.files(cache_path()))
  
  cache_clear("gcm")
  expect_false("gcm" %in% list.files(cache_path()))
  expect_true("normal" %in% list.files(cache_path()))
  
  gcm1 <- gcm_input(
    dbCon, 
    thebb,
    gcm = "BCC-CSM2-MR",
    ssp = c("ssp126"),
    period = "2041_2060",
    max_run = 1
  )
  cache_clear("normal")
  expect_false("normal" %in% list.files(cache_path()))
  expect_true("gcm" %in% list.files(cache_path()))
  
  normal <- normal_input(dbCon = dbCon, bbox = thebb, cache = TRUE)
  cache_clear()
  expect_false("normal" %in% list.files(cache_path()))
  expect_false("gcm" %in% list.files(cache_path()))
  
  
  ## TODO add tests for historic
  browser()
  
}

test_that("test cache in custom location", {
  options("climr.cache.path" = "~/test_climr/")
  expect_identical(cache_path(), "~/test_climr/")
  
  on.exit(unlink(cache_path(), recursive = TRUE, force = TRUE))
  
  cache_clear()
  normal <- normal_input(dbCon = dbCon, bbox = thebb, cache = FALSE)
  expect_identical(length(list.files(cache_path())), 0L)
  
  cache_clear()
  gcm1 <- gcm_input(
    dbCon, 
    thebb,
    gcm = "BCC-CSM2-MR",
    ssp = c("ssp126"),
    period = "2041_2060",
    max_run = 1, cache = FALSE
  )
  expect_identical(length(list.files(cache_path())), 0L)
  
  normal <- normal_input(dbCon = dbCon, bbox = thebb)
  expect_true("normal" %in% list.files(cache_path()))
  
  gcm1 <- gcm_input(
    dbCon, 
    thebb,
    gcm = "BCC-CSM2-MR",
    ssp = c("ssp126"),
    period = "2041_2060",
    max_run = 1
  )
  expect_true("gcm" %in% list.files(cache_path()))
  
  cache_clear("gcm")
  expect_false("gcm" %in% list.files(cache_path()))
  expect_true("normal" %in% list.files(cache_path()))
  
  gcm1 <- gcm_input(
    dbCon, 
    thebb,
    gcm = "BCC-CSM2-MR",
    ssp = c("ssp126"),
    period = "2041_2060",
    max_run = 1
  )
  cache_clear("normal")
  expect_false("normal" %in% list.files(cache_path()))
  expect_true("gcm" %in% list.files(cache_path()))
  
  normal <- normal_input(dbCon = dbCon, bbox = thebb, cache = TRUE)
  cache_clear()
  expect_false("normal" %in% list.files(cache_path()))
  expect_false("gcm" %in% list.files(cache_path()))
  
}


test_that("test cache works within same bbox", {
  browser() ## TODO
}