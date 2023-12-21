test_that("test cache in default location", {
  expect_identical(cache_path(), R_user_dir("climr", "cache"))
  
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