test_that("test gcm_input", {
  testInit("terra")

  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)

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

  # Select GCM
  gcms <- list("BCC-CSM2-MR", c("BCC-CSM2-MR", "INM-CM5-0"))
  ssps <- list("ssp126", c("ssp126", "ssp370"))
  periods <- list("2041_2060", c("2001_2020", "2041_2060"))
  max_runs <- c(0, 2)

  test_combos <- expand.grid(gcms = gcms, ssps = ssps, periods = periods, max_runs = max_runs)
  out <- apply(test_combos, 1, FUN = function(x) {
    gcm <- gcm_input(
      dbCon,
      thebb,
      gcm = x$gcms,
      ssp = x$ssps,
      period = x$periods,
      max_run = x$max_runs,
      cache = TRUE
    )

    testOut <- sapply(gcm, FUN = function(gcm) {
      expect_true(is(gcm, "SpatRaster"))

      test <- all(sapply(x$ssps, function(x) any(grepl(x, names(gcm)))))
      test2 <- all(sapply(x$periods, function(x) any(grepl(x, names(gcm)))))

      baseVars <- 36 ## number of base variables for run

      ## add 1 to max runs for ensemble.
      test3 <- nlyr(gcm) <= baseVars * (x$max_runs + 1) * length(x$ssps) * length(x$periods)

      ## number or layers should be a product of the number of base climate variables
      test4 <- nlyr(gcm) / baseVars == nlyr(gcm) %/% baseVars

      return(c(test, test2, test3, test4))
    })
    return(all(testOut) & all(x$gcms %in% names(gcm)))
  })
  expect_true(all(out))
})

test_that("test gcm_hist_input", {
  library(terra)

  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)

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

  # Select GCM
  gcms <- list("BCC-CSM2-MR", c("BCC-CSM2-MR", "INM-CM5-0"))
  set.seed(123)
  years <- sample(1902:2015, 5)
  years <- lapply(years, function(year) year:2015)
  max_runs <- c(0, 2)

  test_combos <- expand.grid(gcms = gcms, years = years, max_runs = max_runs)

  out <- apply(test_combos, 1, FUN = function(x) {
    gcm_hist <- gcm_hist_input(
      dbCon,
      thebb,
      gcm = x$gcms,
      years = x$years,
      max_run = x$max_runs,
      cache = TRUE
    )

    testOut <- sapply(gcm_hist, FUN = function(gcm_hist) {
      expect_true(is(gcm_hist, "SpatRaster"))

      availYears <- sub(".*([[:digit:]]{4}).*", "\\1", names(gcm_hist)) |>
        unique() |>
        as.numeric()

      test <- isInRange(availYears, x$years)

      baseVars <- 36 ## number of base variables for run

      ## some models may not have all years
      ## add 1 to max runs for ensemble.
      test2 <- nlyr(gcm_hist) <= baseVars * (x$max_runs + 1) * length(x$years)

      ## number or layers should be a product of the number of base climate variables
      test3 <- nlyr(gcm_hist) / baseVars == nlyr(gcm_hist) %/% baseVars
      return(c(test, test2, test3))
    })
    return(all(testOut) & all(x$gcms %in% names(gcm_hist)))
  })
  expect_true(all(out))
})

test_that("test gcm_ts_input", {
  library(terra)

  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)

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

  # Select GCM
  gcms <- list("BCC-CSM2-MR", c("BCC-CSM2-MR", "INM-CM5-0"))
  ssps <- list("ssp126", c("ssp126", "ssp370"))
  set.seed(123)
  years <- sample(2020:2100, 5)
  years <- lapply(years, function(year) year:2100)
  max_runs <- c(0, 2)

  test_combos <- expand.grid(gcms = gcms, ssps = ssps, years = years, max_runs = max_runs)

  out <- apply(test_combos, 1, FUN = function(x) {
    gcm_ts <- gcm_ts_input(
      dbCon,
      thebb,
      gcm = x$gcms,
      ssp = x$ssps,
      years = x$years,
      max_run = x$max_runs,
      cache = TRUE
    )

    testOut <- sapply(gcm_ts, FUN = function(gcm_ts) {
      expect_true(is(gcm_ts, "SpatRaster"))

      availYears <- sub(".*([[:digit:]]{4}).*", "\\1", names(gcm_ts)) |>
        unique() |>
        as.numeric()

      test <- isInRange(availYears, x$years)
      test2 <- all(sapply(x$ssps, function(x) any(grepl(x, names(gcm_ts)))))

      baseVars <- 36 ## number of base variables for run

      ## some models may not have all the years
      ## add 1 to max runs for ensemble.
      test3 <- nlyr(gcm_ts) <= baseVars * (x$max_runs + 1) * length(x$years) * length(x$ssps)

      ## number or layers should be a product of the number of base climate variables
      test4 <- nlyr(gcm_ts) / baseVars == nlyr(gcm_ts) %/% baseVars

      return(c(test, test2, test3, test4))
    })
    return(all(testOut) & all(x$gcms %in% names(gcm_ts)))
  })
  expect_true(all(out))
})
