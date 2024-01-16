test_that("test downscale", {
  testInit("terra")
  
  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)
  
  xyz <- data.frame(lon = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                            -127.18585, -127.1254, -126.94957, -126.95507), 
                    lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025), 
                    elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
                    id = LETTERS[1:8])
  
  ## get bounding box based on input points
  thebb <- get_bb(xyz)
  
  # Create a normal baseline
  normal <- normal_input(dbCon = dbCon, bbox = thebb, cache = TRUE)
  
  # Select GCM
  gcms <- c("BCC-CSM2-MR", "INM-CM5-0")
  gcm2_spp2 <- gcm_input(
    dbCon, 
    thebb,
    gcm = gcms,
    ssp = c("ssp126", "ssp370"),
    period = "2041_2060",
    max_run = 2
  )
  
  # Resample 4000 points from the available data
  dem <- normal$dem2_WNA
  set.seed(678)
  n <- 4000
  xyz <- data.frame(lon = runif(n, xmin(dem), xmax(dem)), 
                    lat = runif(n, ymin(dem), ymax(dem)), 
                    elev = NA,
                    id = 1:n)
  xyz[, 3] <- extract(dem, xyz[, 1:2], method = "bilinear")[, -1L]
  expect_false(any(is.na(xyz)))
  
  # Use downscale with all variables
  results <- downscale(
    xyz = xyz,
    normal = normal,
    gcm = gcm2_spp2,
    var = list_variables()
  )
  
  ## there may be NAs if the points call on areas without data (e.g. ocean and using normal_bc, but points are being BC)
  ## even with NAs some variables may get a 0 (e.g. DD)
  ## sanity checks: use points in BC that we know are not NA/0
  ##    the values should be in similar ranges to climateBC's outputs (use Tonlig Wang's downscaling method in app)
  
  ## fix calc_ functions to output NAs and check that NAs match absent data from normal and gcm.
  
  # Test for creation
  testthat::expect_true(all(list_variables() %in% names(results)))
  # Test for order
  testthat::expect_equal(tail(names(results), length(list_variables())), list_variables())
  
  ## we should sanity check the results too
  
  ## test parallelisation
  results2 <- downscale(
    xyz = xyz,
    normal = normal,
    gcm = gcm2_spp2,
    var = list_variables()[1:3]
  )
  
  results3 <- downscale(
    xyz = xyz,
    normal = normal,
    gcm = gcm2_spp2,
    var = list_variables()[1:3],
    nthread = 2
  )
  
  roundFun <- function(x) {
    if (is.numeric(x)) {
      round(x, 6)
    } else {
      x
    }
  }
  results2 <- results2[, lapply(.SD, roundFun)]
  results3 <- results3[, lapply(.SD, roundFun)]
  
  testthat::expect_true(all.equal(results2, results3))
})

test_that("test downscale outputs with gcm, gcm_hist, gcm_ts, historic and historic_ts", {
  testInit("terra")
  
  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)
  
  xyz <- data.frame(lon = c(-127.70), 
                    lat = c(55.35), 
                    elev = c(291L),
                    id = LETTERS[1])
  
  ## get bounding box based on input points
  thebb <- get_bb(xyz)
  
  # Create a normal baseline
  normal <- normal_input(dbCon = dbCon, bbox = thebb, cache = TRUE)
  
  # Select GCM
  gcms <- c("BCC-CSM2-MR", "INM-CM5-0")
  
  gcm <- gcm_input(
    dbCon, 
    thebb,
    gcm = gcms,
  )
  
  gcm_hist <- gcm_hist_input(
    dbCon, 
    thebb,
    gcm = gcms,
  )
  
  gcm_ts <- gcm_ts_input(
    dbCon, 
    thebb,
    gcm = gcms
  )
  
  historic <- historic_input(
    dbCon, 
    thebb
  )
  
  historic_ts <- historic_input_ts(
    dbCon, 
    thebb
  )
  
  ## read in the reference points and outputs
  ref_xyz <- readRDS("tests/points_downscale_ref.rds")
  list_refs <- list.files("tests", "downscaleout", full.names = TRUE)
  
  refs <- lapply(list_refs, readRDS)
  names(refs) <- sub(".rds", "", basename(list_refs))
  
  list_args <- list(gcm = gcm, gcm_hist = gcm_hist, gcm_ts = gcm_ts, 
                    historic = historic, historic_ts = historic_ts)
  dwnscaleOut <- Map(argname = names(list_args), 
      argvalue = list_args,
      f = function(argname, argvalue, normal) {
        allArgs <- list(xyz = ref_xyz,
                        normal = normal,
                        var = list_variables(),
                        new = argvalue)
        names(allArgs) <- sub("new", argname, names(allArgs))
        
        out <- do.call(downscale, allArgs)
      }, MoreArgs = list(normal = normal))
  
  
  ### here
  
  
  ## there may be NAs if the points call on areas without data (e.g. ocean and using normal_bc, but points are being BC)
  ## even with NAs some variables may get a 0 (e.g. DD)
  ## sanity checks: use points in BC that we know are not NA/0
  ##    the values should be in similar ranges to climateBC's outputs (use Tonlig Wang's downscaling method in app)
  
  # Test for variables
  testthat::expect_true(all(list_variables() %in% names(results)))
  testthat::expect_true(all(list_variables() %in% names(results2)))
  testthat::expect_true(all(list_variables() %in% names(results3)))
  testthat::expect_true(all(list_variables() %in% names(results4)))
  
  testthat::expect_true(all(gcms %in% results$GCM))
  testthat::expect_true(all(gcms %in% results2$GCM))
  testthat::expect_true(all(names(historic) %in% unique(results3$PERIOD)))
  testthat::expect_true(all(eval(parse(text = names(historic_ts))) %in% unique(results4$PERIOD)))
  
  # Test for order
  testthat::expect_equal(tail(names(results), length(list_variables())), list_variables())
  testthat::expect_equal(tail(names(results2), length(list_variables())), list_variables())
  testthat::expect_equal(tail(names(results3), length(list_variables())), list_variables())
  testthat::expect_equal(tail(names(results4), length(list_variables())), list_variables())
})
