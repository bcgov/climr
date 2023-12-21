test_that("test whole workflow with all variables", {
  library(terra)
  on.exit(detach("package:terra", unload = TRUE))
  
  dbCon <- data_connect()
  expect_true(is(dbCon, "Pool"))
  
  xyz <- data.frame(Long = c(-127.70521, -127.62279, -127.56235, -127.7162), 
                     Lat = c(55.3557, 55.38847, 55.28537, 55.25721), 
                     Elev = c(291L, 296L, 626L, 377L))

  ## get bounding box based on input points
  thebb <- get_bb(xyz)
  expect_equal(thebb, c(55.38847, 55.25721, -127.56235, -127.71620))
  
  # Create a normal baseline
  normal <- normal_input(dbCon = dbCon, bbox = thebb, cache = TRUE)

  expect_true(is(normal, "SpatRaster"))
  expect_identical(nlyr(normal), 73)
  expect_identical(crs(normal, proj = TRUE), "+proj=longlat +datum=WGS84 +no_defs")
  expect_true(!is.null(names(normal)))
  
  normal_bc <- normal_input(dbCon = dbCon, bbox = thebb, normal = "normal_bc", cache = TRUE)
  expect_identical(nlyr(normal), nlyr(normal_bc))
  expect_true(all(res(normal) > res(normal_bc)))
  
  expect_error(normal_input(dbCon = dbCon, bbox = thebb,
                            normal = "normal_test", cache = TRUE))
  
  ## TODO: THIS IS FAILING AND IT SHOULDN'T
  # normal_nobb <- normal_input(dbCon = dbCon, cache = TRUE)
  
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

  expect_identical(length(gcm2_spp2), 2L)
  expect_identical(names(gcm2_spp2), gcms)   ## this will fail for now
  expect_true(all(grepl("BCC.CSM2.MR", names(gcm2_spp2[[1]]))))
  expect_true(all(grepl("INM.CM5.0", names(gcm2_spp2[[2]]))))

  out <- sapply(gcm2_spp2, FUN = function(x) {
    ## note that for each GCMxSSP combo there may not be the same number of runs.
    any(grepl("ssp126", names(x))) & any(grepl("ssp370", names(x))) 
  })
  expect_true(all(out))
  
  out <- sapply(gcm2_spp2, FUN = function(x) {
    all(grepl("2041_2060", names(x)))
  })
  expect_true(all(out))  
  
  # Resample 4000 points from the available data
  set.seed(678)
  n <- 4000
  dem <- normal[[73]]
  expect_true(grepl("dem", names(dem)))
  
  xyz <- data.frame(lon = runif(n, xmin(dem), xmax(dem)), 
                    lat = runif(n, ymin(dem), ymax(dem)), elev = NA)
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
  
  # Use downscale with some variables
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
    nthread = 4
  )

  testthat::expect_true(all.equal(results2, results3))
})
