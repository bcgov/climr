test_that("test the whole chain works with all variables", {
  skip("Run manually")

  # This should run without errors

  # Retrieve package data
  data_delete(ask = FALSE)
  data_update()

  # Create a normal baseline
  normal <- normal_input()

  # Select GCM
  gcm <- gcm_input(
    gcm = c("BCC-CSM2-MR", "INM-CM5-0"),
    ssp = c("ssp126", "ssp370"),
    period = "2041_2060",
    max_run = 2
  )

  # Provide or create a points dataframe (lon, lat, elev)
  set.seed(678)
  n <- 40000
  xyz <- data.frame(lon = runif(n, -125, -120), lat = runif(n, 51, 53), elev = numeric(n))
  xyz[, 3] <- terra::extract(normal[[73]], xyz[, 1:2], method = "bilinear")[, -1L]

  # Use downscale with all variables
  results <- downscale(
    xyz = xyz,
    normal = normal,
    gcm = gcm,
    var = list_variables()
  )

  # Test for creation
  testthat::expect_true(all(list_variables() %in% names(results)))
  # Test for order
  testthat::expect_equal(tail(names(results), length(list_variables())), list_variables())

  # Use downscale with some variables
  results2 <- downscale(
    xyz = xyz,
    normal = normal,
    gcm = gcm,
    var = sample(list_variables(), 3)
  )

  results3 <- downscale(
    xyz = xyz,
    normal = normal,
    gcm = gcm,
    var = list_variables(),
    nthread = 4
  )

  testthat::expect_true(all.equal(results, results3))
})
