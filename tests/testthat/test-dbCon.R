test_that("test data_connect", {
  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)
  expect_true(is(dbCon, "Pool"))
})
