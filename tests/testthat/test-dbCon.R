test_that("test data_connect", {
  dbCon <- data_connect()
  expect_true(is(dbCon, "Pool"))
})
