test_that("recycle_borders works", {
  test_mat <- matrix(1:9, nrow = 3, ncol = 3)
  expanded_mat <- recycle_borders(test_mat, nr = 3, nc = 3)
  
  # expanded by 2 col
  expect_equal(ncol(expanded_mat), 5)
  
  # expanded by 2 row
  expect_equal(nrow(expanded_mat), 5)
  
  # values from news col and row should be recylcled
  expect_equal(expanded_mat[1, 2:4], test_mat[1, ])
  expect_equal(expanded_mat[5, 2:4], test_mat[3, ])
  expect_equal(expanded_mat[2:4, 1], test_mat[, 1])
  expect_equal(expanded_mat[5,2:4], test_mat[3, ])
})

test_that("sum_matrix works", {
  test_mat_list <- lapply(1:3, function(x) matrix(x, nrow = 3, ncol = 3))
  # sum of 1 to n should equal (n * (n+1))/2
  expect_true(all(sum_matrix(test_mat_list) == (3 * 4)/2))
})

test_that("prod_matrix works", {
  test_mat_list_1 <- lapply(rep(2,3), function(x) matrix(x, nrow = 3, ncol = 3))
  test_mat_list_2 <- lapply(1:3, function(x) matrix(x, nrow = 3, ncol = 3))
  test_mat_list_3 <- lapply(2 *(1:3), function(x) matrix(x, nrow = 3, ncol = 3))
  expect_equal(prod_matrix(test_mat_list_1, test_mat_list_2), test_mat_list_3)
})

test_that("delta_matrix works", {
  test_mat_list_1 <- lapply(1:3, function(x) matrix(x, nrow = 3, ncol = 3))
  test_mat_list_2 <- lapply(rep(0,3), function(x) matrix(x, nrow = 3, ncol = 3))
  # delta by itself should equal 0 
  expect_equal(delta_matrix(test_mat_list_1, test_mat_list_1), test_mat_list_2)
})

test_that("sup works", {
  test_mat_list_1 <- lapply(1:3, function(x) matrix(x, nrow = 3, ncol = 3))
  test_mat_list_2 <- lapply((1:3)^2, function(x) matrix(x, nrow = 3, ncol = 3))
  expect_equal(sup(test_mat_list_1, 2), test_mat_list_2)
})
