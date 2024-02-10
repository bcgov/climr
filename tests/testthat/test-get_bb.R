test_that("test get_bb", {
  xyz <- data.frame(lon = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                          -127.18585, -127.1254, -126.94957, -126.95507), 
                    lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025), 
                    elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
                    id = LETTERS[1:8])
  
  thebb <- get_bb(xyz)
  expect_equal(get_bb(xyz), c(55.38847, 54.61025, -126.94957, -127.71620))
  
  ## different column order
  xyz2 <- xyz[, c(2,4,1,3)]
  expect_equal(get_bb(xyz2), thebb2)
  
  xyz3 <- xyz
  names(xyz3) <- c("x", "y", "z", "id")
  expect_error(get_bb(xyz3))
  
  xyz4 <- xyz
  xyz4[1, 2] <- NA
  expect_error(get_bb(xyz4))
  
  xyz4 <- xyz
  xyz4[1, ] <- NA
  expect_error(get_bb(xyz4))
  
  xyz4 <- xyz
  xyz4[1, 3] <- NA
  expect_equal(get_bb(xyz4), c(55.38847, 54.61025, -126.94957, -127.71620))
})
