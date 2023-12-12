test_that("calc_* functions work", {
  climr:::calc_DD_below_0(2, -14)

  ## TODO: what are the correct outputs?

  # expect_???

  climr:::calc_DD_above_5(2, -14, "All")

  climr:::calc_DD_below_18(2, -14)

  climr:::calc_DD_above_18(2, -14, "All")

  t_min_list <- list("1" = -35, "2" = -32, "3" = -25, "4" = -10, 
                     "5" = -5, "6" = 3,  "7" = 15, "8" = 17, "9" = 10, "10" = -5,
                     "11" = -20, "12" = -30)
  
  climr:::calc_bFFP(td = 30, NFFD= 10, t_min_list = t_min_list)

  climr:::calc_eFFP(NFFD = 10, t_min_list = t_min_list)

  climr:::calc_FFP(bFFP = 214.5964, eFFP = 265.4581)

  climr:::calc_NFFD(3, 2.05)

  climr:::calc_PAS(4, 2)

  climr:::calc_RH(tmin_mean = 10, tmax_mean = 40)
})
