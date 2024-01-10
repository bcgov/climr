test_that("test gcm_input", {
  dbCon <- data_connect()
  
  library(terra)
  
  xyz <- structure(list(Long = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                                 ## smaller example in BC
                                 -127.18585, -127.1254, -126.94957, -126.95507),
                        Lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
                        Elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
                        ID = LETTERS[1:8],
                        Zone = c(rep("CWH",3), rep("CDF",5)),
                        Subzone = c("vm1","vm2","vs1",rep("mm",3),"dk","dc")),
                   row.names = c(NA, -8L), class = "data.frame")
  
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
      
      test <- all(grepl(paste(x$ssps, collapse = "|"), names(gcm)))
      test2 <- all(grepl(paste(x$periods, collapse = "|"), names(gcm)))
      
      baseVars <- 36 ## number of base variables for run
      
      if (x$max_runs == 0){
        test3 <- nlyr(gcm) == baseVars * length(x$ssps) * length(x$periods)
      }
      if (x$max_runs > 0) {
        test3 <- nlyr(gcm) >= baseVars * length(x$ssps) * length(x$periods)
        ## add 1 to max runs for ensemble.
        test3 <- test3 & (nlyr(gcm) <= baseVars * (x$max_runs + 1) * length(x$ssps) * length(x$periods))
      }
      
      ## number or layers should be a product of the number of base climate variables
      test4 <- nlyr(gcm) / baseVars == nlyr(gcm) %/% baseVars
      
      return(c(test, test2, test3, test4))
    })
    return(all(testOut) & all(names(gcm) %in% x$gcms))
  })
  expect_true(all(out))
})
