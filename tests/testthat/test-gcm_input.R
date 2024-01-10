test_that("test gcm_input", {
  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)
  
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
      
      test <- all(sapply(x$ssps, function(x) anygrepl(x, names(gcm))))
      test2 <- all(sapply(x$periods, function(x) any(grepl(x, names(gcm)))))
      
      baseVars <- 36 ## number of base variables for run
      
      ## add 1 to max runs for ensemble.
      test3 <- nlyr(gcm) <= baseVars * (x$max_runs + 1) * length(x$ssps) * length(x$periods)
      
      ## number or layers should be a product of the number of base climate variables
      test4 <- nlyr(gcm) / baseVars == nlyr(gcm) %/% baseVars
      
      return(c(test, test2, test3, test4))
    })
    return(all(testOut) & all(names(gcm) %in% x$gcms))
  })
  expect_true(all(out))
})

test_that("test gcm_hist_input", {
  library(terra)
  
  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)
  
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
      test2 <- nlyr(gcm_hist) <= baseVars * (x$max_runs + 1) * length(x$years) * length(x$gcms)
      
      ## number or layers should be a product of the number of base climate variables
      test3 <- nlyr(gcm_hist) / baseVars == nlyr(gcm_hist) %/% baseVars
      return(c(test, test2, test3))
    })
    return(all(testOut) & all(names(gcm_hist) %in% x$gcms))
  })
  expect_true(all(out))
})