test_that("test historic_input", {
  testInit("terra")
  
  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)
  
  ## smaller example in BC
  xyz <- data.frame(Long = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                             -127.18585, -127.1254, -126.94957, -126.95507),
                    Lat = c(55.3557, 55.38847, 55.28537, 55.25721, 
                            54.88135, 54.65636, 54.6913, 54.61025),
                    Elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
                    ID = LETTERS[1:8],
                    Zone = c(rep("CWH",3), rep("CDF",5)),
                    Subzone = c("vm1","vm2","vs1",rep("mm",3),"dk","dc"))
  
  thebb <- get_bb(xyz)
  
  periods <- as.list(list_historic())
  
  sapply(periods, FUN = function(period) {
    histout <- historic_input(dbCon, thebb, period = period, cache = TRUE)
    
    expect_true(is(histout, "list"))
    expect_true(!is.null(names(histout)))
    expect_true(all(names(histout) == period))
    expect_true(!is.null(names(histout[[1]])))
    
    ## simplify
    histout <- histout[[1]]
    
    expectednames <- c(paste0("PPT", sprintf("%02d", 1:12)),
                       paste0("Tmax", sprintf("%02d", 1:12)),
                       paste0("Tmin", sprintf("%02d", 1:12)))
    expect_identical(setdiff(expectednames, names(histout)), character(0))
    expect_identical(setdiff(names(histout), expectednames), character(0))
    expect_identical(crs(histout, proj = TRUE), "+proj=longlat +datum=WGS84 +no_defs")
    
    bbExt <- ext(thebb[4], thebb[3], thebb[2], thebb[1])
    outExt <- ext(histout)
    expect_true(bbExt <= outExt)
  })
  
  expect_error(historic_input(dbCon = dbCon, bbox = thebb,
                              period = "period_test", cache = TRUE))
  ## TODO: THIS IS FAILING AND IT SHOULDN'T
  # hist_nobb <- historic_input(dbCon = dbCon, cache = TRUE)
})

test_that("test historic_input_ts", {
  testInit("terra")
  
  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)
  
  ## smaller example in BC
  xyz <- data.frame(Long = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                             -127.18585, -127.1254, -126.94957, -126.95507),
                    Lat = c(55.3557, 55.38847, 55.28537, 55.25721, 
                            54.88135, 54.65636, 54.6913, 54.61025),
                    Elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
                    ID = LETTERS[1:8],
                    Zone = c(rep("CWH",3), rep("CDF",5)),
                    Subzone = c("vm1","vm2","vs1",rep("mm",3),"dk","dc"))
  
  thebb <- get_bb(xyz)
  
  ## make 5 time series periods till 2022 and test each
  years <- sample(1902:2022, 5) 
  years <- lapply(years, function(year) year:2022)
  
  sapply(years, FUN = function(years) {
    histtsout <- historic_input_ts(dbCon, thebb, years = years, cache = TRUE)
    
    expect_true(is(histtsout, "list"))
    expect_true(!is.null(names(histtsout)))
    expect_true(all(grepl(years[1], names(histtsout)), 
                    grepl(tail(years, 1), names(histtsout))))
    expect_true(!is.null(names(histtsout[[1]])))
    
    ## simplify
    histtsout <- histtsout[[1]]
    
    ## check NA's are the same.
    out <- lapply(as.list(histtsout), function(lyr) which(is.na(lyr[])))
    expect_equal(Reduce(setdiff, out), integer(0))
    
    expectednames <- c(paste0("PPT", sprintf("%02d", 1:12)),
                       paste0("Tmax", sprintf("%02d", 1:12)),
                       paste0("Tmin", sprintf("%02d", 1:12)))
    expectednames <- unlist(lapply(years, function(x) paste0(expectednames, "_", x)))
    expect_identical(setdiff(expectednames, names(histtsout)), character(0))
    # expect_identical(setdiff(names(histtsout), expectednames), character(0))  ## TODO: this is failing see issue #157
    expect_identical(crs(histtsout, proj = TRUE), "+proj=longlat +datum=WGS84 +no_defs")
    
    bbExt <- ext(thebb[4], thebb[3], thebb[2], thebb[1])
    outExt <- ext(histtsout)
    expect_true(bbExt <= outExt)
  })
  
  expect_error(historic_input_ts(dbCon = dbCon, bbox = thebb,
                                 years = "years_test", cache = TRUE))
  ## TODO: THIS IS FAILING AND IT SHOULDN'T
  # histts_nobb <- historic_input_ts(dbCon = dbCon, cache = TRUE)
})
  