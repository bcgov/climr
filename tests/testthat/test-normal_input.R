test_that("test normal_input", {
  dbCon <- data_connect()
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
  
  normals <- as.list(list_normal())
  
  sapply(normals, FUN = function(normal) {
    browser()
    normalout <- normal_input(dbCon, thebb, normal = normal, cache = TRUE)
    
    expect_true(is(normalout, "SpatRaster"))
    expect_true(!is.null(names(normalout)))
    
    expectednames <- c(paste0("PPT", sprintf("%02d", 1:12)),
                      paste0("Tmax", sprintf("%02d", 1:12)),
                      paste0("Tmin", sprintf("%02d", 1:12)))
    expectednames <- c(expectednames, paste0("lr_", expectednames), "dem2_WNA")
    expect_identical(setdiff(expectednames, names(normalout)), character(0))
    expect_identical(setdiff(names(normalout), expectednames), character(0))
    
    bbExt <- ext(thebb[4], thebb[3], thebb[2], thebb[1])
    outExt <- ext(normalout)
    
    expect_true(bbExt <= outExt)
    
    ## TODO: using input raster is currently failing
    # normalout2 <- normal_input(dbCon, thebb, normal = normalout[[1:36]], cache = TRUE)
  })
})
