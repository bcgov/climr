test_that("test the whole chain works with all variables", {
  library(terra)
  
  ## TODO TEST WITH INPUT RASTER
  dbCon <- data_connect()
  
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
  
  # Create a normal baseline
  normals <- as.list(list_normal())
  
  sapply(normals, FUN = function(normal) {
    browser()
    normalout <- normal_input(dbCon, thebb, normal = normal,cache = TRUE)
    
  })
  
  
  
})
