test_that("test climr_dowscale", {
testInit("data.table")

dbCon <- data_connect()
on.exit(try(pool::poolClose(dbCon)), add = TRUE)

## a small area
xyz <- structure(list(Long = c(-127.70521, -127.62279, -127.56235, -127.7162,
                               -127.18585, -127.1254, -126.94957, -126.95507),
                      Lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
                      Elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
                      ID = LETTERS[1:8],
                      Zone = c(rep("CWH",3), rep("CDF",5)),
                      Subzone = c("vm1","vm2","vs1",rep("mm",3),"dk","dc")),
                 row.names = c(NA, -8L), class = "data.frame")

## get bounding box based on input points
thebb <- get_bb(xyz)

ds_hist <- climr_downscale(xyz = xyz, which_normal = "auto",
                           historic_period = "2001_2020",
                           return_normal = TRUE, ##put this to TRUE if you want the 1961-1990 period
                           vars = c("PPT","CMD","CMI","Tave01","Tave07")) ##specify desired variablesds_hist <- climr_downscale(xyz = xyz, which_normal = "auto",

expect_true(all(ds_hist[, .N, by = ID][, N] == 2))   ## should have 2 periods (historic and normal) per ID

test <- as.data.table(xyz)[ds_hist, on = .(ID, Zone, Subzone), nomatch = NA]
expect_false(any(is.na(test)))

## ID cols should be present
expect_true(all(c("ID", "Zone", "Subzone") %in% names(ds_hist)))

ds_hist2 <- climr_downscale(xyz = xyz, which_normal = "auto",
                            historic_period = "2001_2020",
                            return_normal = FALSE, ##put this to TRUE if you want the 1961-1990 period
                            vars = c("PPT","CMD","CMI","Tave01","Tave07")) 
expect_true(all(ds_hist2[, .N, by = ID][, N] == 1))   ## should have  only historic period per ID
test <- as.data.table(xyz)[ds_hist2, on = .(ID, Zone, Subzone), nomatch = NA]
expect_false(any(is.na(test)))

## ID cols should be present
expect_true(all(c("ID", "Zone", "Subzone") %in% names(ds_hist2)))

ds_hist_spatial <- climr_downscale(xyz = xyz, which_normal = "auto",
                                   historic_period = "2001_2020",
                                   return_normal = TRUE, ##put this to TRUE if you want the 1961-1990 period
                                   vars = c("PPT","CMD","CMI","Tave01","Tave07"),
                                   out_spatial = TRUE) 
expect_true(is(ds_hist_spatial, "SpatVector"))
test <- as.data.table(ds_hist_spatial)
expect_true(all(c("ID", "Zone", "Subzone") %in% names(test)))
expect_true(all(test[, .N, by = ID][, N] == 2))

ds_hist_spatial2 <- climr_downscale(xyz = xyz, which_normal = "auto",
                                    historic_period = "2001_2020",
                                    return_normal = FALSE, ##put this to TRUE if you want the 1961-1990 period
                                    vars = c("PPT","CMD","CMI","Tave01","Tave07"),
                                    out_spatial = TRUE) 
expect_true(is(ds_hist_spatial2, "SpatVector"))
test <- as.data.table(ds_hist_spatial2)
expect_true(all(c("ID", "Zone", "Subzone") %in% names(test)))
expect_true(all(test[, .N, by = ID][, N] == 1))

## TODO: checks with ts

if (interactive()) {
  ## check plots
  ds_hist_spatial2 <- climr_downscale(xyz = xyz, which_normal = "auto",
                                      historic_period = "2001_2020", return_normal = TRUE,
                                      vars = c("PPT","CMD","CMI","Tave01","Tave07"),
                                      out_spatial = TRUE, plot = "CMD") 
  
  ds_hist_spatial2 <- climr_downscale(xyz = xyz, which_normal = "auto",
                                      historic_period = "2001_2020",
                                      vars = c("PPT","CMD","CMI","Tave01","Tave07"),
                                      out_spatial = TRUE, plot = "CMD") 
  
  ds_hist_spatial2 <- climr_downscale(xyz = xyz, which_normal = "auto",
                                      historic_period = "2001_2020",
                                      gcm_models = list_gcm()[1],
                                      gcm_period = list_gcm_period()[1],
                                      ssp = list_ssp()[1], max_run = 0,
                                      return_normal = TRUE, ##put this to TRUE if you want the 1961-1990 period
                                      vars = c("PPT","CMD","CMI","Tave01","Tave07"),
                                      out_spatial = TRUE, plot = "CMD") 
  
  ds_hist_spatial2 <- climr_downscale(xyz = xyz, which_normal = "auto",
                                      historic_period = "2001_2020",
                                      gcm_models = list_gcm()[1:2],
                                      gcm_period = list_gcm_period()[1:2],
                                      ssp = list_ssp()[1:2], max_run = 2,
                                      return_normal = TRUE, ##put this to TRUE if you want the 1961-1990 period
                                      vars = c("PPT","CMD","CMI","Tave01","Tave07"),
                                      out_spatial = TRUE, plot = "CMD") 
}
})
