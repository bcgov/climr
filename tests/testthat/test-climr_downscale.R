test_that("test climr_dowscale basic and spatial", {
  testInit("data.table")
  
  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)
  
  ## a small area
  xyz <- data.frame(lon = c(-127.70521, -127.62279, -127.56235, -127.7162, 
                            -127.18585, -127.1254, -126.94957, -126.95507), 
                    lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025), 
                    elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
                    id = LETTERS[1:8])
  
  ## get bounding box based on input points
  thebb <- get_bb(xyz)
  
  ds_hist <- climr_downscale(xyz = xyz, which_normal = "auto",
                             historic_period = "2001_2020",
                             return_normal = TRUE, ##put this to TRUE if you want the 1961-1990 period
                             vars = c("PPT","CMD","CMI","Tave01","Tave07")) ##specify desired variablesds_hist <- climr_downscale(xyz = xyz, which_normal = "auto",
  
  test <- length(setdiff(ds_hist$id, xyz$id)) + length(setdiff( xyz$id, ds_hist$id))
  expect_true(test == 0)
  
  expect_true(all(ds_hist[, .N, by = id][, N] == 2))   ## should have 2 periods (historic and normal) per ID
  
  test <- as.data.table(xyz)[ds_hist, on = .(id), nomatch = NA]
  expect_false(any(is.na(test)))
  
  ## ID cols shouldn't be present
  
  
  ds_hist2 <- climr_downscale(xyz = xyz, which_normal = "auto",
                              historic_period = "2001_2020",
                              return_normal = FALSE, ##put this to TRUE if you want the 1961-1990 period
                              vars = c("PPT","CMD","CMI","Tave01","Tave07")) 
  expect_true(all(ds_hist2[, .N, by = id][, N] == 1))   ## should have  only historic period per ID
  test <- as.data.table(xyz)[ds_hist2, on = .(id), nomatch = NA]
  expect_false(any(is.na(test)))
  
  ds_hist_spatial <- climr_downscale(xyz = xyz, which_normal = "auto",
                                     historic_period = "2001_2020",
                                     return_normal = TRUE, ##put this to TRUE if you want the 1961-1990 period
                                     vars = c("PPT","CMD","CMI","Tave01","Tave07"),
                                     out_spatial = TRUE) 
  expect_true(is(ds_hist_spatial, "SpatVector"))
  test <- as.data.table(ds_hist_spatial)
  expect_true(all(test[, .N, by = id][, N] == 2))
  
  ds_hist_spatial2 <- climr_downscale(xyz = xyz, which_normal = "auto",
                                      historic_period = "2001_2020",
                                      return_normal = FALSE, ##put this to TRUE if you want the 1961-1990 period
                                      vars = c("PPT","CMD","CMI","Tave01","Tave07"),
                                      out_spatial = TRUE) 
  expect_true(is(ds_hist_spatial2, "SpatVector"))
  test <- as.data.table(ds_hist_spatial2)
  expect_true(all(test[, .N, by = id][, N] == 1))
  
  if (interactive()) {
    ## check plots
    x11()
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

test_that("test climr_dowscale with more args", {
  testInit("data.table")
  
  ## a small no. of points
  xyz <- data.frame(lon = c(-127.70521, -127.62279, -127.56235, -127.7162,
                            -127.18585, -127.1254, -126.94957, -126.95507),
                    lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
                    elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
                    id = LETTERS[1:8],
                    Zone = c(rep("CWH",3), rep("CDF",5)),
                    Subzone = c("vm1","vm2","vs1",rep("mm",3),"dk","dc"))
  
  argsCombos <- expand.grid(which_normal = c("auto", "normal_na"), historic_period = c(NA, "2001_2020"), 
                            historic_ts = c(NA, "1950:2010"), gcm_models = c(NA, "list_gcm()[1:3]"),
                            ssp = c(NA, "list_ssp()[1:3]"), gcm_period = c(NA, "list_gcm_period()[1:3]"),
                            gcm_ts_years = c(NA, "2020:2050"), gcm_hist_years = c(NA, "1950:2010")) |>
    as.data.table()
  
  ## remove silly combos
  argsCombos[is.na(gcm_models),
             `:=`(gcm_period = NA, gcm_ts_years = NA, gcm_hist_years = NA, ssp = NA)]
  argsCombos[is.na(gcm_period) & is.na(gcm_ts_years) & is.na(gcm_hist_years),
             `:=`(gcm_models = NA, ssp = NA)]
  argsCombos[is.na(gcm_period) & is.na(gcm_ts_years),
             `:=`(ssp = NA)]
  argsCombos <- unique(argsCombos)
  
  out <- apply(argsCombos, 1, function(args, xyz) {
    args <- args[!is.na(args)]
    suppressWarnings(args$xyz <- xyz) # coerces to list.
    args$vars <- c("PPT","CMD")  ## for faster results.
    
    if (!is.null(args$historic_ts)) {
      args$historic_ts <- eval(parse(text = args$historic_ts))
    }
    if (!is.null(args$gcm_models)) {
      args$gcm_models <- eval(parse(text = args$gcm_models))
    }
    if (!is.null(args$ssp)) {
      args$ssp <- eval(parse(text = args$ssp))
    }
    if (!is.null(args$gcm_period)) {
      args$gcm_period <- eval(parse(text = args$gcm_period))
    }
    if (!is.null(args$gcm_ts_years)) {
      args$gcm_ts_years <- eval(parse(text = args$gcm_ts_years))
    }
    if (!is.null(args$gcm_hist_years)) {
      args$gcm_hist_years <- eval(parse(text = args$gcm_hist_years))
    }
    
    out <- try(do.call(climr_downscale, args))
  
    test <- is(out, "data.table")
    test2 <- all(c("id") %in% names(out))
    test3 <- all(args$vars %in% names(out))
    
    ## check outputs of arguments that produce changing output columns
    checkArgs <- setdiff(names(args), c("which_normal", "xyz", "vars"))
    test4 <- logical(0)
    i <- 1
    if (length(checkArgs)) {
      if ("historic_period" %in% checkArgs) {
        test4[i] <- all(args$historic_period %in% unique(out$PERIOD))
        i <- i + 1
      } 
      if ("historic_ts" %in% checkArgs) {
        test4[i] <- all(args$historic_ts %in% unique(out$PERIOD))
        i <- i + 1
      } 
      if ("gcm_models" %in% checkArgs) {
        test4[i] <- all(args$gcm_models %in% unique(out$GCM))
        i <- i + 1
      } 
      
      if ("ssp" %in% checkArgs) {
        test4[i] <- all(args$ssp %in% unique(out$SSP))
        i <- i + 1
      }
      
      if ("gcm_period" %in% checkArgs) {
        test4[i] <- all(args$gcm_period %in% unique(out$PERIOD))
        i <- i + 1
      }
      
      if ("gcm_hist_years" %in% checkArgs) {
        test4[i] <- all(args$gcm_hist_years %in% unique(out$PERIOD))
        i <- i + 1
      }
      
      if ("gcm_ts_years" %in% checkArgs) {
        test4[i] <- all(args$gcm_ts_years %in% unique(out$PERIOD))
        i <- i + 1
      }
    }
    
    return(c(test, test2, test3, test4))
  }, xyz = xyz)
  expect_true(all(unlist(out)))
})
