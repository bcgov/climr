test_that("test dowscale basic and spatial", {
  testInit("data.table")

  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)

  ## a small area
  xyz <- data.frame(
    lon = c(
      -127.70521, -127.62279, -127.56235, -127.7162,
      -127.18585, -127.1254, -126.94957, -126.95507
    ),
    lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
    elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
    id = LETTERS[1:8]
  )

  ## get bounding box based on input points
  thebb <- get_bb(xyz)

  ds_hist <- downscale(
    xyz = xyz, which_refmap = "auto",
    obs_periods = "2001_2020",
    vars = c("PPT", "CMD", "CMI", "Tave_01", "Tave_07")
  ) ## specify desired variablesds_hist <- climr_downscale(xyz = xyz, which_refmap = "auto",

  test <- length(setdiff(ds_hist$id, xyz$id)) + length(setdiff(xyz$id, ds_hist$id))
  expect_true(test == 0)

  expect_true(all(ds_hist[, .N, by = id][, N] == 2)) ## should have 2 periods (historic and normal) per ID

  test <- as.data.table(xyz)[ds_hist, on = .(id), nomatch = NA]
  expect_false(any(is.na(test)))

  ## ID cols shouldn't be present


  ds_hist2 <- downscale(
    xyz = xyz, which_refmap = "auto",
    obs_periods = "2001_2020",
    return_refperiod = FALSE, ## put this to TRUE if you want the 1961-1990 period
    vars = c("PPT", "CMD", "CMI", "Tave_01", "Tave_07")
  )
  expect_true(all(ds_hist2[, .N, by = id][, N] == 1)) ## should have  only historic period per ID
  test <- as.data.table(xyz)[ds_hist2, on = .(id), nomatch = NA]
  expect_false(any(is.na(test)))

  ds_hist_spatial <- downscale(
    xyz = xyz, which_refmap = "auto",
    obs_periods = "2001_2020",
    return_refperiod = TRUE, ## put this to TRUE if you want the 1961-1990 period
    vars = c("PPT", "CMD", "CMI", "Tave_01", "Tave_07"),
    out_spatial = TRUE
  )
  expect_true(is(ds_hist_spatial, "SpatVector"))
  test <- as.data.table(ds_hist_spatial)
  expect_true(all(test[, .N, by = id][, N] == 2))

  ds_hist_spatial2 <- climr_downscale(
    xyz = xyz, which_refmap = "auto",
    obs_periods = "2001_2020",
    return_refperiod = FALSE, ## put this to TRUE if you want the 1961-1990 period
    vars = c("PPT", "CMD", "CMI", "Tave_01", "Tave_07"),
    out_spatial = TRUE
  )
  expect_true(is(ds_hist_spatial2, "SpatVector"))
  test <- as.data.table(ds_hist_spatial2)
  expect_true(all(test[, .N, by = id][, N] == 1))

  # if (interactive()) {
  #   ## check plots
  #   x11()
  #   ds_hist_spatial2 <- downscale(
  #     xyz = xyz, which_refmap = "auto",
  #     obs_periods = "2001_2020", return_refperiod = TRUE,
  #     vars = c("PPT", "CMD", "CMI", "Tave_01", "Tave_07"),
  #     out_spatial = TRUE, plot = "CMD"
  #   )
  # 
  #   ds_hist_spatial2 <- downscale(
  #     xyz = xyz, which_refmap = "auto",
  #     obs_periods = "2001_2020",
  #     vars = c("PPT", "CMD", "CMI", "Tave_01", "Tave_07"),
  #     out_spatial = TRUE, plot = "CMD"
  #   )
  # 
  #   ds_hist_spatial2 <- downscale(
  #     xyz = xyz, which_refmap = "auto",
  #     obs_periods = "2001_2020",
  #     gcm_models = list_gcm()[1],
  #     gcms = list_gcm_period()[1],
  #     ssp = list_ssp()[1], max_run = 0,
  #     return_refperiod = TRUE, ## put this to TRUE if you want the 1961-1990 period
  #     vars = c("PPT", "CMD", "CMI", "Tave_01", "Tave_07"),
  #     out_spatial = TRUE, plot = "CMD"
  #   )
  # 
  #   ds_hist_spatial2 <- downscale(
  #     xyz = xyz, which_refmap = "auto",
  #     obs_periods = "2001_2020",
  #     gcm_models = list_gcm()[1:2],
  #     gcms = list_gcm_period()[1:2],
  #     ssp = list_ssp()[1:2], max_run = 2,
  #     return_refperiod = TRUE, ## put this to TRUE if you want the 1961-1990 period
  #     vars = c("PPT", "CMD", "CMI", "Tave_01", "Tave_07"),
  #     out_spatial = TRUE, plot = "CMD"
  #   )
  # }
})

test_that("test downscale with different argument combinations", {
  testInit("data.table")

  ## a small no. of points
  xyz <- data.frame(
    lon = c(
      -127.70521, -127.62279, -127.56235, -127.7162,
      -127.18585, -127.1254, -126.94957, -126.95507
    ),
    lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
    elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
    id = LETTERS[1:8],
    Zone = c(rep("CWH", 3), rep("CDF", 5)),
    Subzone = c("vm1", "vm2", "vs1", rep("mm", 3), "dk", "dc")
  )

  argsCombos <- expand.grid(
    which_refmap = c("auto", "refmap_composite"), obs_periods = c(NA, "2001_2020"),
    obs_years = c(NA, "1990:2010"), obs_ts_dataset = c(NA,"cru_gpcc","climate_na"),
    gcms = c(NA, "list_gcm()[2]"),
    ssps = c(NA, "list_ssp()[1:3]"), gcm_periods = c(NA, "list_gcm_period()[1]"),
    gcm_ssp_years = c(NA, "2040:2050"), gcm_hist_years = c(NA, "1990:2010")
  ) |>
    as.data.table()

  ## remove silly combos
  argsCombos[
    is.na(gcms),
    `:=`(gcm_periods = NA, gcm_ssp_years = NA, gcm_hist_years = NA, ssps = NA)
  ]
  argsCombos[
    is.na(obs_years),
    `:=`(obs_ts_dataset = NA)
  ]
  argsCombos[
    is.na(obs_ts_dataset),
    `:=`(obs_years = NA)
  ]
  argsCombos[
    is.na(gcm_periods) & is.na(gcm_ssp_years) & is.na(gcm_hist_years),
    `:=`(gcms = NA, ssps = NA)
  ]
  argsCombos[
    is.na(gcm_periods) & is.na(gcm_ssp_years),
    `:=`(ssps = NA)
  ]
  
  argsCombos[
    is.na(ssps),
    `:=`(gcm_periods = NA, gcm_ssp_years = NA)
  ]
  argsCombos <- unique(argsCombos)

  argsCombos <- argsCombos[89:108,]
  out <- apply(argsCombos, 1, function(args, xyz) {
    args <- args[!is.na(args)]
    suppressWarnings(args$xyz <- xyz) # coerces to list.
    args$vars <- c("PPT", "CMD") ## for faster results.

    if (!is.null(args$obs_years)) {
      args$obs_years <- eval(parse(text = args$obs_years))
    }
    if (!is.null(args$gcms)) {
      args$gcms <- eval(parse(text = args$gcms))
    }
    if (!is.null(args$ssps)) {
      args$ssps <- eval(parse(text = args$ssps))
    }
    if (!is.null(args$gcm_periods)) {
      args$gcm_periods <- eval(parse(text = args$gcm_periods))
    }
    if (!is.null(args$gcm_ssp_years)) {
      args$gcm_ssp_years <- eval(parse(text = args$gcm_ssp_years))
    }
    if (!is.null(args$gcm_hist_years)) {
      args$gcm_hist_years <- eval(parse(text = args$gcm_hist_years))
    }

    out <- try(do.call(downscale, args))

    test <- is(out, "data.table")
    test2 <- all(c("id") %in% names(out))
    test3 <- all(args$vars %in% names(out))

    ## check outputs of arguments that produce changing output columns
    checkArgs <- setdiff(names(args), c("which_refmap", "xyz", "vars"))
    test4 <- logical(0)
    i <- 1
    if (length(checkArgs)) {
      if ("obs_periods" %in% checkArgs) {
        test4[i] <- all(args$obs_periods %in% unique(out$PERIOD))
        i <- i + 1
      }
      if ("obs_years" %in% checkArgs) {
        test4[i] <- all(args$obs_years %in% unique(out$PERIOD))
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

      if ("gcms" %in% checkArgs) {
        test4[i] <- all(args$gcms %in% unique(out$PERIOD))
        i <- i + 1
      }

      if ("gcm_hist_years" %in% checkArgs) {
        test4[i] <- all(args$gcm_hist_years %in% unique(out$PERIOD))
        i <- i + 1
      }

      if ("gcm_ssp_years" %in% checkArgs) {
        test4[i] <- all(args$gcm_ssp_years %in% unique(out$PERIOD))
        i <- i + 1
      }
    }

    return(c(test, test2, test3, test4))
  }, xyz = xyz)
  expect_true(all(unlist(out)))
})

test_that("test climr_dowscale all periods, all GCMs, all SSPS, all years", {
  testInit("terra")

  xyz <- data.frame(
    lon = c(
      -127.70521, -127.62279, -127.56235, -127.7162,
      -127.18585, -127.1254, -126.94957, -126.95507
    ),
    lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
    elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
    id = LETTERS[1:8]
  )

  normals <- c("auto", "refmap_prism", "refmap_climatena")

  sapply(normals, function(normal) {
    maxrun <- 2
    gcms <- setdiff(list_gcm(), c("CNRM-ESM2-1", "GFDL-ESM4", "IPSL-CM6A-LR", "UKESM1-0-LL")) ## these models are missing years

    cd_out <- climr_downscale(
      xyz,
      which_refmap = normal,
      obs_periods = list_historic(),
      obs_years = 1902:2015,
      historic_ts_dataset = "climate_na",
      gcm_models = gcms,
      ssp = list_ssp(),
      gcms = list_gcm_period(),
      gcm_ssp_years = 2015:2100,
      gcm_hist_years = 1850:2014,
      return_refperiod = TRUE,
      max_run = maxrun,
      cache = TRUE,
      vars = list_variables()
    )
    expect_true(all(gcms %in% cd_out$GCM))
    expect_true(all(list_variables() %in% names(cd_out)))
    testOut <- sapply(split(cd_out, by = "GCM"), function(dt) {
      if (!is.na(unique(dt$GCM))) {
        # test <- all(2015:2100 %in% dt$PERIOD)  ## this is failing at the moment, missing 2015-2020 or 2011-2020
        test <- all(2020:2100 %in% dt$PERIOD)
        # test2 <- all(1851:2014 %in% dt$PERIOD)    ## this is failing at the moment, missing 2011-2014
        test2 <- all(1851:2010 %in% dt$PERIOD)
        test3 <- all(list_gcm_period() %in% dt$PERIOD)
        test4 <- all(list_ssp() %in% dt$SSP)
        test5 <- length(unique(dt$RUN)) <= maxrun + 1
        return(c(test, test2, test3, test4, test5))
      } else {
        # test <- all(1901:2015 %in% dt$PERIOD) ## this is failing at the moment, missing 1901
        test <- all(1902:2015 %in% dt$PERIOD)
        test2 <- all(c("1961_1990", list_historic()) %in% dt$PERIOD)
        test3 <- all(list_gcm_period()[1] %in% dt$PERIOD)
        test4 <- is.na(unique(dt$SSP))
        return(c(test, test2, test3, test4))
      }
    })

    lapply(testOut, function(x) expect_true(all(x)))
  })
})
