test_that("test downscale", {
  testInit("terra")

  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)

  xyz <- data.frame(
    lon = c(
      -127.70521, -127.62279, -127.56235, -127.7162,
      -127.18585, -127.1254, -126.94957, -126.95507
    ),
    lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
    elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
    id = seq_len(8)
  )

  ## get bounding box based on input points
  thebb <- get_bb(xyz)

  # Create a normal baseline
  normal <- input_refmap(dbCon = dbCon, bbox = thebb, cache = TRUE)

  # Select GCM
  gcms <- c("BCC-CSM2-MR", "INM-CM5-0")
  gcm2_spp2 <- input_gcms(
    dbCon,
    thebb,
    gcms = gcms,
    ssps = c("ssp126", "ssp370"),
    period = "2041_2060",
    max_run = 2
  )

  # Resample 4000 points from the available data
  dem <- normal$dem2_WNA
  set.seed(678)
  n <- 4000
  xyz <- data.frame(
    lon = runif(n, xmin(dem), xmax(dem)),
    lat = runif(n, ymin(dem), ymax(dem)),
    elev = NA,
    id = 1:n
  )
  xyz[, 3] <- extract(dem, xyz[, 1:2], method = "bilinear")[, -1L]
  expect_false(any(is.na(xyz)))

  # Use downscale with all variables
  results <- downscale_core(
    xyz = xyz,
    refmap = normal,
    gcms = gcm2_spp2,
    vars = list_vars()
  )

  ## there may be NAs if the points call on areas without data (e.g. ocean and using normal_bc, but points are being BC)
  ## even with NAs some variables may get a 0 (e.g. DD)
  ## sanity checks: use points in BC that we know are not NA/0
  ##    the values should be in similar ranges to climateBC's outputs (use Tonlig Wang's downscaling method in app)

  ## fix calc_ functions to output NAs and check that NAs match absent data from normal and gcm.

  # Test for creation
  testthat::expect_true(all(list_vars() %in% names(results)))
  # Test for order
  testthat::expect_equal(tail(names(results), length(list_vars())), list_vars())

  ## we should sanity check the results too

  ## test parallelisation
  results2 <- downscale_core(
    xyz = xyz,
    refmap = normal,
    gcms = gcm2_spp2,
    vars = list_vars()[1:3]
  )

  results3 <- downscale_core(
    xyz = xyz,
    refmap = normal,
    gcms = gcm2_spp2,
    vars = list_vars()[1:3],
    nthread = 2
  )

  roundFun <- function(x) {
    if (is.numeric(x)) {
      round(x, 6)
    } else {
      x
    }
  }
  results2 <- results2[, lapply(.SD, roundFun)]
  results3 <- results3[, lapply(.SD, roundFun)]

  testthat::expect_true(all.equal(results2, results3))
})

# test_that("test downscale outputs with gcm, gcm_hist, gcm_ts, historic and historic_ts", {
#   testInit("terra")
#   testInit("data.table")
# 
#   dbCon <- data_connect()
#   on.exit(try(pool::poolClose(dbCon)), add = TRUE)
# 
#   xyz <- data.frame(
#     lon = c(-127.70),
#     lat = c(55.35),
#     elev = c(291L),
#     id = seq_len(1)
#   )
# 
#   ## get bounding box based on input points
#   thebb <- get_bb(xyz)
# 
#   #cache_clear()
# 
#   # Create a normal baseline
#   normal <- input_refmap(dbCon = dbCon, bbox = thebb, reference = "refmap_climr", cache = TRUE)
# 
#   # Select GCM
#   gcms <- c("BCC-CSM2-MR", "INM-CM5-0")
# 
#   gcm <- input_gcms(
#     dbCon,
#     thebb,
#     gcms = gcms,
#   )
# 
#   gcm_hist <- input_gcm_hist(
#     dbCon,
#     thebb,
#     gcms = gcms,
#   )
# 
#   gcm_ts <- input_gcm_ssp(
#     dbCon,
#     thebb,
#     gcm = gcms
#   )
# 
#   historic <- input_obs(
#     dbCon,
#     thebb
#   )
# 
# 
#   ## read in the reference points and outputs
#   ref_xyz <- readRDS(test_path("data", "points_downscale_ref.rds"))
#   list_refs <- list.files(test_path("data"), pattern = "downscaleout", full.names = TRUE)
# 
#   refs <- lapply(list_refs, readRDS)
#   names(refs) <- sub("downscaleout_(.*)_ref.rds", "\\1", basename(list_refs)) ## make the same as names(list_args) below
#   refs <- refs[1:4]
#   
#   list_args <- list(
#     gcms = gcm, gcm_hist_ts = gcm_hist, gcm_ssp_ts = gcm_ts,
#     obs = historic
#   )
#   names(refs) <- names(list_args)
#   dwnscaleOut <- Map(
#     argname = names(list_args),
#     argvalue = list_args,
#     f = function(argname, argvalue, normal) {
#       allArgs <- list(
#         xyz = ref_xyz,
#         refmap = normal,
#         vars = list_vars(),
#         new = argvalue,
#         return_refperiod = TRUE
#       )
#       names(allArgs) <- sub("new", argname, names(allArgs))
#       out <- do.call(downscale_core, allArgs)
#       return(out)
#     }, MoreArgs = list(normal = normal)
#   )
# 
#   testOut <- Map(
#     dwnscaleOut = dwnscaleOut[names(list_args)],
#     ref = refs[names(list_args)],
#     f = function(dwnscaleOut, ref) {
#       dwnscaleOut <- copy(dwnscaleOut)
#       ref <- copy(ref)
#       ## put columns in same order and reorder
#       ## case doesn't matter
#       setnames(dwnscaleOut, tolower(names(dwnscaleOut)))
#       setnames(ref, tolower(names(ref)))
#       dwnscaleOut <- dwnscaleOut[, .SD, .SDcols = names(ref)]
# 
#       cols <- c("id", "gcm", "ssp", "run", "period")
#       cols <- intersect(cols, names(ref))
#       setkeyv(dwnscaleOut, cols)
#       setkeyv(ref, cols)
# 
#       ## round to 4 decimals -- differences are sometimes reported at > 6 decimals
#       cols <- names(which(dwnscaleOut[, sapply(.SD, is.numeric)]))
#       dwnscaleOut[, (cols) := lapply(.SD, round, digits = 4), .SDcols = cols]
#       ref[, (cols) := lapply(.SD, round, digits = 4), .SDcols = cols]
#       return(identical(dwnscaleOut, ref))
#     }
#   )
# 
#   expect_true(all(unlist(testOut)))
# })
