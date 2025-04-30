test_that("test normal_input", {
  testInit("terra")

  dbCon <- data_connect()
  on.exit(try(pool::poolClose(dbCon)), add = TRUE)

  ## smaller example in BC
  xyz <- data.frame(
    lon = c(
      -127.70521, -127.62279, -127.56235, -127.7162,
      -127.18585, -127.1254, -126.94957, -126.95507
    ),
    lat = c(55.3557, 55.38847, 55.28537, 55.25721, 54.88135, 54.65636, 54.6913, 54.61025),
    elev = c(291L, 296L, 626L, 377L, 424L, 591L, 723L, 633L),
    id = seq_len(8)
  )

  thebb <- get_bb(xyz)

  normals <- as.list(list_refmaps())

  sapply(normals, FUN = function(normal) {
    normalout <- normal_input(dbCon, thebb, normal = normal, cache = TRUE)

    expect_true(is(normalout, "SpatRaster"))
    expect_true(!is.null(names(normalout)))

    expectednames <- c(
      paste0("PPT", sprintf("%02d", 1:12)),
      paste0("Tmax", sprintf("%02d", 1:12)),
      paste0("Tmin", sprintf("%02d", 1:12))
    )
    expectednames <- c(expectednames, paste0("lr_", expectednames), "dem2_WNA")
    expect_identical(setdiff(expectednames, names(normalout)), character(0))
    expect_identical(setdiff(names(normalout), expectednames), character(0))
    expect_identical(crs(normalout, proj = TRUE), "+proj=longlat +datum=WGS84 +no_defs")

    bbExt <- ext(thebb[4], thebb[3], thebb[2], thebb[1])
    outExt <- ext(normalout)
    expect_true(bbExt <= outExt)

    if (normal != "normal_bc") {
      normal_bc <- normal_input(dbCon = dbCon, bbox = thebb, normal = "normal_bc", cache = TRUE)

      expect_identical(nlyr(normalout), nlyr(normal_bc))

      if (normal == "normal_na") {
        expect_true(all(round(res(normalout), 4) > round(res(normal_bc), 4)))
      } else {
        expect_true(all(round(res(normalout), 4) == round(res(normal_bc), 4)))
      }
    }
  })

  expect_error(normal_input(
    dbCon = dbCon, bbox = thebb,
    normal = "normal_test", cache = TRUE
  ))

  ## TODO: THIS IS FAILING AND IT SHOULDN'T
  # normal_nobb <- normal_input(dbCon = dbCon, cache = TRUE)

  ## TODO: using input raster is currently failing
  # normalout2 <- normal_input(dbCon, thebb, normal = normalout[[1:36]], cache = TRUE)
})
