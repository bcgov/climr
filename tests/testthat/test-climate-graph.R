
# Wet Periods ----
test_that("plot_climate_graph works when 1 month > 100 mm precip", {
  temp <- c(14, 14, 17, 20, 24, 28, 31, 31, 27, 23, 18, 15)
  precip <- c(93.1, 62.1, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 73.8, 68, 155.3, 76.3)
  vdiffr::expect_doppelganger("Climate Diagram 1 Month Wet", plot_climate_diagram(temp = temp, precip = precip,  elev = 25))
})


test_that("plot_climate_graph works when 0 month > 100 mm precip", {
  temp <- c(14, 14, 17, 20, 24, 28, 31, 31, 27, 23, 18, 15)
  precip <- c(93.1, 62.1, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 73.8, 68, 99.3, 76.3)
  vdiffr::expect_doppelganger("Climate Diagram 0 Month Wet", plot_climate_diagram(temp = temp, precip = precip,  elev = 25))
})

test_that("plot_climate_graph works when 2 non-contiguous months > 100 mm precip", {
  temp <- c(14, 14, 17, 20, 24, 28, 31, 31, 27, 23, 18, 15)
  precip <- c(93.1, 150.3, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 73.8, 68, 155.3, 76.3)
  vdiffr::expect_doppelganger("Climate Diagram 2 Non-Contiguous Months Wet", plot_climate_diagram(temp = temp, precip = precip,  elev = 25))
})


test_that("plot_climate_graph works when 2 contiguous months > 100 mm precip", {
  temp <- c(14, 14, 17, 20, 24, 28, 31, 31, 27, 23, 18, 15)
  precip <- c(93.1, 150.3, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 73.8, 130.3, 155.3, 76.3)
  vdiffr::expect_doppelganger("Climate Diagram 2 Contiguous Months Wet", plot_climate_diagram(temp = temp, precip = precip,  elev = 25))
})

test_that("plot_climate_graph works when 3 months > 100 mm precip", {
  temp <- c(14, 14, 17, 20, 24, 28, 31, 31, 27, 23, 18, 15)
  precip <- c(93.1, 150.3, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 120, 130.3, 155.3, 76.3)
  vdiffr::expect_doppelganger("Climate Diagram 3 Months Wet", plot_climate_diagram(temp = temp, precip = precip,  elev = 25))
})

test_that("plot_climate_graph works when January & December > 100 mm precip", {
  temp <- c(14, 14, 17, 20, 24, 28, 31, 31, 27, 23, 18, 15)
  precip <- c(130, 62.1, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 73.8, 68, 90, 155.3)
  vdiffr::expect_doppelganger("Climate Diagram Jan and Dec Wet", plot_climate_diagram(temp = temp, precip = precip,  elev = 25))
})



# Freeze Periods ----

test_that("plot_climate_graph works when January & December are freeze periods", {
  temp <- c(-3, 14, 17, 20, 24, 28, 31, 31, 27, 23, 18, -5)
  precip <- c(93.1, 62.1, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 73.8, 68, 155.3, 76.3)
  vdiffr::expect_doppelganger("Climate Diagram Jan and Dec Freeze", plot_climate_diagram(temp = temp, precip = precip,  elev = 25))
})


test_that("plot_climate_graph works when January & February are freeze periods", {
  temp <- c(-3, -1, 17, 20, 24, 28, 31, 31, 27, 23, 18, 10)
  precip <- c(93.1, 62.1, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 73.8, 68, 155.3, 76.3)
  vdiffr::expect_doppelganger("Climate Diagram Jan and Feb Freeze", plot_climate_diagram(temp = temp, precip = precip,  elev = 25))
})




# Dry Periods ----
test_that("plot_climate_graph works when January & December are dry periods", {
  temp <- c(28, 14, 17, 20, 24, 28, 31, 31, 27, 23, 18, 28)
  precip <- c(40, 62.1, 58.8, 46.6, 45.9, 23.1, 11.2, 18.9, 73.8, 68, 20, 40)
  vdiffr::expect_doppelganger("Climate Diagram Jan and Dec Dry", plot_climate_diagram(temp = temp, precip = precip,  elev = 25))
})




# Test extremes ----
test_that("plot_climate_graph works when all months are dry", {
  temp <- c(22, 22, 23, 25, 27, 28, 31, 31, 27, 23, 22, 19)
  precip <- c(20, 21, 32, 30, 35, 20, 28, 31, 36, 34, 35, 30)
  vdiffr::expect_doppelganger("Climate Diagram All Months Dry", plot_climate_diagram(temp = temp, precip = precip,  elev = 25))
})

test_that("plot_climate_graph works when all months are humid", {
  temp <- c(22, 22, 23, 25, 27, 28, 31, 31, 27, 23, 22, 19)
  precip <- c(50, 56, 60, 70, 80, 85, 70, 75, 79, 83, 82, 91)
  vdiffr::expect_doppelganger("Climate Diagram All Months Humid", plot_climate_diagram(temp = temp, precip = precip,  elev = 25))
})

test_that("plot_climate_graph works when all months are wet", {
  temp <- c(22, 22, 23, 25, 27, 28, 31, 31, 27, 23, 22, 19)
  precip <- c(150, 156, 160, 170, 180, 185, 170, 175, 179, 183, 182, 191)
  vdiffr::expect_doppelganger("Climate Diagram All Months Wet", plot_climate_diagram(temp = temp, precip = precip,  elev = 25))
})






