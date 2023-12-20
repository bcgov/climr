#' Add extra climate variables to a `data.table`
#' 
#' @param dt A `data.table` with TminXX, TmaxXX, PPTXX for XX in 01 to 12.
#' @param vars A character vector of climate variables to compute.
#' 
#' @return a `data.table`
#'
#' @importFrom data.table set setcolorder
append_clim_vars <- function(dt, vars) {
  # Return variable or create it if not found in dt
  v <- function(nm) {
    if (is.null(res <- .subset2(dt, nm))) {
      f(nm)
      res <- .subset2(dt, nm)
    }
    return(res)
  }

  # Call appender if exists, otherwise print message
  f <- function(nm) {
    if (nm %in% names(dt)) {
      # return if already computed
      return()
    } else if (is.null(expr <- .subset2(appenders, nm))) {
      message(nm, " calculation is not supported yet.")
    } else {
      expr()
    }
  }

  # Big appenders list, access each variable by using v("varname")
  # so it is recursively created
  appenders <- list(
    "PPT_wt" = function() {
      set(dt, j = "PPT_wt", value = v("PPT12") + v("PPT01") + v("PPT02"))
    },
    "PPT_sp" = function() {
      set(dt, j = "PPT_sp", value = v("PPT03") + v("PPT04") + v("PPT05"))
    },
    "PPT_sm" = function() {
      set(dt, j = "PPT_sm", value = v("PPT06") + v("PPT07") + v("PPT08"))
    },
    "PPT_at" = function() {
      set(dt, j = "PPT_at", value = v("PPT09") + v("PPT10") + v("PPT11"))
    },
    "Tmax_wt" = function() {
      set(dt, j = "Tmax_wt", value = (v("Tmax12") + v("Tmax01") + v("Tmax02")) / 3)
    },
    "Tmax_sp" = function() {
      set(dt, j = "Tmax_sp", value = (v("Tmax03") + v("Tmax04") + v("Tmax05")) / 3)
    },
    "Tmax_sm" = function() {
      set(dt, j = "Tmax_sm", value = (v("Tmax06") + v("Tmax07") + v("Tmax08")) / 3)
    },
    "Tmax_at" = function() {
      set(dt, j = "Tmax_at", value = (v("Tmax09") + v("Tmax10") + v("Tmax11")) / 3)
    },
    "Tmin_wt" = function() {
      set(dt, j = "Tmin_wt", value = (v("Tmin12") + v("Tmin01") + v("Tmin02")) / 3)
    },
    "Tmin_sp" = function() {
      set(dt, j = "Tmin_sp", value = (v("Tmin03") + v("Tmin04") + v("Tmin05")) / 3)
    },
    "Tmin_sm" = function() {
      set(dt, j = "Tmin_sm", value = (v("Tmin06") + v("Tmin07") + v("Tmin08")) / 3)
    },
    "Tmin_at" = function() {
      set(dt, j = "Tmin_at", value = (v("Tmin09") + v("Tmin10") + v("Tmin11")) / 3)
    },
    "PPT" = function() {
      set(dt, j = "PPT", value = v("PPT_wt") + v("PPT_sp") + v("PPT_sm") + v("PPT_at"))
    },
    "Tmax" = function() {
      set(dt, j = "Tmax", value = (v("Tmax_wt") + v("Tmax_sp") + v("Tmax_sm") + v("Tmax_at")) / 4)
    },
    "Tmin" = function() {
      set(dt, j = "Tmin", value = (v("Tmin_wt") + v("Tmin_sp") + v("Tmin_sm") + v("Tmin_at")) / 4)
    },
    "Tave01" = function() {
      set(dt, j = "Tave01", value = (v("Tmax01") + v("Tmin01")) / 2)
    },
    "Tave02" = function() {
      set(dt, j = "Tave02", value = (v("Tmax02") + v("Tmin02")) / 2)
    },
    "Tave03" = function() {
      set(dt, j = "Tave03", value = (v("Tmax03") + v("Tmin03")) / 2)
    },
    "Tave04" = function() {
      set(dt, j = "Tave04", value = (v("Tmax04") + v("Tmin04")) / 2)
    },
    "Tave05" = function() {
      set(dt, j = "Tave05", value = (v("Tmax05") + v("Tmin05")) / 2)
    },
    "Tave06" = function() {
      set(dt, j = "Tave06", value = (v("Tmax06") + v("Tmin06")) / 2)
    },
    "Tave07" = function() {
      set(dt, j = "Tave07", value = (v("Tmax07") + v("Tmin07")) / 2)
    },
    "Tave08" = function() {
      set(dt, j = "Tave08", value = (v("Tmax08") + v("Tmin08")) / 2)
    },
    "Tave09" = function() {
      set(dt, j = "Tave09", value = (v("Tmax09") + v("Tmin09")) / 2)
    },
    "Tave10" = function() {
      set(dt, j = "Tave10", value = (v("Tmax10") + v("Tmin10")) / 2)
    },
    "Tave11" = function() {
      set(dt, j = "Tave11", value = (v("Tmax11") + v("Tmin11")) / 2)
    },
    "Tave12" = function() {
      set(dt, j = "Tave12", value = (v("Tmax12") + v("Tmin12")) / 2)
    },
    "PET01" = function() {
      set(dt, j = "PET01", value = calc_PET(v("Tave01"), v("Tmin01"), v("Tmax01"), v("Elev")))
    },
    "PET02" = function() {
      set(dt, j = "PET02", value = calc_PET(v("Tave02"), v("Tmin02"), v("Tmax02"), v("Elev")))
    },
    "PET03" = function() {
      set(dt, j = "PET03", value = calc_PET(v("Tave03"), v("Tmin03"), v("Tmax03"), v("Elev")))
    },
    "PET04" = function() {
      set(dt, j = "PET04", value = calc_PET(v("Tave04"), v("Tmin04"), v("Tmax04"), v("Elev")))
    },
    "PET05" = function() {
      set(dt, j = "PET05", value = calc_PET(v("Tave05"), v("Tmin05"), v("Tmax05"), v("Elev")))
    },
    "PET06" = function() {
      set(dt, j = "PET06", value = calc_PET(v("Tave06"), v("Tmin06"), v("Tmax06"), v("Elev")))
    },
    "PET07" = function() {
      set(dt, j = "PET07", value = calc_PET(v("Tave07"), v("Tmin07"), v("Tmax07"), v("Elev")))
    },
    "PET08" = function() {
      set(dt, j = "PET08", value = calc_PET(v("Tave08"), v("Tmin08"), v("Tmax08"), v("Elev")))
    },
    "PET09" = function() {
      set(dt, j = "PET09", value = calc_PET(v("Tave09"), v("Tmin09"), v("Tmax09"), v("Elev")))
    },
    "PET10" = function() {
      set(dt, j = "PET10", value = calc_PET(v("Tave10"), v("Tmin10"), v("Tmax10"), v("Elev")))
    },
    "PET11" = function() {
      set(dt, j = "PET11", value = calc_PET(v("Tave11"), v("Tmin11"), v("Tmax11"), v("Elev")))
    },
    "PET12" = function() {
      set(dt, j = "PET12", value = calc_PET(v("Tave12"), v("Tmin12"), v("Tmax12"), v("Elev")))
    },
    "CMI" = function() {
      set(dt, j = "CMI", value = v("PPT") - (v("PET01") + v("PET02") + v("PET03") + v("PET04") + v("PET05") + v("PET06") + v("PET07") + v("PET08") + v("PET09") + v("PET10") + v("PET11") + v("PET12")))
    },
    "NFFD01" = function() {
      set(dt, j = "NFFD01", value = calc_NFFD(1, v("Tmin01")))
    },
    "NFFD02" = function() {
      set(dt, j = "NFFD02", value = calc_NFFD(2, v("Tmin02")))
    },
    "NFFD03" = function() {
      set(dt, j = "NFFD03", value = calc_NFFD(3, v("Tmin03")))
    },
    "NFFD04" = function() {
      set(dt, j = "NFFD04", value = calc_NFFD(4, v("Tmin04")))
    },
    "NFFD05" = function() {
      set(dt, j = "NFFD05", value = calc_NFFD(5, v("Tmin05")))
    },
    "NFFD06" = function() {
      set(dt, j = "NFFD06", value = calc_NFFD(6, v("Tmin06")))
    },
    "NFFD07" = function() {
      set(dt, j = "NFFD07", value = calc_NFFD(7, v("Tmin07")))
    },
    "NFFD08" = function() {
      set(dt, j = "NFFD08", value = calc_NFFD(8, v("Tmin08")))
    },
    "NFFD09" = function() {
      set(dt, j = "NFFD09", value = calc_NFFD(9, v("Tmin09")))
    },
    "NFFD10" = function() {
      set(dt, j = "NFFD10", value = calc_NFFD(10, v("Tmin10")))
    },
    "NFFD11" = function() {
      set(dt, j = "NFFD11", value = calc_NFFD(11, v("Tmin11")))
    },
    "NFFD12" = function() {
      set(dt, j = "NFFD12", value = calc_NFFD(12, v("Tmin12")))
    },
    "PAS01" = function() {
      set(dt, j = "PAS01", value = calc_PAS(1, v("Tmin01"), v("PPT01")))
    },
    "PAS02" = function() {
      set(dt, j = "PAS02", value = calc_PAS(2, v("Tmin02"), v("PPT02")))
    },
    "PAS03" = function() {
      set(dt, j = "PAS03", value = calc_PAS(3, v("Tmin03"), v("PPT03")))
    },
    "PAS04" = function() {
      set(dt, j = "PAS04", value = calc_PAS(4, v("Tmin04"), v("PPT04")))
    },
    "PAS05" = function() {
      set(dt, j = "PAS05", value = calc_PAS(5, v("Tmin05"), v("PPT05")))
    },
    "PAS06" = function() {
      set(dt, j = "PAS06", value = calc_PAS(6, v("Tmin06"), v("PPT06")))
    },
    "PAS07" = function() {
      set(dt, j = "PAS07", value = calc_PAS(7, v("Tmin07"), v("PPT07")))
    },
    "PAS08" = function() {
      set(dt, j = "PAS08", value = calc_PAS(8, v("Tmin08"), v("PPT08")))
    },
    "PAS09" = function() {
      set(dt, j = "PAS09", value = calc_PAS(9, v("Tmin09"), v("PPT09")))
    },
    "PAS10" = function() {
      set(dt, j = "PAS10", value = calc_PAS(10, v("Tmin10"), v("PPT10")))
    },
    "PAS11" = function() {
      set(dt, j = "PAS11", value = calc_PAS(11, v("Tmin11"), v("PPT11")))
    },
    "PAS12" = function() {
      set(dt, j = "PAS12", value = calc_PAS(12, v("Tmin12"), v("PPT12")))
    },
    "RH01" = function() {
      set(dt, j = "RH01", value = calc_RH(v("Tmin01"), v("Tmax01")))
    },
    "RH02" = function() {
      set(dt, j = "RH02", value = calc_RH(v("Tmin02"), v("Tmax02")))
    },
    "RH03" = function() {
      set(dt, j = "RH03", value = calc_RH(v("Tmin03"), v("Tmax03")))
    },
    "RH04" = function() {
      set(dt, j = "RH04", value = calc_RH(v("Tmin04"), v("Tmax04")))
    },
    "RH05" = function() {
      set(dt, j = "RH05", value = calc_RH(v("Tmin05"), v("Tmax05")))
    },
    "RH06" = function() {
      set(dt, j = "RH06", value = calc_RH(v("Tmin06"), v("Tmax06")))
    },
    "RH07" = function() {
      set(dt, j = "RH07", value = calc_RH(v("Tmin07"), v("Tmax07")))
    },
    "RH08" = function() {
      set(dt, j = "RH08", value = calc_RH(v("Tmin08"), v("Tmax08")))
    },
    "RH09" = function() {
      set(dt, j = "RH09", value = calc_RH(v("Tmin09"), v("Tmax09")))
    },
    "RH10" = function() {
      set(dt, j = "RH10", value = calc_RH(v("Tmin10"), v("Tmax10")))
    },
    "RH11" = function() {
      set(dt, j = "RH11", value = calc_RH(v("Tmin11"), v("Tmax11")))
    },
    "RH12" = function() {
      set(dt, j = "RH12", value = calc_RH(v("Tmin12"), v("Tmax12")))
    },
    "Eref01" = function() {
      set(dt, j = "Eref01", value = calc_Eref(1, v("Tmin01"), v("Tmax01"), v("Lat")))
    },
    "Eref02" = function() {
      set(dt, j = "Eref02", value = calc_Eref(2, v("Tmin02"), v("Tmax02"), v("Lat")))
    },
    "Eref03" = function() {
      set(dt, j = "Eref03", value = calc_Eref(3, v("Tmin03"), v("Tmax03"), v("Lat")))
    },
    "Eref04" = function() {
      set(dt, j = "Eref04", value = calc_Eref(4, v("Tmin04"), v("Tmax04"), v("Lat")))
    },
    "Eref05" = function() {
      set(dt, j = "Eref05", value = calc_Eref(5, v("Tmin05"), v("Tmax05"), v("Lat")))
    },
    "Eref06" = function() {
      set(dt, j = "Eref06", value = calc_Eref(6, v("Tmin06"), v("Tmax06"), v("Lat")))
    },
    "Eref07" = function() {
      set(dt, j = "Eref07", value = calc_Eref(7, v("Tmin07"), v("Tmax07"), v("Lat")))
    },
    "Eref08" = function() {
      set(dt, j = "Eref08", value = calc_Eref(8, v("Tmin08"), v("Tmax08"), v("Lat")))
    },
    "Eref09" = function() {
      set(dt, j = "Eref09", value = calc_Eref(9, v("Tmin09"), v("Tmax09"), v("Lat")))
    },
    "Eref10" = function() {
      set(dt, j = "Eref10", value = calc_Eref(10, v("Tmin10"), v("Tmax10"), v("Lat")))
    },
    "Eref11" = function() {
      set(dt, j = "Eref11", value = calc_Eref(11, v("Tmin11"), v("Tmax11"), v("Lat")))
    },
    "Eref12" = function() {
      set(dt, j = "Eref12", value = calc_Eref(12, v("Tmin12"), v("Tmax12"), v("Lat")))
    },
    "CMD01" = function() {
      set(dt, j = "CMD01", value = calc_CMD(v("Eref01"), v("PPT01")))
    },
    "CMD02" = function() {
      set(dt, j = "CMD02", value = calc_CMD(v("Eref02"), v("PPT02")))
    },
    "CMD03" = function() {
      set(dt, j = "CMD03", value = calc_CMD(v("Eref03"), v("PPT03")))
    },
    "CMD04" = function() {
      set(dt, j = "CMD04", value = calc_CMD(v("Eref04"), v("PPT04")))
    },
    "CMD05" = function() {
      set(dt, j = "CMD05", value = calc_CMD(v("Eref05"), v("PPT05")))
    },
    "CMD06" = function() {
      set(dt, j = "CMD06", value = calc_CMD(v("Eref06"), v("PPT06")))
    },
    "CMD07" = function() {
      set(dt, j = "CMD07", value = calc_CMD(v("Eref07"), v("PPT07")))
    },
    "CMD08" = function() {
      set(dt, j = "CMD08", value = calc_CMD(v("Eref08"), v("PPT08")))
    },
    "CMD09" = function() {
      set(dt, j = "CMD09", value = calc_CMD(v("Eref09"), v("PPT09")))
    },
    "CMD10" = function() {
      set(dt, j = "CMD10", value = calc_CMD(v("Eref10"), v("PPT10")))
    },
    "CMD11" = function() {
      set(dt, j = "CMD11", value = calc_CMD(v("Eref11"), v("PPT11")))
    },
    "CMD12" = function() {
      set(dt, j = "CMD12", value = calc_CMD(v("Eref12"), v("PPT12")))
    },
    "DD_0_01" = function() {
      set(dt, j = "DD_0_01", value = calc_DD_below_0(1, v("Tave01")))
    },
    "DD_0_02" = function() {
      set(dt, j = "DD_0_02", value = calc_DD_below_0(2, v("Tave02")))
    },
    "DD_0_03" = function() {
      set(dt, j = "DD_0_03", value = calc_DD_below_0(3, v("Tave03")))
    },
    "DD_0_04" = function() {
      set(dt, j = "DD_0_04", value = calc_DD_below_0(4, v("Tave04")))
    },
    "DD_0_05" = function() {
      set(dt, j = "DD_0_05", value = calc_DD_below_0(5, v("Tave05")))
    },
    "DD_0_06" = function() {
      set(dt, j = "DD_0_06", value = calc_DD_below_0(6, v("Tave06")))
    },
    "DD_0_07" = function() {
      set(dt, j = "DD_0_07", value = calc_DD_below_0(7, v("Tave07")))
    },
    "DD_0_08" = function() {
      set(dt, j = "DD_0_08", value = calc_DD_below_0(8, v("Tave08")))
    },
    "DD_0_09" = function() {
      set(dt, j = "DD_0_09", value = calc_DD_below_0(9, v("Tave09")))
    },
    "DD_0_10" = function() {
      set(dt, j = "DD_0_10", value = calc_DD_below_0(10, v("Tave10")))
    },
    "DD_0_11" = function() {
      set(dt, j = "DD_0_11", value = calc_DD_below_0(11, v("Tave11")))
    },
    "DD_0_12" = function() {
      set(dt, j = "DD_0_12", value = calc_DD_below_0(12, v("Tave12")))
    },
    "DD_18_01" = function() {
      set(dt, j = "DD_18_01", value = calc_DD_below_18(1, v("Tave01")))
    },
    "DD_18_02" = function() {
      set(dt, j = "DD_18_02", value = calc_DD_below_18(2, v("Tave02")))
    },
    "DD_18_03" = function() {
      set(dt, j = "DD_18_03", value = calc_DD_below_18(3, v("Tave03")))
    },
    "DD_18_04" = function() {
      set(dt, j = "DD_18_04", value = calc_DD_below_18(4, v("Tave04")))
    },
    "DD_18_05" = function() {
      set(dt, j = "DD_18_05", value = calc_DD_below_18(5, v("Tave05")))
    },
    "DD_18_06" = function() {
      set(dt, j = "DD_18_06", value = calc_DD_below_18(6, v("Tave06")))
    },
    "DD_18_07" = function() {
      set(dt, j = "DD_18_07", value = calc_DD_below_18(7, v("Tave07")))
    },
    "DD_18_08" = function() {
      set(dt, j = "DD_18_08", value = calc_DD_below_18(8, v("Tave08")))
    },
    "DD_18_09" = function() {
      set(dt, j = "DD_18_09", value = calc_DD_below_18(9, v("Tave09")))
    },
    "DD_18_10" = function() {
      set(dt, j = "DD_18_10", value = calc_DD_below_18(10, v("Tave10")))
    },
    "DD_18_11" = function() {
      set(dt, j = "DD_18_11", value = calc_DD_below_18(11, v("Tave11")))
    },
    "DD_18_12" = function() {
      set(dt, j = "DD_18_12", value = calc_DD_below_18(12, v("Tave12")))
    },
    "DD5_01" = function() {
      set(dt, j = "DD5_01", value = calc_DD_above_5(1, v("Tave01"), "West"))
    },
    "DD5_02" = function() {
      set(dt, j = "DD5_02", value = calc_DD_above_5(2, v("Tave02"), "West"))
    },
    "DD5_03" = function() {
      set(dt, j = "DD5_03", value = calc_DD_above_5(3, v("Tave03"), "West"))
    },
    "DD5_04" = function() {
      set(dt, j = "DD5_04", value = calc_DD_above_5(4, v("Tave04"), "West"))
    },
    "DD5_05" = function() {
      set(dt, j = "DD5_05", value = calc_DD_above_5(5, v("Tave05"), "West"))
    },
    "DD5_06" = function() {
      set(dt, j = "DD5_06", value = calc_DD_above_5(6, v("Tave06"), "West"))
    },
    "DD5_07" = function() {
      set(dt, j = "DD5_07", value = calc_DD_above_5(7, v("Tave07"), "West"))
    },
    "DD5_08" = function() {
      set(dt, j = "DD5_08", value = calc_DD_above_5(8, v("Tave08"), "West"))
    },
    "DD5_09" = function() {
      set(dt, j = "DD5_09", value = calc_DD_above_5(9, v("Tave09"), "West"))
    },
    "DD5_10" = function() {
      set(dt, j = "DD5_10", value = calc_DD_above_5(10, v("Tave10"), "West"))
    },
    "DD5_11" = function() {
      set(dt, j = "DD5_11", value = calc_DD_above_5(11, v("Tave11"), "West"))
    },
    "DD5_12" = function() {
      set(dt, j = "DD5_12", value = calc_DD_above_5(12, v("Tave12"), "West"))
    },
    "DD18_01" = function() {
      set(dt, j = "DD18_01", value = calc_DD_above_18(1, v("Tave01"), "The rest"))
    },
    "DD18_02" = function() {
      set(dt, j = "DD18_02", value = calc_DD_above_18(2, v("Tave02"), "The rest"))
    },
    "DD18_03" = function() {
      set(dt, j = "DD18_03", value = calc_DD_above_18(3, v("Tave03"), "The rest"))
    },
    "DD18_04" = function() {
      set(dt, j = "DD18_04", value = calc_DD_above_18(4, v("Tave04"), "The rest"))
    },
    "DD18_05" = function() {
      set(dt, j = "DD18_05", value = calc_DD_above_18(5, v("Tave05"), "The rest"))
    },
    "DD18_06" = function() {
      set(dt, j = "DD18_06", value = calc_DD_above_18(6, v("Tave06"), "The rest"))
    },
    "DD18_07" = function() {
      set(dt, j = "DD18_07", value = calc_DD_above_18(7, v("Tave07"), "The rest"))
    },
    "DD18_08" = function() {
      set(dt, j = "DD18_08", value = calc_DD_above_18(8, v("Tave08"), "The rest"))
    },
    "DD18_09" = function() {
      set(dt, j = "DD18_09", value = calc_DD_above_18(9, v("Tave09"), "The rest"))
    },
    "DD18_10" = function() {
      set(dt, j = "DD18_10", value = calc_DD_above_18(10, v("Tave10"), "The rest"))
    },
    "DD18_11" = function() {
      set(dt, j = "DD18_11", value = calc_DD_above_18(11, v("Tave11"), "The rest"))
    },
    "DD18_12" = function() {
      set(dt, j = "DD18_12", value = calc_DD_above_18(12, v("Tave12"), "The rest"))
    },
    "Tave_wt" = function() {
      set(dt, j = "Tave_wt", value = (v("Tave12") + v("Tave01") + v("Tave02")) / 3)
    },
    "Tave_sp" = function() {
      set(dt, j = "Tave_sp", value = (v("Tave03") + v("Tave04") + v("Tave05")) / 3)
    },
    "Tave_sm" = function() {
      set(dt, j = "Tave_sm", value = (v("Tave06") + v("Tave07") + v("Tave08")) / 3)
    },
    "Tave_at" = function() {
      set(dt, j = "Tave_at", value = (v("Tave09") + v("Tave10") + v("Tave11")) / 3)
    },
    "NFFD_wt" = function() {
      set(dt, j = "NFFD_wt", value = v("NFFD12") + v("NFFD01") + v("NFFD02"))
    },
    "NFFD_sp" = function() {
      set(dt, j = "NFFD_sp", value = v("NFFD03") + v("NFFD04") + v("NFFD05"))
    },
    "NFFD_sm" = function() {
      set(dt, j = "NFFD_sm", value = v("NFFD06") + v("NFFD07") + v("NFFD08"))
    },
    "NFFD_at" = function() {
      set(dt, j = "NFFD_at", value = v("NFFD09") + v("NFFD10") + v("NFFD11"))
    },
    "PAS_wt" = function() {
      set(dt, j = "PAS_wt", value = v("PAS12") + v("PAS01") + v("PAS02"))
    },
    "PAS_sp" = function() {
      set(dt, j = "PAS_sp", value = v("PAS03") + v("PAS04") + v("PAS05"))
    },
    "PAS_sm" = function() {
      set(dt, j = "PAS_sm", value = v("PAS06") + v("PAS07") + v("PAS08"))
    },
    "PAS_at" = function() {
      set(dt, j = "PAS_at", value = v("PAS09") + v("PAS10") + v("PAS11"))
    },
    "RH_wt" = function() {
      set(dt, j = "RH_wt", value = calc_RH(v("Tmin_wt"), v("Tmax_wt")))
    },
    "RH_sp" = function() {
      set(dt, j = "RH_sp", value = calc_RH(v("Tmin_sp"), v("Tmax_sp")))
    },
    "RH_sm" = function() {
      set(dt, j = "RH_sm", value = calc_RH(v("Tmin_sm"), v("Tmax_sm")))
    },
    "RH_at" = function() {
      set(dt, j = "RH_at", value = calc_RH(v("Tmin_at"), v("Tmax_at")))
    },
    "Eref_wt" = function() {
      set(dt, j = "Eref_wt", value = v("Eref12") + v("Eref01") + v("Eref02"))
    },
    "Eref_sp" = function() {
      set(dt, j = "Eref_sp", value = v("Eref03") + v("Eref04") + v("Eref05"))
    },
    "Eref_sm" = function() {
      set(dt, j = "Eref_sm", value = v("Eref06") + v("Eref07") + v("Eref08"))
    },
    "Eref_at" = function() {
      set(dt, j = "Eref_at", value = v("Eref09") + v("Eref10") + v("Eref11"))
    },
    "CMD_wt" = function() {
      set(dt, j = "CMD_wt", value = v("CMD12") + v("CMD01") + v("CMD02"))
    },
    "CMD_sp" = function() {
      set(dt, j = "CMD_sp", value = v("CMD03") + v("CMD04") + v("CMD05"))
    },
    "CMD_sm" = function() {
      set(dt, j = "CMD_sm", value = v("CMD06") + v("CMD07") + v("CMD08"))
    },
    "CMD_at" = function() {
      set(dt, j = "CMD_at", value = v("CMD09") + v("CMD10") + v("CMD11"))
    },
    "DD_0_wt" = function() {
      set(dt, j = "DD_0_wt", value = v("DD_0_12") + v("DD_0_01") + v("DD_0_02"))
    },
    "DD_0_sp" = function() {
      set(dt, j = "DD_0_sp", value = v("DD_0_03") + v("DD_0_04") + v("DD_0_05"))
    },
    "DD_0_sm" = function() {
      set(dt, j = "DD_0_sm", value = v("DD_0_06") + v("DD_0_07") + v("DD_0_08"))
    },
    "DD_0_at" = function() {
      set(dt, j = "DD_0_at", value = v("DD_0_09") + v("DD_0_10") + v("DD_0_11"))
    },
    "DD_18_wt" = function() {
      set(dt, j = "DD_18_wt", value = v("DD_18_12") + v("DD_18_01") + v("DD_18_02"))
    },
    "DD_18_sp" = function() {
      set(dt, j = "DD_18_sp", value = v("DD_18_03") + v("DD_18_04") + v("DD_18_05"))
    },
    "DD_18_sm" = function() {
      set(dt, j = "DD_18_sm", value = v("DD_18_06") + v("DD_18_07") + v("DD_18_08"))
    },
    "DD_18_at" = function() {
      set(dt, j = "DD_18_at", value = v("DD_18_09") + v("DD_18_10") + v("DD_18_11"))
    },
    "DD5_wt" = function() {
      set(dt, j = "DD5_wt", value = v("DD5_12") + v("DD5_01") + v("DD5_02"))
    },
    "DD5_sp" = function() {
      set(dt, j = "DD5_sp", value = v("DD5_03") + v("DD5_04") + v("DD5_05"))
    },
    "DD5_sm" = function() {
      set(dt, j = "DD5_sm", value = v("DD5_06") + v("DD5_07") + v("DD5_08"))
    },
    "DD5_at" = function() {
      set(dt, j = "DD5_at", value = v("DD5_09") + v("DD5_10") + v("DD5_11"))
    },
    "DD18_wt" = function() {
      set(dt, j = "DD18_wt", value = v("DD18_12") + v("DD18_01") + v("DD18_02"))
    },
    "DD18_sp" = function() {
      set(dt, j = "DD18_sp", value = v("DD18_03") + v("DD18_04") + v("DD18_05"))
    },
    "DD18_sm" = function() {
      set(dt, j = "DD18_sm", value = v("DD18_06") + v("DD18_07") + v("DD18_08"))
    },
    "DD18_at" = function() {
      set(dt, j = "DD18_at", value = v("DD18_09") + v("DD18_10") + v("DD18_11"))
    },
    "Tave" = function() {
      set(dt, j = "Tave", value = (v("Tave_wt") + v("Tave_sp") + v("Tave_sm") + v("Tave_at")) / 4)
    },
    "NFFD" = function() {
      set(dt, j = "NFFD", value = v("NFFD_wt") + v("NFFD_sp") + v("NFFD_sm") + v("NFFD_at"))
    },
    "PAS" = function() {
      set(dt, j = "PAS", value = v("PAS_wt") + v("PAS_sp") + v("PAS_sm") + v("PAS_at"))
    },
    "RH" = function() {
      set(dt, j = "RH", value = calc_RH(v("Tmin"), v("Tmax")))
    },
    "Eref" = function() {
      set(dt, j = "Eref", value = v("Eref_wt") + v("Eref_sp") + v("Eref_sm") + v("Eref_at"))
    },
    "CMD" = function() {
      set(dt, j = "CMD", value = v("CMD_wt") + v("CMD_sp") + v("CMD_sm") + v("CMD_at"))
    },
    "DD_0" = function() {
      set(dt, j = "DD_0", value = v("DD_0_wt") + v("DD_0_sp") + v("DD_0_sm") + v("DD_0_at"))
    },
    "DD_18" = function() {
      set(dt, j = "DD_18", value = v("DD_18_wt") + v("DD_18_sp") + v("DD_18_sm") + v("DD_18_at"))
    },
    "DD5" = function() {
      set(dt, j = "DD5", value = v("DD5_wt") + v("DD5_sp") + v("DD5_sm") + v("DD5_at"))
    },
    "DD18" = function() {
      set(dt, j = "DD18", value = v("DD18_wt") + v("DD18_sp") + v("DD18_sm") + v("DD18_at"))
    },
    "MWMT" = function() {
      set(dt, j = "MWMT", value = pmax(v("Tave01"), v("Tave02"), v("Tave03"), v("Tave04"), v("Tave05"), v("Tave06"), v("Tave07"), v("Tave08"), v("Tave09"), v("Tave10"), v("Tave11"), v("Tave12")))
    },
    "MCMT" = function() {
      set(dt, j = "MCMT", value = pmin(v("Tave01"), v("Tave02"), v("Tave03"), v("Tave04"), v("Tave05"), v("Tave06"), v("Tave07"), v("Tave08"), v("Tave09"), v("Tave10"), v("Tave11"), v("Tave12")))
    },
    "MAT" = function() {
      set(dt, j = "MAT", value = v("Tave"))
    },
    "MAP" = function() {
      set(dt, j = "MAP", value = v("PPT01") + v("PPT02") + v("PPT03") + v("PPT04") + v("PPT05") + v("PPT06") + v("PPT07") + v("PPT08") + v("PPT09") + v("PPT10") + v("PPT11") + v("PPT12"))
    },
    "MSP" = function() {
      set(dt, j = "MSP", value = v("PPT05") + v("PPT06") + v("PPT07") + v("PPT08") + v("PPT09"))
    },
    "SHM" = function() {
      set(dt, j = "SHM", value = v("MWMT") / (v("MSP") / 1000L))
    },
    "AHM" = function() {
      set(dt, j = "AHM", value = (v("MAT") + 10L) / (v("MAP") / 1000L))
    },
    "TD" = function() {
      set(dt, j = "TD", value = v("MWMT") - v("MCMT"))
    },
    "EXT" = function() {
      set(dt, j = "EXT", value = calc_EXT(list(v("Tmax01"), v("Tmax02"), v("Tmax03"), v("Tmax04"), v("Tmax05"), v("Tmax06"), v("Tmax07"), v("Tmax08"), v("Tmax09"), v("Tmax10"), v("Tmax11"), v("Tmax12")), v("TD")))
    },
    "EMT" = function() {
      set(dt, j = "EMT", value = calc_EMT(list(v("Tmin01"), v("Tmin02"), v("Tmin03"), v("Tmin04"), v("Tmin05"), v("Tmin06"), v("Tmin07"), v("Tmin08"), v("Tmin09"), v("Tmin10"), v("Tmin11"), v("Tmin12")), v("TD")))
    },
    "bFFP" = function() {
      set(dt, j = "bFFP", value = calc_bFFP(v("TD"), v("NFFD"), list(v("Tmin01"), v("Tmin02"), v("Tmin03"), v("Tmin04"), v("Tmin05"), v("Tmin06"), v("Tmin07"), v("Tmin08"), v("Tmin09"), v("Tmin10"), v("Tmin11"), v("Tmin12"))))
    },
    "eFFP" = function() {
      set(dt, j = "eFFP", value = calc_eFFP(v("NFFD"), list(v("Tmin01"), v("Tmin02"), v("Tmin03"), v("Tmin04"), v("Tmin05"), v("Tmin06"), v("Tmin07"), v("Tmin08"), v("Tmin09"), v("Tmin10"), v("Tmin11"), v("Tmin12"))))
    },
    "FFP" = function() {
      set(dt, j = "FFP", value = calc_FFP(v("bFFP"), v("eFFP")))
    }
  )

  # Append vars except default one
  vars2 <- vars[!vars %in% sprintf(c("PPT%02d", "Tmax%02d", "Tmin%02d"), sort(rep(1:12, 3)))]
  vars2 <- vars2[order(match(vars2, names(appenders)))]   ## run functions in the order of appenders
  for (var in vars2) {
    f(var)
  }

  # Remove unwanted variables
  j_out <- names(dt)[!names(dt) %in% c("ID", "GCM", "SSP", "RUN", "PERIOD", vars)]
  if (length(j_out)) {
    set(dt, j = j_out, value = NULL)
  }

  # Reorder to match vars
  setcolorder(dt, c(names(dt)[names(dt) %in% c("ID", "GCM", "SSP", "RUN", "PERIOD")], vars))
}

#' List climate variables
#' 
#' @param only_extra A boolean. Should Tmin, Tmax and PPT be excluded? Defaults to FALSE.
#' 
#' @return a character vector
#' 
#' @export
list_variables <- function(only_extra = FALSE) {
  if (FALSE) {
    variables <- NULL
  }
  res <- variables[["Code"]]
  if (isTRUE(only_extra)) {
    res <- res[!grepl("(^PPT|^Tmax|^Tmin)", res)]
  }
  return(sort(unique(res)))
}
