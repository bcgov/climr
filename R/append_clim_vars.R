#' Add extra climate variables to a data.table
#' @param dt A data.table with TminXX, TmaxXX, PPTXX for XX in 01 to 12.
#' @param vars A character vector of climate variables to compute.
#' @param xyz A 3-column matrix or data.frame (x, y, z) or (lon, lat, elev).
append_clim_vars <- function(dt, vars, xyz) {
  
  # Return variable or create it if not found in dt
  v <- function(nm) {
    if (is.null(res <- .subset2(dt,nm))) {
      f(nm)
      res <- .subset2(dt,nm)
    }
    return(res)
  }
  
  # Call appender if exists, otherwise print message
  f <- function(nm) {
    if (is.null(expr <- .subset2(appenders, nm))) {
      message(nm, " calculation is not supported yet.")
    } else {
      expr()
    }
  }
  
  # Big appenders list, access each variable by using v("varname")
  # so it is recursively created
  appenders <- list(
    
    "PPT_wt" = function() {set(dt, j = "PPT_wt", value = v("PPT12")+v("PPT01")+v("PPT02"))},
    "PPT_sp" = function() {set(dt, j = "PPT_sp", value = v("PPT03")+v("PPT04")+v("PPT05"))},
    "PPT_sm" = function() {set(dt, j = "PPT_sm", value = v("PPT06")+v("PPT07")+v("PPT08"))},
    "PPT_at" = function() {set(dt, j = "PPT_at", value = v("PPT09")+v("PPT10")+v("PPT11"))},
    
    "Tmax_wt" = function() {set(dt, j = "Tmax_wt", value = (v("Tmax12")+v("Tmax01")+v("Tmax02"))/3)},
    "Tmax_sp" = function() {set(dt, j = "Tmax_sp", value = (v("Tmax03")+v("Tmax04")+v("Tmax05"))/3)},
    "Tmax_sm" = function() {set(dt, j = "Tmax_sm", value = (v("Tmax06")+v("Tmax07")+v("Tmax08"))/3)},
    "Tmax_at" = function() {set(dt, j = "Tmax_at", value = (v("Tmax09")+v("Tmax10")+v("Tmax11"))/3)},
    
    "Tmin_wt" = function() {set(dt, j = "Tmin_wt", value = (v("Tmin12")+v("Tmin01")+v("Tmin02"))/3)},
    "Tmin_sp" = function() {set(dt, j = "Tmin_sp", value = (v("Tmin03")+v("Tmin04")+v("Tmin05"))/3)},
    "Tmin_sm" = function() {set(dt, j = "Tmin_sm", value = (v("Tmin06")+v("Tmin07")+v("Tmin08"))/3)},
    "Tmin_at" = function() {set(dt, j = "Tmin_at", value = (v("Tmin09")+v("Tmin10")+v("Tmin11"))/3)},
    
    "PPT" = function() {set(dt, j = "PPT", value = v("PPT_wt")+v("PPT_sp")+v("PPT_sm")+v("PPT_at"))},
    "Tmax" = function() {set(dt, j = "Tmax", value = (v("Tmax_wt")+v("Tmax_sp")+v("Tmax_sm")+v("Tmax_at"))/4)},
    "Tmin" = function() {set(dt, j = "Tmin", value = (v("Tmin_wt")+v("Tmin_sp")+v("Tmin_sm")+v("Tmin_at"))/4)},
    
    "Tave01" = function() {set(dt, j = "Tave01", value = (v("Tmax01") + v("Tmin01"))/2)},
    "Tave02" = function() {set(dt, j = "Tave02", value = (v("Tmax02") + v("Tmin02"))/2)},
    "Tave03" = function() {set(dt, j = "Tave03", value = (v("Tmax03") + v("Tmin03"))/2)},
    "Tave04" = function() {set(dt, j = "Tave04", value = (v("Tmax04") + v("Tmin04"))/2)},
    "Tave05" = function() {set(dt, j = "Tave05", value = (v("Tmax05") + v("Tmin05"))/2)},
    "Tave06" = function() {set(dt, j = "Tave06", value = (v("Tmax06") + v("Tmin06"))/2)},
    "Tave07" = function() {set(dt, j = "Tave07", value = (v("Tmax07") + v("Tmin07"))/2)},
    "Tave08" = function() {set(dt, j = "Tave08", value = (v("Tmax08") + v("Tmin08"))/2)},
    "Tave09" = function() {set(dt, j = "Tave09", value = (v("Tmax09") + v("Tmin09"))/2)},
    "Tave10" = function() {set(dt, j = "Tave10", value = (v("Tmax10") + v("Tmin10"))/2)},
    "Tave11" = function() {set(dt, j = "Tave11", value = (v("Tmax11") + v("Tmin11"))/2)},
    "Tave12" = function() {set(dt, j = "Tave12", value = (v("Tmax12") + v("Tmin12"))/2)},
    
    "NFFD01" = function() {set(dt, j = "NFFD01", value = calc_NFFD( 1, v("Tmin01")))},
    "NFFD02" = function() {set(dt, j = "NFFD02", value = calc_NFFD( 2, v("Tmin02")))},
    "NFFD03" = function() {set(dt, j = "NFFD03", value = calc_NFFD( 3, v("Tmin03")))},
    "NFFD04" = function() {set(dt, j = "NFFD04", value = calc_NFFD( 4, v("Tmin04")))},
    "NFFD05" = function() {set(dt, j = "NFFD05", value = calc_NFFD( 5, v("Tmin05")))},
    "NFFD06" = function() {set(dt, j = "NFFD06", value = calc_NFFD( 6, v("Tmin06")))},
    "NFFD07" = function() {set(dt, j = "NFFD07", value = calc_NFFD( 7, v("Tmin07")))},
    "NFFD08" = function() {set(dt, j = "NFFD08", value = calc_NFFD( 8, v("Tmin08")))},
    "NFFD09" = function() {set(dt, j = "NFFD09", value = calc_NFFD( 9, v("Tmin09")))},
    "NFFD10" = function() {set(dt, j = "NFFD10", value = calc_NFFD(10, v("Tmin10")))},
    "NFFD11" = function() {set(dt, j = "NFFD11", value = calc_NFFD(11, v("Tmin11")))},
    "NFFD12" = function() {set(dt, j = "NFFD12", value = calc_NFFD(12, v("Tmin12")))},
    
    "PAS01" = function() {set(dt, j = "PAS01", value = calc_PAS( 1, v("Tmin01")))},
    "PAS02" = function() {set(dt, j = "PAS02", value = calc_PAS( 2, v("Tmin02")))},
    "PAS03" = function() {set(dt, j = "PAS03", value = calc_PAS( 3, v("Tmin03")))},
    "PAS04" = function() {set(dt, j = "PAS04", value = calc_PAS( 4, v("Tmin04")))},
    "PAS05" = function() {set(dt, j = "PAS05", value = calc_PAS( 5, v("Tmin05")))},
    "PAS06" = function() {set(dt, j = "PAS06", value = calc_PAS( 6, v("Tmin06")))},
    "PAS07" = function() {set(dt, j = "PAS07", value = calc_PAS( 7, v("Tmin07")))},
    "PAS08" = function() {set(dt, j = "PAS08", value = calc_PAS( 8, v("Tmin08")))},
    "PAS09" = function() {set(dt, j = "PAS09", value = calc_PAS( 9, v("Tmin09")))},
    "PAS10" = function() {set(dt, j = "PAS10", value = calc_PAS(10, v("Tmin10")))},
    "PAS11" = function() {set(dt, j = "PAS11", value = calc_PAS(11, v("Tmin11")))},
    "PAS12" = function() {set(dt, j = "PAS12", value = calc_PAS(12, v("Tmin12")))},
    
    "CMI01" = function() {set(dt, j = "CMI01", value = calc_RH(v("Tmin01"), v("Tmax01")))},
    "CMI02" = function() {set(dt, j = "CMI02", value = calc_RH(v("Tmin02"), v("Tmax02")))},
    "CMI03" = function() {set(dt, j = "CMI03", value = calc_RH(v("Tmin03"), v("Tmax03")))},
    "CMI04" = function() {set(dt, j = "CMI04", value = calc_RH(v("Tmin04"), v("Tmax04")))},
    "CMI05" = function() {set(dt, j = "CMI05", value = calc_RH(v("Tmin05"), v("Tmax05")))},
    "CMI06" = function() {set(dt, j = "CMI06", value = calc_RH(v("Tmin06"), v("Tmax06")))},
    "CMI07" = function() {set(dt, j = "CMI07", value = calc_RH(v("Tmin07"), v("Tmax07")))},
    "CMI08" = function() {set(dt, j = "CMI08", value = calc_RH(v("Tmin08"), v("Tmax08")))},
    "CMI09" = function() {set(dt, j = "CMI09", value = calc_RH(v("Tmin09"), v("Tmax09")))},
    "CMI10" = function() {set(dt, j = "CMI10", value = calc_RH(v("Tmin10"), v("Tmax10")))},
    "CMI11" = function() {set(dt, j = "CMI11", value = calc_RH(v("Tmin11"), v("Tmax11")))},
    "CMI12" = function() {set(dt, j = "CMI12", value = calc_RH(v("Tmin12"), v("Tmax12")))},
    
    "DD_0_01" = function() {set(dt, j = "DD_0_01", value = calc_DD_below_0( 1, v("Tave01")))},
    "DD_0_02" = function() {set(dt, j = "DD_0_02", value = calc_DD_below_0( 2, v("Tave02")))},
    "DD_0_03" = function() {set(dt, j = "DD_0_03", value = calc_DD_below_0( 3, v("Tave03")))},
    "DD_0_04" = function() {set(dt, j = "DD_0_04", value = calc_DD_below_0( 4, v("Tave04")))},
    "DD_0_05" = function() {set(dt, j = "DD_0_05", value = calc_DD_below_0( 5, v("Tave05")))},
    "DD_0_06" = function() {set(dt, j = "DD_0_06", value = calc_DD_below_0( 6, v("Tave06")))},
    "DD_0_07" = function() {set(dt, j = "DD_0_07", value = calc_DD_below_0( 7, v("Tave07")))},
    "DD_0_08" = function() {set(dt, j = "DD_0_08", value = calc_DD_below_0( 8, v("Tave08")))},
    "DD_0_09" = function() {set(dt, j = "DD_0_09", value = calc_DD_below_0( 9, v("Tave09")))},
    "DD_0_10" = function() {set(dt, j = "DD_0_10", value = calc_DD_below_0(10, v("Tave10")))},
    "DD_0_11" = function() {set(dt, j = "DD_0_11", value = calc_DD_below_0(11, v("Tave11")))},
    "DD_0_12" = function() {set(dt, j = "DD_0_12", value = calc_DD_below_0(12, v("Tave12")))},
    
    "DD_18_01" = function() {set(dt, j = "DD_18_01", value = calc_DD_below_18( 1, v("Tave01")))},
    "DD_18_02" = function() {set(dt, j = "DD_18_02", value = calc_DD_below_18( 2, v("Tave02")))},
    "DD_18_03" = function() {set(dt, j = "DD_18_03", value = calc_DD_below_18( 3, v("Tave03")))},
    "DD_18_04" = function() {set(dt, j = "DD_18_04", value = calc_DD_below_18( 4, v("Tave04")))},
    "DD_18_05" = function() {set(dt, j = "DD_18_05", value = calc_DD_below_18( 5, v("Tave05")))},
    "DD_18_06" = function() {set(dt, j = "DD_18_06", value = calc_DD_below_18( 6, v("Tave06")))},
    "DD_18_07" = function() {set(dt, j = "DD_18_07", value = calc_DD_below_18( 7, v("Tave07")))},
    "DD_18_08" = function() {set(dt, j = "DD_18_08", value = calc_DD_below_18( 8, v("Tave08")))},
    "DD_18_09" = function() {set(dt, j = "DD_18_09", value = calc_DD_below_18( 9, v("Tave09")))},
    "DD_18_10" = function() {set(dt, j = "DD_18_10", value = calc_DD_below_18(10, v("Tave10")))},
    "DD_18_11" = function() {set(dt, j = "DD_18_11", value = calc_DD_below_18(11, v("Tave11")))},
    "DD_18_12" = function() {set(dt, j = "DD_18_12", value = calc_DD_below_18(12, v("Tave12")))},
    
    "DD5_01" = function() {set(dt, j = "DD5_01", value = calc_DD_above_5( 1, v("Tave01"), "The rest"))},
    "DD5_02" = function() {set(dt, j = "DD5_02", value = calc_DD_above_5( 2, v("Tave02"), "The rest"))},
    "DD5_03" = function() {set(dt, j = "DD5_03", value = calc_DD_above_5( 3, v("Tave03"), "The rest"))},
    "DD5_04" = function() {set(dt, j = "DD5_04", value = calc_DD_above_5( 4, v("Tave04"), "The rest"))},
    "DD5_05" = function() {set(dt, j = "DD5_05", value = calc_DD_above_5( 5, v("Tave05"), "The rest"))},
    "DD5_06" = function() {set(dt, j = "DD5_06", value = calc_DD_above_5( 6, v("Tave06"), "The rest"))},
    "DD5_07" = function() {set(dt, j = "DD5_07", value = calc_DD_above_5( 7, v("Tave07"), "The rest"))},
    "DD5_08" = function() {set(dt, j = "DD5_08", value = calc_DD_above_5( 8, v("Tave08"), "The rest"))},
    "DD5_09" = function() {set(dt, j = "DD5_09", value = calc_DD_above_5( 9, v("Tave09"), "The rest"))},
    "DD5_10" = function() {set(dt, j = "DD5_10", value = calc_DD_above_5(10, v("Tave10"), "The rest"))},
    "DD5_11" = function() {set(dt, j = "DD5_11", value = calc_DD_above_5(11, v("Tave11"), "The rest"))},
    "DD5_12" = function() {set(dt, j = "DD5_12", value = calc_DD_above_5(12, v("Tave12"), "The rest"))},
    
    "DD18_01" = function() {set(dt, j = "DD18_01", value = calc_DD_above_18( 1, v("Tave01"), "The rest"))},
    "DD18_02" = function() {set(dt, j = "DD18_02", value = calc_DD_above_18( 2, v("Tave02"), "The rest"))},
    "DD18_03" = function() {set(dt, j = "DD18_03", value = calc_DD_above_18( 3, v("Tave03"), "The rest"))},
    "DD18_04" = function() {set(dt, j = "DD18_04", value = calc_DD_above_18( 4, v("Tave04"), "The rest"))},
    "DD18_05" = function() {set(dt, j = "DD18_05", value = calc_DD_above_18( 5, v("Tave05"), "The rest"))},
    "DD18_06" = function() {set(dt, j = "DD18_06", value = calc_DD_above_18( 6, v("Tave06"), "The rest"))},
    "DD18_07" = function() {set(dt, j = "DD18_07", value = calc_DD_above_18( 7, v("Tave07"), "The rest"))},
    "DD18_08" = function() {set(dt, j = "DD18_08", value = calc_DD_above_18( 8, v("Tave08"), "The rest"))},
    "DD18_09" = function() {set(dt, j = "DD18_09", value = calc_DD_above_18( 9, v("Tave09"), "The rest"))},
    "DD18_10" = function() {set(dt, j = "DD18_10", value = calc_DD_above_18(10, v("Tave10"), "The rest"))},
    "DD18_11" = function() {set(dt, j = "DD18_11", value = calc_DD_above_18(11, v("Tave11"), "The rest"))},
    "DD18_12" = function() {set(dt, j = "DD18_12", value = calc_DD_above_18(12, v("Tave12"), "The rest"))},
    
    "Tave_wt" = function() {set(dt, j = "Tave_wt", value = (v("Tave12")+v("Tave01")+v("Tave02"))/3)},
    "Tave_sp" = function() {set(dt, j = "Tave_sp", value = (v("Tave03")+v("Tave04")+v("Tave05"))/3)},
    "Tave_sm" = function() {set(dt, j = "Tave_sm", value = (v("Tave06")+v("Tave07")+v("Tave08"))/3)},
    "Tave_at" = function() {set(dt, j = "Tave_at", value = (v("Tave09")+v("Tave10")+v("Tave11"))/3)},
    
    "NFFD_wt" = function() {set(dt, j = "NFFD_wt", value = v("NFFD_12")+v("NFFD_01")+v("NFFD_02"))},
    "NFFD_sp" = function() {set(dt, j = "NFFD_sp", value = v("NFFD_03")+v("NFFD_04")+v("NFFD_05"))},
    "NFFD_sm" = function() {set(dt, j = "NFFD_sm", value = v("NFFD_06")+v("NFFD_07")+v("NFFD_08"))},
    "NFFD_at" = function() {set(dt, j = "NFFD_at", value = v("NFFD_09")+v("NFFD_10")+v("NFFD_11"))},
    
    "PAS_wt" = function() {set(dt, j = "PAS_wt", value = v("PAS_12")+v("PAS_01")+v("PAS_02"))},
    "PAS_sp" = function() {set(dt, j = "PAS_sp", value = v("PAS_03")+v("PAS_04")+v("PAS_05"))},
    "PAS_sm" = function() {set(dt, j = "PAS_sm", value = v("PAS_06")+v("PAS_07")+v("PAS_08"))},
    "PAS_at" = function() {set(dt, j = "PAS_at", value = v("PAS_09")+v("PAS_10")+v("PAS_11"))},
    
    "CMI_wt" = function() {set(dt, j = "CMI_wt", value = calc_RH(v("Tmin_wt"), v("Tmax_wt")))},
    "CMI_sp" = function() {set(dt, j = "CMI_sp", value = calc_RH(v("Tmin_sp"), v("Tmax_sp")))},
    "CMI_sm" = function() {set(dt, j = "CMI_sm", value = calc_RH(v("Tmin_sm"), v("Tmax_sm")))},
    "CMI_at" = function() {set(dt, j = "CMI_at", value = calc_RH(v("Tmin_at"), v("Tmax_at")))},
    
    "DD_0_wt" = function() {set(dt, j = "DD_0_wt", value = v("DD_0_12")+v("DD_0_01")+v("DD_0_02"))},
    "DD_0_sp" = function() {set(dt, j = "DD_0_sp", value = v("DD_0_03")+v("DD_0_04")+v("DD_0_05"))},
    "DD_0_sm" = function() {set(dt, j = "DD_0_sm", value = v("DD_0_06")+v("DD_0_07")+v("DD_0_08"))},
    "DD_0_at" = function() {set(dt, j = "DD_0_at", value = v("DD_0_09")+v("DD_0_10")+v("DD_0_11"))},
    
    "DD_18_wt" = function() {set(dt, j = "DD_18_wt", value = v("DD_18_12")+v("DD_18_01")+v("DD_18_02"))},
    "DD_18_sp" = function() {set(dt, j = "DD_18_sp", value = v("DD_18_03")+v("DD_18_04")+v("DD_18_05"))},
    "DD_18_sm" = function() {set(dt, j = "DD_18_sm", value = v("DD_18_06")+v("DD_18_07")+v("DD_18_08"))},
    "DD_18_at" = function() {set(dt, j = "DD_18_at", value = v("DD_18_09")+v("DD_18_10")+v("DD_18_11"))},
    
    "DD5_wt" = function() {set(dt, j = "DD5_wt", value = v("DD5_12")+v("DD5_01")+v("DD5_02"))},
    "DD5_sp" = function() {set(dt, j = "DD5_sp", value = v("DD5_03")+v("DD5_04")+v("DD5_05"))},
    "DD5_sm" = function() {set(dt, j = "DD5_sm", value = v("DD5_06")+v("DD5_07")+v("DD5_08"))},
    "DD5_at" = function() {set(dt, j = "DD5_at", value = v("DD5_09")+v("DD5_10")+v("DD5_11"))},
    
    "DD18_wt" = function() {set(dt, j = "DD18_wt", value = v("DD18_12")+v("DD18_01")+v("DD18_02"))},
    "DD18_sp" = function() {set(dt, j = "DD18_sp", value = v("DD18_03")+v("DD18_04")+v("DD18_05"))},
    "DD18_sm" = function() {set(dt, j = "DD18_sm", value = v("DD18_06")+v("DD18_07")+v("DD18_08"))},
    "DD18_at" = function() {set(dt, j = "DD18_at", value = v("DD18_09")+v("DD18_10")+v("DD18_11"))},
    
    "Tave" = function() {set(dt, j = "Tave", value = (v("Tave_wt")+v("Tave_sp")+v("Tave_sm")+v("Tave_at"))/4)},
    "NFFD" = function() {set(dt, j = "NFFD", value = v("NFFD_wt")+v("NFFD_sp")+v("NFFD_sm")+v("NFFD_at"))},
    "PAS" = function() {set(dt, j = "PAS", value = v("PAS_wt")+v("PAS_sp")+v("PAS_sm")+v("PAS_at"))},
    "CMI" = function() {set(dt, j = "CMI", value = calc_RH(v("Tmin"), v("Tmax")))},
    "DD_0" = function() {set(dt, j = "DD_0", value = v("DD_0_wt")+v("DD_0_sp")+v("DD_0_sm")+v("DD_0_at"))},
    "DD_18" = function() {set(dt, j = "DD_18", value = v("DD_18_wt")+v("DD_18_sp")+v("DD_18_sm")+v("DD_18_at"))},
    "DD5" = function() {set(dt, j = "DD5", value = v("DD5_wt")+v("DD5_sp")+v("DD5_sm")+v("DD5_at"))},
    "DD18" = function() {set(dt, j = "DD18", value = v("DD18_wt")+v("DD18_sp")+v("DD18_sm")+v("DD18_at"))},
    
    "MWMT" = function() {set(dt, j = "MWMT", value = (v("Tmax01")+v("Tmax02")+v("Tmax03")+v("Tmax04")+v("Tmax05")+v("Tmax06")+v("Tmax07")+v("Tmax08")+v("Tmax09")+v("Tmax10")+v("Tmax11")+v("Tmax12"))/12)},
    "MCMT" = function() {set(dt, j = "MCMT", value = (v("Tmin01")+v("Tmin02")+v("Tmin03")+v("Tmin04")+v("Tmin05")+v("Tmin06")+v("Tmin07")+v("Tmin08")+v("Tmin09")+v("Tmin10")+v("Tmin11")+v("Tmin12"))/12)},
    "MAT" = function() {set(dt, j = "MAT", value = (v("MWMT")+v("MCMT"))/2)},
    "MAP" = function() {set(dt, j = "MAP", value = v("PPT01")+v("PPT02")+v("PPT03")+v("PPT04")+v("PPT05")+v("PPT06")+v("PPT07")+v("PPT08")+v("PPT09")+v("PPT10")+v("PPT11")+v("PPT12"))},
    "MSP" = function() {set(dt, j = "MAP", value = v("PPT05")+v("PPT06")+v("PPT07")+v("PPT08")+v("PPT09"))},
    "SMH" = function() {set(dt, j = "SMH", value = v("MWMT")/(v("MSP")/1000L))},
    "AHM" = function() {set(dt, j = "MAP", value = (v("MAT")+10L)/(v("MAP")/1000L))},
    "TD" = function() {set(dt, j = "TD", value = v("MWMT")-v("MCMT"))},
    "EXT" = function() {set(dt, j = "EXT", value = calc_EXT(list(v("Tmax01"),v("Tmax02"),v("Tmax03"),v("Tmax04"),v("Tmax05"),v("Tmax06"),v("Tmax07"),v("Tmax08"),v("Tmax09"),v("Tmax10"),v("Tmax11"),v("Tmax12")), v("TD")))},
    "EMT" = function() {set(dt, j = "EMT", value = calc_EMT(list(v("Tmin01"),v("Tmin02"),v("Tmin03"),v("Tmin04"),v("Tmin05"),v("Tmin06"),v("Tmin07"),v("Tmin08"),v("Tmin09"),v("Tmin10"),v("Tmin11"),v("Tmin12")), v("TD")))},
    "bFFP" = function() {set(dt, j = "bFFP", value = calc_bFFP(v("TD"), v("NFFD"), list(v("Tmin01"),v("Tmin02"),v("Tmin03"),v("Tmin04"),v("Tmin05"),v("Tmin06"),v("Tmin07"),v("Tmin08"),v("Tmin09"),v("Tmin10"),v("Tmin11"),v("Tmin12"))))},
    "eFFP" = function() {set(dt, j = "eFFP", value = calc_eFFP(v("NFFD"), list(v("Tmin01"),v("Tmin02"),v("Tmin03"),v("Tmin04"),v("Tmin05"),v("Tmin06"),v("Tmin07"),v("Tmin08"),v("Tmin09"),v("Tmin10"),v("Tmin11"),v("Tmin12"))))},
    "FFP" = function() {set(dt, j = "FFP", value = calc_FFP(v("bFFP"), v("eFFP")))}
    
  )
  
  # Append vars except default one
  for (var in vars[!vars %in% sprintf(c("PPT%02d", "Tmax%02d", "Tmin%02d"),sort(rep(1:12,3)))]) {
    f(var)
  }
  
  # Remove unwanted variables
  set(dt, j = names(dt)[!names(dt) %in% c("ID", "GCM", "SSP", "RUN", "PERIOD", vars)], value = NULL)
  
}

#' List climate variables
#' @param only_extra A boolean. Should Tmin, Tmax and PPT be excluded? Default to FALSE.
#' @export
list_variables <- function(only_extra = FALSE) {
  if (FALSE) { variables <- NULL }
  res <- variables[["Code"]]
  if (isTRUE(only_extra)) {
    res <- res[!grepl("(^PPT|^Tmax|^Tmin)", res)]
  }
  return(sort(unique(res)))
}
