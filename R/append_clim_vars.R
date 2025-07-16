#' Append Additional Climate Variables
#' Add extra climate variables to a SpatRaster or `data.table`
#' @importFrom data.table set setcolorder
#' @param dt object on which to add variables. Eith a SpatRaster or a data.table 
#' @param vars variables to add
#' @keywords internal
#' @export 
append_clim_vars <- function(dt, vars) {
  UseMethod("append_clim_vars")
}

#' @noRd
#' @export 
append_clim_vars.data.frame <- function(dt, vars) {
  # Return variable or create it if not found in dt
  v <- function(nm) {
    if (is.null(res <- dt[[nm]])) {
      f(nm)
      res <- dt[[nm]]
    }
    return(res)
  }

  # Call .calc_def if exists, otherwise print message
  f <- function(nm) {
    if (nm %in% names(dt)) {
      # return if already computed
      return()
    } else if (is.null(expr <- .calc_def[[nm]])) {
      message(nm, " calculation is not supported yet.")
    } else {
      set(dt, j = nm, value = expr(v))
    }
  }

  # Append vars except default one
  vars2 <- vars[!vars %in% sprintf(c("PPT%02d", "Tmax%02d", "Tmin%02d"), sort(rep(1:12, 3)))]
  vars2 <- vars2[order(match(vars2, names(.calc_def)))] ## run functions in the order of .calc_def
  for (var in vars2) {
    f(var)
  }

  # Remove unwanted variables
  j_out <- names(dt)[!names(dt) %in% c("id", "DATASET", "GCM", "SSP", "RUN", "PERIOD", vars)]
  if (length(j_out)) {
    set(dt, j = j_out, value = NULL)
  }

  # Reorder to match vars
  setcolorder(dt, c(names(dt)[names(dt) %in% c("id", "DATASET", "GCM", "SSP", "RUN", "PERIOD")], unique(vars)))
}

#' @noRd
#' @export 
append_clim_vars.SpatRaster <- function(dt, vars) {
  # Return variable or create it if not found in dt
  var_nm_template <- character(0)
  
  findindt <- \(nm) {
    if (nm %in% c("elev", "lat")) {
      which(names(dt) %in% nm)
    } else if (length(var_nm_template)) {
      which(names(dt) %in% gsub("{VAR}", nm, var_nm_template, fixed = TRUE))
    } else {
      grep(nm, names(dt), fixed = TRUE)  
    }
  }
  
  v <- function(nm) {
    loc <- findindt(nm)
    if (!length(loc)) {
      f(nm)
      loc <- findindt(nm)
    } else if (!length(var_nm_template)) {
      var_nm_template <<- gsub(nm, "{VAR}", names(dt)[loc], fixed = TRUE)
    }
    return(terra::subset(dt, loc))
  }
  
  # Call .calc_def if exists, otherwise print message
  f <- function(nm) {
    if (length(findindt(nm))) {
      # return if already computed
      return()
    } else if (is.null(expr <- .calc_def[[nm]])) {
      message(nm, " calculation is not supported yet.")
    } else {
      newrast <- expr(v)
      names(newrast) <- gsub("{VAR}", nm, var_nm_template, fixed = TRUE)
      message("Appending... [%s]" |> sprintf(nm))
      add(dt) <- newrast
      #dt <<- c(dt, newrast)

    }
  }
  
  # Append vars except default one
  vars2 <- vars[!vars %in% sprintf(c("PPT%02d", "Tmax%02d", "Tmin%02d"), sort(rep(1:12, 3)))]
  vars2 <- vars2[order(match(vars2, names(.calc_def)))] ## run functions in the order of .calc_def
  for (var in vars2) {
    f(var)
  }
  
  return(dt)
}

# Big climate var definition list, access each variable by using v("varname")
# so it is recursively created
#' @noRd
.calc_def <- list(
  "PPT_wt"     = function(v) {v("PPT_12") + v("PPT_01") + v("PPT_02")},
  "PPT_sp"     = function(v) {v("PPT_03") + v("PPT_04") + v("PPT_05")},
  "PPT_sm"     = function(v) {v("PPT_06") + v("PPT_07") + v("PPT_08")},
  "PPT_at"     = function(v) {v("PPT_09") + v("PPT_10") + v("PPT_11")},
  "Tmax_wt"    = function(v) {(v("Tmax_12") + v("Tmax_01") + v("Tmax_02")) / 3},
  "Tmax_sp"    = function(v) {(v("Tmax_03") + v("Tmax_04") + v("Tmax_05")) / 3},
  "Tmax_sm"    = function(v) {(v("Tmax_06") + v("Tmax_07") + v("Tmax_08")) / 3},
  "Tmax_at"    = function(v) {(v("Tmax_09") + v("Tmax_10") + v("Tmax_11")) / 3},
  "Tmin_wt"    = function(v) {(v("Tmin_12") + v("Tmin_01") + v("Tmin_02")) / 3},
  "Tmin_sp"    = function(v) {(v("Tmin_03") + v("Tmin_04") + v("Tmin_05")) / 3},
  "Tmin_sm"    = function(v) {(v("Tmin_06") + v("Tmin_07") + v("Tmin_08")) / 3},
  "Tmin_at"    = function(v) {(v("Tmin_09") + v("Tmin_10") + v("Tmin_11")) / 3},
  "PPT_an"        = function(v) {v("PPT_wt") + v("PPT_sp") + v("PPT_sm") + v("PPT_at")},
  "Tmax_an"       = function(v) {(v("Tmax_wt") + v("Tmax_sp") + v("Tmax_sm") + v("Tmax_at")) / 4},
  "Tmin_an"       = function(v) {(v("Tmin_wt") + v("Tmin_sp") + v("Tmin_sm") + v("Tmin_at")) / 4},
  "Tave_01"    = function(v) {(v("Tmax_01") + v("Tmin_01")) / 2},
  "Tave_02"    = function(v) {(v("Tmax_02") + v("Tmin_02")) / 2},
  "Tave_03"    = function(v) {(v("Tmax_03") + v("Tmin_03")) / 2},
  "Tave_04"    = function(v) {(v("Tmax_04") + v("Tmin_04")) / 2},
  "Tave_05"    = function(v) {(v("Tmax_05") + v("Tmin_05")) / 2},
  "Tave_06"    = function(v) {(v("Tmax_06") + v("Tmin_06")) / 2},
  "Tave_07"    = function(v) {(v("Tmax_07") + v("Tmin_07")) / 2},
  "Tave_08"    = function(v) {(v("Tmax_08") + v("Tmin_08")) / 2},
  "Tave_09"    = function(v) {(v("Tmax_09") + v("Tmin_09")) / 2},
  "Tave_10"    = function(v) {(v("Tmax_10") + v("Tmin_10")) / 2},
  "Tave_11"    = function(v) {(v("Tmax_11") + v("Tmin_11")) / 2},
  "Tave_12"    = function(v) {(v("Tmax_12") + v("Tmin_12")) / 2},
  "PET_01"     = function(v) {calc_PET(v("Tave_01"), v("Tmin_01"), v("Tmax_01"), v("elev"))},
  "PET_02"     = function(v) {calc_PET(v("Tave_02"), v("Tmin_02"), v("Tmax_02"), v("elev"))},
  "PET_03"     = function(v) {calc_PET(v("Tave_03"), v("Tmin_03"), v("Tmax_03"), v("elev"))},
  "PET_04"     = function(v) {calc_PET(v("Tave_04"), v("Tmin_04"), v("Tmax_04"), v("elev"))},
  "PET_05"     = function(v) {calc_PET(v("Tave_05"), v("Tmin_05"), v("Tmax_05"), v("elev"))},
  "PET_06"     = function(v) {calc_PET(v("Tave_06"), v("Tmin_06"), v("Tmax_06"), v("elev"))},
  "PET_07"     = function(v) {calc_PET(v("Tave_07"), v("Tmin_07"), v("Tmax_07"), v("elev"))},
  "PET_08"     = function(v) {calc_PET(v("Tave_08"), v("Tmin_08"), v("Tmax_08"), v("elev"))},
  "PET_09"     = function(v) {calc_PET(v("Tave_09"), v("Tmin_09"), v("Tmax_09"), v("elev"))},
  "PET_10"     = function(v) {calc_PET(v("Tave_10"), v("Tmin_10"), v("Tmax_10"), v("elev"))},
  "PET_11"     = function(v) {calc_PET(v("Tave_11"), v("Tmin_11"), v("Tmax_11"), v("elev"))},
  "PET_12"     = function(v) {calc_PET(v("Tave_12"), v("Tmin_12"), v("Tmax_12"), v("elev"))},
  "CMI_an"    = function(v) {(v("PPT_an") - (v("PET_01") + v("PET_02") + v("PET_03") + v("PET_04") + v("PET_05") + v("PET_06") + v("PET_07") + v("PET_08") + v("PET_09") + v("PET_10") + v("PET_11") + v("PET_12"))) / 10},
  "CMI_01"     = function(v) {(v("PPT_01") - v("PET_01")) / 10},
  "CMI_02"     = function(v) {(v("PPT_02") - v("PET_02")) / 10},
  "CMI_03"     = function(v) {(v("PPT_03") - v("PET_03")) / 10},
  "CMI_04"     = function(v) {(v("PPT_04") - v("PET_04")) / 10},
  "CMI_05"     = function(v) {(v("PPT_05") - v("PET_05")) / 10},
  "CMI_06"     = function(v) {(v("PPT_06") - v("PET_06")) / 10},
  "CMI_07"     = function(v) {(v("PPT_07") - v("PET_07")) / 10},
  "CMI_08"     = function(v) {(v("PPT_08") - v("PET_08")) / 10},
  "CMI_09"     = function(v) {(v("PPT_09") - v("PET_09")) / 10},
  "CMI_10"     = function(v) {(v("PPT_10") - v("PET_10")) / 10},
  "CMI_11"     = function(v) {(v("PPT_11") - v("PET_11")) / 10},
  "CMI_12"     = function(v) {(v("PPT_12") - v("PET_12")) / 10},
  "NFFD_01"    = function(v) {calc_NFFD(1, v("Tmin_01"))},
  "NFFD_02"    = function(v) {calc_NFFD(2, v("Tmin_02"))},
  "NFFD_03"    = function(v) {calc_NFFD(3, v("Tmin_03"))},
  "NFFD_04"    = function(v) {calc_NFFD(4, v("Tmin_04"))},
  "NFFD_05"    = function(v) {calc_NFFD(5, v("Tmin_05"))},
  "NFFD_06"    = function(v) {calc_NFFD(6, v("Tmin_06"))},
  "NFFD_07"    = function(v) {calc_NFFD(7, v("Tmin_07"))},
  "NFFD_08"    = function(v) {calc_NFFD(8, v("Tmin_08"))},
  "NFFD_09"    = function(v) {calc_NFFD(9, v("Tmin_09"))},
  "NFFD_10"    = function(v) {calc_NFFD(10, v("Tmin_10"))},
  "NFFD_11"    = function(v) {calc_NFFD(11, v("Tmin_11"))},
  "NFFD_12"    = function(v) {calc_NFFD(12, v("Tmin_12"))},
  "PAS_01"     = function(v) {calc_PAS(1, v("Tave_01"), v("PPT_01"))},
  "PAS_02"     = function(v) {calc_PAS(2, v("Tave_02"), v("PPT_02"))},
  "PAS_03"     = function(v) {calc_PAS(3, v("Tave_03"), v("PPT_03"))},
  "PAS_04"     = function(v) {calc_PAS(4, v("Tave_04"), v("PPT_04"))},
  "PAS_05"     = function(v) {calc_PAS(5, v("Tave_05"), v("PPT_05"))},
  "PAS_06"     = function(v) {calc_PAS(6, v("Tave_06"), v("PPT_06"))},
  "PAS_07"     = function(v) {calc_PAS(7, v("Tave_07"), v("PPT_07"))},
  "PAS_08"     = function(v) {calc_PAS(8, v("Tave_08"), v("PPT_08"))},
  "PAS_09"     = function(v) {calc_PAS(9, v("Tave_09"), v("PPT_09"))},
  "PAS_10"     = function(v) {calc_PAS(10, v("Tave_10"), v("PPT_10"))},
  "PAS_11"     = function(v) {calc_PAS(11, v("Tave_11"), v("PPT_11"))},
  "PAS_12"     = function(v) {calc_PAS(12, v("Tave_12"), v("PPT_12"))},
  "RH_01"      = function(v) {calc_RH(v("Tmin_01"), v("Tmax_01"))},
  "RH_02"      = function(v) {calc_RH(v("Tmin_02"), v("Tmax_02"))},
  "RH_03"      = function(v) {calc_RH(v("Tmin_03"), v("Tmax_03"))},
  "RH_04"      = function(v) {calc_RH(v("Tmin_04"), v("Tmax_04"))},
  "RH_05"      = function(v) {calc_RH(v("Tmin_05"), v("Tmax_05"))},
  "RH_06"      = function(v) {calc_RH(v("Tmin_06"), v("Tmax_06"))},
  "RH_07"      = function(v) {calc_RH(v("Tmin_07"), v("Tmax_07"))},
  "RH_08"      = function(v) {calc_RH(v("Tmin_08"), v("Tmax_08"))},
  "RH_09"      = function(v) {calc_RH(v("Tmin_09"), v("Tmax_09"))},
  "RH_10"      = function(v) {calc_RH(v("Tmin_10"), v("Tmax_10"))},
  "RH_11"      = function(v) {calc_RH(v("Tmin_11"), v("Tmax_11"))},
  "RH_12"      = function(v) {calc_RH(v("Tmin_12"), v("Tmax_12"))},
  "Eref_01"    = function(v) {calc_Eref(1, v("Tmin_01"), v("Tmax_01"), v("lat"))},
  "Eref_02"    = function(v) {calc_Eref(2, v("Tmin_02"), v("Tmax_02"), v("lat"))},
  "Eref_03"    = function(v) {calc_Eref(3, v("Tmin_03"), v("Tmax_03"), v("lat"))},
  "Eref_04"    = function(v) {calc_Eref(4, v("Tmin_04"), v("Tmax_04"), v("lat"))},
  "Eref_05"    = function(v) {calc_Eref(5, v("Tmin_05"), v("Tmax_05"), v("lat"))},
  "Eref_06"    = function(v) {calc_Eref(6, v("Tmin_06"), v("Tmax_06"), v("lat"))},
  "Eref_07"    = function(v) {calc_Eref(7, v("Tmin_07"), v("Tmax_07"), v("lat"))},
  "Eref_08"    = function(v) {calc_Eref(8, v("Tmin_08"), v("Tmax_08"), v("lat"))},
  "Eref_09"    = function(v) {calc_Eref(9, v("Tmin_09"), v("Tmax_09"), v("lat"))},
  "Eref_10"    = function(v) {calc_Eref(10, v("Tmin_10"), v("Tmax_10"), v("lat"))},
  "Eref_11"    = function(v) {calc_Eref(11, v("Tmin_11"), v("Tmax_11"), v("lat"))},
  "Eref_12"    = function(v) {calc_Eref(12, v("Tmin_12"), v("Tmax_12"), v("lat"))},
  "CMD_01"     = function(v) {calc_CMD(v("Eref_01"), v("PPT_01"))},
  "CMD_02"     = function(v) {calc_CMD(v("Eref_02"), v("PPT_02"))},
  "CMD_03"     = function(v) {calc_CMD(v("Eref_03"), v("PPT_03"))},
  "CMD_04"     = function(v) {calc_CMD(v("Eref_04"), v("PPT_04"))},
  "CMD_05"     = function(v) {calc_CMD(v("Eref_05"), v("PPT_05"))},
  "CMD_06"     = function(v) {calc_CMD(v("Eref_06"), v("PPT_06"))},
  "CMD_07"     = function(v) {calc_CMD(v("Eref_07"), v("PPT_07"))},
  "CMD_08"     = function(v) {calc_CMD(v("Eref_08"), v("PPT_08"))},
  "CMD_09"     = function(v) {calc_CMD(v("Eref_09"), v("PPT_09"))},
  "CMD_10"     = function(v) {calc_CMD(v("Eref_10"), v("PPT_10"))},
  "CMD_11"     = function(v) {calc_CMD(v("Eref_11"), v("PPT_11"))},
  "CMD_12"     = function(v) {calc_CMD(v("Eref_12"), v("PPT_12"))},
  "DDsub0_01"  = function(v) {calc_DD_below_0(1, v("Tave_01"))},
  "DDsub0_02"  = function(v) {calc_DD_below_0(2, v("Tave_02"))},
  "DDsub0_03"  = function(v) {calc_DD_below_0(3, v("Tave_03"))},
  "DDsub0_04"  = function(v) {calc_DD_below_0(4, v("Tave_04"))},
  "DDsub0_05"  = function(v) {calc_DD_below_0(5, v("Tave_05"))},
  "DDsub0_06"  = function(v) {calc_DD_below_0(6, v("Tave_06"))},
  "DDsub0_07"  = function(v) {calc_DD_below_0(7, v("Tave_07"))},
  "DDsub0_08"  = function(v) {calc_DD_below_0(8, v("Tave_08"))},
  "DDsub0_09"  = function(v) {calc_DD_below_0(9, v("Tave_09"))},
  "DDsub0_10"  = function(v) {calc_DD_below_0(10, v("Tave_10"))},
  "DDsub0_11"  = function(v) {calc_DD_below_0(11, v("Tave_11"))},
  "DDsub0_12"  = function(v) {calc_DD_below_0(12, v("Tave_12"))},
  "DDsub18_01" = function(v) {calc_DD_below_18(1, v("Tave_01"))},
  "DDsub18_02" = function(v) {calc_DD_below_18(2, v("Tave_02"))},
  "DDsub18_03" = function(v) {calc_DD_below_18(3, v("Tave_03"))},
  "DDsub18_04" = function(v) {calc_DD_below_18(4, v("Tave_04"))},
  "DDsub18_05" = function(v) {calc_DD_below_18(5, v("Tave_05"))},
  "DDsub18_06" = function(v) {calc_DD_below_18(6, v("Tave_06"))},
  "DDsub18_07" = function(v) {calc_DD_below_18(7, v("Tave_07"))},
  "DDsub18_08" = function(v) {calc_DD_below_18(8, v("Tave_08"))},
  "DDsub18_09" = function(v) {calc_DD_below_18(9, v("Tave_09"))},
  "DDsub18_10" = function(v) {calc_DD_below_18(10, v("Tave_10"))},
  "DDsub18_11" = function(v) {calc_DD_below_18(11, v("Tave_11"))},
  "DDsub18_12" = function(v) {calc_DD_below_18(12, v("Tave_12"))},
  "DD5_01"     = function(v) {calc_DD_above_5(1, v("Tave_01"), "West")},
  "DD5_02"     = function(v) {calc_DD_above_5(2, v("Tave_02"), "West")},
  "DD5_03"     = function(v) {calc_DD_above_5(3, v("Tave_03"), "West")},
  "DD5_04"     = function(v) {calc_DD_above_5(4, v("Tave_04"), "West")},
  "DD5_05"     = function(v) {calc_DD_above_5(5, v("Tave_05"), "West")},
  "DD5_06"     = function(v) {calc_DD_above_5(6, v("Tave_06"), "West")},
  "DD5_07"     = function(v) {calc_DD_above_5(7, v("Tave_07"), "West")},
  "DD5_08"     = function(v) {calc_DD_above_5(8, v("Tave_08"), "West")},
  "DD5_09"     = function(v) {calc_DD_above_5(9, v("Tave_09"), "West")},
  "DD5_10"     = function(v) {calc_DD_above_5(10, v("Tave_10"), "West")},
  "DD5_11"     = function(v) {calc_DD_above_5(11, v("Tave_11"), "West")},
  "DD5_12"     = function(v) {calc_DD_above_5(12, v("Tave_12"), "West")},
  "DD18_01"    = function(v) {calc_DD_above_18(1, v("Tave_01"), "The rest")},
  "DD18_02"    = function(v) {calc_DD_above_18(2, v("Tave_02"), "The rest")},
  "DD18_03"    = function(v) {calc_DD_above_18(3, v("Tave_03"), "The rest")},
  "DD18_04"    = function(v) {calc_DD_above_18(4, v("Tave_04"), "The rest")},
  "DD18_05"    = function(v) {calc_DD_above_18(5, v("Tave_05"), "The rest")},
  "DD18_06"    = function(v) {calc_DD_above_18(6, v("Tave_06"), "The rest")},
  "DD18_07"    = function(v) {calc_DD_above_18(7, v("Tave_07"), "The rest")},
  "DD18_08"    = function(v) {calc_DD_above_18(8, v("Tave_08"), "The rest")},
  "DD18_09"    = function(v) {calc_DD_above_18(9, v("Tave_09"), "The rest")},
  "DD18_10"    = function(v) {calc_DD_above_18(10, v("Tave_10"), "The rest")},
  "DD18_11"    = function(v) {calc_DD_above_18(11, v("Tave_11"), "The rest")},
  "DD18_12"    = function(v) {calc_DD_above_18(12, v("Tave_12"), "The rest")},
  "Tave_wt"    = function(v) {(v("Tave_12") + v("Tave_01") + v("Tave_02")) / 3},
  "Tave_sp"    = function(v) {(v("Tave_03") + v("Tave_04") + v("Tave_05")) / 3},
  "Tave_sm"    = function(v) {(v("Tave_06") + v("Tave_07") + v("Tave_08")) / 3},
  "Tave_at"    = function(v) {(v("Tave_09") + v("Tave_10") + v("Tave_11")) / 3},
  "CMI_wt"     = function(v) {v("CMI_12") + v("CMI_01") + v("CMI_02")},
  "CMI_sp"     = function(v) {v("CMI_03") + v("CMI_04") + v("CMI_05")},
  "CMI_sm"     = function(v) {v("CMI_06") + v("CMI_07") + v("CMI_08")},
  "CMI_at"     = function(v) {v("CMI_09") + v("CMI_10") + v("CMI_11")},
  "NFFD_wt"    = function(v) {v("NFFD_12") + v("NFFD_01") + v("NFFD_02")},
  "NFFD_sp"    = function(v) {v("NFFD_03") + v("NFFD_04") + v("NFFD_05")},
  "NFFD_sm"    = function(v) {v("NFFD_06") + v("NFFD_07") + v("NFFD_08")},
  "NFFD_at"    = function(v) {v("NFFD_09") + v("NFFD_10") + v("NFFD_11")},
  "PAS_wt"     = function(v) {v("PAS_12") + v("PAS_01") + v("PAS_02")},
  "PAS_sp"     = function(v) {v("PAS_03") + v("PAS_04") + v("PAS_05")},
  "PAS_sm"     = function(v) {v("PAS_06") + v("PAS_07") + v("PAS_08")},
  "PAS_at"     = function(v) {v("PAS_09") + v("PAS_10") + v("PAS_11")},
  "RH_wt"      = function(v) {calc_RH(v("Tmin_wt"), v("Tmax_wt"))},
  "RH_sp"      = function(v) {calc_RH(v("Tmin_sp"), v("Tmax_sp"))},
  "RH_sm"      = function(v) {calc_RH(v("Tmin_sm"), v("Tmax_sm"))},
  "RH_at"      = function(v) {calc_RH(v("Tmin_at"), v("Tmax_at"))},
  "Eref_wt"    = function(v) {v("Eref_12") + v("Eref_01") + v("Eref_02")},
  "Eref_sp"    = function(v) {v("Eref_03") + v("Eref_04") + v("Eref_05")},
  "Eref_sm"    = function(v) {v("Eref_06") + v("Eref_07") + v("Eref_08")},
  "Eref_at"    = function(v) {v("Eref_09") + v("Eref_10") + v("Eref_11")},
  "CMD_wt"     = function(v) {v("CMD_12") + v("CMD_01") + v("CMD_02")},
  "CMD_sp"     = function(v) {v("CMD_03") + v("CMD_04") + v("CMD_05")},
  "CMD_sm"     = function(v) {v("CMD_06") + v("CMD_07") + v("CMD_08")},
  "CMD_at"     = function(v) {v("CMD_09") + v("CMD_10") + v("CMD_11")},
  "DDsub0_wt"  = function(v) {v("DDsub0_12") + v("DDsub0_01") + v("DDsub0_02")},
  "DDsub0_sp"  = function(v) {v("DDsub0_03") + v("DDsub0_04") + v("DDsub0_05")},
  "DDsub0_sm"  = function(v) {v("DDsub0_06") + v("DDsub0_07") + v("DDsub0_08")},
  "DDsub0_at"  = function(v) {v("DDsub0_09") + v("DDsub0_10") + v("DDsub0_11")},
  "DDsub18_wt" = function(v) {v("DDsub18_12") + v("DDsub18_01") + v("DDsub18_02")},
  "DDsub18_sp" = function(v) {v("DDsub18_03") + v("DDsub18_04") + v("DDsub18_05")},
  "DDsub18_sm" = function(v) {v("DDsub18_06") + v("DDsub18_07") + v("DDsub18_08")},
  "DDsub18_at" = function(v) {v("DDsub18_09") + v("DDsub18_10") + v("DDsub18_11")},
  "DD5_wt"     = function(v) {v("DD5_12") + v("DD5_01") + v("DD5_02")},
  "DD5_sp"     = function(v) {v("DD5_03") + v("DD5_04") + v("DD5_05")},
  "DD5_sm"     = function(v) {v("DD5_06") + v("DD5_07") + v("DD5_08")},
  "DD5_at"     = function(v) {v("DD5_09") + v("DD5_10") + v("DD5_11")},
  "DD18_wt"    = function(v) {v("DD18_12") + v("DD18_01") + v("DD18_02")},
  "DD18_sp"    = function(v) {v("DD18_03") + v("DD18_04") + v("DD18_05")},
  "DD18_sm"    = function(v) {v("DD18_06") + v("DD18_07") + v("DD18_08")},
  "DD18_at"    = function(v) {v("DD18_09") + v("DD18_10") + v("DD18_11")},
  "Tave_an"       = function(v) {(v("Tave_wt") + v("Tave_sp") + v("Tave_sm") + v("Tave_at")) / 4},
  "NFFD_an"       = function(v) {v("NFFD_wt") + v("NFFD_sp") + v("NFFD_sm") + v("NFFD_at")},
  "PAS_an"        = function(v) {v("PAS_wt") + v("PAS_sp") + v("PAS_sm") + v("PAS_at")},
  "RH_an"         = function(v) {calc_RH(v("Tmin_an"), v("Tmax_an"))},
  "Eref_an"       = function(v) {v("Eref_wt") + v("Eref_sp") + v("Eref_sm") + v("Eref_at")},
  "CMD_an"        = function(v) {v("CMD_wt") + v("CMD_sp") + v("CMD_sm") + v("CMD_at")},
  "DDsub0_an"     = function(v) {v("DDsub0_wt") + v("DDsub0_sp") + v("DDsub0_sm") + v("DDsub0_at")},
  "DDsub18_an"    = function(v) {v("DDsub18_wt") + v("DDsub18_sp") + v("DDsub18_sm") + v("DDsub18_at")},
  "DD5_an"        = function(v) {v("DD5_wt") + v("DD5_sp") + v("DD5_sm") + v("DD5_at")},
  "DD18_an"       = function(v) {v("DD18_wt") + v("DD18_sp") + v("DD18_sm") + v("DD18_at")},
  "MWMT"       = function(v) {calc_MWMT(list(v("Tave_01"), v("Tave_02"), v("Tave_03"), v("Tave_04"), v("Tave_05"), v("Tave_06"), v("Tave_07"), v("Tave_08"), v("Tave_09"), v("Tave_10"), v("Tave_11"), v("Tave_12")))},
  "MCMT"       = function(v) {calc_MCMT(list(v("Tave_01"), v("Tave_02"), v("Tave_03"), v("Tave_04"), v("Tave_05"), v("Tave_06"), v("Tave_07"), v("Tave_08"), v("Tave_09"), v("Tave_10"), v("Tave_11"), v("Tave_12")))},
  "MAT"        = function(v) {v("Tave_an")},
  "MAP"        = function(v) {v("PPT_01") + v("PPT_02") + v("PPT_03") + v("PPT_04") + v("PPT_05") + v("PPT_06") + v("PPT_07") + v("PPT_08") + v("PPT_09") + v("PPT_10") + v("PPT_11") + v("PPT_12")},
  "MSP"        = function(v) {v("PPT_05") + v("PPT_06") + v("PPT_07") + v("PPT_08") + v("PPT_09")},
  "SHM"        = function(v) {v("MWMT") / (v("MSP") / 1000L)},
  "AHM"        = function(v) {(v("MAT") + 10L) / (v("MAP") / 1000L)},
  "TD"         = function(v) {v("MWMT") - v("MCMT")},
  "EXT"        = function(v) {calc_EXT(list(v("Tmax_01"), v("Tmax_02"), v("Tmax_03"), v("Tmax_04"), v("Tmax_05"), v("Tmax_06"), v("Tmax_07"), v("Tmax_08"), v("Tmax_09"), v("Tmax_10"), v("Tmax_11"), v("Tmax_12")), v("MWMT"), v("TD"))},
  "EMT"        = function(v) {calc_EMT(list(v("Tmin_01"), v("Tmin_02"), v("Tmin_03"), v("Tmin_04"), v("Tmin_05"), v("Tmin_06"), v("Tmin_07"), v("Tmin_08"), v("Tmin_09"), v("Tmin_10"), v("Tmin_11"), v("Tmin_12")), v("MCMT"), v("TD"))},
  "bFFP"       = function(v) {calc_bFFP(v("TD"), v("NFFD_an"), list(v("Tmin_01"), v("Tmin_02"), v("Tmin_03"), v("Tmin_04"), v("Tmin_05"), v("Tmin_06"), v("Tmin_07"), v("Tmin_08"), v("Tmin_09"), v("Tmin_10"), v("Tmin_11"), v("Tmin_12")))},
  "eFFP"       = function(v) {calc_eFFP(v("NFFD_an"), list(v("Tmin_01"), v("Tmin_02"), v("Tmin_03"), v("Tmin_04"), v("Tmin_05"), v("Tmin_06"), v("Tmin_07"), v("Tmin_08"), v("Tmin_09"), v("Tmin_10"), v("Tmin_11"), v("Tmin_12")))},
  "FFP"        = function(v) {calc_FFP(v("bFFP"), v("eFFP"))}
)
