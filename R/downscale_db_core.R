#' @export
#' @rdname downscale_core
downscale_db_core <- function(
  xyz,
  refmap,
  gcms = NULL,
  obs = NULL,
  gcm_ssp_ts = NULL,
  gcm_hist_ts = NULL,
  obs_ts = NULL,
  return_refperiod = TRUE,
  vars = sort(sprintf(c("PPT_%02d", "Tmax_%02d", "Tmin_%02d"), sort(rep(1:12, 3)))),
  ppt_lr = FALSE,
  nthread = 1L,
  out_spatial = FALSE,
  plot = NULL
) {

  ## checks
  .checkDwnsclCoreArgs(
    xyz, refmap, gcms, obs, gcm_ssp_ts, gcm_hist_ts,
    obs_ts, return_refperiod, out_spatial, plot, vars
  )
  
  expectedCols <- c("lon", "lat", "elev", "id")
  xyz <- .checkXYZ(copy(xyz), expectedCols)
  
  res <- downscale_db_(
    xyz,
    refmap,
    gcms,
    gcm_ssp_ts,
    gcm_hist_ts,
    obs,
    obs_ts,
    return_refperiod,
    vars,
    ppt_lr
  )
  
  IDcols <- names(res)[!names(res) %in% vars]
  setkeyv(res, IDcols)
  if (out_spatial) {
    res <- as.data.table(xyz)[res, on = "id"]
    
    res <- vect(res, geom = c("lon", "lat"), crs = crs(refmap, proj = TRUE))
    
    if (!is.null(plot)) {
      if (!plot %in% vars) {
        stop("The variable you wish to plot was not downscaled. Please pass a variable listed in 'vars'")
      }
      ## make a mask of the normals data "extent"
      msk <- refmap[[1]]
      msk[!is.na(msk[])] <- 1
      msk <- as.polygons(msk)
      
      ## round values
      res2 <- res
      res2[[plot]] <- round(res2[[plot]], 4)
      
      ## make table of available runs
      cols <- c("GCM", "SSP", "RUN", "PERIOD")
      cols <- cols[cols %in% names(res2)]
      uniqueCombos <- unique(as.data.table(res2)[, ..cols])
      uniqueCombos <- uniqueCombos[complete.cases(uniqueCombos)]
      
      if (nrow(uniqueCombos) > 1) {
        message("Plotting results for a single period/GCM/run/SSP")
        uniqueCombos <- uniqueCombos[1]
        
        res2 <- as.data.table(res2, geom = "XY")
        res2 <- res2[uniqueCombos, on = names(uniqueCombos)]
        res2 <- vect(res2, geom = c("x", "y"), crs = crs(res, proj = TRUE))
      }
      
      plotTitle <- paste(paste(names(uniqueCombos), uniqueCombos, sep = ": "), collapse = "; ")
      plotTitle <- paste0(plot, "\n", plotTitle)
      plot(msk, col = "grey", main = plotTitle, legend = FALSE, mar = c(3.1, 3.1, 3.1, 7.1))
      plot(res2,
        y = plot, axes = FALSE, ext = ext(msk),
        col = hcl.colors(100, "viridis"), sort = TRUE,
        add = TRUE, type = "continuous"
      )
    }
  }
  return(res)
}
  
#' Simple downscale
#'
#' @noRd
#' @inheritParams downscale
#' @template xyz
#'
#' @return A `data.table`.
#'
#' @import data.table
#' @importFrom DBI dbWriteTable dbExecute dbGetQuery SQL
#' @noRd
downscale_db_ <- function(
  xyz,
  refmap,
  gcms,
  gcm_ssp_ts,
  gcm_hist_ts,
  obs,
  obs_ts,
  return_refperiod = FALSE,
  vars,
  ppt_lr = FALSE
) {
  
  # Offload to database
  message("Extracting [%s] bands from %s"|> sprintf(format(nrow(refmap[["layers"]]), big.mark = ","), refmap[["tbl"]]))
  res <- extract_db(
    rastertbl = refmap[["tbl"]],
    layers = refmap[["layers"]],
    hull = attr(xyz, "hull")
  )

  # Compute elevation differences between provided points elevation and reference
  # Dem at position 74 (ID column + 36 reference layers + 36 lapse rate layers + 1 dem layer)
  elev_delta <- xyz[["elev"]] - res[, "dem2_WNA"]
  # print(elev_delta)
  # print(res)
  # Compute individual point lapse rate adjustments
  # Lapse rate position 38:73 (ID column + 36 reference layers + 36 lapse rate layers)
  lrCols <- grep("^lr_", names(res), value = TRUE)
  lr <- elev_delta * res[, lrCols] ## do we need anything other than the lapse rate?
  
  # Replace any NAs left with 0s
  lr[is.na(lr)] <- 0L
  
  # Remove lapse rates and digital elevation model from res
  lrDemCols <- grep("^lr_|dem2_WNA", names(res), value = TRUE)
  if (length(lrCols) == length(lrDemCols)) {
    stop("Error 01: can't find DEM layer. Please contact developer and supply error code")
  }
  
  res[, lrDemCols] <- NULL
  
  # Combine results (ignoring ID column)
  res <- as.data.frame(res) ## TODO: convert code below to data.table
  if (isTRUE(ppt_lr)) {
    notIDcols <- names(res)[which(tolower(names(res)) != "id")]
    
    if (any(paste0("lr_", notIDcols) != names(lr))) {
      stop(
        "Error 02: lapse rates and downscale output column names do not match.",
        "\n   Please contact developers."
      )
    }
    
    res[, notIDcols] <- res[, notIDcols] + lr
  } else {
    notpptLRDEM <- grep("^PPT|ID|^lr_|dem2_WNA", names(res), invert = TRUE, value = TRUE)
    lr_notpptLRDEM <- grep("^lr_PPT", names(lr), invert = TRUE, value = TRUE)
    if (any(paste0("lr_", notpptLRDEM) != lr_notpptLRDEM)) {
      stop(
        "Error 02: lapse rates and downscale output column names do not match.",
        "\n   Please contact developers."
      )
    }
    res[, notpptLRDEM] <- res[, notpptLRDEM] + lr[, lr_notpptLRDEM]
  }
  res <- as.data.table(res)

  # Process one GCM stacked layers
  if (!is.null(gcms)) {
    # Process each gcms and rbind resulting tables
    res_gcm <- data.table::rbindlist(
      lapply(gcms, process_one_climate_db, res = res, xyz = xyz, type = "gcms"),
      use.names = TRUE
    )
  } else {
    res_gcm <- NULL
  }
  if (!is.null(gcm_ssp_ts)) {
    # Process each gcms and rbind resulting tables
    res_gcmts <- data.table::rbindlist(
      lapply(gcm_ssp_ts, process_one_climate_db, res = res, xyz = xyz, timeseries = TRUE, type = "gcms"),
      use.names = TRUE
    )
  } else {
    res_gcmts <- NULL
  }
  if (!is.null(gcm_hist_ts)) {
    # Process each gcms and rbind resulting tables
    res_gcm_hist <- data.table::rbindlist(
      lapply(gcm_hist_ts, process_one_climate_db, res = res, xyz = xyz, type = "gcm_hist_ts"),
      use.names = TRUE
    )
  } else {
    res_gcm_hist <- NULL
  }
  if (!is.null(obs)) {
    # print(obs)
    res_hist <- data.table::rbindlist(
      lapply(obs, process_one_climate_db, res = res, xyz = xyz, type = "obs"),
      use.names = TRUE
    )
  } else {
    res_hist <- NULL
  }
  if (!is.null(obs_ts)) {
    # print(obs)
    res_hist_ts <- data.table::rbindlist(
      lapply(obs_ts, process_one_climate_db, res = res, xyz = xyz, timeseries = TRUE, type = "obs_ts"),
      use.names = TRUE
    )
  } else {
    res_hist_ts <- NULL
  }
  
  if (return_refperiod) {
    nm <- names(res)[-1]
    labels <- nm
    normal_ <- res
    # Reshape (melt / dcast) to obtain final form
    # ref_dt <- tstrsplit(nm, "_")
    ref_dt <- data.table::data.table(VAR = nm)
    # setDT(ref_dt)
    # setnames(ref_dt, c("VAR"))
    data.table::set(ref_dt, j = "variable", value = nm)
    data.table::set(ref_dt, j = "PERIOD", value = "1961_1990")
    data.table::setkey(ref_dt, "variable")
    # Set Latitude elevation and ID
    normal_[["lat"]] <- xyz[["lat"]]
    normal_[["elev"]] <- xyz[["elev"]]
    normal_[["id"]] <- xyz[["id"]]
    
    # Melt gcm_ and set the same key for merging
    normal_ <- melt(
      setDT(normal_),
      id.vars = c("id", "lat", "elev"),
      variable.factor = FALSE
    )
    setkey(normal_, "variable")
    
    # Finally, dcast back to final form to get original 36 columns
    normal_ <- dcast(
      # The merge with shared keys is as simple as that
      normal_[ref_dt, ],
      id + PERIOD + lat + elev ~ VAR,
      value.var = "value",
      sep = ""
    )
  } else {
    normal_ <- NULL
  }
  res <- rbind(res_gcm, res_gcmts, res_gcm_hist, res_hist, res_hist_ts, normal_, use.names = TRUE, fill = TRUE)
  # print(names(res))
  # Compute extra climate variables, assign by reference
  append_clim_vars(res, vars)
  
  return(res)
}

#' @noRd
process_one_climate_db <- function(
  r,
  res,
  xyz,
  timeseries = FALSE,
  type = c("gcms", "gcm_hist_ts", "obs", "obs_ts")
) {
  type <- match.arg(type)
  
  # Store names for later use
  nm <- r[["layers"]][["var_nm"]]
  if (!is.null(r[["VAR"]]) && length(r[["VAR"]])) {
    for (v in r[["VAR"]][-1]) {
      nm <- c(nm, gsub(r[["VAR"]][1], v, r[["layers"]][["var_nm"]], fixed = TRUE))
    }
  }

  # Run in database
  message("Extracting [%s] bands from %s"|> sprintf(format(nrow(r[["layers"]]), big.mark = ","), r[["tbl"]]))
  climaterast <- extract_db(
    rastertbl = r[["tbl"]],
    layers = r[["layers"]],
    VAR = r[["VAR"]],
    hull = attr(xyz, "hull")
  )

  labels <- vapply(
    strsplit(nm, "_"),
    function(x) {
      paste0(x[2:3], collapse = "_")
    },
    character(1)
  )
  
  if (type %in% c("obs")) {
    ## Create match set to match with res names
    labels <- nm
  }
    
  # Add matching column to climaterast
  res <- as.data.frame(res)
  # Locate PPT columns in labels 
  ppt_ <- grep("PPT", labels)
  # Labels does not have ID column, position in climaterast will be offset by 1
  ppt_offset <- ppt_ + 1L
  climaterast[, ppt_offset] <- climaterast[, ppt_offset] * res[, match(labels[ppt_], names(res))] ## PPT
  climaterast[, -c(1L, ppt_offset)] <- climaterast[, -c(1L, ppt_offset)] + res[, match(labels[-ppt_], names(res))] ## Temperatures
  res <- as.data.table(res)
  climaterast <- as.data.table(climaterast)
    
  # Reshape (melt / dcast) to obtain final form
  ref_dt <- tstrsplit(nm, "_")
  
  # Recombine PERIOD into one field
  if (!timeseries & type == "gcms") {
    ref_dt[[6]] <- paste(ref_dt[[6]], ref_dt[[7]], sep = "_")
    ref_dt[7] <- NULL
  }
  
  setDT(ref_dt)
  if (type %in% c("obs", "obs_ts")) {
    if (timeseries) {
      setnames(ref_dt, c("DATASET", "VAR", "MONTH", "PERIOD"))
      set(ref_dt, j = "variable", value = nm)
    } else {
      setnames(ref_dt, c("VAR", "MONTH"))
      set(ref_dt, j = "variable", value = nm)
      set(ref_dt, j = "PERIOD", value = "2001_2020")
    }
  }
    
  # Transform ref_dt to data.table for remerging
  if (type %in% c("gcms", "gcm_hist_ts")) {
    switch(type,
      gcms = setnames(ref_dt, c("GCM", "VAR", "MONTH", "SSP", "RUN", "PERIOD")),
      gcm_hist_ts = setnames(ref_dt, c("GCM", "VAR", "MONTH", "RUN", "PERIOD"))
    )
    set(ref_dt, j = "variable", value = nm)
    set(ref_dt, j = "GCM", value = gsub(".", "-", ref_dt[["GCM"]], fixed = TRUE))
  }
  
  setkey(ref_dt, "variable")
  
  # Set Latitude and possibly ID
  climaterast[["lat"]] <- xyz[["lat"]]
  climaterast[["elev"]] <- xyz[["elev"]]
  climaterast[["id"]] <- xyz[["id"]]
  
  # Melt climaterast and set the same key for merging
  climaterast <- melt(
    setDT(climaterast),
    id.vars = c("id", "lat", "elev"),
    variable.factor = FALSE
  )
  setkey(climaterast, "variable")
  
  # Finally, dcast back to final form to get original 36 columns
  form <- switch(type,
    gcms = quote(id + GCM + SSP + RUN + PERIOD + lat + elev ~ VAR + MONTH),
    gcm_hist_ts = quote(id + GCM + RUN + PERIOD + lat + elev ~ VAR + MONTH),
    obs = quote(id + PERIOD + lat + elev ~ VAR + MONTH),
    obs_ts = quote(id + DATASET + PERIOD + lat + elev ~ VAR + MONTH)
  )
  
  climaterast <- dcast(
    # The merge with shared keys is as simple as that
    climaterast[ref_dt, ],
    as.formula(form),
    value.var = "value",
    sep = "_"
  )
  
  return(climaterast)
}
    
#' #' Check xyz conforms to standards
#' #'
#' #' @template xyz
#' #' @param expectedCols character. Columns to look for in `xyz`.
#' #'
#' #' @return NULL
#' #' @noRd
#' .checkXYZ <- function(
#'   xyz,
#'   expectedCols = c("lon", "lat", "elev", "id")
#' ) {
#'   if (!is(xyz, "data.table")) {
#'     xyz <- tryCatch(as.data.table(xyz), error = function(e) e)
#'     if (is(xyz, "error")) {
#'       stop("Can't coherce xyz to a data.table object. Please pass a data.table object or another cohercible object class.")
#'     }
#'   }
#'   
#'   if (length(setdiff(expectedCols, names(xyz)))) {
#'     stop("'xyz' must have the columns ", paste(expectedCols, collapse = ", "))
#'   }
#'   
#'   if (length(unique(xyz$id)) != nrow(xyz)) {
#'     stop("'xyz$id' must be a column of unique IDs")
#'   }
#'   colTypes <- c("integer", "numeric", "character", "factor")
#'   if (!inherits(xyz$id, colTypes)) {
#'     stop("'xyz$id' must be an column of type ", paste(colTypes, collapse = ", "))
#'   } else {
#'     set(xyz, j = "id", value = as.integer(xyz[["id"]]))
#'   }
#'   
#'   return(xyz)
#' }


#' Check `downscale_core` arguments
#'
#' @inheritParams downscale_core
#'
#' @return NULL
#' @noRd
.checkDwnsclCoreArgs <- function(
  xyz,
  refmap,
  gcms = NULL,
  obs = NULL,
  gcm_ssp_ts = NULL,
  gcm_hist_ts = NULL,
  obs_ts = NULL,
  return_refperiod = FALSE,
  out_spatial = FALSE,
  plot = NULL,
  vars = list_vars()
) {
  notSupportedVars <- setdiff(vars, list_vars())
  if (length(notSupportedVars)) {
    stop(
      "The following variables are not supported:",
      "\n  ", paste(notSupportedVars, collapse = ", "),
      "\n  Please see 'list_vars' for list of supported variables."
    )
  }
  
  if (!return_refperiod %in% c(TRUE, FALSE)) {
    stop("'return_refperiod' must be TRUE or FALSE")
  }
  if (!out_spatial %in% c(TRUE, FALSE)) {
    stop("'out_spatial' must be TRUE or FALSE")
  }
  
  plot <- if (!is.null(plot)) {
    match.arg(plot, list_vars())
  }
  
  if (!isTRUE(attr(refmap, "builder") == "climr")) {
    stop(
      "Please use `input_refmap` function to create `refmap`.",
      " See `?input_refmap` for details."
    )
  }
  
  # Make sure gcms was built using input_gcms
  if (!is.null(gcms) && !isTRUE(attr(gcms, "builder") == "climr")) {
    stop(
      "Please use `input_gcms` function to create `gcms`.",
      " See `?input_gcms` for details."
    )
  }
  
  # Make sure gcm_ssp_ts was built using input_gcm_ssp
  if (!is.null(gcm_ssp_ts) && !isTRUE(attr(gcm_ssp_ts, "builder") == "climr")) {
    stop(
      "Please use `input_gcm_ssp` function to create `gcm_ssp_ts`.",
      " See `?input_gcm_ssp` for details."
    )
  }
  
  # Make sure gcm_hist_ts was built using input_gcm_hist
  if (!is.null(gcm_hist_ts) && !isTRUE(attr(gcm_hist_ts, "builder") == "climr")) {
    stop(
      "Please use `input_gcm_hist` function to create `gcm_hist_ts`.",
      " See `?input_gcm_hist` for details."
    )
  }
  
  # Make sure obs was built using input_obs
  if (!is.null(obs) && !isTRUE(attr(obs, "builder") == "climr")) {
    stop(
      "Please use `input_obs` function to create `obs`.",
      " See `?input_obs` for details."
    )
  }
  
  # Make sure obs_ts was built using input_obs_ts
  if (!is.null(obs_ts) && !isTRUE(attr(obs_ts, "builder") == "climr")) {
    stop(
      "Please use `input_obs_ts` function to create `obs_ts`.",
      " See `?input_obs_ts` for details."
    )
  }
  
  return(invisible(NULL))
}
      