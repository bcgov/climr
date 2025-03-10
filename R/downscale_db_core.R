#' @export
#' @rdname downscale_core
downscale_db_core <- function(
  dbCon,
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
  
  res <- downscale_db_(
    dbCon,
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
  dbCon,
  xyz,
  refmap,
  gcms,
  gcm_ssp_ts,
  gcm_hist_ts,
  obs,
  obs_ts,
  return_refperiod,
  vars,
  ppt_lr = FALSE
) {
  
  # Offload all it to the database

  # Locate fields position
  demlyr <- refmap[["layers"]][grep("^dem", var_nm, ignore.case = TRUE), laynum]
  lrlyr <- refmap[["layers"]][grep("^lr", var_nm, ignore.case = TRUE), laynum]
  vlyr <- setdiff(refmap[["layers"]][["laynum"]], c(demlyr, lrlyr))
  mlrlyr <- lrlyr[match(refmap[["layers"]][["var_nm"]][vlyr], refmap[["layers"]][["var_nm"]][lrlyr] |> gsub("lr_", "", x = _))]
  pptlyr <- refmap[["layers"]][mlrlyr, grep("ppt", var_nm, ignore.case = TRUE)]
  
  # Run in database

  # Query building
  lradj <- "+ COALESCE(delta * ST_Value(rast, %s, xyz.geom, false, 'bilinear'), 0)" |>
    sprintf(mlrlyr)
  if (!isTRUE(ppt_lr)) {
    lradj[pptlyr] <- ""
  }
  vals <- "(ST_Value(rast, %s, xyz.geom, false, 'bilinear') %s) as \"%s\"" |>
    sprintf(vlyr, lradj, refmap[["layers"]][["var_nm"]][vlyr]) |>
    paste(collapse = ",\n     ")
  dem <- "ST_Value(rast, %s, xyz.geom, false, 'bilinear')" |> sprintf(demlyr)

  q <- DBI::SQL("
    SELECT %s
    FROM \"%s\" xyz, \"%s\" normal
    CROSS JOIN LATERAL (
      SELECT (xyz.elev - %s) AS delta
    ) delta_val
    WHERE ST_Intersects(normal.rast,xyz.geom)
    ORDER BY xyz.id;" |>
    sprintf(vals, xyz, refmap[["tbl"]], dem)
  )

  res <- DBI::dbGetQuery(dbCon, q) |> data.table::setDT()

  # Process one GCM stacked layers
  if (!is.null(gcms)) {
    # Process each gcms and rbind resulting tables
    res_gcm <- data.table::rbindlist(
      lapply(gcms, process_one_climate_db, dbCon = dbCon, res = res, xyz = xyz, type = "gcms"),
      use.names = TRUE
    )
  } else {
    res_gcm <- NULL
  }
  if (!is.null(gcm_ssp_ts)) {
    # Process each gcms and rbind resulting tables
    res_gcmts <- data.table::rbindlist(
      lapply(gcm_ssp_ts, process_one_climate_db, dbCon = dbCon, res = res, xyz = xyz, timeseries = TRUE, type = "gcms"),
      use.names = TRUE
    )
  } else {
    res_gcmts <- NULL
  }
  if (!is.null(gcm_hist_ts)) {
    # Process each gcms and rbind resulting tables
    res_gcm_hist <- data.table::rbindlist(
      lapply(gcm_hist_ts, process_one_climate_db, dbCon = dbCon, res = res, xyz = xyz, type = "gcm_hist_ts"),
      use.names = TRUE
    )
  } else {
    res_gcm_hist <- NULL
  }
  if (!is.null(obs)) {
    # print(obs)
    res_hist <- data.table::rbindlist(
      lapply(obs, process_one_climate_db, dbCon = dbCon, res = res, xyz = xyz, type = "obs"),
      use.names = TRUE
    )
  } else {
    res_hist <- NULL
  }
  if (!is.null(obs_ts)) {
    # print(obs)
    res_hist_ts <- data.table::rbindlist(
      lapply(obs_ts, process_one_climate_db, dbCon = dbCon, res = res, xyz = xyz, timeseries = TRUE, type = "obs_ts"),
      use.names = TRUE
    )
  } else {
    res_hist_ts <- NULL
  }
  
  if (return_refperiod) {
    xyzdb <- DBI::dbGetQuery(dbCon, "SELECT \"id\",\"lat\",\"elev\" FROM %s" |> sprintf(xyz))
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
    normal_[["lat"]] <- xyzdb[["lat"]]
    normal_[["elev"]] <- xyzdb[["elev"]]
    normal_[["id"]] <- xyzdb[["id"]]
    
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
  dbCon,
  r,
  res,
  xyz,
  timeseries = FALSE,
  type = c("gcms", "gcm_hist_ts", "obs", "obs_ts")
) {
  type <- match.arg(type)
  
  # Store names for later use
  nm <- r[["layers"]][["var_nm"]]
    
  # Run in database
  climaterast <- extract_db(dbCon, xyz, r[["tbl"]], r[["layers"]][["laynum"]], nm)
    
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
  ppt_ <- grep("PPT", labels)
  ppt_next <- ppt_ + 1L
  climaterast[, ppt_next] <- climaterast[, ppt_next] * res[, match(labels[ppt_], names(res))] ## PPT
  climaterast[, -c(1L, ppt_next)] <- climaterast[, -c(1L, ppt_next)] + res[, match(labels[-ppt_], names(res))] ## Temperature
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
  
  xyzdb <- DBI::dbGetQuery(dbCon, "SELECT \"id\",\"lat\",\"elev\" FROM %s ORDER by \"id\"" |> sprintf(xyz))
  # Set Latitude and possibly ID
  climaterast[["lat"]] <- xyzdb[["lat"]]
  climaterast[["elev"]] <- xyzdb[["elev"]]
  climaterast[["id"]] <- xyzdb[["id"]]
  
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
    
#' Check xyz conforms to standards
#'
#' @template xyz
#' @param expectedCols character. Columns to look for in `xyz`.
#'
#' @return NULL
#' @noRd
.checkXYZ <- function(
  xyz,
  expectedCols = c("lon", "lat", "elev", "id")
) {
  if (!is(xyz, "data.table")) {
    xyz <- tryCatch(as.data.table(xyz), error = function(e) e)
    if (is(xyz, "error")) {
      stop("Can't coherce xyz to a data.table object. Please pass a data.table object or another cohercible object class.")
    }
  }
  
  if (length(setdiff(expectedCols, names(xyz)))) {
    stop("'xyz' must have the columns ", paste(expectedCols, collapse = ", "))
  }
  
  if (length(unique(xyz$id)) != nrow(xyz)) {
    stop("'xyz$id' must be a column of unique IDs")
  }
  colTypes <- c("integer", "numeric", "character", "factor")
  if (!inherits(xyz$id, colTypes)) {
    stop("'xyz$id' must be an column of type ", paste(colTypes, collapse = ", "))
  } else {
    set(xyz, j = "id", value = as.integer(xyz[["id"]]))
  }
  
  return(xyz)
}


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
  
  ## check for "silly" parameter combinations
  if (all(
    is.null(gcms), is.null(gcm_ssp_ts), is.null(gcm_hist_ts),
    is.null(obs), is.null(obs_ts)
  )) {
    warning("'gcms', 'gcm_ssp_ts', 'gcm_hist_ts', 'obs' and 'obs_ts' are missing. Nothing to downscale.")
  }
  
  return(invisible(NULL))
}
      