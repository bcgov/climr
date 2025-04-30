test_that("test ts tables", {
  testInit("data.table")
  
  dbCon <- climr::data_con()
  
  t <- expand.grid(
    tbl = climr:::dbnames_ts$dbname,
    v = c("PPT", "Tmin", "Tmax")
  )
  t$vtbl <- stringi::stri_replace_all_fixed(t$tbl, "VAR", tolower(t$v), vectorize_all = TRUE)
  q <- paste0(collapse = " UNION ALL ", 
              "SELECT '%s' AS tbl, '%s' AS vtbl, MAX(ST_NUMBANDS(rast)) maxnbands, MIN(ST_NUMBANDS(rast)) minnbands FROM \"%s\"" |> sprintf(t$tbl, t$vtbl, t$vtbl)
  )
  res <- DBI::dbGetQuery(dbCon, q) |> data.table::setDT()
  res[order(tbl, vtbl)]
  ts_table_with_differings_num_bands_across_VAR <- res[,min(maxnbands) != max(minnbands), by = "tbl"][which(V1), tbl]
  
  testthat::expect_length(ts_table_with_differings_num_bands_across_VAR, 0)
  
  cat("\n")
  print(res[tbl %in% ts_table_with_differings_num_bands_across_VAR])
  
})