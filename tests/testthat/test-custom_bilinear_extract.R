test_that("testing that the custom bilinear function works", {
  
  # Establish database connection
  dbCon <- DBI::dbConnect(
    RPostgres::Postgres(),
    dbname = 'climr',
    host = '146.190.244.244',
    port = 5432,
    user = 'postgres',
    password = 'climr2022'
  )

  # Drop the table if it exists ----
  DBI::dbExecute(dbCon, "DROP TABLE IF EXISTS test_rasters;")
  ### Create 10x10 raster with 10 bands with random values and NAs
  # Create a table to store the raster
  DBI::dbExecute(dbCon, "
    CREATE TABLE test_rasters (
      rid serial PRIMARY KEY,
      rast raster
    );
  ")
  
  # Generate a 10x10 raster with 10 bands using PostGIS functions ----
  DBI::dbExecute(dbCon, "
    DO $$
    DECLARE
        rast raster;
        i integer;
        j integer;
        b integer;
    BEGIN
        -- Create an empty 10x10 raster (upper-left at 0,0, scale_x=1, scale_y=-1)
        rast := ST_MakeEmptyRaster(10, 10, 0, 0, 1, -1, 0, 0, 4326);
        
        -- Add 10 bands with a defined NODATA value
        FOR b IN 1..10 LOOP
            -- Add band with '32BF' type, initial value 0.0, and NODATA value NULL
            rast := ST_AddBand(rast, '32BF'::text, 0.0, -9999.0);
            FOR i IN 1..10 LOOP
                FOR j IN 1..10 LOOP
                    IF random() < 0.1 THEN
                        -- Set pixel to NULL (NODATA)
                        rast := ST_SetValue(rast, b, i, j, -9999.0);
                    ELSE
                        -- Random value between 0 and 100
                        rast := ST_SetValue(rast, b, i, j, random() * 100);
                    END IF;
                END LOOP;
            END LOOP;
        END LOOP;
        
        -- Insert the raster into the table
        INSERT INTO test_rasters (rast) VALUES (rast);
    END $$;
  ")
  
  # Drop the table if it exists ----
  DBI::dbExecute(dbCon, "DROP TABLE IF EXISTS tmp_xyz;")
  
  ### Create 5 geometry POINTs within the raster ----
  # Create a table to store the points
  DBI::dbExecute(dbCon, "
    CREATE TABLE tmp_xyz (
        id serial PRIMARY KEY,
        geom geometry(Point, 4326)
    );
  ")
  
  # Insert 5 points within the raster extent (x: 0 to 10, y: -10 to 0) ----
  DBI::dbExecute(dbCon, "
    INSERT INTO tmp_xyz (geom) VALUES
        (ST_SetSRID(ST_MakePoint(1.1, -1.1), 4326)),
        (ST_SetSRID(ST_MakePoint(2, -2), 4326)),
        (ST_SetSRID(ST_MakePoint(3.56, -3.22), 4326)),
        (ST_SetSRID(ST_MakePoint(4.42, -4.78), 4326)),
        (ST_SetSRID(ST_MakePoint(5.5, -5.5), 4326));
  ")
  
  ### Compare the results of the function with ST_Value for each band ----
  # Query to get results from both the custom function and ST_Value
  bands <- c(3:1,10:8)
  results_1 <- extract_db(
    dbCon,
    "test_rasters",
    data.table::data.table(
      var_nm = as.character(bands),
      laynum = bands
    )
  )
  
  results_2 <- DBI::dbGetQuery(dbCon, "
    SELECT 
        p.id \"ID\",
        %s
    FROM tmp_xyz p
    CROSS JOIN test_rasters r
    WHERE r.rid = 1;
  " |> sprintf(
    "ST_Value(r.rast, %s, p.geom, true, 'bilinear') AS \"%s\"" |>
      sprintf(bands, bands) |>
      paste0(collapse = ",")
    )
  )
  
  testthat::expect_equal(results_1, results_2, info = "Testing that the custom SQL returns the same value as ST_Value")
  
  # Clean up by dropping test tables and disconnecting
  DBI::dbExecute(dbCon, "DROP TABLE test_rasters;")
  DBI::dbExecute(dbCon, "DROP TABLE tmp_xyz;")
  DBI::dbDisconnect(dbCon)
  
})