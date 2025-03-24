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
  DBI::dbExecute(dbCon, "SET client_min_messages TO WARNING;")
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
  DBI::dbExecute(dbCon, "DROP TABLE IF EXISTS tmp_xyz;") |> invisible()
  
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
    ),
    hull = DBI::dbGetQuery(dbCon, "SELECT ST_ASTEXT(ST_ConvexHull(ST_Collect(geom))) FROM tmp_xyz")[[1]]
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
  
  
  # Test tiles interpolation using normal_composite
  q1 <- "
  CREATE TABLE tmp_xyz AS (
    WITH tmp_xyz_1 AS (
      SELECT ST_SetSRID(ST_MAKEPOINT(ST_UpperLeftX(rast), ST_UpperLeftY(rast)), 4326) AS g0,
             ST_PixelWidth(rast) AS pw,
    		 ABS(ST_PixelHeight(rast)) AS ph
      FROM normal_composite
      WHERE rid = 500
    )
    -- 4 corner point
    SELECT 0 as id, ST_Translate(g0, 0 ,0) AS geom FROM tmp_xyz_1
    UNION ALL
    -- left up from corner point
    SELECT 1 as id, ST_Translate(g0, -pw/4 ,+ph/4) AS geom FROM tmp_xyz_1
    UNION ALL
    -- right up corner point
    SELECT 2 as id, ST_Translate(g0, +pw/4 ,+ph/4) AS geom FROM tmp_xyz_1
    UNION ALL
    -- left bottom corner point
    SELECT 3 as id, ST_Translate(g0, -pw/4 ,-ph/4) AS geom FROM tmp_xyz_1
    UNION ALL
    -- right bottom corner point
    SELECT 4 as id, ST_Translate(g0, +pw/4 ,-ph/4) AS geom FROM tmp_xyz_1
    UNION ALL
    -- left up edge point
    SELECT 5 as id, ST_Translate(g0, -pw*2 ,+ph/4) AS geom FROM tmp_xyz_1
    UNION ALL
    -- right up edge point
    SELECT 6 as id, ST_Translate(g0, +pw*2 ,+ph/4) AS geom FROM tmp_xyz_1
    UNION ALL
    -- left bottom edge point
    SELECT 7 as id, ST_Translate(g0, -pw*2 ,-ph/4) AS geom FROM tmp_xyz_1
    UNION ALL
    -- right bottom edge point
    SELECT 8 as id, ST_Translate(g0, +pw*2 ,-ph/4) AS geom FROM tmp_xyz_1
    UNION ALL
    -- up left edge point
    SELECT 9 as id, ST_Translate(g0, -pw/4 ,+ph*2) AS geom FROM tmp_xyz_1
    UNION ALL
    -- up right edge point
    SELECT 10 as id, ST_Translate(g0, +pw/4 ,+ph*2) AS geom FROM tmp_xyz_1
    UNION ALL
    -- bottom left edge point
    SELECT 11 as id, ST_Translate(g0, -pw/4 ,-ph*2) AS geom FROM tmp_xyz_1
    UNION ALL
    -- bottom right edge point
    SELECT 12 as id, ST_Translate(g0, +pw/4 ,-ph*2) AS geom FROM tmp_xyz_1
  )"
  
  DBI::dbExecute(dbCon, q1)
  
  bands <- c(6,18,30,42,54,66)
  results_1 <- extract_db(
    dbCon,
    "normal_composite",
    data.table::data.table(
      var_nm = paste("value", bands, sep = "_"),
      laynum = bands
    ),
    hull = DBI::dbGetQuery(dbCon, "SELECT ST_ASTEXT(ST_ConvexHull(ST_Collect(geom))) FROM tmp_xyz")[[1]]
  )
  
  q2 <- "
  SELECT id \"ID\",
       %s
  FROM tmp_xyz
  CROSS JOIN (
    SELECT ST_UNION(rast) rast
    FROM normal_composite
    WHERE ST_Intersects(ST_Convexhull(rast), (SELECT ST_ConvexHull(ST_Collect(geom)) FROM tmp_xyz))
  );
  " |> sprintf(
    paste0("ST_Value(rast, %s, geom, true, 'bilinear') value_%s" |> sprintf(bands, bands), collapse = ",\n       ")
  )
  results_2 <- DBI::dbGetQuery(dbCon, q2)
  
  testthat::expect_equal(results_1, results_2, info = "Testing that the custom SQL returns the same value as ST_Value (multi tiles crossing)")
  
  DBI::dbExecute(dbCon, "DROP TABLE tmp_xyz;")
  DBI::dbDisconnect(dbCon)
  
})