# Details

## Description
This opportunity is to work with the Climate Change Informed Species Selection (CCISS) research team at the Ministry of Forests, Lands, Natural Resource Operations and Rural Development. We are looking for a C++ and R programmer to develop an R-package for downscaling of global climate data for Central-Western North America (106-142W, 37-60N). The package will replicate the basic approach of ClimateNA, but with (1) improved speed using core C++ code and parallelization, (2) cloud delivery, and (3) programmatic access via R. A simple downscaling method will be used to bias-correct low-resolution (~100km) global climate model anomalies to high-resolution (2km) maps of historical climate, with further elevation adjustment to user-specified elevation grids/points based on empirical lapse rates (local relationship of climate to elevation) of the 2km climate maps. Elevation-adjusted monthly values of basic climate elements (temperature and precipitation) will then be used to estimate derived variables (e.g., degree-days) based on published equations and parameters. The input data for the application are complete and included in the GitHub repository.

## Tasks
1. Develop efficient and fully annotated core C++ code (linking to R via Rcpp) for the main computations. Code should support parallelisation for processing site locations, probably using the RcppParallel package. Specifically, the core code will:
  - Pull required GCM data from database.
  - Interpolate GCM anomalies to user-specified points.
  - Elevation-adjust basic elements (temp and precip) and bias correct anomalies using linear regression on a moving window to calculate lapse rate.
  - Calculate derived variables (e.g. degree-days) based on equations provided.
2. Setup postgresql cloud database on Digital Ocean for storage and retrieval of base climate data.
3. Fully annotated R package for programmatic access.
  - User-specified location and elevation input.
  - User-specified variable sets, GCMs, GCM runs, and time periods.
  - Optional data caching for speed.
4. Report providing necessary documentation for the use and further modification of the application.

The following resources are available in the GitHub repo:

1. 2km digital elevation model (DEM) for the study area
2. Gridded climate normals for the 2km DEM grid for the 1961-1990 period (12 monthly rasters for three climate elements: Tmin, Tmax, Precip).
3. Global climate model data: Average climate for 1961-1990 and five 20-year periods (2001-2020 through 2081-2100) for 13 models x multiple runs x 4 scenarios. Data are provided in NCDF format with accompanying csv metadata files.
4. A paper (Wang et al. 2016) describing the method for bias correction, elevation adjustment, and derived variables.
5. Parameter tables for calculating derived variables from basic climate elements.
