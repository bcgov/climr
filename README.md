<!-- badges: start -->
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](<Redirect-URL>)
<!-- badges: end -->

# Data

This branch is used exclusively to store climRpnw input data. This is an orphan with no relation to the main branch.

## Data format

For netCDF files, a name matched layer labels `.csv` file should be saved alongside the actual data.

## Directory structure

### `inputs_pkg`

Data source are in three folders : `dem` (Digital Elevation Model), `gcm` (Global Circulation Models) and `normal`.

#### `dem`

One folder per `dem`, folder name should represent `dem` label. Only one file, one layer per `dem` subdirectory. `dem` extent must match `normal` extent. Resampling can be done with `terra::resample` if this is not the case.

#### `gcm`

One folder per `gcm`. Folder name must match `gcm` label.

#### `normal`

One folder per `normal`. Folder name must match `normal` label. `normal` extent must match `dem` extent. Resampling can be done with `terra::resample` if this is not the case.

### `inputs_raw`

Same as `inputs_pkg`, but storage optimized for faster downloads and loading using ["./scripts/reformat_inputs.R""](./scripts/reformat_inputs.R).

# climR-pnw
An R package for downscaled global climate model normals in the Pacific Northwest

Copyright 2021 Province of British Columbia

Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with the License. You may obtain a copy of the License at

http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language governing permissions and limitations under the License.
