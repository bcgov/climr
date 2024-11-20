# Leaflet vector grid plugin
dir.create("inst/shiny/www/htmlwidgets", showWarnings = FALSE, recursive = TRUE)
utils::download.file("https://unpkg.com/leaflet.vectorgrid@latest/dist/Leaflet.VectorGrid.bundled.min.js",
                     "inst/shiny/www/htmlwidgets/lfx-vgrid-prod.js")

# Western North-America BEC Subzone coloring from CCISS
dir.create("inst/shiny/data", showWarnings = FALSE, recursive = TRUE)
utils::download.file("https://github.com/bcgov/ccissr/raw/refs/heads/main/data-raw/data_tables/WNAv12_3_SubzoneCols.csv",
                     "inst/shiny/data/WNAv12_3_SubzoneCols.csv")
