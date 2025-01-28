# Testing
library(jsonlite)
library(leaflet)

target_server <- "http://143.198.35.16:8080"

index <- jsonlite::fromJSON("%s/index.json" |> sprintf(target_server), simplifyDataFrame = FALSE)

t <- index[[1]]

leaflet::leaflet() |>
    leaflet::addTiles() |>
    leaflet::addTiles(
      urlTemplate = t$tiles,
       options = leaflet::tileOptions(maxNativeZoom = t$maxzoom, minZoom = t$minzoom, opacity = 0.5, )
    )
