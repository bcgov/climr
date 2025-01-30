# Testing
library(leaflet)
library(leafem)

leaflet::leaflet() |>
  leaflet::addTiles() |>
  leafem::addGeotiff(      
    url = "http://143.198.35.16/climr-tif/WNA/800m/Normal_1981_2010MSY/Tave_sp.tif",
    layerId = "overlay",
    opacity = 0.9,
    colorOptions = leafem::colorOptions(
      palette = grDevices::hcl.colors(256, palette = "Plasma"),
      na.color = "transparent"
    ),
    imagequery = TRUE
  )
