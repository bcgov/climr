
# Setup ----

library(shiny)
library(leaflet)
library(leaflet.extras)
library(DT)

source("scripts/utils.R")

# Data source poll ----

# precipitation <- lapply(1L:90L, function(i) {
#   f <- \() sprintf("%sT06Z_MSC_HRDPA_APCP-Accum24h_Sfc_RLatLon0.0225_PT0H.tif", format(Sys.Date()-i+1L, "%Y%m%d"))
#   dt_prep("model_hrdpa/2.5km/06", f, terra::rast)
# })
# pcols <- local({
#   z <- rev(terra:::.default.pal()) |> grDevices::col2rgb()
#   grDevices::rgb(red = z[1,], green = z[2,], blue = z[3,], maxColorValue = 255, alpha = (seq_along(z[1,]) - 1) * 0.91 / 100 * 255)
# })

# MapBox values
mbtk <- Sys.getenv("BCGOV_MAPBOX_TOKEN")
mblbstyle <- Sys.getenv("BCGOV_MAPBOX_LABELS_STYLE")
mbhsstyle <- Sys.getenv("BCGOV_MAPBOX_HILLSHADE_STYLE")

# Base map ----
l <- leaflet::leaflet() |>
  # base layer
  leaflet::addProviderTiles(
    provider = leaflet::providers$CartoDB.PositronNoLabels,
    options = leaflet::pathOptions(pane = "mapPane"),
    group = "Light"
  ) |>
  leaflet::addProviderTiles(
    provider = leaflet::providers$CartoDB.DarkMatterNoLabels, 
    options = leaflet::pathOptions(pane = "mapPane"),
    group = "Dark"
  ) |>
  leaflet::addProviderTiles(
    provider = leaflet::providers$Esri.WorldImagery, 
    options = leaflet::pathOptions(pane = "mapPane"),
    group = "Satellite"
  ) |>
  leaflet::addProviderTiles(
    provider = leaflet::providers$OpenStreetMap, 
    options = leaflet::pathOptions(pane = "mapPane"),
    group = "OpenStreetMap"
  ) |>
  leaflet::addTiles(
    urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mbhsstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
    attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
    options = leaflet::pathOptions(pane = "mapPane"),
    group = "Hillshade"
  ) |>
  # overlay layer
  leaflet::addTiles(
    urlTemplate = paste0("https://api.mapbox.com/styles/v1/", mblbstyle, "/tiles/{z}/{x}/{y}?access_token=", mbtk),
    attribution = '&#169; <a href="https://www.mapbox.com/feedback/">Mapbox</a>',
    options = leaflet::pathOptions(pane = "overlayPane"),
    group = "Labels"
  ) |>
  add_wna() |>
  # extensions
  leaflet.extras::addSearchOSM(
    options = leaflet.extras::searchOptions(
      collapsed = TRUE,
      hideMarkerOnCollapse = TRUE,
      autoCollapse = TRUE,
      zoom = 11
    )
  ) |>
  leaflet::addLayersControl(
    baseGroups = c("Light", "Dark", "Satellite", "OpenStreetMap", "Hillshade"),
    overlayGroups = c("Labels", "WNA BEC"),
    position = "topright"
  ) |>
  leaflet::setView(lng = -100, lat = 50, zoom = 5) |>
  leaflet::addMiniMap(toggleDisplay = TRUE, minimized = TRUE)

# Shiny App ----

shiny::shinyApp(

# Shiny UI ----
    
  ui = shiny::navbarPage(collapsible = TRUE, 
    theme = bslib::bs_theme(
      preset = "bcgov",
      "navbar-brand-padding-y" = "0rem",
      "navbar-brand-margin-end" = "4rem"
    ), 
    title = shiny::tagList(
      shiny::tags$image(
        src = "images/bcid-logo-rev-en.svg",
        style = "display: inline-block",
        height = "35px",
        alt = "British Columbia"
      ),
      "ClimR"
    ),
    shiny::tabPanel(title = "Map",
      shiny::div(class="outer",
        leaflet::leafletOutput("climr", width = "100%", height = "100%"),
        shiny::absolutePanel(
          top = "204px",
          right = "10px",
          shiny::fileInput("upload", "Upload file or geometry")
        )
      )
    ),
    shiny::navbarMenu(
      "Data",
      "Locations",
      shiny::tabPanel(title = "Points",
        DT::DTOutput(outputId = "points_dt")
      )
    ),
    shiny::navbarMenu(
      "About",
      "----",
      shiny::tabPanel("How to use")
    ),
    header = list(
      shiny::includeCSS("www/style.css"),
      shiny::includeScript("www/script.js")
    )
  )

  ,

# Shiny server ----

  server = function(input, output, session) {

    output$climr <- leaflet::renderLeaflet(l)
    
# Climate layers ----
    
    # lyr_added <- list()
    # current_date_idx <- list()
    
    # # Flood Zones
    # shiny::observe({
    #   selected_groups <- shiny::req(input$geonesis_groups)
    #   if (overlays$fz %in% selected_groups & !isTRUE(lyr_added[[overlays$fz]])) {
    #     shiny::withProgress({
    #       shiny::setProgress(0.33, "Reading spatial feature")
    #       fz <- flood_zones()
    #       if (is.null(fz)) {
    #         shiny::setProgress(1, "Cancelled")
    #         shiny::showNotification("Flood zones data is currently unavailable.", type = "warning")
    #         leaflet::leafletProxy("geonesis") |>
    #           leaflet::hideGroup(overlays$fz)
    #       } else {
    #         shiny::setProgress(0.66, "Adding flood zones data to map")
    #         leaflet::leafletProxy("geonesis") |>
    #           leaflet::addPolygons(
    #             data = fz, color = "blue", weight = 1, smoothFactor = 0.25,
    #             opacity = 1.0, fillOpacity = 0.25,
    #             popup = shiny::HTML("Présence de zones inondables cartographiées ", shiny::a("(Details)", target = "_blank", href = "https://geoinondations.gouv.qc.ca/glossaire.html") |> as.character()),
    #             group = overlays$fz
    #           )
    #         lyr_added[[overlays$fz]] <<- TRUE; rm(fz)
    #         shiny::setProgress(1, "Drawing polygons")
    #       }
    #     })
    #   }
    # })
      
    # # Burned Zones
    # shiny::observe({
    #   selected_groups <- shiny::req(input$geonesis_groups)
    #   if (overlays$bz %in% selected_groups & !isTRUE(lyr_added[[overlays$bz]])) {
    #     shiny::withProgress({
    #       shiny::setProgress(0.33, "Reading spatial feature")
    #       p <- perimeters()
    #       if (is.null(p)) {
    #         shiny::setProgress(1, "Cancelled")
    #         shiny::showNotification("Burning zones data is currently unavailable.", type = "warning")
    #         leaflet::leafletProxy("geonesis") |>
    #           leaflet::hideGroup(overlays$bz)
    #       } else {          
    #         shiny::setProgress(0.66, "Adding burning zones data to map")
    #         leaflet::leafletProxy("geonesis") |>
    #           leaflet::addPolygons(
    #             data = p, color = "red", weight = 1, smoothFactor = 0.25,
    #             opacity = 1.0, fillOpacity = 0.25,
    #             popup = leafpop::popupTable(
    #               local({
    #                 a <- p[,3:5]
    #                 a$i <- shiny::HTML(
    #                   "Season-to-date buffered hotspots<br />(> 1000 Ha) ", 
    #                   shiny::a(
    #                     "(Details)",
    #                     target = "_blank", 
    #                     href = "https://cwfis.cfs.nrcan.gc.ca/mini-entrepot/metadata/fm3buffered"
    #                   ) |> as.character()
    #                 )
    #                 names(a)[1] <- "Hectares";a
    #               }),
    #               zcol = c(1:3,5), row.numbers = FALSE, feature.id = FALSE
    #             ),
    #             group = overlays$bz
    #           )
    #         lyr_added[[overlays$bz]] <<- TRUE; rm(p)
    #         shiny::setProgress(1, "Drawing polygons")
    #       }
    #     })
    #   }
    # })

    # # Active Fires
    # shiny::observe({
    #   selected_groups <- shiny::req(input$geonesis_groups)
    #   if (overlays$af %in% selected_groups & !isTRUE(lyr_added[[overlays$af]])) {
    #     shiny::withProgress({
    #       shiny::setProgress(0.33, "Reading spatial feature")
    #       af <- active_fires()
    #       if (is.null(af)) {
    #         shiny::setProgress(1, "Cancelled")
    #         shiny::showNotification("Active fires data is currently unavailable.", type = "warning")
    #         leaflet::leafletProxy("geonesis") |>
    #           leaflet::hideGroup(overlays$af)
    #       } else {  
    #         shiny::setProgress(0.66, "Adding active fires data to map")
    #         leaflet::leafletProxy("geonesis") |>
    #            leaflet::addCircleMarkers(
    #             data = af, radius = ~radius, color = "#000", weight = 0.5,
    #             opacity = 1, fillColor = ~stage_color, fillOpacity = 1,
    #             popup = leafpop::popupTable(
    #               af, row.numbers = FALSE, feature.id = FALSE,
    #               zcol = c(1:7)
    #             ), group = overlays$af
    #           ) |>
    #           leaflet::addLegend(
    #             colors = c("Red", "Yellow", "DodgerBlue", "Orange"),
    #             labels = c("Out of control", "Being held", "Under control", "Other"),
    #             group = overlays$af,
    #             opacity = 0.8, title = overlays$af, position = "bottomright"
    #           )
    #         lyr_added[[overlays$af]] <<- TRUE; rm(af)
    #         shiny::setProgress(1, "Drawing polygons")
    #       }
    #     })
    #   }
    # })
      
    # # Precipitation
    # shiny::observe({
    #   selected_groups <- shiny::req(input$geonesis_groups)
    #   selected_date <- shiny::req(input$as_of_date)
    #   if (overlays$p %in% selected_groups) {
    #     if (!isTRUE(lyr_added[[overlays$p]])) {
    #       shiny::withProgress({
    #         shiny::setProgress(0.33, "Reading spatial raster")
    #         current_date_idx[[overlays$p]] <<- i <- {Sys.Date() - selected_date} |> as.integer()
    #         p <- precipitation[[i]]()
    #         if (is.null(p)) {
    #           shiny::setProgress(1, "Cancelled")
    #           shiny::showNotification("Precipitation (HRDPA) data is currently unavailable for selected date.", type = "warning")
    #           leaflet::leafletProxy("geonesis") |>
    #             leaflet::hideGroup(overlays$p)
    #         } else {  
    #           shiny::setProgress(0.66, "Adding precipitation (HRDPA) data to map")
    #           r <- terra::minmax(p)
    #           v <- seq(r[1], r[2], length.out=5)
    #           pal <- leaflet::colorNumeric(pcols, v, reverse = TRUE)
    #           leaflet::leafletProxy("geonesis") |>
    #             leaflet::addRasterImage(p, colors = pcols, opacity=1, project = TRUE, group = overlays$p, layerId = overlays$p, options = gridOptions(zIndex = 10)) |>
    #             leaflet::addLegend(
    #               position = "bottomright", pal = pal, values = v, opacity = 1,
    #               title = "Precipitation (HRDPA) [mm]", group = overlays$p, layerId = overlays$p,
    #               labFormat = leaflet::labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    #             )
    #           lyr_added[[overlays$p]] <<- TRUE; rm(p)
    #           shiny::setProgress(1, "Drawing raster")
    #         }
    #       }) 
    #     } else {
    #       j <- {Sys.Date() - selected_date} |> as.integer()
    #       if (current_date_idx[[overlays$p]] != j) {
    #         shiny::withProgress({
    #           shiny::setProgress(0.33, "Replacing spatial raster")
    #           current_date_idx[[overlays$p]] <<- j
    #           p <- precipitation[[j]]()
    #           if (is.null(p)) {
    #             shiny::setProgress(1, "Cancelled")
    #             shiny::showNotification("Precipitation (HRDPA) data is currently unavailable for selected date.", type = "warning")
    #             leaflet::leafletProxy("geonesis") |>
    #               leaflet::hideGroup(overlays$p)
    #           } else {  
    #             shiny::setProgress(0.66, "Adding precipitation (HRDPA) data to map")
    #             r <- terra::minmax(p)
    #             v <- seq(r[1], r[2], length.out=5)
    #             pal <- leaflet::colorNumeric(pcols, v, reverse = TRUE)
    #             leaflet::leafletProxy("geonesis") |>
    #               leaflet::clearGroup(overlays$p) |>
    #               leaflet::removeControl(overlays$p) |>
    #               leaflet::addRasterImage(p, colors = pcols, opacity=1, project = TRUE, group = overlays$p, layerId = overlays$p, options = gridOptions(zIndex = 10)) |>
    #               leaflet::addLegend(
    #                 position = "bottomright", pal = pal, values = v, opacity = 1,
    #                 title = "Precipitation (HRDPA) [mm]", group = overlays$p, layerId = overlays$p,
    #                 labFormat = leaflet::labelFormat(transform = function(x) sort(x, decreasing = TRUE))
    #               )
    #             lyr_added[[overlays$p]] <<- TRUE; rm(p)
    #             shiny::setProgress(1, "Drawing raster")
    #           }
    #         })
    #       }
    #     }
    #   }
    # })
  }
)
