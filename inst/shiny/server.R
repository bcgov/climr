#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(raster) 
library(data.table)


bc_tasmax <- raster::brick("../../../climR-data/BC/tasmax_mClimMean_PRISM_historical_19710101-20001231.nc")
bc_tasmin <- raster::brick("../../../climR-data/BC/tasmin_mClimMean_PRISM_historical_19710101-20001231.nc")
bc_pr <- raster::brick("../../../climR-data/BC/pr_mClimMean_PRISM_historical_19710101-20001231.nc")
mat <- raster::brick("../../../climR-data/BC/map_1961-1990.tif")
rat <- subset(mat, 1)
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(rat),
                    na.color = "transparent")

iris_dt <- as.data.table(iris)[1:12]
iris_dt[, month := 1:12]
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  downscale_monthly <- reactive({
    xy_coords <- coord()
    bilinear_values <- lapply(list(pr = bc_pr,
                                   tasmin = bc_tasmin,
                                   tasmax = bc_tasmax),
                              raster:::.bilinearValue,
                              xyCoords = xy_coords)
    monthly_values <- lapply(bilinear_values, t)
    monthly_values <- data.table(month = 1:12,
                                 monthly_values$pr,
                                 monthly_values$tasmin,
                                 monthly_values$tasmax)
    setnames(monthly_values, c("month", "pr", "tasmin", "tasmax"))
    set(monthly_values, j = "Tavg", value = (monthly_values$tasmin + monthly_values$tasmax )/2)
    set(monthly_values, j = "DD_below_0", value = calc_DD_below_0(monthly_values$month, tm = monthly_values$Tavg))
    set(monthly_values, j = "DD_above_5", value = calc_DD_above_5(monthly_values$month, tm = monthly_values$Tavg, region = "West"))
    set(monthly_values, j = "DD_below_18", value = calc_DD_below_18(monthly_values$month, tm = monthly_values$Tavg))
    set(monthly_values, j = "DD_above_18", value = calc_DD_above_18(monthly_values$month, tm = monthly_values$Tavg, region = "The rest"))
    set(monthly_values, j = "RH", value = calc_RH(monthly_values$tasmin, monthly_values$tasmax))
    set(monthly_values, j = "RH", value = calc_RH(monthly_values$tasmin, monthly_values$tasmax))
    set(monthly_values, j = "NFFD", value = calc_NFFD(1:12, tm = monthly_values$tasmin))
    set(monthly_values, j = "PAS", value = calc_PAS(1:12, tm = monthly_values$Tavg))
    return(monthly_values[ , lapply(.SD, function(x) round(x, 1))])
  })
  
  downscale_annualy <- reactive({
    return(downscale_monthly()[ , lapply(.SD, function(x) round(mean(x),1))][ , -1])
  })
  
  
  output$annual_dt <- renderDT({
    datatable(downscale_annualy(),
              rownames = FALSE,
              options=list(iDisplayLength=1,
                           bLengthChange=0,
                           bFilter=0,
                           bInfo=0,
                           bPaginate = 0,
                           initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'background-color' : '#3c8dbc', 'color' : 'white'});}")
                           #bAutoWidth=0,                            # automatic column width calculation, disable if passing column width via aoColumnDefs
                           #aoColumnDefs = list(list(sWidth="300px", aTargets=c(list(0),list(1))))    # custom column size                       
              )
              )
  })
  
  # output$seasonal_dt <- renderDT({
  #   datatable(iris_dt[1],
  #             rownames = FALSE,
  #             options=list(iDisplayLength=1,
  #                          bLengthChange=0,
  #                          bFilter=0,
  #                          bInfo=0,
  #                          bPaginate = 0,
  #                          initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'background-color' : '#3c8dbc', 'color' : 'white'});}")
  #                          )
  #   )
  # })

  output$monthly_dt <- renderDT({
    datatable(downscale_monthly(),
              rownames = FALSE,
              options=list(iDisplayLength=12,
                           bLengthChange=0,
                           bFilter=0,
                           bInfo=0,
                           bPaginate = 0,
                           initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'background-color' : '#3c8dbc', 'color' : 'white'});}")
                           )
    )
  })
  
  output$monthly_subplot <- renderPlotly({

  monthly_plots <- list()
  i <- 1
    for (variable in names(downscale_monthly())) {
      if (class(downscale_monthly()[[variable]]) == "numeric" & variable != "month") {
        monthly_plots[[i]] <- plot_ly(x = downscale_monthly()[["month"]], y = downscale_monthly()[[variable]], type = 'scatter', mode = 'lines+markers', name = variable)
        i <- i + 1
      }
     
    }
  subplot(monthly_plots, nrows = 4)
  })
  
  
  output$map <- renderLeaflet({
    leaflet() |> addTiles() |>
      addRasterImage(rat, colors = pal, opacity = 0.8) |>
      addLegend(pal = pal, values = values(rat),
                title = "mat_1961-1990") |>
      setView(lng = -125.222385126562, lat = 54.2914890653002, zoom = 5) |>
      addMarkers(lng = -115.02, lat = 48.98)
  })
  
  map_proxy <- leafletProxy("map")
  
  observeEvent(input$map_click, {
    click <- input$map_click

    updateNumericInput(session, "latitude", value = click$lat)
    updateNumericInput(session, "longitude", value = click$lng)
  })
  
  coord <- reactive({
    cbind(input$longitude, input$latitude)
  })
  
  observeEvent(coord(), {
    map_proxy |> clearMarkers()
    map_proxy |> addMarkers(lng = coord()[1], lat = coord()[2])
  })
  

})
