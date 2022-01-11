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

mat <- raster::brick("C:/Users/nicol/OneDrive/Documents/GitHub/climR-data/BC/mat_1961-1990.tif")
rat <- subset(mat, 1)
pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(rat),
                    na.color = "transparent")

iris_dt <- as.data.table(iris)[1:12]
iris_dt[, month := 1:12]
# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  output$annual_dt <- renderDT({
    datatable(iris_dt[1],
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
  
  output$seasonal_dt <- renderDT({
    datatable(iris_dt[1],
              rownames = FALSE,
              options=list(iDisplayLength=1,
                           bLengthChange=0,
                           bFilter=0,
                           bInfo=0,
                           bPaginate = 0,
                           initComplete = JS("function(settings, json) {$(this.api().table().header()).css({'background-color' : '#3c8dbc', 'color' : 'white'});}")
                           )
    )
  })

  output$monthly_dt <- renderDT({
    datatable(iris_dt,
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
    for (variable in names(iris_dt)) {
      if (class(iris_dt[[variable]]) == "numeric" & variable != "month") {
        monthly_plots[[i]] <- plot_ly(x = iris_dt[["month"]], y = iris_dt[[variable]], type = 'scatter', mode = 'lines+markers', name = variable)
          
      }
      i <- i + 1
    }
  subplot(monthly_plots, nrows = 4)
  })
  
  
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addRasterImage(rat, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = values(rat),
                title = "mat_1961-1990")
  })
  
  
  observeEvent(input$map_click, {
    click <- input$map_click
    leafletProxy("map") |> clearMarkers()
    leafletProxy("map") |> addMarkers(lng = click$lng, lat = click$lat)
    updateNumericInput(session, "latitude", value = click$lat)
    updateNumericInput(session, "longitude", value = click$lng)
  })
  

})
