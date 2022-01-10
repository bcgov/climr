#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  titlePanel("ClimateNA Map"),
  sidebarLayout(
    sidebarPanel(width = 5,
                   fluidRow(
                     column(width = 6,
                            numericInput(inputId = "latitude", label = "Latitude", min = -90, max = 90, value = 48.98)
                     ),
                     column(width = 6,
                            numericInput(inputId = "longitude", label = "Longitude", min = -180, max = 180, value = -115.02)
                     )
                   ),
                   fluidRow(
                     column(width = 6,
                            numericInput(inputId = "elevation", label = "Elevation (m)", min = -430, max = 8848, value = 1000)
                     ),
                     column(width = 6,
                            selectInput(inputId = "historical", label = "Historical", choices = c("Normal_1961_1990", "Normal_1901_1930"))
                     )
                   ),
                   fluidRow(
                     column(width = 12,
                            selectInput(inputId = "future", label = "Future", choices = c("Select a GCM period", "13GCMs_ensemble_ssp126_2011-2040.gcm"))
                     )
                   ),
                   br(),
                   fluidRow(
                     column(width = 4,
                            actionButton(inputId = "tutorial", label = "Quick Tutorial",  width = "80%")
                     ),
                     column(width = 4,
                            actionButton(inputId = "help", label = "Help", width = "80%")
                     ),
                     column(width = 4,
                            actionButton(inputId = "calculate", label = "Calculate",  width = "80%")
                     )
                   ),
                   br(),
                   br(),
                   br(),
                   fluidRow(
                     column(width = 12,
                            DTOutput(outputId = "result_dt")
                     )
                   ),
                   br(),
                   fluidRow(
                     column(width = 1,
                            actionButton(inputId = "save", label = "Save")
                     ),
                     column(width = 1,
                            actionButton(inputId = "clear", label = "Clear")
                     )
                   )
              ),
    mainPanel("main panel", width = 7,
              plotOutput(outputId = "map", height = "940px"))
  )
))
