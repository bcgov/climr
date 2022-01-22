setClass("basepoints", slots = c(
    latitude = "numeric",
    longitude = "numeric",
    elevation = "numeric",
    historical = "character",
    future = "character"))

setGeneric("downscale", function(latitude, longitude, elevation, historical, future) {latitude})

method.skeleton("downscale", "basepoint")

setMethod("downscale", signature = c(latitude = "numeric", longitude = "numeric", 
                                     elevation = "numeric", historical = "character", future = "character"), 
          function(latitude, longitude, elevation, historical, future){latitude})