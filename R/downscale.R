setClass("basepoints", slots = c(
    latitude = "numeric",
    longitude = "numeric",
    elevation = "numeric",
    historical = "character",
    future = "character"))

setGeneric("downscale", function(x) {x})

method.skeleton("downscale", "basepoint")

setMethod("downscale", signature = c(numeric(), numeric(), numeric(), character(), character()))
downscale <- function(latitude, longitude, elevation, )