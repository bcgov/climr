setMethod("downscale",
    signature(
      latitude = "numeric",
      longitude = "numeric",
      elevation = "numeric",
      historical = "character",
      future = "character"
    ),
    function (latitude, longitude, elevation, historical, future) 
    {
      #Validates parameters
    
    }
)

available_historical_periods <- function() {

}   

available_future_models <- function() {
    #TODO use system.file 
  sort(unique(gsub("^[^.]+\\.([^.]+).*$", "\\1", dir("./inputs/gcmData/"))))
}