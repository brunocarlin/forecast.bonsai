#' Executes all forecasts in a list of functions
#' 
#' Takes in any value and squares
#' @param y A time series to be forecasted
#' @param list_functions A list of functions to apply
#' @param h the horizon to forecast
#' @return A list with all of the forecast means
#' @export 
bonsai_forecast_saver <- function(y, list_functions, h) {
  number_functions <- length(list_functions)
  Names <- vector("character", number_functions)
  forecast_object <- vector("list", number_functions)
  for (i in seq_len(number_functions)) {
    forecast_object[[i]] <- list_functions[[i]](y, h)
    Names[i] <- names(list_functions[i])
  }
  names(forecast_object) <- Names
  return(forecast_object)
}