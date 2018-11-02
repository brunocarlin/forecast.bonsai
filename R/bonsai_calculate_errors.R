#' Creates a list with all specified errors measures
#' 
#' Takes in a list of errors and return a list of error measuares
#' @param y A time series to be forecasted
#' @param list_errors A list of errors to apply
#' @param error_function a function with all errors measures
#' @return A list with all of the calculated error measures
#' @export 
bonsai_calculate_errors <- function(y, list_errors, error_function) {
  number_models <- length(list_errors)
  error_measures <- vector("list", number_models)
  for (i in seq_len(number_models)) {
    error_measures[[i]] <- error_function(y, list_errors[[i]])
  }
  names(error_measures) <- names(list_errors)
  return(list(Mean_Error_Metrics = error_measures))
}