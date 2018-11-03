#' Uses cross validation with a list of functions
#' 
#' Commodity function to facilitate the usage of the bonsai.cv function, may be depriciated
#' @param y Univariate time series
#' @param list_functions a list of functions to test
#' @param h Forecast horizon
#' @param window Length of the rolling window, if NULL, a rolling window will not be used.
#' @param Start how many observations before starting to calculate errors
#' @param max_fold how many times should the function calculate errors
#' @param min_lenght how many observations are nescessary before starting to calculate errors 
#' @details Let y contain the time series y[1:T]. Then forecastfunction is applied successively to the time series y[1:t], for t=1,…,T-h, making predictions f[t+h]. The errors are given by e[t+h] = y[t+h]-f[t+h]. If h=1, these are returned as a vector, e[1:T]. For h>1, they are returned as a matrix with the hth column containing errors for forecast horizon h. The first few errors may be missing as it may not be possible to apply forecastfunction to very short time series.
#' @return Numerical time series object containing the forecast errors as a vector (if h=1) and a matrix otherwise. The time index corresponds to the last period of the training data. The columns correspond to the forecast horizons.
#' @return A matrix with the weighted object
#' @author Rob Hyndman created the tsCV {forecast}, heavily based on it
#' @export 
bonsai_cv_models <- function(
  y,
  list_functions,
  h,
  window = NULL,
  Start = 1,
  min_lenght = 0,
  max_fold = NULL
){
  number_functions <- length(list_functions)
  names <- vector("character", number_functions)
  matrix_errors <- vector("list", number_functions)
  for (i in seq_len(number_functions)) {
    names[i] <- names(list_functions[i])
    matrix_errors[[i]] <- bonsai_cv(
      y = y,
      forecast_function = list_functions[[i]],
      h = h,
      window = window,
      Start = Start,
      min_lenght = min_lenght,
      max_fold = max_fold
    )
  }
  names(matrix_errors) <- names
  return(matrix_errors)
}