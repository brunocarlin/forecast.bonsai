#' Time series cross-validation with extra parameters
#' 
#' bonsai_cv computes the forecast errors obtained by applying forecast_function to subsets of the time series y using a diverse set of rules.

#' @param y Univariate time series
#' @param forecast_function Function to return an object of class forecast. Its first argument must be a univariate time series, and it must have an argument h for the forecast horizon.
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
bonsai_cv <- function(y,
                      forecast_function,
                      h = 1,
                      window = NULL,
                      Start= 1,
                      max_fold = NULL,
                      min_lenght = 0,
                      ...) {
  y <- as.ts(y)
  n <- length(y)
  e <- ts(matrix(NA_real_, nrow = n, ncol = h))
  tsp(e) <- tsp(y)
  first_fold <-
    ifelse(is.null(max_fold),
           n %% h,
           ifelse(n > h * max_fold, n - h * max_fold, n %% h))
  for (i in seq(first_fold, n - 1, h)) {
    fc <- try(suppressWarnings(forecast_function(subset(
      y,
      start = ifelse(
        i - Start >= 0L &
          i - min_lenght >= 0L,
            ifelse(is.null(window), 1L, ifelse(i - window >= 0L,
              i - window + 1L, stop("small window"))),
        stop("Too Short")
      ),
      end = i
    ), h = h, ...)), silent = TRUE
    )
    if (!is.element("try-error", class(fc))) {
      e[i, ] <- y[i + (1:h)] - fc$mean
    }
  }
  if (h == 1) {
    return(e[, 1L])
  }
  else {
    colnames(e) <- paste("h=", 1:h, sep = "")
    return(e)
  }
}
