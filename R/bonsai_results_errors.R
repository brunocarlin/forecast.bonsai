#' Calculates the mase and smape errors
#' 
#' uses the weighted forecast generated beforehand to calculate error measures
#' @param matrix_weights a matrix of weights
#' @param object_to_weight the object that will get averaged weighted 
#' @return A matrix with the weighted object
#' @export 
bonsai_results_errors <- function(forecasts, y, test_ts) {
  error <- -sweep(forecasts, 2, test_ts)
  pcerror <- (200 * abs(error) / sweep(abs(forecasts),
                                       2,
                                       abs(test_ts),
                                       FUN = "+"))
  scalederror <- (abs(error) / mean(abs(diff(y,
                                             lag = frequency(y)))))
  errors <- rbind(Symmetric_Errors = pcerror,
                  Scaled_Errors = scalederror)
  rownames(errors) <- c("symmetric_errors", "scaled_errors")
  return(errors)
}
