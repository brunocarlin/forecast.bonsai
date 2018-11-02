#' Chosen accuracy measure
#' 
#' Calculates the chosen accuracy measure
#' @param y A time series
#' @param error the error created by some cross-validation method
#' @return A ? with all of the forecast means
#' @export 
bonsai_accuracy_measure <- function(y, error, error_function) {
  measure <- rbind(
    choice = error_function(y, error)
  )
  colnames(measure) <- "Averaged_Time"
  return(measure)
}
