#' Calculates weighted object
#' 
#' uses the weights generated beforehand to calculte a new object, usually used for weighted forecasts or residuals
#' @param matrix_weights a matrix of weights
#' @param object_to_weight the object that will get averaged weighted 
#' @return A matrix with the weighted object
#' @export 
#' 
bonsai_results <- function(matrix_weights, object_to_weight) {
  if (ncol(matrix_weights) > 1) {
    results <- diag(t(matrix_weights) %*% object_to_weight, names = FALSE)
  } else {
    results <-  t(matrix_weights) %*% object_to_weight
  }
  results <- 1 %*% results
  colnames(results) <- colnames(object_to_weight)
  rownames(results) <- "Results"
  return(results)
}