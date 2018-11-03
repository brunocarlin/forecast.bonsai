upper_limt <- function(x) {
  Q1 <- quantile(x, probs = 0.25)
  Q3 <- quantile(x, probs = 0.75)
  IQR <- Q3 - Q1
  results <- Q3 + 1.5 * IQR
  return(results)
}

lower_limit <-  function(x) {
  Q1 <- quantile(x, probs = 0.25)
  Q3 <- quantile(x, probs = 0.75)
  IQR <- Q3 - Q1
  results <- Q1 - 1.5 * IQR
  return(results)
}

Diff <- function(x) diff(c(head(x, 1), x))

#' Applis the islands method
#' 
#' chooses the best forecasts based on the Islands method
#' @param list_accuracy a list with all the accuracy measures
#' @param matrix_errors A matrix of weights
#' @param function_ordered_vector a function to apply in the ordered vector
#' @return A list with all the errors
#' @export

bonsai_islands <- function(matrix_errors, function_ordered_vector) {
  number_predictions <- length(matrix_errors[1, ])
  number_models <- length(matrix_errors[, 1])
  for (i in seq_len(number_predictions )) {
    vector_errors <- matrix_errors[, i]
    outlier_dected <- 0
    ord <- order(vector_errors, decreasing = FALSE)
    vector_errors <- vector_errors[order(vector_errors, decreasing = FALSE)]
    for (j in seq_len(number_models)) {
      considered_vector <-  Diff(head(vector_errors, j))
      if (outlier_dected != 1) {
        if (function_ordered_vector(considered_vector) <
            tail(considered_vector, 1)) {
          outlier_dected <- 1
          vector_errors[[j]] <- Inf
        }
      } else{
        vector_errors[[j]] <- Inf
      }
    }
    vector_errors <- vector_errors[order(ord)]
    matrix_errors[, i] <- vector_errors
  }
  return(matrix_errors)
}
