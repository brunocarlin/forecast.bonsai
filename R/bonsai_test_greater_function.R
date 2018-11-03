#' Calculate the weights of all models and choose the selected ones based on a function
#' 
#' Checks all the weights using a function to determine a value usually mean.
#' @param matrix_weights A matrix of weights
#' @param function_vector a function to apply in the vector of weights
#' @param if_all_bellow a choice to avoid the case where all weights = 0
#' @param ... arguments to be passed to the function
#' @return A list with all of the weights
#' @export
bonsai_test_greater_function <- function(matrix_weights,
                                         function_vector,
                                         if_all_bellow = FALSE,
                                         ...) {
  number_models <- length(matrix_weights[, 1])
  number_predictions   <- length(matrix_weights[1, ])
  for (i in seq_len(number_predictions)) {
    vector_weights <- matrix_weights[, i]
    value <- function_vector(vector_weights)
    for (j in  seq_len(number_models)) {
      if (value > vector_weights[[j]]) {
        vector_weights[[j]] <- 0
      }
      if (mean(vector_weights) != 0) {
      } else {
        if (if_all_bellow == TRUE) {
          for (j in  seq_len(number_models)) {
            vector_weights[[j]] <- 1 / number_models
          }
        }
      }
      matrix_weights[, i] <- vector_weights
    }
  }
  return(matrix_weights)
}
