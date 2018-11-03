#' Calculate the weights of all models and choose the selected ones
#' 
#' Checks all the weights for values greather than min_value.
#' @param matrix_weights A matrix of weights
#' @param min_value a minimun weight to use
#' @param if_all_bellow a choice to avoid the case where all weights = 0
#' @return A list with all of the weights
#' @export
bonsai_test_greater_value <- function(matrix_weights,
                                      min_value,
                                      if_all_bellow = FALSE) {
  number_models <- length(matrix_weights[, 1])
  number_predictions   <- length(matrix_weights[1, ])
  for (i in seq_len(number_predictions)) {
    vector_weights <- matrix_weights[, i]
    for (j in  seq_len(number_models)) {
      if (min_value > vector_weights[[j]]) {
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