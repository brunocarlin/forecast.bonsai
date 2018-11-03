#' Equalises "surviving" weights"
#' 
#' Turn all non-zero weigths into the same
#' @param matrix_weights A matrix of weights
#' @return A list with all the adjusted weights
#' @export
bonsai_weights_equalise <- function(matrix_weights) {
  number_predictions <- length(matrix_weights[1, ])
  number_models   <- length(matrix_weights[, 1])
  for (i in seq_len(number_predictions)) {
    vector_weights <- matrix_weights[, i]
    for (j in  seq_len(number_models)) {
      if (vector_weights[[j]] > 0) {
        vector_weights[[j]] <- 1L
      }
    }
    matrix_weights[, i] <- vector_weights
  }
  matrix_weights <- sweep(matrix_weights, 2, colSums(matrix_weights), FUN = "/")
  return(matrix_weights)
}

#' Calculates all the "surviving"  weights
#' 
#' Turn all non-zero weigths into the an weighted average
#' @param matrix_weights A matrix of weights
#' @return A list with all the adjusted weights
#' @export
bonsai_weights_differentiate <- function(matrix_weights) {
  matrix_weights <- sweep(matrix_weights, 2, colSums(matrix_weights), FUN = "/")
  return(matrix_weights)
}
