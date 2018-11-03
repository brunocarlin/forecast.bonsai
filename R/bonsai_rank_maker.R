#' Calculate the ranks of all models and choose the selected ones
#' 
#' Calculates the position in a rank and assings 0 weight to all the losers.
#' @param matrix_weights A matrix of weights
#' @param position_winner The forecast model to be choosen
#' @return A list with all of the weights
#' @export
bonsai_rank_maker <- function(matrix_weights, position_winner) {
  number_predictions <- length(matrix_weights[1, ])
  number_models <- length(matrix_weights[, 1])
  if (number_models > position_winner) {
    for (i in seq_len(number_predictions )) {
      vector_weights <- matrix_weights[, i]
      ord <- order(vector_weights, decreasing = T)
      vector_weights <- vector_weights[order(vector_weights, decreasing = TRUE)]
      vector_weights[(position_winner + 1 ):number_models] <- 0
      vector_weights <- vector_weights[order(ord)]
      matrix_weights[, i] <- vector_weights
    }
  }
  return(matrix_weights)
}