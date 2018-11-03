#' Tests all the ranks
#' 
#' Uses the rank maker function to try all possible combinations.
#' @param matrix_weights A matrix of weights
#' @return A list with all of the weights
#' @export
bonsai_test_ranks <- function(matrix_weights) {
  number_models <- length(matrix_weights[[1]][[1]][, 1])
  weight_of_all_ranks <- vector("list", number_models)
  for (i in seq_len(number_models)) {
    weight_of_all_ranks[[i]] <- lapply(matrix_weights,
                                       lapply,
                                       bonsai_rank_maker,
                                       i)
  }
  names(weight_of_all_ranks) <- paste(1:number_models,
                                      "_Selected_Models",
                                      sep = "")
  return(weight_of_all_ranks)
}