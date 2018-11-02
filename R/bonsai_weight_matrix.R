#' Create weight matrix
#' 
#' Creates the weight matrix responsible for all the decisions made by the bonsain
#' @param list_accuracy a list with all the accuracy measures
#' @return A list with all the weights
#' @export 
bonsai_weight_matrix <- function(list_accuracy) {
  row_names <- names(list_accuracy)
  col_names <- colnames(list_accuracy[[1]])
  list_names <- rownames(list_accuracy[[1]])
  number_models <- length(list_accuracy)
  number_errors <- length(list_accuracy[[1]][, 1])
  number_predictions <- length(list_accuracy[[1]][1, ])
  weight_matrix <- vector("list", number_errors)
  temporary_matrix <-
    matrix(0, nrow = number_models, ncol = number_predictions)
  rownames(temporary_matrix) <- row_names
  colnames(temporary_matrix) <- col_names
  for (i in seq_len(number_errors)){
    for (j in seq_len(number_models)) {
      temporary_matrix[j, ] <- list_accuracy[[j]][i, 1:number_predictions]
    }
    temporary_matrix <- sweep(temporary_matrix,
                              2,
                              colSums(temporary_matrix),
                              FUN = "/")
    weight_matrix[[i]] <- temporary_matrix
  }
  names(weight_matrix) <- list_names
  return(weight_matrix)
}
