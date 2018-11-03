#' Create error matrix
#' 
#' Creates the error matrix instead of an weight matrix, usefull for some methods
#' @param list_accuracy a list with all the accuracy measures
#' @return A list with all the errors
#' @export
bonsai_error_matrix <- function(list_accuracy) {
  row_names <- names(list_accuracy)
  col_names <- colnames(list_accuracy[[1]])
  list_names <- rownames(list_accuracy[[1]])
  number_models <- length(list_accuracy)
  number_errors <- length(list_accuracy[[1]][, 1])
  number_predictions <- length( list_accuracy[[1]][1, ])
  error_matrix <- vector("list", number_errors)
  temporay_matrix <- matrix(0,
                            nrow = number_models,
                            ncol = number_predictions)
  rownames(temporay_matrix) <- row_names
  colnames(temporay_matrix) <- col_names
  for (i in seq_len(number_errors)) {
    for (j in seq_len(number_models)) {
      temporay_matrix[j, ] <- list_accuracy[[j]][i, 1:number_predictions]
    }
    error_matrix[[i]] <- temporay_matrix
  }
  names(error_matrix) <- list_names
  return(error_matrix)
}