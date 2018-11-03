bonsai_cv_models <- function(
  y,
  list_functions,
  h,
  window = NULL,
  Start = 1,
  min_lenght = 0,
  max_fold = NULL
){
  number_functions <- length(list_functions)
  names <- vector("character", number_functions)
  matrix_errors <- vector("list", number_functions)
  for (i in seq_len(number_functions)) {
    names[i] <- names(list_functions[i])
    matrix_errors[[i]] <- bonsai_cv(
      y = y,
      forecast_function = list_functions[[i]],
      h = h,
      window = window,
      Start = Start,
      min_lenght = min_lenght,
      max_fold = max_fold
    )
  }
  names(matrix_errors) <- names
  return(matrix_errors)
}
