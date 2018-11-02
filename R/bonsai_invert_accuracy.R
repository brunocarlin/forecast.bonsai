#' Inverts a list of accuracy measures
#' 
#' Inverts a list of accuracy measures to estimate weights, also deals with 0s
#' @param accuracy A list of accuracy measures
#' @param if_zero What value to assingn when a perfect forecast happens
#' @return A ? with all inverted accuracy`s
#' @export 
bonsai_invert_accuracy <- function(accuracy, if_zero = 100000L) {
  ifelse(accuracy != 0, 1 / accuracy, if_zero)
}