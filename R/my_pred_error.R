#' Title
#'
#' @param Y Response vector.
#' @param Y_pred Prediction of the response vector.
#'
#' @return the L2 norm of the difference between observations and predictions.
#' @export
#'
my_pred_error <- function(Y, Y_pred){
  err <- sqrt(sum((Y-Y_pred)^2))
}
