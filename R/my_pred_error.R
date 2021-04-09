#' Title
#'
#' @param Y Response vector.
#' @param Y_pred Prediction of the response vector.
#'
#' @return Returns the mean squared error of the prediction.
#' @export
#'
my_pred_error <- function(Y, Y_pred){

  err <- mean((Y-Y_pred)^2)

  return(err)

}
