#' Prediction of a linear model
#'
#' @param X Data matrix predictiors.
#' @param par Parameters vector to be used in the prediction.
#'
#' @return Returns the vector containing the predictions computed for the data contained in X
#' @export
#'
#' @examples
#'
#'
my_linear_predict <- function(par, X){

  return(as.numeric(X%*%par))

}
