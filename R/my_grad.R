#' Gradient computation
#'
#' This function provides a vectorized version of the gradient computation
#'
#' @param par Parameters values.
#' @param X Data predictiors.
#' @param Y Response variables.
#'
#' @return Vector containing the gradient scores.
#'
#' @examples
#'
#' # Having the parameters vector "par", the predictors matrix "X",
#' # and the outcome vector "Y", the standard usage for computing the
#' # descend gradient direction is the following:
#'
#' my_grad(par, X, Y)
#'
#' @export
#'

my_grad <- function(par, X, Y, verbose = T){

  N <- length(Y)       # number of observations
  I <- dim(X)[2]       # number of predictors

  grad <- vector(mode = "numeric", length = I)

  grad <- 2/N * t(X) %*% (X%*%par - Y)

  return(as.numeric(grad))

}
