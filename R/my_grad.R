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
#' @example
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

  grad <- rep(0,I)     # initialization of gradient vector

  grad <- 2/N * t(X) %*% (X%*%par - Y)

  # for (i in 1:I) {     # computation of the gradient component for component
  #
  #   grad[i] <- 2/N * t(X%*%par - Y) %*% X[,i]
  #
  # }

  return(grad)

}
