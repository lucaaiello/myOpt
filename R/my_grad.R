#' linear_gd_optim
#'
#' This function provides a personalized way for optimizing functions
#'
#' @param par Parameters values.
#' @param X Data predictiors.
#' @param Y Response variables.
#'
#' @return Vector containing the gradient scores.
#' @export
#'

my_grad <- function(par, X, Y, verbose = T){

  N <- length(Y)       # number of observations
  I <- dim(X)[2]       # number of predictors

  grad <- rep(0,I)     # initialization of gradient vector

  for (i in 1:I) {     # computation of the gradient component for component

    grad[i] <- 2/N * (X%*%par - Y) %*% X[,i]
    # for (n in 1:N) {
    #
    #   grad[i] <- grad[i] + 2/N * (X[n,]%*%par - Y[n]) * X[n,i]
    #
    # }
  }

  return(grad)

}
