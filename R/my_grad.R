#' Gradient computation
#'
#' This function provides a vectorized version of the gradient computation
#'
#' @param par Parameters values.
#' @param X Data predictors.
#' @param Y Response variables.
#'
#' @return Returns the vector containing the gradient scores.
#'
#' @examples
#'
#' # Having the parameters vector "par", the predictors matrix "X",
#' # and the outcome vector "Y",
#' # the gradient is calculated as follows:
#'
#' set.seed(8675309)
#'
#' # data simulation for example purposes
#'
#' n = 1000
#'
#' x1 = rnorm(n)
#' x2 = rnorm(n)
#' X <- cbind(rep(1,n),x1,x2)
#'
#' Y = 1 + 0.5*x1 + 0.2*x2 + rnorm(n)
#'
#' # random initial values for the parameters of the linear model
#'
#' par <- rnorm(dim(X)[2])
#'
#' # gradient calculation
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
