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

my_grad <- function(par, X, Y, verbose = TRUE){

  N <- length(Y)
  I <- dim(X)[2]
  grad <- rep(0,I)

  for (i in 1:I) {
    for (n in 1:N) {
      grad[i] <- grad[i] + 2/N * (Y[n] - X[n,]%*%par) * (-X[n,i])
    }
  }

  return(grad)

}
