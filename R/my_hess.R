#' Hessian computation
#'
#' This function performs the Hessian computation.
#'
#' @param X Data predictors.
#' @param verbose If set TRUE the function produce messages during the computation.
#'
#' @return Returns the matrix containing the Hessian scores.
#' @examples
#'
#' # Having the predictors matrix "X",
#' # the Hessian is calculated as follows:
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
#' # Hessian calculation
#'
#' my_hess(X)
#'
#' @export
#'

my_hess <- function(X, verbose = T){

  hess <- 4 * t(X) %*% X

  return(hess)

}
