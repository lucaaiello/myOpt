#' Hessian computation
#'
#' This function provides a vectorized version of the Hessian computation
#'
#' @param X Data predictors.
#'
#' @return Vector containing the Hessian scores.
#' @export
#'

my_hess <- function(X, verbose = T){

  hess <- 4 * t(X) %*% X

  return(hess)

}
