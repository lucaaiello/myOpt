#' Hessian computation
#'
#' This function performs the Hessian computation.
#'
#' @param X Data predictors.
#' @param verbose If set TRUE the function produce messages during the computation.
#'
#' @return Returns the matrix containing the Hessian scores.
#' @export
#'

my_hess <- function(X, verbose = T){

  hess <- 4 * t(X) %*% X

  return(hess)

}
