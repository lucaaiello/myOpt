#' linear_sd_optim
#'
#' This function provides a personalized efficient way for optimizing functions
#'
#' @param par Initial values.
#' @param X Data predictors.
#' @param Y Response variables.
#' @param tolerance Value for the stopping criterion. The default value is set to 1e-6.
#' @param maxit Maximum iterations allowed. The default value is set to 10000.
#' @param verbose If set TRUE the function produce messages during the computation.
#'
#' @return Vector containing the estimated parameters.
#' @export
#'

linear_sd_optim <- function(par, X, Y,
                            tolerance=1e-6, maxit=10000,
                            verbose=T){

  I <- dim(X)[2] # numbers of predictors (including the intercept)

  it <- 1  # iteration index initialization
  err <- 1 # error initialization
  stepsize <- 1 # step initialization

  par_new <- vector(mode = "numeric", length = I) # vector for the update in the cicle

  while (err > tolerance & it < maxit) {

    grad <- my_grad(par, X, Y) # gradient computation with the current values of the parameters

    hess <- my_hess(X) # Hessian computation with the current values of the parameters

    stepsize <- sqrt(sum(grad^2)) / (t(grad) %*% hess %*% grad)   # update of the step

    par_new <- par - stepsize * grad # update of the parameters

    err <- max(abs(par_new - par)) # computation of the error
    it <- it + 1                   # updating the iteration index

    par <- par_new # assigning the new value to the old one as to begin a possible new iteration
    names(par) <- c('(Intercept)', paste('x',1:(dim(X)[2]-1),sep=""))

  }

  return(par_new)

}
