#' linear_sd_optim
#'
#' This function is used to fit linear models, applying the steepest descent method.
#'
#' @param par Initial values.
#' @param X Data matrix predictors (first column has to contain values 1 to consider the intercept).
#' @param Y Response vector.
#' @param tolerance Value for the stopping criterion. The default value is set to 1e-6.
#' @param maxit Maximum iterations allowed. The default value is set to 15000.
#' @param verbose If set TRUE the function produce messages during the computation.
#'
#' @return Returns the vector containing the estimated parameters.
#'
#' @examples
#' library(myOpt)
#' ## basic example code
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
# the function returns a vector containing the values of the estimated parameters
#'
#' est_par <- linear_sd_optim(par, X, Y)
#'
#' @export
#'

linear_sd_optim <- function(par, X, Y,
                            tolerance=1e-6, maxit=15000,
                            verbose=T){

  I <- dim(X)[2] # numbers of predictors (including the intercept)

  it <- 1  # iteration index initialization
  err <- 1 # error initialization
  stepsize <- 1 # step initialization

  par_new <- vector(mode = "numeric", length = I) # vector for the update in the loop

  while (err > tolerance & it < maxit) {

    grad <- my_grad(par, X, Y) # gradient computation with the current values of the parameters

    hess <- my_hess(X) # Hessian computation with the current values of the parameters

    stepsize <- as.numeric(sum(grad^2) / (t(grad) %*% hess %*% grad))   # update of the step

    par_new <- par - stepsize * grad # update of the parameters

    err <- max(abs(par_new - par)) # computation of the error
    it <- it + 1                   # updating the iteration index

    par <- par_new # assigning the new value to the old one as to begin a possible new iteration

  }

  names(par_new) <- c('(Intercept)', paste('x',1:(dim(X)[2]-1),sep=""))

  return(par_new)

}
