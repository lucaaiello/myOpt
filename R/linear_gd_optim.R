#' linear_gd_optim
#'
#' This function provides a personalized way for optimizing functions
#'
#' @param par Initial values.
#' @param X Data predictiors.
#' @param Y Response variables.
#' @param tolerance Value for the stopping criterion. The default value is set to 1e-6.
#' @param maxit Maximum iterations allowed. The default value is set to 10000.
#' @param stepsize Length of the stepsize parameter. The default value is set to 1e-3.
#' @param verbose If set TRUE the function produce messages during the computation.
#'
#' @return Vector containing the estimated parameters.
#' @export
#'


linear_gd_optim <- function(par, X, Y,
                            tolerance=1e-6, maxit=10000, stepsize=1e-3,
                            verbose=T){

  I <- dim(X)[2] # numbers of predictors (including the intercept)

  it <- 1 # iteration index iniziation
  err <- 1 # error iniziation

  upd <- vector(mode = "numeric", length = I)# rep(0,I)

  while (err > tolerance & it < maxit) {

    grad <- my_grad(par, X, Y)

    for (i in 1:I) {

      upd[i] <- par[i] - stepsize * grad[i]

    }

    err <- max(abs(upd - par))
    it <- it + 1

    par <- upd

  }

  return(upd)

}
