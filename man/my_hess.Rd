% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_hess.R
\name{my_hess}
\alias{my_hess}
\title{Hessian computation}
\usage{
my_hess(X, verbose = T)
}
\arguments{
\item{X}{Data predictors.}

\item{verbose}{If set TRUE the function produce messages during the computation.}
}
\value{
Returns the matrix containing the Hessian scores.
}
\description{
This function performs the Hessian computation.
}
\examples{

# Having the predictors matrix "X",
# the Hessian is calculated as follows:

set.seed(8675309)

# data simulation for example purposes

n = 1000

x1 = rnorm(n)
x2 = rnorm(n)
X <- cbind(rep(1,n),x1,x2)

# Hessian calculation

my_hess(X)

}
