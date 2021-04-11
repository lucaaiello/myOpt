# myOpt

<!-- badges: start -->
<!-- badges: end -->

The goal of myOpt is to provide users an efficient way to estimate parameters of a linear model through optimization techniques.

## Installation

You can install the released version of myOpt from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("myOpt")
```

## Example

This is a basic example which shows you how to solve the problem of parameters estimation in linear models:

``` r
library(myOpt)
## basic example code

set.seed(8675309)

# data simulation for example purposes

n = 1000

x1 = rnorm(n)
x2 = rnorm(n)
X <- cbind(rep(1,n),x1,x2)

Y = 1 + 0.5*x1 + 0.2*x2 + rnorm(n)

# random initial values (i.e. starting point) for the parameters of the linear model

par <- rnorm(3)

# these functions, which are the main part of this package, return a vector 
# containing the values of the estimated parameters

# gradient descend
est_par_gd <- linear_gd_optim(par, X, Y)

# steepest descend
est_par_sd <- linear_sd_optim(par, X, Y)

# after the estimation of the parameters, for both method it can be applied a
# prediction function, that predicts the outcome given the estimated parameters
# vector of the model and a predictors data matrix

# prediction with the parameters estimated through the gradient descend method
prediction_gd <- my_linear_predict(est_par_gd, X)

# prediction with the parameters estimated through the steepest descend method
prediction_sd <- my_linear_predict(est_par_sd, X)

# after the computation of the predictions this package allows also to compute
# the error associated with the prediction

# prediction error associated to the gradient descend estimated parameters
my_pred_error(Y, prediction_gd)

# prediction error associated to the steepest descend estimated parameters
my_pred_error(Y, prediction_sd)

# k-fold cross validation 

library(doSNOW) # inside the functions there are parallelized procedures

K <- 5
set.seed(8675309)
my_k_fold_cv(par, X, Y, K)
set.seed(8675309)
my_k_fold_cv(par, X, Y, K, method = "sd")


```

