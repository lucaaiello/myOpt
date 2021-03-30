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

This is a basic example which shows you how to solve a common problem:

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

# random initial values for the parameters of the linear model

par <- rnorm(3)

# the function returns a vector containing the values of the estimated parameters

est_par <- linear_gd_optim(par, X, Y)


```

