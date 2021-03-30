---
output:
  pdf_document: default
  html_document: default
---

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
n = 1000
x1 = rnorm(n)
x2 = rnorm(n)
Y = 1 + 0.5*x1 + 0.2*x2 + rnorm(n)

X <- cbind(rep(1,n),x1,x2)

par <- rnorm(3)

est_par <- linear_gd_optim(par, X, Y)
names(est_par) <- c("(Intercept)", "x1", "x2")

```

