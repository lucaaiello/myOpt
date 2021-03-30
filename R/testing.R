rm(list = ls())

library(myOpt)

set.seed(8675309)
n = 1000
x1 = rnorm(n)
x2 = rnorm(n)
Y = 1 + 0.5*x1 + 0.2*x2 + rnorm(n)

X <- cbind(rep(1,n),x1,x2)

par <- rnorm(3)


est_par <- linear_gd_optim(par, X, Y)


exp_par <- lm(Y ~ x1 + x2)
