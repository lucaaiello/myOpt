test_that("my gradient method works", {

  n <- 1000
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  Y <- 1 + 0.5*x1 + 0.2*x2 + rnorm(n)
  X <- cbind(rep(1,n),x1,x2)
  par <- rnorm(3)

  my_result <- linear_gd_optim(par, X, Y)

  exp_result <- lm(Y ~ x1 + x2)$coefficients

  expect_equal(my_result,
               exp_result,
               tolerance = 0.001)

})
