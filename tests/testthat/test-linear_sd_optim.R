test_that("my steepest descent method works", {

  set.seed(8675309)
  n <- 1000
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  Y <- 1 + 0.5*x1 + 0.2*x2 + rnorm(n)
  X <- cbind(rep(1,n),x1,x2)
  par <- rnorm(dim(X)[2])

  my_result <- linear_sd_optim(par, X, Y)

  exp_result <- lm(Y ~ x1 + x2)$coefficients

  expect_equal(my_result,
               exp_result,
               tolerance = 0.01)

})


test_that("my steepest descent method works also using 3 predictors", {

  set.seed(8675309)
  n <- 1000
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  x3 <- rnorm(n)
  Y <- 1 + 0.5*x1 + 0.2*x2 + 0.4*x3 + rnorm(n)
  X <- cbind(rep(1,n),x1,x2,x3)
  par <- rnorm(dim(X)[2])

  my_result <- linear_sd_optim(par, X, Y)

  exp_result <- lm(Y ~ x1 + x2 + x3)$coefficients

  expect_equal(my_result,
               exp_result,
               tolerance = 0.01)

})
