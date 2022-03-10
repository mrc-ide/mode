test_that("can integrate logistic", {
  y0 <- c(1, 1)
  r <- c(0.1, 0.2)
  K <- c(100, 100)
  times <- 0:25

  analytic <- logistic_analytic(r, K, times, y0)
  dde <- logistic_dde(r, K, times, y0)

  path <- mode_file("examples/logistic.cpp")
  gen <- mode(path, quiet = TRUE)
  pars <- list(r1 = r[[1]], r2 = r[[2]], K1 = K[[1]], K2 = K[[2]])
  mod <- gen$new(pars, 0)

  actual <- vapply(times, function(t) mod$solve(t), numeric(length(y0)))
  expect_equal(actual, analytic, tolerance = 1e-7)
  expect_equal(actual, dde, tolerance = 1e-7)
  expect_equal(mod$time(), 25)
})
