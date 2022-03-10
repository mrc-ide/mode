test_that("can integrate logistic", {
  y0 <- c(1, 1)
  r <- c(0.1, 0.2)
  K <- c(100, 100)
  times <- 0:25

  analytic <- logistic_analytic(r, K, times, y0)
  dde <- logistic_dde(r, K, times, y0)

  actual <- logistic_mode(r, K, times, y0)
  expect_equal(actual, analytic, tolerance = 1e-7)
  expect_equal(actual, dde, tolerance = 1e-7)
})
