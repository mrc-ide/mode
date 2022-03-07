test_that("can integrate logistic", {
  y0 <- c(1, 1)
  r <- c(0.1, 0.2)
  K <- c(100, 100)
  end_time <- 25

  expected <- logistic_analytic(r, K, end_time, y0)
  actual <- logistic_dopri(r, K, end_time, y0)

  expect_equal(expected, actual, tolerance = 1e-7)
})

test_that("integration agrees with dde", {
  y0 <- c(1, 1)
  r <- c(0.1, 0.2)
  K <- c(100, 100)
  end_time <- 25

  expected <- logistic_dde(r, K, end_time, y0)
  actual <- logistic_dopri(r, K, end_time, y0)

  expect_equal(expected, actual, tolerance = 1e-7)
})
