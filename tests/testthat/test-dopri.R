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
  n_particles <- 5
  mod <- gen$new(pars, 0, n_particles)

  actual <- vapply(times, function(t) mod$solve(t), numeric(length(y0) * n_particles))
  expect_equal(actual[1:2,], analytic, tolerance = 1e-7)
  expect_equal(actual[1:2,], dde, tolerance = 1e-7)
  expect_equal(mod$time(), 25)

  # all particles should have the same values
  y0 <- actual[c(TRUE, FALSE),]
  y1 <- actual[c(FALSE, TRUE),]
  expect_equal(nrow(unique(y0)), 1)
  expect_equal(nrow(unique(y1)), 1)
})
